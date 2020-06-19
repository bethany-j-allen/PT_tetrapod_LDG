#Bethany Allen   5th February 2020
#Code to bootstrap for even collection numbers between time bins on tetrapod data (from a csv) with species level IDs

#setwd("#####")

#Load packages
library(tidyverse)

#Read in dataset
tetrapods <- read_csv("data/tetrapod_database_ScotesePLL.csv")
glimpse(tetrapods)

#Remove synonymy repeats (combinations of the same collection no. AND accepted name)
tetrapods <- distinct(tetrapods, accepted_name, collection_no, .keep_all = T)

#Trim data without stage assignment
tetrapods_trim <- tetrapods %>% filter(!is.na(stage_assignment))

#Add column which indicates latitude bin
bins <- seq(from = -90, to = 90, by = 20)
labels <- seq(from = -80, to = 80, by = 20)
tetrapods_trim <- mutate(tetrapods_trim, paleolat_code = cut(Latitude, breaks = bins, labels = labels))

#Filter as desired
tetrapods_trim <- tetrapods_trim %>% filter(pres_mode == "body") %>% 
  filter(taxon_environment == "marine") %>% 
  filter(stage_assignment == "Induan" | stage_assignment == "Olenekian")


collections <- unique(tetrapods_trim$collection_no)

#Create frames to store sampled collection numbers and the matching curve for each iteration
sample_list <- data.frame()
gradients <- data.frame()

#Indicate number of bootstraps and reps required -> 250 for terrestrial, 30 for marine, 100 reps of each
bootstraps <- 30
reps <- 100

for (x in 1:reps){

  print(x)
  
  #Create subsampled bootstrap of 100 occurrences (with replacement)
  colls_to_sample <- base::sample(collections, bootstraps, replace = T)
  subset <- tetrapods_trim[FALSE,]
  for (k in 1:bootstraps){
  one_coll <- filter(tetrapods_trim, collection_no == colls_to_sample[k])
  subset <- rbind(subset, one_coll)
  }

  #Filter unique taxa
  #Create an empty dataset with PBDB column names to collect unique occurrences in
  unique_by_bin <- tetrapods_trim[FALSE,]

  #Loop through each collection
  #For that collection, retain species occurrences, then retain unique taxa at gradually
  #   higher taxonomic levels which are not already represented in that collection
  #   i.e. if there is an indeterminate dicynodont but no occurrences in that collection which
  #   are dicynodonts but more specifically identified, the occurrence is retained
  for (i in 1:(length(labels))) {
    one_bin <- subset %>% filter(paleolat_code == labels[i])
    for (j in 1:(nrow(one_bin))) {
      if (nrow(one_bin) != 0)
        if (one_bin$accepted_rank[j] == "species") unique_by_bin <- rbind(unique_by_bin, one_bin[j,]) else
          if (!is.na(one_bin$genus[j]))
            (if (one_bin$genus[j] %in% one_bin$genus[-j] == F) unique_by_bin <- rbind(unique_by_bin, one_bin[j,])) else
              if (!is.na(one_bin$family[j]))
                (if (one_bin$family[j] %in% one_bin$family[-j] == F) unique_by_bin <- rbind(unique_by_bin, one_bin[j,])) else
                  if (!is.na(one_bin$order[j]))
                    (if (one_bin$order[j] %in% one_bin$order[-j] == F) unique_by_bin <- rbind(unique_by_bin, one_bin[j,]))
    }
  }

  #Remove repeats of species names, to get one occurrence per unique species per substage
  unique_by_bin <- distinct(unique_by_bin, accepted_name, paleolat_code, .keep_all = T)

  #Generate counts
  raw_curve <- count(unique_by_bin, paleolat_code)

  #Fill in gaps
  for(m in 1:length(labels)) {
    if((labels[m] %in% raw_curve$paleolat_code) == FALSE) raw_curve <- rbind(raw_curve, c(labels[m], 0))
  }

  #Reorder for ease of checking
  raw_curve <- arrange(raw_curve, desc(paleolat_code))
  raw_curve$paleolat_code <- as.numeric(as.character(raw_curve$paleolat_code))

  #Add collection sample and curve for each iteration to a list
  sample_list <- rbind(sample_list, colls_to_sample)
  gradients <- rbind(gradients, raw_curve$n)
}

names(sample_list) <- 1:bootstraps
names(gradients) <- sort(labels, decreasing = T) #reorder labels to match order in output

write.csv(sample_list, file = "data/Bootstrap sample list.csv") #write csv of sampled collections
write.csv(gradients, file = "data/Bootstrap gradients.csv") #write csv of gradients produced for each bootstrap

sample_list <- read_csv("data/Bootstrap samples LP terr.csv")
gradients <- read_csv("data/Bootstrap gradients LP terr.csv")
gradients <- gradients[,-1]

#Generate summary statistics
means <- colMeans(gradients)
stand_devs <- apply(gradients, 2, sd)
error <- qnorm(0.95)*(stand_devs/sqrt(reps))
lower <- means - error
upper <- means + error
#Create data frame containing summary statistics ready for plotting
to_plot <- cbind(labels = sort(labels, decreasing = T), means, lower, upper)
to_plot <- data.frame(to_plot)

#Designate colour, limegreen or blue
colour <- "limegreen"

#Plot mean raw richness across bootstraps, with 95% confidence intervals
ggplot(to_plot, aes(x = labels, y = means, ymin = lower, ymax = upper)) +
  geom_line(size = 1, col = colour) + geom_point(size = 2, col = colour) + geom_linerange(col = colour) +
  labs(x = "Palaeolatitude", y = "Raw species richness count") + coord_flip() +
  scale_x_continuous(limits = c(-90, 90)) + geom_vline(aes(xintercept = 0), colour = "black") +
  scale_colour_manual(values = "green") +
  theme_classic()
