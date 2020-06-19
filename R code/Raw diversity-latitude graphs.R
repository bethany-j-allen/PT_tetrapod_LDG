#Bethany Allen   12th June 2018 (edited 12th November 2018, 7th February 2019)
#Code to plot graphs of raw diversity with latitude of the PBDB tetrapod data (from a csv)
#Current code plots SPECIES-STAGE occurrences

#setwd("#####")

#Remember that there are only marine occurrences from the Olenekian

#Load packages
library(tidyverse)

#Create a vector giving the chronological order of stages
stages <- c("Wuchiapingian", "Changhsingian", "Induan", "Olenekian", "Anisian", "Ladinian",
            "Carnian")

#Create a vector of stage midpoints
midpoints <- c(256.6, 253, 251.6, 249.2, 244.6, 239.5, 232)


#Read in dataset
tetrapods <- read_csv("data/tetrapod_database_ScotesePLL.csv")
glimpse(tetrapods)

#Wrangling
#Check no incorrect spelling
#unique(tetrapods$stage_assignment)
#unique(tetrapods$substage_assignment)

#Remove synonymy repeats (combinations of the same collection no. AND accepted name)
tetrapods <- distinct(tetrapods, accepted_name, collection_no, .keep_all = T)

#Filter to occurrences with stage assignment
tetrapods <- tetrapods %>% filter(!is.na(stage_assignment))


###Latitude line graphs of RICHNESS###
#Add column which indicates latitude bin
#Figure 1
bins <- seq(from = -90, to = 90, by = 20)
labels <- seq(from = -80, to = 80, by = 20)
#Figure 2
#bins <- seq(from = -90, to = 90, by = 5)
#labels <- seq(from = -87.5, to = 87.5, by = 5)

tetrapods_lat <- mutate(tetrapods, paleolat_code = cut(Latitude, breaks = bins, labels = labels))

#Apply filters
tetrapods_lat_stage <- tetrapods_lat %>% filter(stage_assignment == "Wuchiapingian")
#tetrapods_lat_stage <- tetrapods_lat %>% filter(stage_assignment == "Wuchiapingian" | stage_assignment == "Changhsingian")

#Filter unique taxa
#Create an empty dataset with PBDB column names to collect unique occurrences in
unique_by_bin <- tetrapods_lat_stage[FALSE,]

#Loop through each collection
#For that collection, retain species occurrences, then retain unique taxa at gradually
#   higher taxonomic levels which are not already represented in that collection
#   i.e. if there is an indeterminate dicynodont but no occurrences in that collection which
#   are dicynodonts but more specifically identified, the occurrence is retained
for (i in 1:(length(labels))) {
  print(i)
  one_bin <- tetrapods_lat_stage %>% filter(paleolat_code == labels[i])
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

#Subset data as trace vs. marine vs. terrestrial
traces <- unique_by_bin %>% filter(pres_mode == "trace") %>% count(paleolat_code)
marine <- unique_by_bin %>% filter(pres_mode == "body") %>%
  filter(taxon_environment == "marine") %>% count(paleolat_code)
terrestrial <- unique_by_bin %>% filter(pres_mode == "body") %>%
  filter(taxon_environment == "terrestrial") %>% count(paleolat_code)

#Fill in gaps
for(i in 1:length(labels)) {
if((labels[i] %in% traces$paleolat_code) == FALSE) traces <- rbind(traces, c(labels[i], 0))
}
for(i in 1:length(labels)) {
  if((labels[i] %in% marine$paleolat_code) == FALSE) marine <- rbind(marine, c(labels[i], 0))
}
for(i in 1:length(labels)) {
  if((labels[i] %in% terrestrial$paleolat_code) == FALSE) terrestrial <- rbind(terrestrial, c(labels[i], 0))
}

#Reorder for ease of checking
traces <- arrange(traces, desc(paleolat_code))
marine <- arrange(marine, desc(paleolat_code))
terrestrial <- arrange(terrestrial, desc(paleolat_code))

#Combine into single data frame for plotting
traces$type <- "Number of footprint occurrences"
marine$type <- "Number of marine occurrences"
terrestrial$type <- "Number of terrestrial occurrences"
lat_counts <- rbind(traces, marine, terrestrial)

#Convert to numbers for plotting
#terrestrial$paleolat_code <- as.numeric(as.character(terrestrial$paleolat_code))
lat_counts$paleolat_code <- as.numeric(as.character(lat_counts$paleolat_code))

#Plot
ggplot(lat_counts, aes(x = paleolat_code, y = n, group = type, colour = type)) +
  geom_line(size = 1) + geom_point(size = 2) +
  labs(x = "Palaeolatitude", y = "Raw species richness count") + coord_flip() +
  scale_x_continuous(limits = c(-90, 90)) + geom_vline(aes(xintercept = 0), colour = "black") + 
  scale_colour_manual(values = c("orange", "blue", "limegreen")) +
  theme_classic()

ggplot(terrestrial, aes(x = paleolat_code, y = n, group = 1)) +
  geom_line(size = 1, colour = "limegreen") + geom_point(size = 2, colour = "limegreen") +
  labs(x = "Palaeolatitude", y = "Raw species richness count") + coord_flip() +
  scale_x_continuous(limits = c(-90, 90)) + geom_vline(aes(xintercept = 0), colour = "black") + 
  theme_classic()
