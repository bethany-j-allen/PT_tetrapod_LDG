#Bethany Allen   11th September 2018
#Code to run SQS on tetrapod data (from a csv) with species level IDs, on the basis of palaeolatitude

#setwd("#####")

#Load packages
library(tidyverse)
library(iNEXT)

#Read in dataset
tetrapods <- read_csv("data/tetrapod_database_ScotesePLL.csv")
glimpse(tetrapods)

#Remove synonymy repeats (combinations of the same collection no. AND accepted name)
tetrapods <- distinct(tetrapods, accepted_name, collection_no, .keep_all = T)

#Trim data without stage assignment
tetrapods_trim <- tetrapods %>% filter(!is.na(stage_assignment))

#Trim to body fossils
tetrapods_trim <- filter(tetrapods_trim, pres_mode == "body")

#Add column which indicates latitude bin
bins <- seq(from = -90, to = 90, by = 20)
labels <- seq(from = -80, to = 80, by = 20)
tetrapods_trim <- mutate(tetrapods_trim, paleolat_code = cut(Latitude, breaks = bins, labels = labels))

###CHOOSE ENVIRONMENT AND STAGE###
environment <- "marine"

#Filter data
stage_tetrapods <- tetrapods_trim %>% filter(stage_assignment == "Ladinian") %>%
  filter(taxon_environment == environment)

#Filter unique taxa
#Create an empty dataset with PBDB column names to collect unique occurrences in
unique_by_bin <- stage_tetrapods[FALSE,]

#Loop through each collection
#For that collection, retain species occurrences, then retain unique taxa at gradually
#   higher taxonomic levels which are not already represented in that collection
#   i.e. if there is an indeterminate dicynodont but no occurrences in that collection which
#   are dicynodonts but more specifically identified, the occurrence is retained
for (i in 1:(length(labels))) {
  print(i)
  one_bin <- stage_tetrapods %>% filter(paleolat_code == labels[i])
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

#Generate a table of sample sizes by latitude bin, ordered numerically, and remove NAs
stage_totals <- count(unique_by_bin, paleolat_code)
for(i in 1:length(labels)) {
  if((labels[i] %in% stage_totals$paleolat_code) == FALSE) stage_totals <- rbind(stage_totals, c(labels[i], 1))
}

#Reorder for ease of checking
stage_totals <- arrange(stage_totals, paleolat_code)
stage_totals_list <- pull(stage_totals, n)

#Generate list of frequencies by stage, starting with total value, as needed by iNEXT
#Achieved by using and trimming the 'count' function in dplyr, across a loop of stage names
lat_freq <- list()

for (i in 1:length(labels)) {
lat_list <- unique_by_bin %>% filter(paleolat_code == labels[i]) %>%
    count(., accepted_name) %>% arrange(desc(n)) %>% add_row(n = sum(.$n), .before = 1) %>%
    select(n)
lat_list <- unlist(lat_list, use.names = F)
    if(lat_list[1] < 3){lat_list <- NA}
lat_freq[[i]] <- lat_list
}
names(lat_freq) <- labels
glimpse(lat_freq)

#Filter out empty lists
lat_freq <- lat_freq[!is.na(lat_freq)]


#Set quorum levels
quorum_levels <- round(seq(from = 0.3, to = 0.8, by = 0.1), 1)

#Estimate D using estimateD in iNEXT
estD_list <- list()

for(i in 1:length(quorum_levels)) {
  estD <- estimateD(lat_freq, datatype = "incidence_freq", base = "coverage", level = quorum_levels[i])
  estD <- filter(estD, order == 0)        #filter for species richness values
  estD$quorum_level <- quorum_levels[i]
  estD$reference_t <- stage_totals_list[(match(names(lat_freq), stage_totals$paleolat_code))]  #add sample sizes in extra column
  estD$midpoints <- names(lat_freq)
  estD_list[[i]] <- estD
}

#Bind the individual dataframes into one
estD_list <- bind_rows(estD_list)

#Remove values when t is more than three times the sample size
estD_list[which(estD_list$t >= 3 * estD_list$reference_t), c("qD", "qD.LCL", "qD.UCL")] <- rep(NA, 3) #no more than twice reference sample size

View(estD_list)

#Add empty bins as a plot point
for(i in 1:length(labels)){
  if((labels[i] %in% estD_list$site) == FALSE) estD_list <- rbind(estD_list, c(rep(NA, 5), 0, NA, NA, 0.4, NA, as.numeric(labels[i])))
}

#Make quorum level a factor
estD_list$midpoints <- as.numeric(as.character(estD_list$midpoints))
estD_list$quorum_level <- as.factor(estD_list$quorum_level)

#Used "chartreuse" and "dodgerblue" colour sets
colours <- c("dodgerblue1", "dodgerblue2", "dodgerblue3", "dodgerblue4")

#Plot quorum levels 0.4 - 0.7
ggplot(filter(estD_list, quorum_level %in% quorum_levels[2:5]), aes(x = midpoints, y = qD, ymin = qD.LCL, ymax = qD.UCL, group = quorum_level, colour = quorum_level)) +
  geom_line(size = 1.5) + geom_point(size = 2) + geom_linerange(size = 1) + labs(x = "Palaeolatitude", y = "Subsampled diversity", colour = "Quorum level") + 
  coord_flip() + scale_x_continuous(limits = c(-90, 90)) +
  geom_vline(aes(xintercept = 0), colour = "black", size = 0.7) +
  scale_colour_manual(values = colours) +
  theme_classic()
