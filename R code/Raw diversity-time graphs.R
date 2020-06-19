#Bethany Allen   12th June 2018 (edited 12th November 2018, 30th January 2019)
#Code to plot graphs of raw diversity through time for the PBDB tetrapod data (from a csv)

#setwd("#####")

#Load packages
library(tidyverse)

#Create a vector giving the chronological order of stages
stages <- c("Wuchiapingian", "Changhsingian", "Induan", "Olenekian", "Anisian", "Ladinian",
            "Carnian")

#Create a vector of stage midpoints
midpoints <- c(256.6, 253, 251.6, 249.2, 244.6, 239.5, 232)

#Create a vector giving the chronological order of substages
substages <- c("Wuchiapingian", "Changhsingian", "Griesbachian", "Dienerian", "Smithian", "Spathian",
               "Aegean-Bithynian", "Pelsonian-Illyrian", "Fassanian", "Longobardian", "Julian",
               "Tuvalian")

#Create a vector of substage midpoints
submidpoints <- c(256.6, 253, 251.7, 251.4, 250.2, 248.2, 245.9, 243.3, 240.8, 238.3, 234.5, 229.5)

#Read in dataset
tetrapods <- read_csv("data/tetrapod_database_ScotesePLL.csv")
glimpse(tetrapods)

#Wrangling
#Check no incorrect spelling
#unique(tetrapods$stage_assignment)
#unique(tetrapods$substage_assignment)

#Remove synonymy repeats (combinations of the same collection no. AND accepted name)
tetrapods <- distinct(tetrapods, accepted_name, collection_no, .keep_all = T)

#Remove occurrences without substage-level temporal resolution
tetrapods <- filter(tetrapods, !is.na(substage_assignment))

###Time line graph comparing fossil types/ecology OCCURRENCES###
#Trace fossil species
trace_counts <- tetrapods %>% filter(pres_mode == "trace") %>%
  count(substage_assignment)
trace_counts <- trace_counts[match(substages, trace_counts$substage_assignment),]
trace_counts$midpoints <- submidpoints
trace_counts$n[is.na(trace_counts$n)] <- 0

#Terrestrial fossil species
terres_counts <- tetrapods %>% filter(pres_mode == "body") %>%
  filter(taxon_environment == "terrestrial") %>% count(substage_assignment)
terres_counts <- terres_counts[match(substages, terres_counts$substage_assignment),]
terres_counts$midpoints <- submidpoints
terres_counts$n[is.na(terres_counts$n)] <- 0

#Marine fossil species
marine_counts <- tetrapods %>% filter(pres_mode == "body") %>%
  filter(taxon_environment == "marine") %>% count(substage_assignment)
marine_counts <- marine_counts[match(substages, marine_counts$substage_assignment),]
marine_counts$midpoints <- submidpoints
marine_counts$n[is.na(marine_counts$n)] <- 0

#Combine into a single data frame
trace_counts$type <- "Number of ichnospecies"
terres_counts$type <- "Number of terrestrial species"
marine_counts$type <- "Number of marine species"
type_counts <- rbind(trace_counts, terres_counts, marine_counts)

#Plot with geom_line
ggplot(type_counts, aes(x = midpoints, y = n, group = type, colour = type)) +
  geom_line(size = 2) + scale_x_reverse(limits = c(260, 227)) + labs(x = "Ma", y = "Raw count") +
  geom_vline(aes(xintercept = 259.8), colour = "grey") + #Start of Wuchiapingian
  geom_vline(aes(xintercept = 254.14), colour = "grey") + #Changhsingian
  geom_vline(aes(xintercept = 252.17), colour = "grey") + #Induan
  geom_vline(aes(xintercept = 252.17), linetype = "longdash", colour = "red") + #PT
  geom_vline(aes(xintercept = 251.2), colour = "grey") + #Olenekian
  geom_vline(aes(xintercept = 247.2), colour = "grey") + #Anisian
  geom_vline(aes(xintercept = 242), colour = "grey") + #Ladinian
  geom_vline(aes(xintercept = 237), colour = "grey") + #Carnian
  geom_vline(aes(xintercept = 227), colour = "grey") + #End of Carnian
  scale_colour_manual(values = c("orange", "blue", "limegreen")) + theme_classic() +
  theme(legend.title=element_blank())


###Time line graph comparing fossil types/ecology RICHNESS###
#Create an empty dataset with PBDB column names to collect unique occurrences in
unique_by_substage <- tetrapods[FALSE,]

#Loop through each collection
#For that collection, retain species occurrences, then retain unique taxa at gradually
#   higher taxonomic levels which are not already represented in that collection
#   i.e. if there is an indeterminate dicynodont but no occurrences in that collection which
#   are dicynodonts but more specifically identified, the occurrence is retained
for (i in 1:(length(substages))) {
  print(i)
  one_substage <- tetrapods %>% filter(substage_assignment == substages[i])
  for (j in 1:(nrow(one_substage))) {
    if (one_substage$accepted_rank[j] == "species") unique_by_substage <- rbind(unique_by_substage, one_substage[j,]) else
      if (!is.na(one_substage$genus[j]))
        (if (one_substage$genus[j] %in% one_substage$genus[-j] == F) unique_by_substage <- rbind(unique_by_substage, one_substage[j,])) else
          if (!is.na(one_substage$family[j]))
            (if (one_substage$family[j] %in% one_substage$family[-j] == F) unique_by_substage <- rbind(unique_by_substage, one_substage[j,])) else
              if (!is.na(one_substage$order[j]))
                (if (one_substage$order[j] %in% one_substage$order[-j] == F) unique_by_substage <- rbind(unique_by_substage, one_substage[j,]))
  }
}

#Remove repeats of species names, to get one occurrence per unique species per substage
unique_by_substage <- distinct(unique_by_substage, accepted_name, substage_assignment, .keep_all = T)

#Trace fossil species
trace_counts2 <- unique_by_substage %>% filter(pres_mode == "trace") %>%
  count(substage_assignment)
trace_counts2 <- trace_counts2[match(substages, trace_counts2$substage_assignment),]
trace_counts2$midpoints <- submidpoints
trace_counts2$n[is.na(trace_counts2$n)] <- 0

#Terrestrial fossil species
terres_counts2 <- unique_by_substage %>% filter(pres_mode == "body") %>%
  filter(taxon_environment == "terrestrial") %>% count(substage_assignment)
terres_counts2 <- terres_counts2[match(substages, terres_counts2$substage_assignment),]
terres_counts2$midpoints <- submidpoints
terres_counts2$n[is.na(terres_counts2$n)] <- 0

#Marine fossil species
marine_counts2 <- unique_by_substage %>% filter(pres_mode == "body") %>%
  filter(taxon_environment == "marine") %>% count(substage_assignment)
marine_counts2 <- marine_counts2[match(substages, marine_counts2$substage_assignment),]
marine_counts2$midpoints <- submidpoints
marine_counts2$n[is.na(marine_counts2$n)] <- 0

#Combine into a single data frame
trace_counts2$type <- "Number of ichnospecies"
terres_counts2$type <- "Number of terrestrial species"
marine_counts2$type <- "Number of marine species"
type_counts2 <- rbind(trace_counts2, terres_counts2, marine_counts2)

#Plot with geom_line
ggplot(type_counts2, aes(x = midpoints, y = n, group = type, colour = type)) +
  geom_line(size = 2) + scale_x_reverse(limits = c(260, 227)) + labs(x = "Ma", y = "Raw count") +
  geom_vline(aes(xintercept = 259.8), colour = "grey") + #Start of Wuchiapingian
  geom_vline(aes(xintercept = 254.14), colour = "grey") + #Changhsingian
  geom_vline(aes(xintercept = 252.17), colour = "grey") + #Induan
  geom_vline(aes(xintercept = 252.17), linetype = "longdash", colour = "red") + #PT
  geom_vline(aes(xintercept = 251.2), colour = "grey") + #Olenekian
  geom_vline(aes(xintercept = 247.2), colour = "grey") + #Anisian
  geom_vline(aes(xintercept = 242), colour = "grey") + #Ladinian
  geom_vline(aes(xintercept = 237), colour = "grey") + #Carnian
  geom_vline(aes(xintercept = 227), colour = "grey") + #End of Carnian
  scale_colour_manual(values = c("orange", "blue", "limegreen")) + theme_classic() +
  theme(legend.title=element_blank())


###Time line graph with collection and formation counts###
#Total number of tetrapod collections (filtered by unique collection numbers)
tp_collection_counts <- tetrapods %>% filter(!is.na(substage_assignment)) %>% filter(accepted_rank == "species") %>%
  distinct(collection_no, .keep_all = T) %>% count(substage_assignment)
tp_collection_counts <- tp_collection_counts[match(substages, tp_collection_counts$substage_assignment),]
tp_collection_counts$midpoints <- submidpoints
tp_collection_counts$n[is.na(tp_collection_counts$n)] <- 0

#Total number of formations (filtered by unique formation names)
tp_formation_counts <- tetrapods %>% filter(!is.na(substage_assignment)) %>% filter(accepted_rank == "species") %>%
  group_by(substage_assignment) %>% summarize(n = n_distinct(formation))
tp_formation_counts <- tp_formation_counts[match(substages, tp_formation_counts$substage_assignment),]
tp_formation_counts$midpoints <- submidpoints
tp_formation_counts$n[is.na(tp_formation_counts$n)] <- 0

#Read total P-T collections and formations summary tables (see "Wrangle collections.R")
collection_counts <- read_csv("data/P-T collection counts.csv")
colnames(collection_counts)[1] <- "substage_assignment"
collection_counts <- collection_counts[match(substages, collection_counts$substage_assignment),]
formation_counts <- read_csv("data/P-T formation counts.csv")
colnames(formation_counts)[1] <- "substage_assignment"
formation_counts <- formation_counts[match(substages, formation_counts$substage_assignment),]

#Combine into a single data frame
tp_collection_counts$type <- "Tetrapod-bearing collections"
tp_formation_counts$type <- "Tetrapod-bearing formations"
collection_counts$type <- "Total collections"
formation_counts$type <- "Total formations"
data_counts <- rbind(tp_collection_counts, tp_formation_counts, collection_counts, formation_counts)

ggplot(data_counts, aes(x = midpoints, y = log(n), group = type, colour = type)) + geom_line(size = 2) +
  scale_x_reverse(limits = c(260, 227)) + scale_y_continuous (position = "right") +
  labs(x = "Ma", y = "log(Count)") +
  geom_vline(aes(xintercept = 259.8), colour = "grey") + #Start of Wuchiapingian
  geom_vline(aes(xintercept = 254.14), colour = "grey") + #Changhsingian
  geom_vline(aes(xintercept = 252.17), colour = "grey") + #Induan
  geom_vline(aes(xintercept = 252.17), linetype = "longdash", colour = "red") + #PT
  geom_vline(aes(xintercept = 251.2), colour = "grey") + #Olenekian
  geom_vline(aes(xintercept = 247.2), colour = "grey") + #Anisian
  geom_vline(aes(xintercept = 242), colour = "grey") + #Ladinian
  geom_vline(aes(xintercept = 237), colour = "grey") + #Carnian
  geom_vline(aes(xintercept = 227), colour = "grey") + #End of Carnian
  scale_colour_manual(values = c("cyan1", "red1", "cyan4", "red4")) + theme_classic() +
  theme(legend.title=element_blank())
