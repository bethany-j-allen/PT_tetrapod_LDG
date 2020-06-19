#Bethany Allen   30th January 2019
#Code to wrangle PBDB download of all Wuch-Nor collections entered before 19/10/18

#setwd("#####")

#Load packages
library(tidyverse)

#Create a vector giving the chronological order of stages
stages <- c("Wuchiapingian", "Changhsingian", "Induan", "Olenekian", "Anisian", "Ladinian", "Carnian",
            "Norian")

#Create a vector of stage midpoints
midpoints <- c(256.6, 253, 251.6, 249.2, 244.6, 239.5, 232, 217.8)

#Create a vector giving the chronological order of substages
substages <- c("Wuchiapingian", "Changhsingian", "Griesbachian", "Dienerian", "Smithian", "Spathian",
               "Aegean-Bithynian", "Pelsonian-Illyrian", "Fassanian", "Longobardian", "Julian",
               "Tuvalian", "Lacian", "Alaunian", "Sevatian")

#Create a vector of substage midpoints
submidpoints <- c(256.6, 253, 251.7, 251.4, 250.2, 248.2, 245.9, 243.3, 240.8, 238.3, 234.5, 229.5, 221.3, 213.8, 210.3)

#Read in dataset
collections <- read_csv("data/P-T collections.csv")
View(collections)

#Retain only collections with substage dating
collsfilt <- collections %>% filter(is.na(late_interval)) %>% filter(early_interval %in% substages)

#Create table of collection numbers per substage
coll_counts <- count(collsfilt, early_interval)
coll_counts <- coll_counts[match(substages, coll_counts$early_interval),]
coll_counts$midpoints <- submidpoints
coll_counts <- drop_na(coll_counts, n)
write_csv(coll_counts, "data/P-T collection counts.csv")

#Create table of formation numbers per substage
form_counts <- collsfilt %>% filter(!is.na(formation)) %>% group_by(early_interval) %>%
  summarize(n = n_distinct(formation))
form_counts <- form_counts[match(substages, form_counts$early_interval),]
form_counts$midpoints <- submidpoints
form_counts <- drop_na(form_counts, n)
write_csv(form_counts, "data/P-T formation counts.csv")

