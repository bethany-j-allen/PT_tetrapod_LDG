
#setwd("#####")

#Load packages
library(tidyverse)
library(iNEXT)

#Create a vector giving the chronological order of substages
substages <- c("Wuchiapingian", "Changhsingian", "Griesbachian", "Smithian", "Spathian",
               "Aegean-Bithynian", "Pelsonian-Illyrian", "Fassanian", "Longobardian", "Julian",
               "Tuvalian")

#Create a vector of substage midpoints
midpoints <- c(256.6, 253, 251.7, 250.2, 248.2, 245.9, 243.3, 240.8, 238.3, 234.5, 229.5)

#Add Dienerian if needed, 251.4

#Read in dataset
tetrapods <- read_csv("data/tetrapod_database_ScotesePLL.csv")
glimpse(tetrapods)

#Remove synonymy repeats (combinations of the same collection no. AND accepted name)
tetrapods <- distinct(tetrapods, accepted_name, collection_no, .keep_all = T)

View(tetrapods)

#Create a list of collection numbers
collections <- unique(tetrapods$collection_no)

#Create an empty dataset with PBDB column names to collect unique occurrences in
new_dataset <- tetrapods[FALSE,]

#Loop through each collection
#For that collection, retain species occurrences, then retain unique taxa at gradually
#   higher taxonomic levels which are not already represented in that collection
#   i.e. if there is an indeterminate dicynodont but no occurrences in that collection which
#   are dicynodonts but more specifically identified, the occurrence is retained
for (i in 1:(length(collections))) {
  print(i)
  one_coll <- tetrapods %>% filter(collection_no == collections[i])
    for (j in 1:(nrow(one_coll))) {
     if (one_coll$accepted_rank[j] == "species") new_dataset <- rbind(new_dataset, one_coll[j,]) else
          if (!is.na(one_coll$genus[j]))
            (if (one_coll$genus[j] %in% one_coll$genus[-j] == F) new_dataset <- rbind(new_dataset, one_coll[j,])) else
          if (!is.na(one_coll$family[j]))
            (if (one_coll$family[j] %in% one_coll$family[-j] == F) new_dataset <- rbind(new_dataset, one_coll[j,])) else
          if (!is.na(one_coll$order[j]))
            (if (one_coll$order[j] %in% one_coll$order[-j] == F) new_dataset <- rbind(new_dataset, one_coll[j,]))
    }
}
