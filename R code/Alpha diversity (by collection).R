#Bethany Allen   5th February 2020
#Code to find alpha diversity (mean species count per collection) on tetrapod data (from a csv) with species level IDs

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

#Filter unique taxa
#Create an empty dataset with PBDB column names to collect unique occurrences in
new_dataset <- tetrapods_trim[FALSE,]

#Create a vector of unique collection numbers
collections <- unique(tetrapods_trim$collection_no)

#Loop through each collection
#For that collection, retain species occurrences, then retain unique taxa at gradually
#   higher taxonomic levels which are not already represented in that collection
#   i.e. if there is an indeterminate dicynodont but no occurrences in that collection which
#   are dicynodonts but more specifically identified, the occurrence is retained
for (i in 1:(length(collections))) {
  print(i)
  one_coll <- tetrapods_trim %>% filter(collection_no == collections[i])
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

#Filter as desired
subset <- new_dataset %>% filter(pres_mode == "body") %>% 
  filter(taxon_environment == "terrestrial") %>% filter(stage_assignment == "Wuchiapingian")

#Create string of frequencies per collection, and generate a mean across it
count_string <- subset %>% filter(paleolat_code == -60) %>% count(collection_no) %>% pull(n)
mean(count_string)

