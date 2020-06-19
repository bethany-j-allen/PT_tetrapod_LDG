#Bethany Allen   15th May 2019
#Code to produce rarefaction curves for SQS analyses

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
stage_tetrapods <- tetrapods_trim %>% filter(stage_assignment == "Anisian" | stage_assignment == "Ladinian") %>%
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
  lat_list <- stage_tetrapods %>% filter(paleolat_code == labels[i]) %>%
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

#Run iNEXT function
inc.data <- iNEXT(lat_freq, q = 0, datatype = "incidence_freq", knots = 100)

#Extract rarefaction points
cov_rare <- inc.data$iNextEst

for(i in 1:length(cov_rare)) {
  cov_rare[[i]]$stage_int <- names(cov_rare)[i]
}

cov_rare <- do.call(rbind, cov_rare) %>% tbl_df() #convert to tibble for ease of plotting

#Plot
ggplot(data = cov_rare, aes(x = SC, y = qD, ymin = qD.LCL, ymax = qD.UCL, fill = stage_int, colour = stage_int, lty = method)) + 
  geom_line(size = 1) + 
  scale_linetype_manual(values=c("dotted", "solid", "longdash")) +
  labs(x = "Coverage", y = "Species richness") +
  theme_classic()
