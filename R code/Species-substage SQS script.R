#Bethany Allen   3rd July 2018 (edited 7th February 2019)
#Code to run SQS on tetrapod data (from a csv) with species level IDs and substage level dating

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

#Trim data without substage assignment or species-level identification
tetrapods <- tetrapods %>% filter(!is.na(substage_assignment))

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

###SQS - terrestrial###
#Filter terrestrial fossils
terrestrial <- unique_by_substage %>% filter(pres_mode == "body") %>%
  filter(taxon_environment == "terrestrial")
#Generate a table of sample sizes by stage, ordered chronologically
terres_totals <- count(terrestrial, substage_assignment)
#terres_totals <- group_by(terrestrial, substage_assignment) %>% summarise(coll_count = n_distinct(collection_no))
terres_totals <- terres_totals[match(substages, terres_totals$substage_assignment),]

#Generate list of frequencies by substage, starting with total value, as needed by iNEXT
#Achieved by using and trimming the 'count' function in dplyr, across a loop of substage names
terres_substage_freq <- list()

for (i in 1:length(substages)) {
  t_list <- terrestrial %>% filter(substage_assignment == substages[i]) %>%
    count(., accepted_name) %>% arrange(desc(n)) %>% add_row(n = sum(.$n), .before = 1) %>%
    select(n)
  t_list <- unlist(t_list, use.names = F)
  if(t_list[1] < 3){t_list <- NA}
  terres_substage_freq[[i]] <- t_list
}
names(terres_substage_freq) <- substages
glimpse(terres_substage_freq)

#Filter out empty lists
terres_substage_freq <- terres_substage_freq[!is.na(terres_substage_freq)]

###Code from Dunne et al. 2018###
#"1  Coverage-standardised diversity curve"

#Set quorum levels
quorum_levels <- round(seq(from = 0.1, to = 0.9, by = 0.1), 1)

#Estimate D using estimateD in iNEXT
t.estD_list <- list()

for(i in 1:length(quorum_levels)) {
  t.estD <- estimateD(terres_substage_freq, datatype = "incidence_freq", base = "coverage", level = quorum_levels[i])
  t.estD <- filter(t.estD, order == 0)        #filter for species richness values
  t.estD$quorum_level <- quorum_levels[i]
  t.estD$reference_t <- terres_totals$n            #add sample sizes in extra column
  t.estD$midpoints <- midpoints[match(names(terres_substage_freq), substages)]
  t.estD_list[[i]] <- t.estD
}

#Bind the individual dataframes into one
t.estD_list <- bind_rows(t.estD_list)

#Remove values when t is more than three times the sample size
t.estD_list[which(t.estD_list$t >= 3 * t.estD_list$reference_t), c("qD", "qD.LCL", "qD.UCL")] <- rep(NA, 3)

View(t.estD_list)

#Make quorum level a factor
t.estD_list$quorum_level <- as.factor(t.estD_list$quorum_level)

#Plot quorum levels 0.4 - 0.7
ggplot(filter(t.estD_list, quorum_level %in% quorum_levels[4:7]), aes(x = midpoints, y = qD, ymin = qD.LCL, ymax = qD.UCL, group = quorum_level, colour = quorum_level)) +
  geom_line(size = 1) + geom_point() + geom_linerange() +
  scale_colour_manual(values = c("chartreuse1", "chartreuse2", "chartreuse3", "chartreuse4")) +
  scale_x_reverse(limits = c(260, 227)) + labs(x = "Ma", y = "Subsampled diversity", colour = "Quorum level") +
  geom_vline(aes(xintercept = 259.8), colour = "grey") + #Start of Wuchiapingian
  geom_vline(aes(xintercept = 254.14), colour = "grey") + #Changhsingian
  geom_vline(aes(xintercept = 252.17), colour = "grey") + #Induan
  geom_vline(aes(xintercept = 252.17), linetype = "longdash", colour = "red") + #PT
  geom_vline(aes(xintercept = 251.2), colour = "grey") + #Olenekian
  geom_vline(aes(xintercept = 247.2), colour = "grey") + #Anisian
  geom_vline(aes(xintercept = 242), colour = "grey") + #Ladinian
  geom_vline(aes(xintercept = 237), colour = "grey") + #Carnian
  geom_vline(aes(xintercept = 227), colour = "grey") + #End of Carnian
  theme_classic()


###SQS - marine###
#Filter marine fossils
marine <- unique_by_substage %>% filter(pres_mode == "body") %>%
  filter(taxon_environment == "marine")
#Generate a table of sample sizes by stage, ordered chronologically
marine_totals <- count(marine, substage_assignment)
marine_totals <- marine_totals[match(substages, marine_totals$substage_assignment),]
marine_totals <- filter(marine_totals, !is.na(n))

#Generate list of frequencies by substage, starting with total value, as needed by iNEXT
#Achieved by using and trimming the 'count' function in dplyr, across a loop of substage names
marine_substage_freq <- list()

for (i in 1:length(substages)) {
  m_list <- marine %>% filter(substage_assignment == substages[i]) %>%
    count(., accepted_name) %>% arrange(desc(n)) %>% add_row(n = sum(.$n), .before = 1) %>%
    select(n)
  m_list <- unlist(m_list, use.names = F)
  if(m_list[1] < 3){m_list <- NA}
  marine_substage_freq[[i]] <- m_list
}
names(marine_substage_freq) <- substages
glimpse(marine_substage_freq)

#Filter out empty lists
marine_substage_freq <- marine_substage_freq[!is.na(marine_substage_freq)]

###Code from Dunne et al. 2018###
#"1  Coverage-standardised diversity curve"

#Set quorum levels
quorum_levels <- round(seq(from = 0.1, to = 0.9, by = 0.1), 1)

#Estimate D using estimateD in iNEXT
m.estD_list <- list()

for(i in 1:length(quorum_levels)) {
  m.estD <- estimateD(marine_substage_freq, datatype = "incidence_freq", base = "coverage", level = quorum_levels[i])
  m.estD <- filter(m.estD, order == 0)        #filter for species richness values
  m.estD$quorum_level <- quorum_levels[i]
  m.estD$reference_t <- marine_totals$n            #add sample sizes in extra column
  m.estD$midpoints <- midpoints[match(names(marine_substage_freq), substages)]
  m.estD_list[[i]] <- m.estD
}

#Bind the individual dataframes into one
m.estD_list <- bind_rows(m.estD_list)

#Remove values when t is more than three times the sample size
m.estD_list[which(m.estD_list$t >= 3 * m.estD_list$reference_t), c("qD", "qD.LCL", "qD.UCL")] <- rep(NA, 3)

View(m.estD_list)

#Make quorum level a factor
m.estD_list$quorum_level <- as.factor(m.estD_list$quorum_level)

#Plot quorum levels 0.4 - 0.7
ggplot(filter(m.estD_list, quorum_level %in% quorum_levels[4:7]), aes(x = midpoints, y = qD, ymin = qD.LCL, ymax = qD.UCL, group = quorum_level, colour = quorum_level)) +
  geom_line(size = 1) + geom_point() + geom_linerange() +
  scale_colour_manual(values = c("dodgerblue1", "dodgerblue2", "dodgerblue3", "dodgerblue4")) +
  scale_x_reverse(limits = c(260, 227)) + scale_y_continuous (position = "right") +
  labs(x = "Ma", y = "Subsampled diversity", colour = "Quorum level") +
  geom_vline(aes(xintercept = 259.8), colour = "grey") + #Start of Wuchiapingian
  geom_vline(aes(xintercept = 254.14), colour = "grey") + #Changhsingian
  geom_vline(aes(xintercept = 252.17), colour = "grey") + #Induan
  geom_vline(aes(xintercept = 252.17), linetype = "longdash", colour = "red") + #PT
  geom_vline(aes(xintercept = 251.2), colour = "grey") + #Olenekian
  geom_vline(aes(xintercept = 247.2), colour = "grey") + #Anisian
  geom_vline(aes(xintercept = 242), colour = "grey") + #Ladinian
  geom_vline(aes(xintercept = 237), colour = "grey") + #Carnian
  geom_vline(aes(xintercept = 227), colour = "grey") + #End of Carnian
  theme_classic()
