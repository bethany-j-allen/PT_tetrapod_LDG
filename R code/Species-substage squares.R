#Bethany Allen   31st Jan 2020
#Code to run squares on tetrapod data (from a csv) with species level IDs and substage level dating

#setwd("#####")

#Load packages
library(tidyverse)

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

###Squares - terrestrial###
#Filter terrestrial fossils
terrestrial <- unique_by_substage %>% filter(pres_mode == "body") %>%
  filter(taxon_environment == "terrestrial")

#Generate list of frequencies by substage
#Achieved by using and trimming the 'count' function in dplyr, across a loop of substage names
terres_substage_freq <- list()

for (k in 1:length(substages)) {
  t_list <- terrestrial %>% filter(substage_assignment == substages[k]) %>%
    count(., accepted_name) %>% arrange(desc(n)) %>% select(n)
  t_list <- unlist(t_list, use.names = F)
  terres_substage_freq[[k]] <- t_list
}
names(terres_substage_freq) <- substages
glimpse(terres_substage_freq)


#Estimate diversity using squares method (Alroy, 2018)
t_squares_list <- vector("numeric", length = 0)

for(i in 1:length(terres_substage_freq)) {
  t_freq_list <- terres_substage_freq[[i]]
  if(is.na(t_freq_list[1])){t_freq_list <- 0}
  if(t_freq_list[1] == 0){t_squares <- 0} else {
  t_sp_count <- length(t_freq_list)
  t_sing_count <- sum(t_freq_list == 1)
  t_ind_count <- sum(t_freq_list)
  t_sum_nsq <- sum(t_freq_list^2)
  t_squares <- t_sp_count + (((t_sing_count^2)*t_sum_nsq)/((t_ind_count^2) - (t_sing_count*t_sp_count)))
  if(t_squares == Inf){t_squares <- length(t_freq_list)}
  }
  t_squares_list <- append(t_squares_list, t_squares)
}

t_to_plot <- data.frame(substages, squares = t_squares_list, midpoints)

###Squares - marine###
#Filter marine fossils
marine <- unique_by_substage %>% filter(pres_mode == "body") %>%
  filter(taxon_environment == "marine")

#Generate list of frequencies by substage
#Achieved by using and trimming the 'count' function in dplyr, across a loop of substage names
marine_substage_freq <- list()

for (i in 1:length(substages)) {
  m_list <- marine %>% filter(substage_assignment == substages[i]) %>%
    count(., accepted_name) %>% arrange(desc(n)) %>% select(n)
  m_list <- unlist(m_list, use.names = F)
  marine_substage_freq[[i]] <- m_list
}
names(marine_substage_freq) <- substages
glimpse(marine_substage_freq)


#Estimate diversity using squares method (Alroy, 2018)
m_squares_list <- vector("numeric", length = 0)

for(i in 1:length(marine_substage_freq)) {
  m_freq_list <- marine_substage_freq[[i]]
  if(is.na(m_freq_list[1])){m_freq_list <- 0}
  if(m_freq_list[1] == 0){m_squares <- 0} else {
  m_sp_count <- length(m_freq_list)
  m_sing_count <- sum(m_freq_list == 1)
  m_ind_count <- sum(m_freq_list)
  m_sum_nsq <- sum(m_freq_list^2)
  m_squares <- m_sp_count + (((m_sing_count^2)*m_sum_nsq)/((m_ind_count^2) - (m_sing_count*m_sp_count)))
  if(m_squares == Inf){m_squares <- length(m_freq_list)}
  }
  m_squares_list <- append(m_squares_list, m_squares)
}

m_to_plot <- data.frame(substages, squares = m_squares_list, midpoints)

#Plot
t_to_plot$type <- "Terrestrial"
m_to_plot$type <- "Marine"
all_to_plot <- rbind(t_to_plot, m_to_plot)

ggplot(all_to_plot, aes(x = midpoints, y = squares, group = type, colour = type)) +
  geom_line(size = 1) + geom_point() +
  scale_colour_manual(values = c("blue", "limegreen")) +
  scale_x_reverse(limits = c(260, 227)) + labs(x = "Ma", y = "Squares diversity") +
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
