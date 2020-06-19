#Bethany Allen   26th Feb 2020
#Code to run squares on tetrapod data (from a csv) with species level IDs, on the basis of palaeolatitude

#setwd("#####")

#Load packages
library(tidyverse)

#Read in dataset
tetrapods <- read_csv("data/tetrapod_database_ScotesePLL.csv")
glimpse(tetrapods)

#Remove synonymy repeats (combinations of the same collection no. AND accepted name)
tetrapods <- distinct(tetrapods, accepted_name, collection_no, .keep_all = T)

#Trim data without substage assignment or species-level identification
tetrapods_trim <- tetrapods %>% filter(!is.na(stage_assignment))

#Add column which indicates latitude bin
bins <- seq(from = -90, to = 90, by = 20)
labels <- seq(from = -80, to = 80, by = 20)
tetrapods_trim <- mutate(tetrapods_trim, paleolat_code = cut(Latitude, breaks = bins, labels = labels))


###CHOOSE ENVIRONMENT AND STAGE###
#Filter data to body fossils for specified environment/time interval
stage_tetrapods <- tetrapods_trim %>% filter(pres_mode == "body") %>% 
  filter(taxon_environment == "terrestrial") %>%
  filter(stage_assignment == "Wuchiapingian")
  
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

#Generate list of frequencies by stage
#Achieved by using and trimming the 'count' function in dplyr, across a loop of stage names
lat_freq <- list()

for (i in 1:length(labels)) {
  lat_list <- unique_by_bin %>% filter(paleolat_code == labels[i]) %>%
    count(., accepted_name) %>% arrange(desc(n)) %>% select(n)
  lat_list <- unlist(lat_list, use.names = F)
  lat_freq[[i]] <- lat_list
}
names(lat_freq) <- labels
glimpse(lat_freq)

#Estimate diversity using squares method (Alroy, 2018)
squares_list <- vector("numeric", length = 0)

for(i in 1:length(lat_freq)) {
  freq_list <- lat_freq[[i]]
  if(is.na(freq_list[1])){freq_list <- 0}
  if(freq_list[1] == 0){squares <- 0} else {
   sp_count <- length(freq_list)
   sing_count <- sum(freq_list == 1)
   ind_count <- sum(freq_list)
   sum_nsq <- sum(freq_list^2)
   squares <- sp_count + (((sing_count^2)*sum_nsq)/((ind_count^2) - (sing_count*sp_count)))
   if(squares == Inf){squares <- length(freq_list)}
  }
  squares_list <- append(squares_list, squares)
}

to_plot <- data.frame(labels, squares_list)

#Used "limegreen" and "blue" colour sets
colour <- "limegreen"

#Plot
ggplot(to_plot, aes(x = labels, y = squares_list, group = 1)) +
  geom_line(size = 1.5, col = colour) + geom_point(size = 2, col = colour) + labs(x = "Palaeolatitude", y = "Subsampled diversity") + 
  coord_flip() + scale_x_continuous(limits = c(-90, 90)) +
  geom_vline(aes(xintercept = 0), colour = "black", size = 0.7) +
  theme_classic()


#Put terrestrial and marine on same graph
###CHOOSE STAGE###
#Filter data to body fossils for specified environment/time interval
stage_tetrapods <- tetrapods_trim %>% filter(pres_mode == "body") %>% 
  filter(stage_assignment == "Olenekian")

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

##Terrestrial##
#Filter terrestrial fossils
terrestrial <- filter(unique_by_bin, taxon_environment == "terrestrial")

#Generate list of frequencies by stage
#Achieved by using and trimming the 'count' function in dplyr, across a loop of stage names
terres_lat_freq <- list()

for (i in 1:length(labels)) {
  t_list <- terrestrial %>% filter(paleolat_code == labels[i]) %>%
    count(., accepted_name) %>% arrange(desc(n)) %>% select(n)
  t_list <- unlist(t_list, use.names = F)
  terres_lat_freq[[i]] <- t_list
}
names(terres_lat_freq) <- labels
glimpse(terres_lat_freq)

#Estimate diversity using squares method (Alroy, 2018)
t_squares_list <- vector("numeric", length = 0)

for(i in 1:length(terres_lat_freq)) {
  t_freq_list <- terres_lat_freq[[i]]
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

t_to_plot <- data.frame(labels, squares = t_squares_list)

##Marine##
#Filter marine fossils
marine <- filter(unique_by_bin, taxon_environment == "marine")

#Generate list of frequencies by stage
#Achieved by using and trimming the 'count' function in dplyr, across a loop of stage names
marine_lat_freq <- list()

for (i in 1:length(labels)) {
  m_list <- marine %>% filter(paleolat_code == labels[i]) %>%
    count(., accepted_name) %>% arrange(desc(n)) %>% select(n)
  m_list <- unlist(m_list, use.names = F)
  marine_lat_freq[[i]] <- m_list
}
names(marine_lat_freq) <- labels
glimpse(marine_lat_freq)

#Estimate diversity using squares method (Alroy, 2018)
m_squares_list <- vector("numeric", length = 0)

for(i in 1:length(marine_lat_freq)) {
  m_freq_list <- marine_lat_freq[[i]]
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

m_to_plot <- data.frame(labels, squares = m_squares_list)

#Plot
t_to_plot$type <- "Terrestrial"
m_to_plot$type <- "Marine"
all_to_plot <- rbind(t_to_plot, m_to_plot)

#Plot
ggplot(all_to_plot, aes(x = labels, y = squares, group = type, colour = type)) +
  geom_line(size = 1.5) + geom_point(size = 2) + labs(x = "Palaeolatitude", y = "Subsampled diversity") + 
  coord_flip() + scale_x_continuous(limits = c(-90, 90)) +
  scale_colour_manual(values = c("blue", "limegreen")) +
  geom_vline(aes(xintercept = 0), colour = "black", size = 0.7) +
  theme_classic()
