#Bethany Allen   3rd July 2018
#Code to run SQS on tetrapod data (from a csv) with species level IDs and stage level dating

#setwd("#####")

#Load packages
library(tidyverse)
library(iNEXT)

#Create a vector giving the chronological order of stages
stages <- c("Wuchiapingian", "Changhsingian", "Induan", "Olenekian", "Anisian", "Ladinian", "Carnian",
            "Norian")

#Create a vector of stage midpoints
midpoints <- c(256.6, 253, 251.6, 249.2, 244.6, 239.5, 232, 217.8)

#Read in dataset
tetrapods <- read_csv("data/Tetrapoda.csv")
glimpse(tetrapods)

#Trim data without stage assignment or species-level identification
tetrapods_trim <- tetrapods %>% filter(!is.na(stage_assignment)) %>% filter(accepted_rank == "species")
glimpse(tetrapods_trim)

#Generate a table of sample sizes by stage, ordered chronologically
totals <- count(tetrapods_trim, stage_assignment)
totals <- totals[match(stages, totals$stage_assignment),]

#Generate list of frequencies by stage, starting with total value, as needed by iNEXT
#Achieved by using and trimming the 'count' function in dplyr, across a loop of stage names
stage_freq <- list()

for (i in 1:length(stages)) {
  stage_freq[[stages[i]]] <- tetrapods_trim %>% filter(stage_assignment == stages[i]) %>%
    count(., accepted_name) %>% arrange(desc(n)) %>% add_row(n = sum(.$n), .before = 1) %>%
    select(n) %>% unlist(., use.names = F)
}

glimpse(stage_freq)

###Code from Dunne et al. 2018

#"1  Coverage-standardised diversity curve"

#Set quorum levels
quorum_levels <- round(seq(from = 0.1, to = 0.9, by = 0.1), 1)

#Estimate D using estimateD in iNEXT
estD_list <- list()

for(i in 1:length(quorum_levels)) {
  estD <- estimateD(stage_freq, datatype = "incidence_freq", base = "coverage", level = quorum_levels[i])
  estD <- filter(estD, order == 0)        #filter for species richness values
  estD$quorum_level <- quorum_levels[i]
  estD$reference_t <- totals$n            #add sample sizes in extra column
  estD$midpoints <- midpoints
  estD_list[[i]] <- estD
}

#Bind the individual dataframes into one
estD_list <- bind_rows(estD_list)

#Remove values when t is more than twice the sample size
estD_list[which(estD_list$t >= 2 * estD_list$reference_t), c("qD", "qD.LCL", "qD.UCL")] <- rep(NA, 3) #no more than twice reference sample size

View(estD_list)

#Make quorum level a factor
estD$quorum_level <- as.factor(estD$quorum_level)

#Plot quorum levels 0.4 - 0.7
ggplot(filter(estD_list, quorum_level %in% quorum_levels[4:7]), aes(x = midpoints, y = qD, ymin = qD.LCL, ymax = qD.UCL, group = quorum_level, colour = quorum_level)) +
  geom_segment(aes(x = 251.9, xend = 251.9, y = 0, yend = Inf), linetype = "longdash", colour = "black", size = 0.7) +
  geom_line() + geom_point() + geom_linerange() + scale_colour_gradientn(colours = rainbow(4)) +
  scale_x_reverse() + labs(x = "Ma", y = "qD") + theme_classic()
