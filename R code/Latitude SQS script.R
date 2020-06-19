#Bethany Allen   3rd July 2018
#Code to run SQS on tetrapod data (from a csv) with species level IDs, on the basis of palaeolatitude

#setwd("#####")

#Load packages
library(tidyverse)
library(iNEXT)

#Read in dataset
tetrapods <- read_csv("data/Tetrapoda.csv")
glimpse(tetrapods)

#Trim data without stage assignment or species-level identification
tetrapods_trim <- tetrapods %>% filter(!is.na(stage_assignment)) %>% filter(accepted_rank == "species")


#Add column which indicates latitude bin
bins <- seq(from = -90, to = 90, by = 30)
labels <- seq(from = -75, to = 75, by = 30)
tetrapods_trim <- mutate(tetrapods_trim, paleolat_code = cut(paleolat, breaks = bins, labels = labels))

#Check for NAs
count(tetrapods_trim, paleolat_code)

#Filter data to time bins
LPtetrapods <- filter(tetrapods_trim, stage_assignment == "Wuchiapingian" | stage_assignment == "Changhsingian")
ETtetrapods <- filter(tetrapods_trim, stage_assignment == "Induan" | stage_assignment == "Olenekian")
MTtetrapods <- filter(tetrapods_trim, stage_assignment == "Anisian" | stage_assignment == "Ladinian")

#Generate a table of sample sizes by latitude bin, ordered numerically, and remove NAs
#latbins <- c("D", "C", "B", "A", "a", "b", "c", "d")
#latbinmids <- c(105, 75, 45, 15, -15, -45, -75, -105)

LPtotals <- count(LPtetrapods, paleolat_code)
ETtotals <- count(ETtetrapods, paleolat_code)
ETtotals <- ETtotals[1:5,]
MTtotals <- count(MTtetrapods, paleolat_code)

#Generate list of frequencies by stage, starting with total value, as needed by iNEXT
#Achieved by using and trimming the 'count' function in dplyr, across a loop of stage names

#Late Permian -
LPlat_freq <- list()
for (i in 1:length(labels)) {
  LPlat_freq[[i]] <- LPtetrapods %>% filter(paleolat_code == labels[i]) %>%
    count(., accepted_name) %>% arrange(desc(n)) %>% add_row(n = sum(.$n), .before = 1) %>%
    select(n) %>% unlist(., use.names = F)
}
glimpse(LPlat_freq)
names(LPlat_freq) <- labels
LPlat_freq[6] <- NULL

#Early Triassic -
ETlat_freq <- list()
for (i in 1:length(labels)) {
  ETlat_freq[[i]] <- ETtetrapods %>% filter(paleolat_code == labels[i]) %>% 
    count(., accepted_name) %>% arrange(desc(n)) %>% add_row(n = sum(.$n), .before = 1) %>%
    select(n) %>% unlist(., use.names = F)
}
glimpse(ETlat_freq)
names(ETlat_freq) <- labels
ETlat_freq[6] <- NULL

#Middle Triassic - "C" is 0, and is removed
MTlat_freq <- list()
for (i in 1:length(labels)) {
  MTlat_freq[[i]] <- MTtetrapods %>% filter(paleolat_code == labels[i]) %>%
    count(., accepted_name) %>% arrange(desc(n)) %>% add_row(n = sum(.$n), .before = 1) %>%
    select(n) %>% unlist(., use.names = F)
}
glimpse(MTlat_freq)
MTlat_freq[6] <- NULL


#Set quorum levels
quorum_levels <- round(seq(from = 0.1, to = 0.9, by = 0.1), 1)

#Late Permian
#Estimate D using estimateD in iNEXT
LPestD_list <- list()

for(i in 1:length(quorum_levels)) {
  LPestD <- estimateD(LPlat_freq, datatype = "incidence_freq", base = "coverage", level = quorum_levels[i])
  LPestD <- filter(LPestD, order == 0)        #filter for species richness values
  LPestD$quorum_level <- quorum_levels[i]
  LPestD$reference_t <- LPtotals$n            #add sample sizes in extra column
  LPestD$midpoints <- LPtotals$paleolat_code
  LPestD_list[[i]] <- LPestD
}

#Bind the individual dataframes into one
LPestD_list <- bind_rows(LPestD_list)

#Remove values when t is more than twice the sample size
LPestD_list[which(LPestD_list$t >= 2 * LPestD_list$reference_t), c("qD", "qD.LCL", "qD.UCL")] <- rep(NA, 3) #no more than twice reference sample size

View(LPestD_list)

#Make quorum level a factor
LPestD_list$midpoints <- as.numeric(as.character(LPestD_list$midpoints))
#LPestD_list$quorum_level <- as.factor(LPestD_list$quorum_level)

#Early Triassic
#Estimate D using estimateD in iNEXT
ETestD_list <- list()

for(i in 1:length(quorum_levels)) {
  ETestD <- estimateD(ETlat_freq, datatype = "incidence_freq", base = "coverage", level = quorum_levels[i])
  ETestD <- filter(ETestD, order == 0)        #filter for species richness values
  ETestD$quorum_level <- quorum_levels[i]
  ETestD$reference_t <- ETtotals$n            #add sample sizes in extra column
  ETestD$midpoints <- ETtotals$paleolat_code
  ETestD_list[[i]] <- ETestD
}

#Bind the individual dataframes into one
ETestD_list <- bind_rows(ETestD_list)

#Remove values when t is more than twice the sample size
ETestD_list[which(ETestD_list$t >= 2 * ETestD_list$reference_t), c("qD", "qD.LCL", "qD.UCL")] <- rep(NA, 3) #no more than twice reference sample size

View(ETestD_list)

#Make quorum level a factor
ETestD_list$midpoints <- as.numeric(as.character(ETestD_list$midpoints))
ETestD$quorum_level <- as.factor(ETestD$quorum_level)

#Middle Triassic
#Estimate D using estimateD in iNEXT
MTestD_list <- list()

for(i in 1:length(quorum_levels)) {
  MTestD <- estimateD(MTlat_freq, datatype = "incidence_freq", base = "coverage", level = quorum_levels[i])
  MTestD <- filter(MTestD, order == 0)        #filter for species richness values
  MTestD$quorum_level <- quorum_levels[i]
  MTestD$reference_t <- MTtotals$n            #add sample sizes in extra column
  MTestD$midpoints <- MTtotals$paleolat_code
  MTestD_list[[i]] <- MTestD
}

#Bind the individual dataframes into one
MTestD_list <- bind_rows(MTestD_list)

#Remove values when t is more than twice the sample size
MTestD_list[which(MTestD_list$t >= 2 * MTestD_list$reference_t), c("qD", "qD.LCL", "qD.UCL")] <- rep(NA, 3) #no more than twice reference sample size

View(MTestD_list)

#Make quorum level a factor
MTestD_list$midpoints <- as.numeric(as.character(MTestD_list$midpoints))
MTestD$quorum_level <- as.factor(MTestD$quorum_level)


#Plot Late Permian, quorum levels 0.4 - 0.7
ggplot(filter(LPestD_list, quorum_level %in% quorum_levels[4:7]), aes(x = midpoints, y = qD, ymin = qD.LCL, ymax = qD.UCL, group = quorum_level, colour = quorum_level)) +
  geom_line(size = 1) + geom_point() + geom_linerange() + labs(x = "Palaeolatitude", y = "Subsampled diversity", colour = "Quorum level") + 
  coord_flip() + scale_x_continuous(limits = c(-75, 75)) +
  geom_vline(aes(xintercept = 0), colour = "white", size = 0.7) +
  scale_colour_gradient(low = "cyan", high = "darkblue") + theme_black() 

#Plot Early Triassic, quorum levels 0.4 - 0.7
ggplot(filter(ETestD_list, quorum_level %in% quorum_levels[4:7]), aes(x = midpoints, y = qD, ymin = qD.LCL, ymax = qD.UCL, group = quorum_level, colour = quorum_level)) +
  geom_line(size = 1) + geom_point() + geom_linerange() + labs(x = "Palaeolatitude", y = "Subsampled diversity", colour = "Quorum level") + 
  coord_flip() + scale_x_continuous(limits = c(-75, 75)) +
  geom_vline(aes(xintercept = 0), colour = "white", size = 0.7) + 
  scale_colour_gradient(low = "darkorange", high = "red") + theme_black()

#Plot Middle Triassic, quorum levels 0.4 - 0.7
ggplot(filter(MTestD_list, quorum_level %in% quorum_levels[4:7]), aes(x = midpoints, y = qD, ymin = qD.LCL, ymax = qD.UCL, group = quorum_level, colour = quorum_level)) +
  geom_line(size = 1) + geom_point() + geom_linerange() + labs(x = "Palaeolatitude", y = "Subsampled diversity", colour = "Quorum level") + 
  coord_flip() + scale_x_continuous(limits = c(-75, 75)) +
  geom_vline(aes(xintercept = 0), colour = "white", size = 0.7) + 
  scale_colour_gradient(low = "gold", high = "forestgreen") + theme_black()
