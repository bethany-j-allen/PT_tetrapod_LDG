#Bethany Allen   19th June 2018
#Code to run SQS on tetrapod data (from a csv) with genus level IDs and stage level dating

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

#Trim data without stage assignment or genus-level identification
tetrapods_trim <- tetrapods %>% filter(!is.na(stage_assignment)) %>% filter(!is.na(genus))
glimpse(tetrapods_trim)

#Generate a table of sample sizes by stage, ordered chronologically
totals <- count(tetrapods_trim, stage_assignment)
totals <- totals[match(stages, totals$stage_assignment),]

#Generate list of frequencies by stage, starting with total value, as needed by iNEXT
#Achieved by using and trimming the 'count' function in dplyr, across a loop of stage names
stage_freq <- list()

for (i in 1:length(stages)) {
  stage_freq[[stages[i]]] <- tetrapods_trim %>% filter(stage_assignment == stages[i]) %>%
    count(., genus) %>% arrange(desc(n)) %>% add_row(n = sum(.$n), .before = 1) %>%
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


#Emma's plot
estD_plot <- ggplot(filter(estD_list, quorum_level %in% quorum_levels[4:7]), aes(x = midpoints, y = qD, ymin = qD.LCL, ymax = qD.UCL, colour = quorum_level, lty = method)) +
  geom_segment(aes(x = 251.9, xend = 251.9, y = 0, yend = Inf), linetype = "longdash", colour = "grey80", size = 0.7) +
  geom_line(size = 1.1) +
  geom_point(aes(pch = method), size = 4.5) +
  scale_shape_manual(values = c(15, 16, 17)) +
  scale_colour_manual(values = rev(c("grey20", "grey40", "grey60", "grey75"))) +
  scale_x_reverse(expand = c(0,0), limits = c(257, 215)) +
  scale_y_continuous(breaks = c(10, 50, 100, 150), limits = c(0, 150), expand = c(0,0)) +
  labs(x = "", y = "Coverage rarefied richness (log scale)") +
  theme(panel.background = element_blank(),
        panel.grid.minor.y = element_line(colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 12),
        aspect.ratio = 1)

#Display plot
estD_plot


#"2   Coverage-based rarefaction for each interval"

#Run iNEXT
incidences <- iNEXT(stage_freq, q = 0, datatype = "incidence_freq", knots = 100)

#Extract iNEXT estimates
rare <- incidences$iNextEst

#Add column of stage names
for(i in 1:length(rare)) {
  rare[[i]]$stage_int <- names(rare)[i]
}

#Convert to tibble to help plotting
rare <- do.call(rbind, rare) %>% tbl_df()

#Plot rarefaction curve
ggplot(rare, aes(x = SC, y = qD, group = stage_int, colour = stage_int)) + geom_line() +
  labs(x = "Coverage (Good's u)", y = "Species Diversity") + theme_classic()

#Emma's plot
rare_plot <- ggplot(rare, aes(x = SC, y = qD, ymin = qD.LCL, ymax = qD.UCL, colour = stage_int, lty = method)) +
  geom_line(size = 1) +
  scale_linetype_manual(values = c("dotted", "solid", "dotdash")) +
  #scale_colour_manual(values = c("20B2AA", "brown2")) +
  geom_point(data = filter(rare, method == "observed"), aes(x = SC, y = qD), size = 3, inherit.aes = F) +
  theme(panel.background = element_blank(),
        panel.grid.minor.y = element_line(colour = "grey90"),
        panel.grid.minor.x = element_line(colour = "grey90"),
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 12, angle = 0),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 14)) +
  labs(x = "Coverage", y = "Species richness")
  #scale_x_continuous = (limits = c(0, 1), expand = c(0,0), breaks = seq(0, 1, 0.25)) +
  #scale_y_continuous = (limits = c(0, 180), expand = c(0,0), breaks = seq(0, 180, 30))

#Display plot
rare_plot
