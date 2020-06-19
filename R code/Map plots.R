#Bethany Allen   12th June 2018 (edited 12th November 2018, 31st January 2019)
#Code to plot maps of the PBDB tetrapod data (from a csv)
#Current code plots ALL occurrences

#setwd("#####")

#Load packages
library(tidyverse)
library(jpeg)

#Create a vector giving the chronological order of stages
stages <- c("Wuchiapingian", "Changhsingian", "Induan", "Olenekian", "Anisian", "Ladinian",
            "Carnian")

#Create a vector of stage midpoints
midpoints <- c(256.6, 253, 251.6, 249.2, 244.6, 239.5, 232)

#Create a vector giving the chronological order of substages
substages <- c("Wuchiapingian", "Changhsingian", "Griesbachian", "Dienerian", "Smithian", "Spathian",
               "Aegean-Bithynian", "Pelsonian-Illyrian", "Fassanian", "Longobardian", "Julian",
               "Tuvalian")

#Create a vector of substage midpoints
submidpoints <- c(256.6, 253, 251.7, 251.4, 250.2, 248.2, 245.9, 243.3, 240.8, 238.3, 234.5, 229.5)

#Read in dataset
tetrapods <- read_csv("data/tetrapod_database_ScotesePLL.csv")
glimpse(tetrapods)

#Wrangling
#Check no incorrect spelling
#unique(tetrapods$stage_assignment)
#unique(tetrapods$substage_assignment)

#Remove synonymy repeats (combinations of the same collection no. AND accepted name)
tetrapods <- distinct(tetrapods, accepted_name, collection_no, .keep_all = T)


###Plot data resolution using Scotese modern raster###
highres_tetrapods <- tetrapods %>% filter(!is.na(substage_assignment)) %>%
  filter(accepted_rank == "species")
#Label data as high resolution (T) or not (F)
tetrapods$highres <- tetrapods$occurrence_no %in% highres_tetrapods$occurrence_no

#Plot all localities
ModernMap <- readJPEG("data/Map1a PALEOMAP Paleoatlas_000.jpg")
ggplot(tetrapods, aes(x = lng, y = lat, group = highres, colour = highres)) + xlim(c(-180, 180)) + ylim(c(-90, 90)) +
  annotation_custom(grid::rasterGrob(ModernMap, width=unit(0.9, "npc"), height=unit(0.9, "npc"), just = "centre")) +
  geom_segment(aes(x = -180, xend = 180, y = 0, yend = 0), linetype = "solid", colour = "black") +
  geom_point(size = 2, alpha = 0.7) + scale_colour_manual(values = c("red", "blue"))

#Plot distribution of latitudes between high and low-res data
ggplot(tetrapods, aes(x = lat, group = highres, colour = highres)) + geom_freqpoly(size = 2, binwidth = 5) +
  geom_vline(aes(xintercept = 0), colour = "black") + coord_flip() +
  scale_colour_manual(values = c("red", "blue")) + theme_classic()


###Plot using Scotese palaeo-rasters, LP/ET/MT###
LopingianMap <- readJPEG("data/LP trace_shade.jpg")
LopingianPoints <- filter(tetrapods, stage_assignment == "Wuchiapingian"| stage_assignment == "Changhsingian")
ggplot(LopingianPoints, aes(x = Longitude, y = Latitude, group = pres_mode, colour = pres_mode)) + xlim(c(-180, 180)) + ylim(c(-90, 90)) +
  annotation_custom(grid::rasterGrob(LopingianMap, width=unit(0.9, "npc"), height=unit(0.9, "npc"), just = "centre")) +
  geom_segment(aes(x = -180, xend = 180, y = 0, yend = 0), linetype = "solid", colour = "black") +
  geom_point(size = 3) + scale_colour_manual(values = c("limegreen", "orange")) +
  guides(fill = F, colour = F) + theme_classic()

ETMap <- readJPEG("data/ET trace_shade.jpg")
ETPoints <- filter(tetrapods, stage_assignment == "Induan" | stage_assignment == "Olenekian")
ggplot(ETPoints, aes(x = Longitude, y = Latitude, group = interaction(pres_mode, taxon_environment), colour = interaction(pres_mode, taxon_environment))) + xlim(c(-180, 180)) + ylim(c(-90, 90)) +
  annotation_custom(grid::rasterGrob(ETMap, width=unit(0.9, "npc"), height=unit(0.9, "npc"), just = "centre")) +
  geom_segment(aes(x = -180, xend = 180, y = 0, yend = 0), linetype = "solid", colour = "black") +
  geom_point(size = 3) + scale_colour_manual(values = c("blue", "limegreen", "orange")) +
  guides(fill = F, colour = F) + theme_classic()

MTMap <- readJPEG("data/MT trace_shade.jpg")
MTPoints <- filter(tetrapods, stage_assignment == "Anisian" | stage_assignment == "Ladinian")
ggplot(MTPoints, aes(x = Longitude, y = Latitude, group = interaction(pres_mode, taxon_environment), colour = interaction(pres_mode, taxon_environment))) + xlim(c(-180, 180)) + ylim(c(-90, 90)) +
  annotation_custom(grid::rasterGrob(MTMap, width=unit(0.9, "npc"), height=unit(0.9, "npc"), just = "centre")) +
  geom_segment(aes(x = -180, xend = 180, y = 0, yend = 0), linetype = "solid", colour = "black") +
  geom_point(size = 3) + scale_colour_manual(values = c("blue", "limegreen", "orange")) + 
  guides(fill = F, colour = F) + theme_classic()


###Plot using Scotese palaeo-rasters, each stage###
LopingianMap <- readJPEG("data/Map50a LtP Lopingian_255.jpg")
WuchiapingianPoints <- filter(tetrapods, stage_assignment == "Wuchiapingian")
ggplot(WuchiapingianPoints, aes(x = Longitude, y = Latitude, group = pres_mode, colour = pres_mode)) + xlim(c(-180, 180)) + ylim(c(-90, 90)) +
  annotation_custom(grid::rasterGrob(LopingianMap, width=unit(0.9, "npc"), height=unit(0.9, "npc"), just = "centre")) +
  geom_segment(aes(x = -180, xend = 180, y = 0, yend = 0), linetype = "solid", colour = "black") +
  geom_point(size = 2) + scale_colour_manual(values = c("limegreen", "orange")) +
  guides(fill = F, colour = F) + theme_classic()

ChanghsingianPoints <- filter(tetrapods, stage_assignment == "Changhsingian")
ggplot(ChanghsingianPoints, aes(x = Longitude, y = Latitude, group = pres_mode, colour = pres_mode)) + xlim(c(-180, 180)) + ylim(c(-90, 90)) +
  annotation_custom(grid::rasterGrob(LopingianMap, width=unit(0.9, "npc"), height=unit(0.9, "npc"), just = "centre")) +
  geom_segment(aes(x = -180, xend = 180, y = 0, yend = 0), linetype = "solid", colour = "black") +
  geom_point(size = 2) + scale_colour_manual(values = c("limegreen", "orange")) +
  guides(fill = F, colour = F) + theme_classic()

InduanMap <- readJPEG("data/Map49a Permo-Triassic Boundary_250.jpg")
InduanPoints <- filter(tetrapods, stage_assignment == "Induan")
ggplot(InduanPoints, aes(x = Longitude, y = Latitude, group = interaction(pres_mode, taxon_environment), colour = interaction(pres_mode, taxon_environment))) + xlim(c(-180, 180)) + ylim(c(-90, 90)) +
  annotation_custom(grid::rasterGrob(InduanMap, width=unit(0.9, "npc"), height=unit(0.9, "npc"), just = "centre")) +
  geom_segment(aes(x = -180, xend = 180, y = 0, yend = 0), linetype = "solid", colour = "black") +
  geom_point(size = 2) + scale_colour_manual(values = c("limegreen", "orange")) + 
  guides(fill = F, colour = F) + theme_classic()

OlenekianMap <- readJPEG("data/Map48a ETr Induan-Olenekian_245.jpg")
OlenekianPoints <- filter(tetrapods, stage_assignment == "Olenekian")
ggplot(OlenekianPoints, aes(x = Longitude, y = Latitude, group = interaction(pres_mode, taxon_environment), colour = interaction(pres_mode, taxon_environment))) + xlim(c(-180, 180)) + ylim(c(-90, 90)) +
  annotation_custom(grid::rasterGrob(OlenekianMap, width=unit(0.9, "npc"), height=unit(0.9, "npc"), just = "centre")) +
  geom_segment(aes(x = -180, xend = 180, y = 0, yend = 0), linetype = "solid", colour = "black") +
  geom_point(size = 2) + scale_colour_manual(values = c("blue", "limegreen", "orange")) + 
  guides(fill = F, colour = F) + theme_classic()

AnisianMap <- readJPEG("data/Map47a MTr Anisian_240.jpg")
AnisianPoints <- filter(tetrapods, stage_assignment == "Anisian" | stage_assignment == "Ladinian")
ggplot(AnisianPoints, aes(x = Longitude, y = Latitude, group = interaction(pres_mode, taxon_environment), colour = interaction(pres_mode, taxon_environment))) + xlim(c(-180, 180)) + ylim(c(-90, 90)) +
  annotation_custom(grid::rasterGrob(AnisianMap, width=unit(0.9, "npc"), height=unit(0.9, "npc"), just = "centre")) +
  geom_segment(aes(x = -180, xend = 180, y = 0, yend = 0), linetype = "solid", colour = "black") +
  geom_point(size = 2) + scale_colour_manual(values = c("blue", "limegreen", "orange")) +
  guides(fill = F, colour = F) + theme_classic()

LadinianMap <- readJPEG("data/Map46a MTr Ladinian_230.jpg")
LadinianPoints <- filter(tetrapods, stage_assignment == "Anisian" | stage_assignment == "Ladinian")
ggplot(LadinianPoints, aes(x = Longitude, y = Latitude, group = interaction(pres_mode, taxon_environment), colour = interaction(pres_mode, taxon_environment))) + xlim(c(-180, 180)) + ylim(c(-90, 90)) +
  annotation_custom(grid::rasterGrob(LadinianMap, width=unit(0.9, "npc"), height=unit(0.9, "npc"), just = "centre")) +
  geom_segment(aes(x = -180, xend = 180, y = 0, yend = 0), linetype = "solid", colour = "black") +
  geom_point(size = 2) + scale_colour_manual(values = c("blue", "limegreen", "orange")) +
  guides(fill = F, colour = F) + theme_classic()
