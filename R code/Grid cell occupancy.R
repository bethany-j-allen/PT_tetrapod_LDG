#Bethany Allen   25th February 2020
#Code to calculate number of occupied grid cells (1deg cells)

#setwd("#####")

#Load packages
library(tidyverse)
library(raster)

#Be wary that some function names are used in both packages - notation is explicit in places to 
#   reduce the chance of error

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

#Filter as desired
tetrapods_trim <- tetrapods_trim %>% filter(pres_mode == "body") %>% 
  filter(taxon_environment == "terrestrial") %>% 
  filter(stage_assignment == "Induan" | stage_assignment == "Olenekian")

#Filter to unique collection numbers
tetrapods_trim <- distinct(tetrapods_trim, collection_no, .keep_all = T)

#Create grid raster with one degree cells (rather than equal area)
x_raster <- raster(ncol=360, nrow=180, xmn=-180, xmx=180, ymn=-90, ymx=90)
x_raster <- setValues(x_raster, c(1:ncell(x_raster)))

#Create empty string to add values to
cell_count <- vector(mode = "numeric", length = 0)

for (i in 1:length(labels)){
  #Filter to bin
  bin_colls <- tetrapods_trim %>% filter(paleolat_code == labels[i])
  
  #Pull coordinates into a spatial points data frame
  bin_colls <- dplyr::select(bin_colls, collection_no, Longitude, Latitude)
  if (nrow(bin_colls) > 0) 
    points <- SpatialPointsDataFrame(bin_colls[,c("Longitude", "Latitude")], bin_colls) else
      points <- NA

  #Calculate occupancy
  occupancy <- raster::extract(x_raster, points)
  if (nrow(bin_colls) > 0) 
    no_occ <- length(unique(occupancy)) else no_occ <- 0
  
  #Append to vector
  cell_count <- append(cell_count, no_occ)
}

rbind(labels, cell_count)
