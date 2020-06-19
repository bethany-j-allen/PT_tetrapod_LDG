#Erin Saupe   downloaded 27th November 2018
#[Code to palaeo-rotate modern coordinates into palaeocoordinates via Gplates]
#1. Code to convert modern coordinates into a shapefile for Gplates
#2. Code to convert Gplates shapefile output into palaeocoordinates

#Rotated as follows:
#Wuchiapingian & Changhsingian -> Map #50 "Late Permian(Lopingian, 255.7Ma)"
#Induan -> Map #49 "Permo-Triassic boundary (251Ma)"
#Olenekian -> Map #48 "Early Triassic (Induan & Olenekian, 248.5Ma)"
#Anisian -> Map #47 "Middle Triassic (Anisian, 241.5Ma)"
#Ladinian -> Map #46 "Middle Triassic (Ladinian, 232.9Ma)"

#setwd("#####")

#Load packages
library(tidyverse)
library(sp)
library(rgdal)
library(rgeos)
library(maptools)

###Part 1. Convert modern lat-longs to a shape file
#Read in csv file with the present-day lat/long coordinates
dat <- read.csv("data/tetrapod_database_final.csv", stringsAsFactors = FALSE)
dat <- data.frame(dat)

#Lopingian
#Create data frame with collection numbers, longs, lats
LopingianFilter <- filter(dat, stage_assignment == "Wuchiapingian" | stage_assignment == "Changhsingian")
LopingianPoints <- LopingianFilter[c("collection_no", "lng", "lat")]

#Filter to unique points (one per collection)
LopingianPoints <- distinct(LopingianPoints, collection_no, .keep_all = T)
Lopingian_s2xy <- LopingianPoints[c("lng", "lat")]

#Label as coordinates
coordinates(Lopingian_s2xy) <- ~ lng + lat

#Convert to 'Spatial Points Data Frame'
Lopingian_shp <- SpatialPointsDataFrame(Lopingian_s2xy, LopingianPoints, proj4string = CRS("+proj=longlat +datum=WGS84"))

#Write shape file
writeOGR(Lopingian_shp, "data/Modern shapefiles", "Lopingian", driver="ESRI Shapefile")


#Induan
#Create data frame with collection numbers, longs, lats
InduanFilter <- filter(dat, stage_assignment == "Induan")
InduanPoints <- InduanFilter[c("collection_no", "lng", "lat")]

#Filter to unique points (one per collection)
InduanPoints <- distinct(InduanPoints, collection_no, .keep_all = T)
Induan_s2xy <- InduanPoints[c("lng", "lat")]

#Label as coordinates
coordinates(Induan_s2xy) <- ~ lng + lat

#Convert to 'Spatial Points Data Frame'
Induan_shp <- SpatialPointsDataFrame(Induan_s2xy, InduanPoints, proj4string = CRS("+proj=longlat +datum=WGS84"))

#Write shape file
writeOGR(Induan_shp, "data/Modern shapefiles", "Induan", driver="ESRI Shapefile")


#Olenekian
#Create data frame with collection numbers, longs, lats
OlenekianFilter <- filter(dat, stage_assignment == "Olenekian")
OlenekianPoints <- OlenekianFilter[c("collection_no", "lng", "lat")]

#Filter to unique points (one per collection)
OlenekianPoints <- distinct(OlenekianPoints, collection_no, .keep_all = T)
Olenekian_s2xy <- OlenekianPoints[c("lng", "lat")]

#Label as coordinates
coordinates(Olenekian_s2xy) <- ~ lng + lat

#Convert to 'Spatial Points Data Frame'
Olenekian_shp <- SpatialPointsDataFrame(Olenekian_s2xy, OlenekianPoints, proj4string = CRS("+proj=longlat +datum=WGS84"))

#Write shape file
writeOGR(Olenekian_shp, "data/Modern shapefiles", "Olenekian", driver="ESRI Shapefile")


#Anisian
#Create data frame with collection numbers, longs, lats
AnisianFilter <- filter(dat, stage_assignment == "Anisian")
AnisianPoints <- AnisianFilter[c("collection_no", "lng", "lat")]

#Filter to unique points (one per collection)
AnisianPoints <- distinct(AnisianPoints, collection_no, .keep_all = T)
Anisian_s2xy <- AnisianPoints[c("lng", "lat")]

#Label as coordinates
coordinates(Anisian_s2xy) <- ~ lng + lat

#Convert to 'Spatial Points Data Frame'
Anisian_shp <- SpatialPointsDataFrame(Anisian_s2xy, AnisianPoints, proj4string = CRS("+proj=longlat +datum=WGS84"))

#Write shape file
writeOGR(Anisian_shp, "data/Modern shapefiles", "Anisian", driver="ESRI Shapefile")


#Ladinian
#Create data frame with collection numbers, longs, lats
LadinianFilter <- filter(dat, stage_assignment == "Ladinian")
LadinianPoints <- LadinianFilter[c("collection_no", "lng", "lat")]

#Filter to unique points (one per collection)
LadinianPoints <- distinct(LadinianPoints, collection_no, .keep_all = T)
Ladinian_s2xy <- LadinianPoints[c("lng", "lat")]

#Label as coordinates
coordinates(Ladinian_s2xy) <- ~ lng + lat

#Convert to 'Spatial Points Data Frame'
Ladinian_shp <- SpatialPointsDataFrame(Ladinian_s2xy, LadinianPoints, proj4string = CRS("+proj=longlat +datum=WGS84"))

#Write shape file
writeOGR(Ladinian_shp, "data/Modern shapefiles", "Ladinian", driver="ESRI Shapefile")


###Part 2. Convert shape file back to palaeo lat-longs
#Read shape file, convert to data frame, write as csv
dat <- read.csv("data/tetrapod_database_final.csv", stringsAsFactors = FALSE)
dat <- data.frame(dat)

#Lopingian
LopingianFilter <- filter(dat, stage_assignment == "Wuchiapingian" | stage_assignment == "Changhsingian")
LopingianPoints <- LopingianFilter[c("collection_no", "lng", "lat")]

#Filter to unique points (one per collection)
LopingianPoints <- distinct(LopingianPoints, collection_no, .keep_all = T)

Lopingian_shp <- readOGR("data/Lopingian/reconstructed_255.70Ma.shp")
Lopingian_dat <- data.frame(Lopingian_shp)
Lopingian_s2xy <- Lopingian_dat[c("coords.x1", "coords.x2")]
Lopingian_s2xy$collection_no <- LopingianPoints$collection_no


#Induan
InduanFilter <- filter(dat, stage_assignment == "Induan")
InduanPoints <- InduanFilter[c("collection_no", "lng", "lat")]

#Filter to unique points (one per collection)
InduanPoints <- distinct(InduanPoints, collection_no, .keep_all = T)

Induan_shp <- readOGR("data/Induan/reconstructed_251.00Ma.shp")
Induan_dat <- data.frame(Induan_shp)
Induan_s2xy <- Induan_dat[c("coords.x1", "coords.x2")]
Induan_s2xy$collection_no <- InduanPoints$collection_no


#Olenekian
OlenekianFilter <- filter(dat, stage_assignment == "Olenekian")
OlenekianPoints <- OlenekianFilter[c("collection_no", "lng", "lat")]

#Filter to unique points (one per collection)
OlenekianPoints <- distinct(OlenekianPoints, collection_no, .keep_all = T)

Olenekian_shp <- readOGR("data/Olenekian/reconstructed_248.50Ma.shp")
Olenekian_dat <- data.frame(Olenekian_shp)
Olenekian_s2xy <- Olenekian_dat[c("coords.x1", "coords.x2")]
Olenekian_s2xy$collection_no <- OlenekianPoints$collection_no


#Anisian
AnisianFilter <- filter(dat, stage_assignment == "Anisian")
AnisianPoints <- AnisianFilter[c("collection_no", "lng", "lat")]

#Filter to unique points (one per collection)
AnisianPoints <- distinct(AnisianPoints, collection_no, .keep_all = T)

Anisian_shp <- readOGR("data/Anisian/reconstructed_241.50Ma.shp")
Anisian_dat <- data.frame(Anisian_shp)
Anisian_s2xy <- Anisian_dat[c("coords.x1", "coords.x2")]
Anisian_s2xy$collection_no <- AnisianPoints$collection_no


#Ladinian
LadinianFilter <- filter(dat, stage_assignment == "Ladinian")
LadinianPoints <- LadinianFilter[c("collection_no", "lng", "lat")]

#Filter to unique points (one per collection)
LadinianPoints <- distinct(LadinianPoints, collection_no, .keep_all = T)

Ladinian_shp <- readOGR("data/Ladinian/reconstructed_232.90Ma.shp")
Ladinian_dat <- data.frame(Ladinian_shp)
Ladinian_s2xy <- Ladinian_dat[c("coords.x1", "coords.x2")]
Ladinian_s2xy$collection_no <- LadinianPoints$collection_no


#Compile
AllPoints <- rbind(Lopingian_s2xy, Induan_s2xy, Olenekian_s2xy, Anisian_s2xy, Ladinian_s2xy)

#Write a csv of points
write.csv(AllPoints, "data/rotated_points.csv")

#Match points to collection numbers
new_db <- left_join(dat, AllPoints, by = "collection_no")

#Write a new csv with the paleolat & long added
write_csv(new_db, path = "data/tetrapod_database_final.csv")
