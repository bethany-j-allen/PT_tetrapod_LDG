#Bethany Allen   4th September 2019
#Code to plot LDGs using modern bird, mammal and amphibian data

#setwd("#####")

#Load packages
library(tidyverse)

#Read in dataset
birds <- read_csv("data/Bird.csv")
mammals <- read_csv("data/Mammal.csv")
amphibs <- read_csv("data/Amphibian.csv")

ggplot(birds, aes(x = y, y = mean, group = 1)) +
  geom_line(size = 1, colour = "blue") + geom_point(size = 2, colour = "blue") +
  labs(x = "Palaeolatitude", y = "Raw occurrence count") + coord_flip() +
  scale_x_continuous(limits = c(-90, 90)) + geom_vline(aes(xintercept = 0), colour = "black") + 
  theme_classic()

ggplot(mammals, aes(x = y, y = mean, group = 1)) +
  geom_line(size = 1, colour = "red") + geom_point(size = 2, colour = "red") +
  labs(x = "Palaeolatitude", y = "Raw occurrence count") + coord_flip() +
  scale_x_continuous(limits = c(-90, 90)) + geom_vline(aes(xintercept = 0), colour = "black") + 
  theme_classic()

ggplot(amphibs, aes(x = y, y = mean, group = 1)) +
  geom_line(size = 1, colour = "limegreen") + geom_point(size = 2, colour = "limegreen") +
  labs(x = "Palaeolatitude", y = "Raw occurrence count") + coord_flip() +
  scale_x_continuous(limits = c(-90, 90)) + geom_vline(aes(xintercept = 0), colour = "black") + 
  theme_classic()
