#Bethany Allen   26th February 2019
#Code to plot Sun et al. conodont oxygen isotope ratios

#setwd("#####")

#Load packages
library(tidyverse)

#Read in dataset
conodonts <- read_csv("data/Oxygen isotope measurements.csv")
glimpse(conodonts)

#Plot
ggplot(conodonts, aes(x = Ages, y = delta18O, colour = Paper, shape = Depth)) + geom_point(size = 2) +
  scale_x_reverse(limits = c(260, 227)) + scale_y_reverse(position = "right", limits = c(22.5, 17)) + labs(x = "Ma", y = "delta18O") +
  geom_vline(aes(xintercept = 259.8), colour = "grey") + #Start of Wuchiapingian
  geom_vline(aes(xintercept = 254.14), colour = "grey") + #Changhsingian
  geom_vline(aes(xintercept = 252.17), colour = "grey") + #Induan
  geom_vline(aes(xintercept = 252.17), linetype = "longdash", colour = "red") + #PT
  geom_vline(aes(xintercept = 251.2), colour = "grey") + #Olenekian
  geom_vline(aes(xintercept = 247.2), colour = "grey") + #Anisian
  geom_vline(aes(xintercept = 242), colour = "grey") + #Ladinian
  geom_vline(aes(xintercept = 237), colour = "grey") + #Carnian
  geom_vline(aes(xintercept = 227), colour = "grey") + #End of Carnian
  theme_classic() + theme(legend.title=element_blank())
#Points removed from plot are outside of this time window
