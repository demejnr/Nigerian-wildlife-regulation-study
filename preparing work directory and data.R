
#Clear memory----------------------------------------------------------------------------------------------------------
rm(list = ls())

#set work directory-------------------------------------------------------------------------------------------------
setwd("C:/Users/HP/Desktop/Umar_project")
getwd()


# Packages needed for this-------------------------------------------------------------
library(reshape2)
library(ggplot2)
library(plyr)
library(Rmisc)
library(likert)
library(cowplot)

# Set plot theme
mytheme <- theme_bw() + {theme(panel.border          = element_rect(fill = NA, colour = "black"), # set border around plot.
                               panel.grid.major      = element_blank(), # remove major grid lines
                               panel.grid.minor      = element_blank(), # remove minor grid lines
                               axis.line             = element_blank(), # remove axis lines
                               axis.ticks            = element_line(colour = "black"),
                               axis.text             = element_text(size = 10, colour = "black"), # axis text size
                               axis.title            = element_text(size = 10), # axis title size
                               axis.title.y          = element_text(vjust = 3), # increase distance from the y-axis
                               axis.title.x          = element_text(vjust = -1), # increase distance from the x-axis
                               panel.background      = element_rect(fill = NA),
                               plot.background       = element_rect(fill = NA, color = NA), # remove background colour
                               plot.margin           = unit(c(1, 1, 1, 1), units = , "cm"), 
                               legend.background     = element_rect(fill = NA, color = NA), # get rid of legend bg
                               legend.box.background = element_rect(fill = NA, color = NA), # get rid of legend panel bg
                               strip.text.x          = element_text(size = 10, color = "black", face = "bold"), # for facet plots
                               strip.background      = element_rect(fill = NA, color = NA))
}

#Import and explore data
dt <- read.csv("Questionnare_data.csv", header=TRUE)
head(dt)
str(dt)
names(dt)