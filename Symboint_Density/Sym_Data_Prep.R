# Title: 
# Author:
# Date: 
# Input files:
#  
# Output files: 
# 
# Notes

# Libraries ---------------------------------------------------------------

library(tidyverse)

# Data -------------------------------------------------------------------

#raw symbiont data 
sym <- read.csv('~/Desktop/GITHUB/TLPR21/Symboint_Density/TL_Trans_Sym_D_Counts.csv')

# Load homogenate volume
vol <- read_csv("~/Desktop/GITHUB/TLPR21/TL_Trans_Raw_Master.csv") %>%                                              #####
select(colony_id, airbrush_volume) %>%
  filter(!is.na(airbrush_volume))

# Load surface area
sa <- read_csv("~/Desktop/GITHUB/TLPR21/Surface_Area/TL_Trans_Surface_Area.csv") %>%                                #####
select(colony_id, surface_area) %>%
  filter(!is.na(surface_area))

# standardize -------------------------------------------------------------------
# Join DF with homogenate 
sym <- full_join(sym, vol)

# Join df with surface area 
sym <- full_join(sym, sa)

# Multiply chlorophyll by the homogenate volume and divide by surface area
sym <- sym %>%
  filter(!is.na(surface_area)) %>%
  mutate(sym.cm2 = average_per_square * as.numeric(airbrush_volume) / as.numeric(surface_area))

# just the columns I want 
sym_small <- sym[,c(1,23)]

# write the file 
write.csv(sym_small, '~/Desktop/GITHUB/TLPR21/Symboint_Density/TL_Trans_Sym_Results.csv')    


