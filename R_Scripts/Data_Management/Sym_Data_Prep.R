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
sym <- read.csv('~/Desktop/GITHUB/TLPR21/Symboint_Density/TLPR21_Sym_D_Counts.csv') %>%
  select(colony_id,mean_sym,CV) %>%
  filter(mean_sym !="#DIV/0!") 

# Load homogenate volume
vol <- read_csv("~/Desktop/GITHUB/TLPR21/TLPR21_Raw_Master.csv") %>%                                              #####
select(colony_id, airbrush_volume) %>%
  filter(!is.na(airbrush_volume)) 


# Load surface area
sa <- read_csv("~/Desktop/GITHUB/TLPR21/Surface_Area/TLPR21_Surface_Area.csv") %>%                                #####
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
  mutate(sym.cm2 = as.numeric(mean_sym) * as.numeric(airbrush_volume) / as.numeric(surface_area))

sym_small <- sym %>%
  select(colony_id,sym.cm2,CV)

# write the file 
write.csv(sym_small, '~/Desktop/GITHUB/TLPR21/Symboint_Density/TLPR21_Sym_Results.csv')    


