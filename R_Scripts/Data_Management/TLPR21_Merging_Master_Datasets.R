# Title: TLPR21_Data_Merge
# Author: Taylor Lindsay
# Date: 08.29.2023
# Input files:
#  TL_Trans master files & individual phys/morph data files 
# Output files: 
#   TLPR21_Results.csv 
# Notes 


# Libraries ---------------------------------------------------------------

library(tidyverse)

# Data  -------------------------------------------------------------------

# Master Dataset 
raw <- read.csv('~/Desktop/GITHUB/TLPR21/TLPR21_Raw_Master.csv') %>% 
  select(bag_number, depth, act_depth, species, colony_number, site, colony_id, airbrush_volume)

# AFDW
AFDW <- read.csv('~/Desktop/GITHUB/TLPR21/AFDW/TLPR21_AFDW_results.csv') %>%
  select(!X)

# CHL
chl <- read.csv('~/Desktop/GITHUB/TLPR21/CHL/TLPR21_CHL_Results.csv') %>%
  select(colony_id,	chla.ug.cm2,	chlc2.ug.cm2)

#symbionts 
sym <- read.csv('~/Desktop/GITHUB/TLPR21/Symboint_Density/TLPR21_Sym_Results.csv') %>%
  select(colony_id, sym.cm2,CV) %>%
  filter(CV < 15) %>%
  select(colony_id, sym.cm2)

#protein
#not done for any yet

# Morphology 
morph <- read.csv('~/Desktop/GITHUB/TLPR21/Morphology/TLPR21_Morphology/TLPR21_Morphology_Results.csv')%>%
  select(!X)

full <- full_join(raw, AFDW)
full <- full_join(full,sym)
full <- full_join(full,morph)
full <- full_join(full,chl)

#write the data set 
write.csv(full, '~/Desktop/GITHUB/TLPR21/TLPR21_Results.csv')

