# Title: Merging Master Datasets
# Author: Taylor Lindsay
# Date: 03.06.2023
# Input files:
#  TL_Trans master files & phys/morph data files 
# Output files: 
#   TL_Trans_Results.csv 
# Notes 


# Libraries ---------------------------------------------------------------

library(tidyverse)

# Data  -------------------------------------------------------------------

# Master Dataset 
raw <- read.csv('~/Desktop/GITHUB/TLPR21/TL_Trans_Raw_Master.csv') %>% 
  select(colony_id, species, treatment, recovered, airbrush_volume)

# Surface Area 
sa <- read.csv('~/Desktop/GITHUB/TLPR21/Surface_Area/TL_Trans_Surface_Area.csv') %>%
  select(colony_id, surface_area)

# Morphology 
morph <- read.csv('~/Desktop/GITHUB/TLPR21/Morphology/TL_Trans_Morphology_Results.csv') 

# CHL
chl <- read.csv('~/Desktop/GITHUB/TLPR21/CHL/TL_Trans_CHL_Results.csv') %>%
  select(colony_id, chla.ug.cm2, chlc2.ug.cm2)

# Protein 
prot <- read.csv('~/Desktop/GITHUB/TLPR21/Protein/TL_Trans_Protein_Results.csv')%>%
  select(colony_id, prot_mg.cm2)

# symbiont density 
sym <- read.csv('~/Desktop/GITHUB/TLPR21/Symboint_Density/TL_Trans_Sym_Results.csv')  %>%
  select(colony_id, sym.cm2)

# join & Write  -------------------------------------------------------------------

# join all the data 
full <- full_join(raw, sa)
full <- full_join(full, morph)
full <- full_join(full, chl)
full <- full_join(full, sym)
full <- full_join(full, prot)
  
# combine species & treatment 

full <- full %>%
  mutate(., full_treatment = paste0(species,"_",treatment)) %>%  # combine species and treatment 
  filter(recovered != "MISSING") 

full_final <- full[,c(1:3,18, 7:13, 5:6, 14:17)]

# write

write.csv(full_final, '~/Desktop/GITHUB/TLPR21/TL_Trans_Results.csv')
