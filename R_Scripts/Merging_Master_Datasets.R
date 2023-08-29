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
# remove outliers
sa <- sa %>%
  filter(!is.na(surface_area)) %>%
  mutate(zscore = (.$surface_area - mean(.$surface_area))/sd(.$surface_area)) %>%
  filter(abs(zscore)<3)%>%
  select(colony_id, surface_area)

# CHLA
chla <- read.csv('~/Desktop/GITHUB/TLPR21/CHL/TL_Trans_CHL_Results.csv') %>%
  select(colony_id, chla.ug.cm2)
chla <- chla %>%
  filter(!is.na(chla.ug.cm2)) %>%
  mutate(zscore = (.$chla.ug.cm2 - mean(.$chla.ug.cm2))/sd(.$chla.ug.cm2)) %>%
  filter(abs(zscore)<3) %>%
  select(colony_id, chla.ug.cm2)

  
# CHLc2
chlc2 <- read.csv('~/Desktop/GITHUB/TLPR21/CHL/TL_Trans_CHL_Results.csv') %>%
  select(colony_id,chlc2.ug.cm2)
chlc2 <- chlc2 %>%
  filter(!is.na(chlc2.ug.cm2)) %>%
  mutate(zscore = (.$chlc2.ug.cm2 - mean(.$chlc2.ug.cm2))/sd(.$chlc2.ug.cm2)) %>%
  filter(abs(zscore)<3)%>%
  select(colony_id, chlc2.ug.cm2)

# Protein 
prot <- read.csv('~/Desktop/GITHUB/TLPR21/Protein/TL_Trans_Protein_Results.csv')%>%
  select(colony_id, prot_mg.cm2)
prot <- prot %>%
  filter(!is.na(prot_mg.cm2)) %>%
  mutate(zscore = (.$prot_mg.cm2 - mean(.$prot_mg.cm2))/sd(.$prot_mg.cm2)) %>%
  filter(abs(zscore)<3) %>%
  select(colony_id, prot_mg.cm2)

# symbiont density 
sym <- read.csv('~/Desktop/GITHUB/TLPR21/Symboint_Density/TL_Trans_Sym_Results.csv')  %>%
  select(colony_id, sym.cm2)
sym <- sym %>%
  filter(!is.na(sym.cm2)) %>%
  mutate(zscore = (.$sym.cm2 - mean(.$sym.cm2))/sd(.$sym.cm2)) %>%
  filter(abs(zscore)<3)%>%
  select(colony_id, sym.cm2)

# sym AFDW
sym_AFDW <- read.csv('~/Desktop/GITHUB/TLPR21/AFDW/TL_Trans_AFDW_Results.csv')  %>%
  select(colony_id, Sym_AFDW_mg.cm2)
sym_AFDW <- sym_AFDW %>%
  filter(!is.na(Sym_AFDW_mg.cm2)) %>%
  mutate(zscore = (.$Sym_AFDW_mg.cm2 - mean(.$Sym_AFDW_mg.cm2))/sd(.$Sym_AFDW_mg.cm2)) %>%
  filter(abs(zscore)<3)%>%
  select(colony_id, Sym_AFDW_mg.cm2)

# sym AFDW
host_AFDW <- read.csv('~/Desktop/GITHUB/TLPR21/AFDW/TL_Trans_AFDW_Results.csv')  %>%
  select(colony_id, Host_AFDW_mg.cm2)
host_AFDW <- host_AFDW %>%
  filter(!is.na(Host_AFDW_mg.cm2)) %>%
  mutate(zscore = (.$Host_AFDW_mg.cm2 - mean(.$Host_AFDW_mg.cm2))/sd(.$Host_AFDW_mg.cm2)) %>%
  filter(abs(zscore)<3)%>%
  select(colony_id, Host_AFDW_mg.cm2)

# Morphology 
A <- read.csv('~/Desktop/GITHUB/TLPR21/Morphology/TL_Trans_Morphology/TL_Trans_Morphology_Results.csv') %>%
  select(colony_id, A)
A <- A %>%
  filter(!is.na(A)) %>%
  mutate(zscore = (.$A - mean(.$A))/sd(.$A)) %>%
  filter(abs(zscore)<3)%>%
  select(colony_id, A)

CA <- read.csv('~/Desktop/GITHUB/TLPR21/Morphology/TL_Trans_Morphology/TL_Trans_Morphology_Results.csv') %>%
  select(colony_id, CA)
CA <- CA %>%
  filter(!is.na(CA)) %>%
  mutate(zscore = (.$CA - mean(.$CA))/sd(.$CA)) %>%
  filter(abs(zscore)<3)%>%
  select(colony_id, CA)

D <- read.csv('~/Desktop/GITHUB/TLPR21/Morphology/TL_Trans_Morphology/TL_Trans_Morphology_Results.csv') %>%
  select(colony_id, D)
D <- D %>%
  filter(!is.na(D)) %>%
  mutate(zscore = (.$D - mean(.$D))/sd(.$D)) %>%
  filter(abs(zscore)<3)%>%
  select(colony_id, D)

di <- read.csv('~/Desktop/GITHUB/TLPR21/Morphology/TL_Trans_Morphology/TL_Trans_Morphology_Results.csv') %>%
  select(colony_id, di)
di <- di %>%
  filter(!is.na(di)) %>%
  mutate(zscore = (.$di - mean(.$di))/sd(.$di)) %>%
  filter(abs(zscore)<3)%>%
  select(colony_id, di)

Cdi <- read.csv('~/Desktop/GITHUB/TLPR21/Morphology/TL_Trans_Morphology/TL_Trans_Morphology_Results.csv') %>%
  select(colony_id, Cdi)
Cdi <- Cdi %>%
  filter(!is.na(Cdi)) %>%
  mutate(zscore = (.$Cdi - mean(.$Cdi))/sd(.$Cdi)) %>%
  filter(abs(zscore)<3)%>%
  select(colony_id, Cdi)

# join & Write  -------------------------------------------------------------------

# join all the data 
full <- full_join(raw, sa)
full <- full_join(full, A)
full <- full_join(full, CA)
full <- full_join(full, D)
full <- full_join(full, di)
full <- full_join(full, Cdi)
full <- full_join(full, chla)
full <- full_join(full, chlc2)
full <- full_join(full, sym)
full <- full_join(full, prot)
full <- full_join(full, host_AFDW)
full <- full_join(full, sym_AFDW)
  
# make a column where chlorophyl is standardized by symbionts 
full <- mutate(full, chla.sym = chla.ug.cm2/sym.cm2)

# combine species & treatment 

full <- full %>%
  mutate(., full_treatment = paste0(species,"_",treatment)) %>%  # combine species and treatment 
  filter(recovered != "MISSING") 

# write

write.csv(full, '~/Desktop/GITHUB/TLPR21/TL_Trans_Results.csv')

