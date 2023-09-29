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
chla <- read.csv('~/Desktop/GITHUB/TLPR21/CHL/TLPR21_CHL_Results.csv') %>%
  select(colony_id,	chla.ug.cm2)
chla <- chla %>%
  filter(!is.na(chla.ug.cm2)) %>%
  mutate(zscore = (.$chla.ug.cm2 - mean(.$chla.ug.cm2))/sd(.$chla.ug.cm2)) %>%
  filter(abs(zscore)<3) %>%
  select(colony_id, chla.ug.cm2)

chlc2 <- read.csv('~/Desktop/GITHUB/TLPR21/CHL/TLPR21_CHL_Results.csv') %>%
  select(colony_id,	chlc2.ug.cm2)
chlc2 <- chlc2 %>%
  filter(!is.na(chlc2.ug.cm2)) %>%
  mutate(zscore = (.$chlc2.ug.cm2 - mean(.$chlc2.ug.cm2))/sd(.$chlc2.ug.cm2)) %>%
  filter(abs(zscore)<3)%>%
  select(colony_id, chlc2.ug.cm2)

#symbionts 
sym <- read.csv('~/Desktop/GITHUB/TLPR21/Symboint_Density/TLPR21_Sym_Results.csv') %>%
  select(colony_id, sym.cm2,CV) %>%
  filter(CV < 15) %>%
  select(colony_id, sym.cm2)
sym <- sym %>%
  filter(!is.na(sym.cm2)) %>%
  mutate(zscore = (.$sym.cm2 - mean(.$sym.cm2))/sd(.$sym.cm2)) %>%
  filter(abs(zscore)<3)%>%
  select(colony_id, sym.cm2)

# Protein
#prot <- read.csv('~/Desktop/GITHUB/TLPR21/Protein/TL_Trans_Protein_Results.csv')%>%
  #select(colony_id, prot_mg.cm2)
#prot <- prot %>%
  #filter(!is.na(prot_mg.cm2)) %>%
  #mutate(zscore = (.$prot_mg.cm2 - mean(.$prot_mg.cm2))/sd(.$prot_mg.cm2)) %>%
  #filter(abs(zscore)<3) %>%
  #select(colony_id, prot_mg.cm2)

#sym AFDW
#sym_AFDW <- read.csv('~/Desktop/GITHUB/TLPR21/AFDW/TL_Trans_AFDW_Results.csv')  %>%
  #select(colony_id, Sym_AFDW_mg.cm2)
#sym_AFDW <- sym_AFDW %>%
  #filter(!is.na(Sym_AFDW_mg.cm2)) %>%
  #mutate(zscore = (.$Sym_AFDW_mg.cm2 - mean(.$Sym_AFDW_mg.cm2))/sd(.$Sym_AFDW_mg.cm2)) %>%
  #filter(abs(zscore)<3)%>%
  #select(colony_id, Sym_AFDW_mg.cm2)

# sym AFDW
#host_AFDW <- read.csv('~/Desktop/GITHUB/TLPR21/AFDW/TL_Trans_AFDW_Results.csv')  %>%
  #select(colony_id, Host_AFDW_mg.cm2)
#host_AFDW <- host_AFDW %>%
  #filter(!is.na(Host_AFDW_mg.cm2)) %>%
  #mutate(zscore = (.$Host_AFDW_mg.cm2 - mean(.$Host_AFDW_mg.cm2))/sd(.$Host_AFDW_mg.cm2)) %>%
  #filter(abs(zscore)<3)%>%
  #select(colony_id, Host_AFDW_mg.cm2)


# Morphology 
morph <- read.csv('~/Desktop/GITHUB/TLPR21/Morphology/TLPR21_Morphology/TLPR21_Morphology_Results.csv')%>%
  select(!X)

di <- morph %>%
  select(colony_id,di) %>%
  filter(!is.na(di)) %>%
  mutate(zscore = (.$di - mean(.$di))/sd(.$di)) %>%
  filter(abs(zscore)<3)%>%
  select(colony_id, di)

Cdi <- morph %>%
  select(colony_id,Cdi) %>%
  filter(!is.na(Cdi)) %>%
  mutate(zscore = (.$Cdi - mean(.$Cdi))/sd(.$Cdi)) %>%
  filter(abs(zscore)<3)%>%
  select(colony_id, Cdi)

A <- morph %>%
  select(colony_id,A) %>%
  filter(!is.na(A)) %>%
  mutate(zscore = (.$A - mean(.$A))/sd(.$A)) %>%
  filter(abs(zscore)<3)%>%
  select(colony_id, A)

CA <- morph %>%
  select(colony_id,CA) %>%
  filter(!is.na(CA)) %>%
  mutate(zscore = (.$CA - mean(.$CA))/sd(.$CA)) %>%
  filter(abs(zscore)<3)%>%
  select(colony_id, CA)

D <- morph %>%
  select(colony_id,D) %>%
  filter(!is.na(D)) %>%
  mutate(zscore = (.$D - mean(.$D))/sd(.$D)) %>%
  filter(abs(zscore)<3)%>%
  select(colony_id, D)




full <- full_join(raw, AFDW)
full <- full_join(full,sym)
full <- full_join(full,chla)
full <- full_join(full,chlc2)
full <- full_join(full,D)
full <- full_join(full,di)
full <- full_join(full,Cdi)
full <- full_join(full,A)
full <- full_join(full,CA)

#write the data set 
write.csv(full, '~/Desktop/GITHUB/TLPR21/TLPR21_Results.csv')

