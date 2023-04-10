# Title: TL_Trans_AFDW.R
# Author: Taylor Lindsay
# Date: 04.09.2023
# Input files:
#  TL_Trans_AFDW.csv
# Output files: 
# 
# Notes 

# Packages & Data Import --------------------------------------------------

# Install Packages
library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)

#AFDW data
AFDW <- read.csv('~/Desktop/GITHUB/TLPR21/AFDW/TL_TRANS_AFDW.csv') %>%
  select(colony_id,sym_host,AFDW)

# Load homogenate volume
vol <- read_csv("~/Desktop/GITHUB/TLPR21/TL_Trans_Raw_Master.csv") %>%                                              #####
select(colony_id, airbrush_volume) %>%
  filter(!is.na(airbrush_volume))

# Load surface area
sa <- read_csv("~/Desktop/GITHUB/TLPR21/Surface_Area/TL_Trans_Surface_Area.csv") %>%                                #####
select(colony_id, surface_area) %>%
  filter(!is.na(surface_area))

# Merge & Edit Data -------------------------------------------------------

#separate sym and host data 


Sym <- AFDW %>%
  filter(.,sym_host=="SYM") %>%
  filter(.,AFDW>0)%>%
  setNames(c("colony_id","sym_host","AFDW_sym")) %>%
  .[,c(1,3)]

Host <- AFDW %>%
  filter(.,sym_host=="HOST") %>%
  filter(.,AFDW>0)%>%
  setNames(c("colony_id","sym_host","AFDW_host")) %>%
  .[,c(1,3)]

#merge two together
merged1 <- full_join(Sym,Host)
#merge with metadata 
AFDW_Merge <- full_join(sa,merged1, by="colony_id")
AFDW_Merge <- full_join(vol,AFDW_Merge, by="colony_id")

# Original data was g/ml

AFDW_fin <- AFDW_Merge %>%
  mutate(Sym_AFDW_mg.cm2 = ((as.numeric(AFDW_sym) * as.numeric(airbrush_volume) / as.numeric(surface_area))*1000)) %>%
  mutate(Host_AFDW_mg.cm2 = ((as.numeric(AFDW_host) * as.numeric(airbrush_volume) / as.numeric(surface_area))*1000))

# just the columns I want 
AFDW_small <- AFDW_fin[,c(1,6,7)]

# write the file 
write.csv(AFDW_small, '~/Desktop/GITHUB/TLPR21/AFDW/TL_Trans_AFDW_Results.csv')

