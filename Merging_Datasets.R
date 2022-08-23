#Merging Datasets
library(tidyverse)


# merge morphology dataset with master ------------------------------------

# Import data 
raw_calice <- read.csv('~/Desktop/GITHUB/CF_2022/Transplants_Calice_Master.csv')
raw_master <- read.csv('~/Desktop/GITHUB/TLPR21/Transplants_Raw_Master.csv')

# Merge the two datasets by the label 
merged <- merge(x= raw_master, y=raw_calice, by= 'Label', all=T)
NAS <- merged[is.na(c(merged$Min)),]
wrong_label <-NAS[(NAS$Airbrush_Date==""),]
need_data <-NAS[!(NAS$Species=="PRACTICE" | NAS$Airbrush_Date=="MISSING"),]

# save the file 
write.csv(need_data, '~/Desktop/Need_Data.csv')


# Cleaning morphology dataset ---------------------------------------------

# original data set had all the samples starting with 0 as O so we need to replace those 

# Clean benthic dataset
raw_calice$Label <- raw_calice$Label %>% 
  gsub("OFAV_SP_OAZ24", "OFAV_SP_0AZ24", .)  %>%
  gsub("OFAV_SP_OAZ4", "OFAV_SP_0AZ4", .)  %>%
  gsub("OFAV_SS_OAZ4", "OFAV_SS_0AZ4", .)  %>%
  gsub("OFRA_PP_OCX4", "OFRA_PP_0CX4", .)  %>%
  gsub("OFRA_PS_OCX4", "OFRA_PS_0CX4", .)  %>%
  gsub("OFRA_PP_OCX5", "OFRA_PP_0CX5", .)  %>%
  gsub("OFRA_PS_OCX5", "OFRA_PS_0CX5", .)  %>%
  gsub("OFAV_SS_ODW46", "OFAV_SS_0DW46", .)  %>%
  gsub("OFAV_SP_OEV3_P1", "OFAV_SP_0EV3_P1", .)  %>%
  gsub("OFAV_SP_OEV3_P2", "OFAV_SP_0EV3_P2", .)  %>%
  gsub("OFAV_SS_OEV3", "OFAV_SS_0EV3", .)  %>%
  gsub("OFAV_PP_OGT1", "OFAV_PP_0GT1", .)  %>%
  gsub("OFRA_PP_OGT1", "OFRA_PP_0GT1", .)  %>%
  gsub("OFRA_PS_OGT1", "OFRA_PS_0GT1", .)  %>%
  gsub("OFRA_PS_OGT3", "OFRA_PS_0GT3", .)  %>%
  gsub("OFRA_PP_OGT5", "OFRA_PP_0GT5", .)  %>%
  gsub("OFRA_PS_OGT5_P1", "OFRA_PS_0GT5_P1", .)  %>%
  gsub("OFRA_PS_OGT5_P2", "OFRA_PS_0GT5_P2", .)  %>%
  gsub("OFAV_PP_OHS3", "OFAV_PP_0HS3", .)  %>%
  gsub("OFAV_PS_OHS3", "OFAV_PS_0HS3", .)  %>%
  gsub("OFAV_SP_OEV35", "OFAV_SP_0EV35", .)  %>%
  gsub("OFAV_PS_OHS2", "OFAV_PS_0HS2", .)  %>%
  gsub("OFAV_PS_OHS12", "OFAV_PS_0HS12", .)  %>%
  gsub("OFAV_PP_OHS4", "OFAV_PP_0HS4", .)  %>%
  gsub("OFRA_PP_96T30_P2", "OFRA_PP_9GT30_P2", .)  %>%
  gsub("OFRA_PP_26T7_P2", "OFRA_PP_2GT7_P2", .)  %>%
  gsub("OFRA_PP_2673_P1", "OFRA_PP_2GT3_P1", .)  %>%
  gsub("OFAV_PS_OHS4", "OFAV_PS_0HS4", .) 



raw_master$Label <- raw_master$Label %>% 
  gsub("OFAV_SP_OHS2", "OFAV_SP_0HS2", .) %>%
  gsub("OFAV_PS_OHS2", "OFAV_PS_0HS2", .) %>%
  gsub("OFRA_PS_OCX4", "OFRA_PS_0CX4", .) %>%
  gsub("OFRA_PS_OGT1", "OFRA_PS_0GT1", .) %>%
  gsub("OFRA_PS_OCX5", "OFRA_PS_0CX5", .) 

raw_master$Sample <- raw_master$Sample %>% 
  gsub("OHS2", "0HS2", .) 
  
# rewrite the files 

write.csv(raw_calice, '~/Desktop/GITHUB/CF_2022/Transplants_Calice_Master.csv')
write.csv(raw_master, '~/Desktop/GITHUB/TLPR21/Transplants_Raw_Master.csv')
