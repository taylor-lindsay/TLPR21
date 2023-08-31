#Merging Calice Morphology Data sets 
#Written by Taylor Lindsay Summer '22

# Packages & Data Import  -------------------------------------------------
# Import Libraries
library(tidyverse)

# Import Data 
raw_calice <- read.csv('~/Desktop/GITHUB/TLPR21/Morphology/TL_Trans_Morphology/TL_Trans_Morphology_Raw.csv')
raw_master <- read.csv('~/Desktop/GITHUB/TLPR21/TL_Trans_Raw_Master.csv')
calice_density <- read.csv('~/Desktop/GITHUB/TLPR21/Morphology/TL_Trans_Morphology/TL_Trans_Calice_Density.csv')

# Cleaning morphology dataset ---------------------------------------------

# Change "O" to "0" in sample labels CALICE FILE
raw_calice$colony_id <- raw_calice$colony_id %>% 
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

# Change "O" to "0" in sample labels MASTER FILE
raw_master$Label <- raw_master$Label %>% 
  gsub("OFAV_SP_OHS2", "OFAV_SP_0HS2", .) %>%
  gsub("OFAV_PS_OHS2", "OFAV_PS_0HS2", .) %>%
  gsub("OFRA_PS_OCX4", "OFRA_PS_0CX4", .) %>%
  gsub("OFRA_PS_OGT1", "OFRA_PS_0GT1", .) %>%
  gsub("OFRA_PS_OCX5", "OFRA_PS_0CX5", .) 

raw_master$Sample <- raw_master$Sample %>% 
  gsub("OHS2", "0HS2", .) 
#remove .jpg
raw_calice$Label <- raw_calice$Label %>% 
  gsub(".JPG", "", .)

  
#remove excess columns from raw_master if needed 
raw_master <- select(raw_master, -c('X'))
raw_calice <- select(raw_calice, -c('X'))

# rewrite the files 
write.csv(raw_calice, '~/Desktop/GITHUB/TLPR21/Transplants_Calice_Master.csv')
write.csv(raw_master, '~/Desktop/GITHUB/TLPR21/Transplants_Raw_Master.csv')


# Create summary dataset  -------------------------------------------------

#Seperating ID's
raw_calice2 <-separate(raw_calice, colony_id, into= c("species", "treatment", "sample", "replicate"), sep = "_", remove = FALSE,)

#combining treatment and species values
raw_calice2 <- raw_calice2 %>%
  unite('full_treatment', species:treatment, remove= FALSE)

#Merge multiple calices per sample
calice_W_means <- raw_calice2 %>% 
  filter(MEASUREMENT=="W") %>%
  group_by(colony_id,full_treatment,species,treatment, sample, replicate) %>%
  summarise(., W = mean(Length)) 

calice_H_means <- raw_calice2 %>% 
  filter(MEASUREMENT=="H") %>%
  group_by(colony_id,full_treatment,species,treatment, sample, replicate) %>%
  summarise(., H = mean(Length)) 

calice_CW_means <- raw_calice2 %>% 
  filter(MEASUREMENT=="CW") %>%
  group_by(colony_id,full_treatment,species,treatment, sample, replicate) %>%
  summarise(., CW = mean(Length)) 

calice_CH_means <- raw_calice2 %>% 
  filter(MEASUREMENT=="CH") %>%
  group_by(colony_id,full_treatment,species,treatment, sample, replicate) %>%
  summarise(., CH = mean(Length)) 

calice_A_means <- raw_calice2 %>% 
  filter(MEASUREMENT=="A") %>%
  group_by(colony_id,full_treatment,species,treatment, sample, replicate) %>%
  summarise(., A = mean(Area)) 

calice_CA_means <- raw_calice2 %>% 
  filter(MEASUREMENT=="CA") %>%
  group_by(colony_id,full_treatment,species,treatment, sample, replicate) %>%
  summarise(., CA = mean(Area))



calice_HW_merged <- merge(x= calice_W_means, y=calice_H_means, by= c('colony_id','full_treatment','species','treatment','sample','replicate'), all=T)
calice_C_merged <- merge(x= calice_CW_means, y=calice_CH_means, by= c('colony_id','full_treatment','species','treatment','sample','replicate'), all=T)
calice_A_merged <- merge(x= calice_A_means, y=calice_CA_means, by= c('colony_id','full_treatment','species','treatment','sample','replicate'), all=T)

calice_new_merged <- merge(x= calice_HW_merged, y=calice_C_merged, by= c('colony_id','full_treatment','species','treatment','sample','replicate'), all=T)
calice_merged_averages <-merge(x= calice_new_merged, y=calice_A_merged, by= c('colony_id','full_treatment','species','treatment','sample','replicate'), all=T)

# I want to average the h and w together to get one diameter unit 
calice_diameter_means <- raw_calice2 %>% 
  filter(MEASUREMENT=="W"|MEASUREMENT=="H") %>%
  group_by(colony_id,full_treatment,species,treatment, sample, replicate) %>%
  summarise(., di = mean(Length)) 

calice_Cdiameter_means <- raw_calice2 %>% 
  filter(MEASUREMENT=="CW"|MEASUREMENT=="CH") %>%
  group_by(colony_id,full_treatment,species,treatment, sample, replicate) %>%
  summarise(., Cdi = mean(Length)) 

calice_diameters_merged <- merge(x=calice_diameter_means, y=calice_Cdiameter_means, by= c('colony_id','full_treatment','species','treatment','sample','replicate'), all=T)
calice_merged_averages <-merge(x= calice_merged_averages, y=calice_diameters_merged, by= c('colony_id','full_treatment','species','treatment','sample','replicate'), all=T)

# Make a graph to double check that these values worked out 
calice_merged_averages %>%
  filter(.,full_treatment=="OFAV_PP") %>%
  ggplot()+ 
  geom_point(aes(x=colony_id, y=W)) + 
  geom_point(aes(x=colony_id, y=H), color="red") + 
  geom_point(aes(x=colony_id,y=di), color="blue")
  
# make a column that has the ratio of the columella area and calice area 
calice_merged_averages <- calice_merged_averages %>%
  mutate(A.CA = A / CA)

# Adding Calice density into the calice averages data sheet
merged2 <- merge(x=calice_merged_averages, y=calice_density[,c("colony_id","D")], by= "colony_id")

# Write the csv data 
write.csv(merged2, '~/Desktop/GITHUB/TLPR21/Morphology/TL_Trans_Morphology/TL_Trans_Morphology_Results.csv')


# Figure out which ones are missing  --------------------------------------

#subset the samples with NA values 
NAS <- merged[is.na(c(merged$Min)),]
#Look for samples with no value for airbrush date 
wrong_label <-NAS[(NAS$Airbrush_Date==""),]
#Save file of the ones that are missing data 
need_data <-NAS[!(NAS$Species=="PRACTICE" | NAS$Airbrush_Date=="MISSING"),]
# save the file 
write.csv(need_data, '~/Desktop/Need_Data.csv')












# XXXX DONT USE: TLPR21 Morphology data managment  ----------------------------------------
TLPR21_raw_calice <- read.csv('~/Desktop/GITHUB/TLPR21/Morphology/TLPR21_Data/TLPR21_Calice_Data.csv') 
TLPR21_meta <- read.csv('~/Desktop/GITHUB/TLPR21/TLPR21_Raw_Master.csv') 

# fix raw calice data sets 
# Rename Column so we can merge 
TLPR21_raw_calice <- TLPR21_raw_calice %>%
  rename("Sample_ID" = "Label")

#Merge multiple calices per sample
calice_W_means <- TLPR21_raw_calice  %>% 
  filter(MEASUREMENT=="W") %>%
  group_by(Sample_ID,MEASUREMENT) %>%
  summarise(., value = mean(Length)) 

calice_H_means <- TLPR21_raw_calice %>% 
  filter(MEASUREMENT=="H") %>%
  group_by(Sample_ID,MEASUREMENT) %>%
  summarise(., value = mean(Length)) 

calice_CW_means <- TLPR21_raw_calice %>% 
  filter(MEASUREMENT=="CW") %>%
  group_by(Sample_ID,MEASUREMENT) %>%
  summarise(., value = mean(Length)) 

calice_CH_means <- TLPR21_raw_calice %>% 
  filter(MEASUREMENT=="CH") %>%
  group_by(Sample_ID,MEASUREMENT) %>%
  summarise(., value = mean(Length)) 

calice_A_means <- TLPR21_raw_calice %>% 
  filter(MEASUREMENT=="A") %>%
  group_by(Sample_ID,MEASUREMENT) %>%
  summarise(., value = mean(Area)) 

calice_CA_means <- TLPR21_raw_calice %>% 
  filter(MEASUREMENT=="CA") %>%
  group_by(Sample_ID,MEASUREMENT) %>%
  summarise(., value = mean(Area))

TLPR21_Calice_means <- rbind(calice_CA_means,calice_A_means,calice_CH_means,calice_CW_means,calice_H_means,calice_W_means)

# Merge the datasets 
TLPR21_calice_merged <- merge(x= TLPR21_Calice_means, y=TLPR21_meta[3:8], by= "Sample_ID")

# Save 
write.csv(TLPR21_calice_merged, '~/Desktop/GITHUB/TLPR21/Morphology/TLPR21_Data/TLPR21_Calice_Merged.csv')

