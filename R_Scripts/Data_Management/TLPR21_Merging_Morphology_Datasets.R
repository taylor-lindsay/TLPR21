#Merging Calice Morphology Data sets 
#Written by Taylor Lindsay Summer '22

# Packages & Data Import  -------------------------------------------------
# Import Libraries
library(tidyverse)

# Import Data 
raw_calice <- read.csv('~/Desktop/GITHUB/TLPR21/Morphology/TLPR21_Morphology/TLPR21_Morphology_Merged.csv')
raw_master <- read.csv('~/Desktop/GITHUB/TLPR21/TLPR21_Raw_Master.csv')
calice_density <- read.csv('~/Desktop/GITHUB/TLPR21/Morphology/TLPR21_Morphology/TLPR21_Calice_density_all.csv')
  
#remove excess columns from raw_master if needed 
raw_master <- select(raw_master, -c('X'))
raw_calice <- select(raw_calice, -c('X'))

# rewrite the files 
write.csv(raw_calice, '~/Desktop/GITHUB/TLPR21/Transplants_Calice_Master.csv')
write.csv(raw_master, '~/Desktop/GITHUB/TLPR21/Transplants_Raw_Master.csv')


# Create summary dataset  -------------------------------------------------

#Merge multiple calices per sample
calice_W_means <- raw_calice %>% 
  filter(MEASUREMENT=="W") %>%
  group_by(colony_id) %>%
  summarise(., W = mean(value)) 

calice_H_means <- raw_calice %>% 
  filter(MEASUREMENT=="H") %>%
  group_by(colony_id) %>%
  summarise(., H = mean(value)) 

calice_CW_means <- raw_calice %>% 
  filter(MEASUREMENT=="CW") %>%
  group_by(colony_id) %>%
  summarise(., CW = mean(value)) 

calice_CH_means <- raw_calice %>% 
  filter(MEASUREMENT=="CH") %>%
  group_by(colony_id) %>%
  summarise(., CH = mean(value)) 

calice_A_means <- raw_calice %>% 
  filter(MEASUREMENT=="A") %>%
  group_by(colony_id) %>%
  summarise(., A = mean(value)) 

calice_CA_means <- raw_calice %>% 
  filter(MEASUREMENT=="CA") %>%
  group_by(colony_id) %>%
  summarise(., CA = mean(value))

calice_HW_merged <- merge(x= calice_W_means, y=calice_H_means, by= c('colony_id'), all=T)
calice_C_merged <- merge(x= calice_CW_means, y=calice_CH_means, by= c('colony_id'), all=T)
calice_A_merged <- merge(x= calice_A_means, y=calice_CA_means, by= c('colony_id'), all=T)

calice_new_merged <- merge(x= calice_HW_merged, y=calice_C_merged, by= c('colony_id'), all=T)
calice_merged_averages <-merge(x= calice_new_merged, y=calice_A_merged, by= c('colony_id'), all=T)

# I want to average the h and w together to get one diameter unit 
calice_diameter_means <- raw_calice %>% 
  filter(MEASUREMENT=="W"|MEASUREMENT=="H") %>%
  group_by(colony_id) %>%
  summarise(., di = mean(value)) 

calice_Cdiameter_means <- raw_calice %>% 
  filter(MEASUREMENT=="CW"|MEASUREMENT=="CH") %>%
  group_by(colony_id) %>%
  summarise(., Cdi = mean(value)) 

calice_diameters_merged <- merge(x=calice_diameter_means, y=calice_Cdiameter_means, by= c('colony_id'), all=T)
calice_merged_averages <-merge(x= calice_merged_averages, y=calice_diameters_merged, by= c('colony_id'), all=T)

# Make a graph to double check that these values worked out 
calice_merged_averages %>%
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
write.csv(merged2, '~/Desktop/GITHUB/TLPR21/Morphology/TLPR21_Morphology/TLPR21_Morphology_Results.csv')



