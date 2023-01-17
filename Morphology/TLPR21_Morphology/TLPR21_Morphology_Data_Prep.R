# Title: Morphology Data Prep
# Author: Taylor Lindsay 
# Date: 01.17.2023
# Input files:
    #  
# Output files: 
    # 
# Notes 
    # This version of the image j converter will go through each list and do the following 
    # Create a new column for the file name
    # merge in the cheatsheet which gives the file the C, H, W labels
    # merge them all together
    # clean up the final dataset & write it 


# Install Packages --------------------------------------------------------

library(tidyverse)
library(stats)
library(data.table)


# Load Data ---------------------------------------------------------------

# Command for loading cheatsheet
cheatsheet <- read.csv('~/Desktop/GitHub/TLPR21/Morphology/CALICE.CHEATSHEET.csv')

# Read the list of files in each folder that are CSVs 
OFAV_ML_files = list.files(path = '~/Desktop/GitHub/TLPR21/Morphology/TLPR21_Morphology/TLPR21_Calice_photos/OFAV_ML', pattern = "csv$", full.names = TRUE)
OFAV_T_files = list.files(path = '~/Desktop/GitHub/TLPR21/Morphology/TLPR21_Morphology/TLPR21_Calice_photos/OFAV_T', pattern = "csv$", full.names = TRUE)
OFRA_ML_files = list.files(path = '~/Desktop/GitHub/TLPR21/Morphology/TLPR21_Morphology/TLPR21_Calice_photos/OFRA_ML', pattern = "csv$", full.names = TRUE)
OFRA_T_files = list.files(path = '~/Desktop/GitHub/TLPR21/Morphology/TLPR21_Morphology/TLPR21_Calice_photos/OFRA_T', pattern = "csv$", full.names = TRUE)

meta <- read.csv('~/Desktop/GITHUB/TLPR21/TLPR21_Raw_Master.csv') %>%
  select(depth,act_depth,species,site,colony_id)

# Merge & clean up data sets ----------------------------------------------

# Append each of these lists together 
list_of_files <- c(OFAV_T_files,OFAV_ML_files,OFRA_ML_files,OFRA_T_files)

# I messed up and did not include the labels in the original data sets So here's my attempt to fix that
# This will add a column to each data frame with the label column 
labeled_list <- lapply(seq_along(list_of_files), function(x) transform(read.csv(list_of_files[x]), Label = list_of_files[x]))

# write a function that will merge each file with the cheatsheet file 
f <- function(input) {
  converted <- merge(input, cheatsheet, by = 0)
  return(converted)
}

# apply the function to all of my files 
data_with_cheatsheet = lapply(labeled_list, f)

# merged the data!
merged <- Reduce(full_join,data_with_cheatsheet)

# Get rid of the exess info in the labels
merged$Label <- merged$Label %>% 
  gsub("/Users/tayrlindsay/Desktop/GitHub/TLPR21/Morphology/TLPR21_Morphology/TLPR21_Calice_photos/OFAV_ML/", "", .) %>%
  gsub("/Users/tayrlindsay/Desktop/GitHub/TLPR21/Morphology/TLPR21_Morphology/TLPR21_Calice_photos/OFAV_T/", "", .) %>%
  gsub("/Users/tayrlindsay/Desktop/GitHub/TLPR21/Morphology/TLPR21_Morphology/TLPR21_Calice_photos/OFRA_ML/", "", .) %>%
  gsub("/Users/tayrlindsay/Desktop/GitHub/TLPR21/Morphology/TLPR21_Morphology/TLPR21_Calice_photos/OFRA_T/", "", .) %>%
  gsub(".csv", "", .)

merged2 <- merged %>% 
  mutate(colony_id = .$Label) %>%
  .[,c(12, 3,8,10:11)]

# isolate the values we want for length and area distances, then combine back together 
lengths <- merged2 %>%
  filter(MEASUREMENT == "W" | MEASUREMENT == "H" | MEASUREMENT == "CW" | MEASUREMENT == "CH") %>%
  mutate(value = .$Length) %>%
  .[,c(1,4:6)] 
areas <- merged2 %>%
  filter(MEASUREMENT == "CA" | MEASUREMENT == "A") %>%
  mutate(value = .$Area) %>%
  .[,c(1,4:6)] 
full <- rbind(lengths, areas)


# Merge with metadata -----------------------------------------------------

complete <- merge(full, meta, by = "colony_id")

#write the Data set 
write.csv(complete, '~/Desktop/GITHUB/TLPR21/Morphology/TLPR21_Morphology/TLPR21_Morphology_Merged.csv', row.names=FALSE)

