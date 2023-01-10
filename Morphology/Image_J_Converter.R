#ImageJ Converter 

# This version of the image j converter will go through each list and do the following 
    # Create a new column for the file name
    # merge in the cheatsheet which gives the file the C, H, W labels
    # merge them all together
    # clean up the final dataset & write it 

library(tidyverse)
library(stats)
library(data.table)

# Command for loading cheatsheet
cheatsheet <- read.csv('~/Desktop/GitHub/TLPR21/Morphology/CALICE.CHEATSHEET.csv')

# Read the list of files in each folder that are CSVs 
OFAV_ML_files = list.files(path = '~/Desktop/GitHub/TLPR21/Morphology/TLPR21_Calice_photos/OFAV_ML', pattern = "csv$", full.names = TRUE)
OFAV_T_files = list.files(path = '~/Desktop/GitHub/TLPR21/Morphology/TLPR21_Calice_photos/OFAV_T', pattern = "csv$", full.names = TRUE)
OFRA_ML_files = list.files(path = '~/Desktop/GitHub/TLPR21/Morphology/TLPR21_Calice_photos/OFRA_ML', pattern = "csv$", full.names = TRUE)
OFRA_T_files = list.files(path = '~/Desktop/GitHub/TLPR21/Morphology/TLPR21_Calice_photos/OFRA_T', pattern = "csv$", full.names = TRUE)

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
  gsub("/Users/tayrlindsay/Desktop/GitHub/TLPR21/Morphology/TLPR21_Calice_photos/OFAV_ML/", "", .) %>%
  gsub("/Users/tayrlindsay/Desktop/GitHub/TLPR21/Morphology/TLPR21_Calice_photos/OFAV_T/", "", .) %>%
  gsub("/Users/tayrlindsay/Desktop/GitHub/TLPR21/Morphology/TLPR21_Calice_photos/OFRA_ML/", "", .) %>%
  gsub("/Users/tayrlindsay/Desktop/GitHub/TLPR21/Morphology/TLPR21_Calice_photos/OFRA_T/", "", .) %>%
  gsub(".csv", "", .)
  
# Rearrange and reduce the columns 
merged2 <- merged[c(9,3:8,10:11)]

merged2

#write the Data set 
write.csv(merged2, '~/Desktop/GITHUB/TLPR21/Morphology/TLPR21_Data/TLPR21_Calice_Data.csv', row.names=FALSE)

