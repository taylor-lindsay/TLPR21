#ImageJ Converter 
library(tidyverse)

# Command for loading cheatsheet
cheatsheet <- read.csv('~/Desktop/calice_photos/CALICE.CHEATSHEET.csv')
# Loading second sample
ImageJSample <- read.csv('~/Desktop/calice_photos/OFRA_PP_2GT7_P1.csv', na.strings=c("",".","NaN"))
# load in master data set 
master <- read.csv('~/Desktop/Github/TLPR21/Transplants_Calice_Master.csv')

#Erase .jpg files
ImageJSample$Label <- gsub(".jpg","",as.character(ImageJSample$Label))
#Merging Samples 
df <- merge(x= ImageJSample, y=cheatsheet, by= 0)
#Write new csv 
write.csv(df, '~/Desktop/calice_photos/New_OFRA_PP_2GT7_P1.csv') #######

#append the new data to the master
#the distinct command makes sure replicate lines are not being added 
master <- rbind(master, df) %>% distinct()
#save updated master
write.csv(master, '~/Desktop/Github/TLPR21/Transplants_Calice_Master.csv', row.names=FALSE)


