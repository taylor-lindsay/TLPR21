# figuring out which CHL samples are missing 


# Import Libraries
library(tidyverse)

# Import Data 
raw_master <- read.csv('~/Desktop/GITHUB/TLPR21/Transplants_Raw_Master.csv')
CHL_Sampled <-  read.csv('~/Desktop/Missing_CHL.csv')

CHL2 <- unique(CHL_Sampled)


x<- CHL2$Label
y <- raw_master$Label

setdiff(x,y)
