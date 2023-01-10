#Environmental Data
#Taylor Lindsay
#Jan '23 

# Summary of findings
# Oxygen is significantly higher at depth (p = 2.2e-16)
# Temperature is significantly lower at depth (p = 4.541e-05)
# Light is significantly higher in shallow (p = 2.2e-16)

#           Oxygen            Temp                Light
# Units     mg/L              °C                  umol quanta m-2 s-1
# Deep      7.476 +\- 0.322   29.373 +\- 0.268    50.330 +\- 34.923 
# Shallow   7.018 +\- 0.674   29.388 +\- 0.285    225.306 +\- 101.585
# % higher  6.53%             0.051%              347.65%

# all paired t-tests, with first 100 observations removed & outliers with a z score more than 3 removed 
# light only included day time, 2 hrs after and before sunrise/sunset 
# yes the two p-values that are the same are correct! 


# Packages & Data Import  -------------------------------------------------

# Install Packages
library(tidyverse)
library(tidyr)
library(ggplot2)
library("lubridate")
library(dplyr)
library(stringr)
library(hms)
#library(ggpubr)

# Read csv ----------------------------------------------------------------

shallow_light <- read.csv('~/Desktop/GITHUB/TLPR21/Meta_Environmental_Data/LIGHT_SHALLOW_All.csv') 
deep_light <- read.csv('~/Desktop/GITHUB/TLPR21/Meta_Environmental_Data/LIGHT_DEEP_All.csv') 
temp_DO <- read.csv('~/Desktop/GITHUB/TLPR21/Meta_Environmental_Data/TLPR21_Temp_DO_Raw.csv') 
# read datetime! 
temp_DO$Datetime <- as.POSIXct( temp_DO$Datetime, tz = "UTC" )


# Light Analysis ----------------------------------------------------------

# Merge the shallow and Deep dataframes 
merged_light <- merge(x=shallow_light, y=deep_light, by = c('Date','Time'), all = TRUE)

# Separate mdy columns 
merged_light2 <- separate(merged_light,col=Date, c("Day","Month", "Year"), sep = "/", convert = TRUE)
# Separate time column
merged_light2 <- separate(merged_light2,col=Time, c("Hour","Minute", "Second"), sep = ":", convert = TRUE)

# create datetime column  
merged_light2 <- merged_light2  %>%
  mutate(., datetime = make_datetime(Year, Month, Day, Hour, Minute))

#create a differences column 
merged_light2 <- merged_light2 %>%
  mutate(., difference = .$S_LightCrrx - .$D_LightCrrx)

# Graph one variable at a time 
ggplot(merged_light2, aes(x=datetime, y=difference)) +
  geom_line() 

# ___________________________________________
#make a longer df with only the Crrx data 
merged_longer <- merged_light2 %>%
  .[c(9,13,15)] %>%
  pivot_longer(.,cols=c(D_LightCrrx,S_LightCrrx),names_to = "depth")

# graph to compare shallow and dark 
ggplot(merged_longer, aes(x=datetime, y=value, color=depth)) +
  geom_line() 

# ___________________________________________
# scatter plot across all days 
time_only <- merged_light %>%
  .[c(2,5,9)] %>%
  pivot_longer(.,cols=c(D_LightCrrx,S_LightCrrx),names_to = "depth")

ggplot(time_only, aes(x=Time, y=value, color=depth)) +
  geom_point() 

# ___________________________________________
# include only times 2hrs before and after sunrise (6am) and set (7pm)

# filter by time of day 
merged_Day <- merged_light2 %>%
  mutate(time2 = as_hms(datetime)) %>%
  filter(time2 >= hms::as_hms('08:00:00'),
         time2 <= hms::as_hms('17:00:00')) %>%
  filter(!is.na(S_LightCrrx))%>%
  filter(!is.na(D_LightCrrx))

# pivot longer to make a graph 
time_only_day <- merged_Day %>%
  .[c(9,13,17)] %>%
  pivot_longer(.,cols=c(D_LightCrrx,S_LightCrrx),names_to = "depth") 

# graph 
ggplot(time_only_day, aes(x=time2, y=value, color=depth)) +
  geom_point() 

# _____________________________________________

# calculate the daily percent higher that 
merged_Day <- merged_Day %>%
  mutate(percent_increase = (S_LightCrrx - D_LightCrrx)/D_LightCrrx*100)

merged_Day %>%
  filter(!is.na(percent_increase)) %>%
  summarise(mean(percent_increase))

# 526.5% increase 
# Light intensity
# umol quanta m-2 s-1

merged_Day %>%
  filter(!is.na(S_LightCrrx)) %>%
  summarise(mean(S_LightCrrx))

# Shallow mean = 222.41 umol quanta m-2 s-1

merged_Day %>%
  filter(!is.na(D_LightCrrx)) %>%
  summarise(mean(D_LightCrrx))

# Deep mean = 50.59 umol quanta m-2 s-1

# REAL STATS! 

time_only_day <- time_only_day %>%
  filter(!is.na(depth)) %>%
  filter(!is.na(value))

#Get summary Statistics 
group_by(time_only_day, depth) %>%
  summarise(
    count = n(),
    mean = sprintf("%0.3f",mean(value)),
    sd = sprintf("%0.3f",sd(value)))

#depth          count   mean    sd     
#<chr>          <int>   <chr>   <chr>  
#1 D_LightCrrx  1595  50.330    34.923 
#2 S_LightCrrx  1595  225.306   101.585

# Calculate p-value 
t.test(value ~ depth, data=time_only_day, paired = TRUE)

# Oxygen Analysis ---------------------------------------------------------

# get rid of DO outliers 
deep_DO <- temp_DO[,c(2,5)] %>%
  filter(!is.na(Deep_DO)) %>%
  mutate(zscore = (.$Deep_DO - mean(.$Deep_DO))/sd(.$Deep_DO)) %>%
  filter(abs(zscore)<3)
shallow_DO <- temp_DO[,c(2,3)] %>%
  filter(!is.na(Shallow_DO)) %>%
  mutate(zscore = (.$Shallow_DO - mean(.$Shallow_DO))/sd(.$Shallow_DO)) %>%
  filter(abs(zscore)<3) 
# merge the two back together 
DO <- merge(x=deep_DO[,c(1,2)], y=shallow_DO[,c(1,2)], by = "Datetime") %>%
  pivot_longer(.,cols=c(Shallow_DO,Deep_DO),names_to = "depth") %>%
  filter(!is.na(value)) %>%
  filter(!is.na(depth)) 

# quick plots 
ggplot(DO, aes(x=Datetime,y=value, color=depth)) + 
  geom_point()
ggplot(DO, aes(x=Datetime,y=value, color=depth)) + 
  geom_boxplot()

#Get summary Statistics 
group_by(DO, depth) %>%
  summarise(
    count = n(),
    mean = sprintf("%0.3f",mean(value, na.rm = TRUE)),
    sd = sprintf("%0.3f",sd(value, na.rm = TRUE)))

# Calculate p-value 
t.test(value ~ depth, data=DO, paired = TRUE)

# OXYGEN 
#depth        count   mean    sd
#<chr>        <int>   <dbl>   <dbl>
#Deep_DO      11855   7.476   0.322
#2 Shallow_DO 11855   7.018   0.674
# PVALUE = < 2.2e-16
# Oxygen is significantly higher at depth? 

# Tempreature Analysis ----------------------------------------------------

#get rid of outliers 
deep_temp <- temp_DO[,c(2,6)] %>%
  filter(!is.na(Deep_Temp)) %>%
  mutate(zscore = (.$Deep_Temp - mean(.$Deep_Temp))/sd(.$Deep_Temp)) %>%
  filter(abs(zscore)<3)
shallow_temp <- temp_DO[,c(2,4)] %>%
  filter(!is.na(Shallow_Temp)) %>%
  mutate(zscore = (.$Shallow_Temp - mean(.$Shallow_Temp))/sd(.$Shallow_Temp)) %>%
  filter(abs(zscore)<3)
#merge them back together 
TEMP <- merge(x=deep_temp[,c(1,2)], y=shallow_temp[,c(1,2)], by = "Datetime") %>%
  pivot_longer(.,cols=c(Shallow_Temp,Deep_Temp),names_to = "depth") 

# quick plots 
ggplot(TEMP, aes(x=Datetime,y=value, color=depth)) + 
  geom_point()
ggplot(TEMP, aes(x=Datetime,y=value, color=depth)) + 
  geom_boxplot() 

#Get summary Statistics 
group_by(TEMP, depth) %>%
  summarise(
    count = n(),
    mean = sprintf("%0.3f",mean(value, na.rm = TRUE)),
    sd = sprintf("%0.3f",sd(value, na.rm = TRUE)))

# Calculate p-value 
t.test(value ~ depth, data=TEMP, paired = TRUE)

# TEMPERATURE STATS 
#depth          count   mean        sd
#<chr>          <int>   <dbl>       <dbl>
#1 Deep_Temp    12013   29.373      0.268
#2 Shallow_Temp 12013   29.388      0.285
# PVALUE = 4.541e-05  
# Temperature is significantly higher in shallow water, but the confidence interval is -0.021548181 -0.007560285 






