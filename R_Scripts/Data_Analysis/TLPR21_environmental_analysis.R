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
library(ggpubr)

# Edit & Prep files ----------------------------------------------------------------
# Temp & DO
temp_DO <- read.csv('~/Desktop/GITHUB/TLPR21/Meta_Environmental_Data/TLPR21_Temp_DO_Raw.csv') 
temp_DO <- temp_DO %>% mutate(datetime = mdy_hms(Datetime)) %>% select(!Datetime)  # Fix datetimes
temp_DO$Date <- as_date(temp_DO$datetime)


# Light 
light_shallow <- read.csv('~/Desktop/GITHUB/TLPR21/Meta_Environmental_Data/LIGHT_SHALLOW_ALL.csv') %>% select(!c(SENSOR,old_date))
light_deep <- read.csv('~/Desktop/GITHUB/TLPR21/Meta_Environmental_Data/LIGHT_DEEP_ALL.csv') %>% select(!c(SENSOR, old_date))

light <- rbind(light_shallow, light_deep)
light$Date <- as_date(light$Date)
light$datetime <- as.POSIXct(paste(light$Date, light$Time), format = "%Y-%m-%d %H:%M:%S")

ggplot(light, aes(x=datetime, y=LightRaw)) +
  geom_point()

#write_csv(light, '~/Desktop/GitHub/TLPR21/Meta_Environmental_Data/TL_TRANS_Light_Final.csv')

# Combine Files 
merged <- merge(light, temp_DO, all=TRUE) %>%
  select(datetime, Date, Time, depth, LightCrrx, DO_mg.L, Temp_C)

write_csv(merged, '~/Desktop/GitHub/TLPR21/Meta_Environmental_Data/TL_TRANS_Enviro_all.csv')


# Create daytime light column  --------------------------------------------
# include only times 1hr before and after sunrise (6am) and set (7pm)

# filter by time of day 
merged$Time <- as_hms(merged$Time)
merged$daylight <- ifelse(merged$Time >= hms::as_hms('5:00:00') & merged$Time <= hms::as_hms('19:00:00'), paste(merged$LightCrrx), NA) %>%
  as.numeric(merged$daylight)  

#plot by date 
ggplot(merged, aes(x=datetime, y=daylight)) + 
  geom_point()

# plot by time of day 
ggplot(merged, aes(x=Time, y=daylight)) + 
  geom_point()

# Daily Means Graphs ----------------------------------------------------------

#merged <- read.csv('~/Desktop/GITHUB/TLPR21/Meta_Environmental_Data/TL_TRANS_Enviro_all.csv') 
#merged$datetime <- as.POSIXct(merged$datetime, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
                             
# daily means 
daily_light <- merged %>%
  group_by(Date,depth) %>%
  summarize(mean_light = mean(LightCrrx, na.rm = TRUE))

daily_DO <- merged %>%
  group_by(Date,depth) %>%
  summarize(mean_DO = mean(DO_mg.L, na.rm = TRUE))

daily_temp <- merged %>%
  group_by(Date,depth) %>%
  summarize(mean_temp = mean(Temp_C, na.rm = TRUE))

daily_daylight <- merged %>%
  group_by(Date,depth) %>%
  summarize(mean_daylight = mean(daylight, na.rm = TRUE))

# graph daily values 
ggplot(daily_light, aes(x=Date, y=mean_light, color=depth)) + 
  geom_point()

ggplot(daily_temp, aes(x=Date, y=mean_temp, color=depth)) + 
  geom_point()

ggplot(daily_DO, aes(x=Date, y=mean_DO, color=depth)) + 
  geom_point()

ggplot(daily_daylight, aes(x=Date, y=mean_daylight, color=depth)) + 
  geom_point()

# Boxplot 
merged_longer <- merged %>%
  pivot_longer(!c(datetime, Date, Time, depth), names_to = "measurement", values_to = "value")

ggplot(merged_longer, aes(x=depth, y=value)) + 
  geom_boxplot() + 
  stat_compare_means(method = "t.test") + 
  facet_wrap(~measurement,scales = "free")

# summary tables ----------------------------------------------------------

means <- merged %>%
  group_by(depth) %>%
  summarize_all(funs(mean(!is.na(.)))) 



