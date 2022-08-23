# Calice morphology data analysis 

# Install Packages
library(tidyverse)
library(ggplot2)

# Import data 
raw_calice <- read.csv('~/Desktop/GITHUB/CF_2022/Transplants_Calice_Master.csv')

#separate info from sample ID
raw_calice <- separate(raw_calice, Label, into= c("species","treatment", "sample", "replicate"), sep = "_")

# Merge multiple calices per sample 

calice_means <- raw_calice %>%
  #filter(MEASUREMENT == "W") %>%
  group_by(species, treatment, sample, replicate, MEASUREMENT) %>%
  summarize(., mean_length = mean(Length))

calice_area_means <- raw_calice %>%
  #filter(MEASUREMENT == "W") %>%
  group_by(species, treatment, sample, replicate, MEASUREMENT) %>%
  summarize(., mean_area = mean(Area)) %>%
  .[(.$MEASUREMENT=="CA"),] %>%
  .[!(.$species=="IMG"),]

calice_W_means <-  calice_means[(calice_means$MEASUREMENT=="W"),] %>%
  .[!(.$species=="IMG"),]
calice_H_means <-  calice_means[(calice_means$MEASUREMENT=="H"),] %>%
  .[!(.$species=="IMG"),]

### add steps: only do the above step for length measurements then do a seperate one for area 

# graph 

ggplot(calice_area_means, aes(x=1:nrow(calice_W_means), y=mean_length, color=treatment)) +
  geom_point() +
  labs(y= "Mean Calice Width", x = "Sample Name") +
  facet_wrap(~ species)

ggplot(calice_H_means, aes(x=treatment, y=mean_length)) + 
  geom_boxplot() + 
  facet_wrap(~ species)

ggsave("calice_w.jpg", plot = graph1, path = '~/Desktop')
