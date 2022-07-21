# Calice morphology data analysis 

# Install Packages
library(tidyverse)
library(ggplot2)


# Import data 
raw_calice <- read.csv('~/Desktop/GITHUB/TLPR21/Transplants_Calice_Master.csv')

#separate info from sample ID
raw_calice <- separate(raw_calice, Label, into= c("species","treatment", "sample", "replicate"), sep = "_")

# Merge multiple calices per sample 

calice_W_means <- raw_calice %>%
  #filter(MEASUREMENT == "W") %>%
  group_by(species, treatment, sample, replicate, MEASUREMENT) %>%
  summarize(., mean_W = mean(Length))

# graph 

ggplot(calice_W_means, aes(x=Label, y=mean_W)) +
  geom_point() +
  labs(y= "Mean Calice Width", x = "Sample Name")

ggplot(calice_W_means, aes(x=treatment, y=mean_W)) + 
  geom_boxplot() +
  facet_wrap(~ MEASUREMENT)

ggsave("calice_w.jpg", plot = graph1, path = '~/Desktop')
