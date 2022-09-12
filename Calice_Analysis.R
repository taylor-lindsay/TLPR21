# Calice Morphology Data Analysis 
# Written by Taylor Lindsay & Hana-lei Evans, Summer '22
# Data analysis on coral calice morphometrics from common garden experiment in Puerto Rico. 

# Packages & Data Import  -------------------------------------------------

# Install Packages
library(tidyverse)
library(ggplot2)

# Import data 
raw_calice <- read.csv('~/Desktop/GITHUB/TLPR21/Transplants_Calices_Merged.csv')

# Prepare Dataframe -------------------------------------------------------

# Create individual columns with info by separating the label column 
raw_calice <- separate(raw_calice, Label, into= c("species","treatment", "sample", "replicate"), sep = "_") %>%
  # Create column with full treatment information (species & treatment together)
  unite('full_treatment', species:treatment, remove = FALSE)

# Merge multiple calices per sample 
calice_W_means <- raw_calice %>%
  #filter(MEASUREMENT == "W") %>%
  group_by(full_treatment, species, treatment, sample, replicate, MEASUREMENT) %>%
  summarize(., mean_length = mean(Length))

# Measurement column  -----------------------------------------------------



# Example Graphs ------------------------------------------------------------------

ggplot(calice_area_means, aes(x=1:nrow(calice_W_means), y=mean_length, color=treatment)) +
  geom_point() +
  labs(y= "Mean Calice Width", x = "Sample Name") +
  facet_wrap(~ species)

ggplot(calice_W_means, aes(x=full_treatment, y=mean_length)) + 
  geom_boxplot()

ggsave("calice_w.jpg", plot = graph1, path = '~/Desktop')

# Example Analysis  ----------------------------------------------------------------

summary(calice_W_means)
tapply(calice_W_means$mean_length, calice_W_means$full_treatment, summary)

calice_area_means <- raw_calice %>%
  #filter(MEASUREMENT == "W") %>%
  group_by(species, treatment, sample, replicate, MEASUREMENT) %>%
  summarize(., mean_area = mean(Area)) %>%
  .[(.$MEASUREMENT=="CA"),] %>%
  .[!(.$species=="IMG"),]

calice_W_means <-  calice_W_means[(calice_W_means$MEASUREMENT=="W"),] %>%
  .[!(.$full_treatment=="IMG_3872"),]
calice_H_means <-  calice_means[(calice_means$MEASUREMENT=="H"),] %>%
  .[!(.$species=="IMG"),]

# T-tests!  ---------------------------------------------------------------

# To run a t-test, you have to create a linear model 

# Build linear model, the ~ indicates "by", so here we are asking it to analyse mean_length by species
model_W <- lm(mean_length ~ species, data = calice_W_means)

# To view summary statistics of the linear model, use the summary command. 
# You can also save these stats as an item 
summary(model_W)
