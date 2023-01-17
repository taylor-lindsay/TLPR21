# Title: Calice Analysis
# Author: Taylor Lindsay, Hana-lei Evans 
# Date: Summer '22
# Input files:
    #  raw calice data 
# Output files: 
    # 
# Notes 


# Calice Morphology Data Analysis 
# Written by Taylor Lindsay & Hana-lei Evans, Summer '22
# Data analysis on coral calice morphometrics from common garden experiment in Puerto Rico. 

# Packages & Data Import  -------------------------------------------------

# Install Packages
library(tidyverse)
library(tidyr)
library(ggplot2)
library("gridExtra")
library("cowplot")
library(ggpubr)
library("RColorBrewer")

# TL_Trans Import & first boxplots ------------------------------------------------------------------

# Import data 
raw_calice <- read.csv('~/Desktop/GITHUB/TLPR21/Transplants_Calice_Averages.csv') 


#width 
plot_W <- ggplot(raw_calice, aes(x=Treatment, y=mean_W, color=Species)) +
  geom_boxplot() +
  labs(y= "Mean Width", x = "Treatment")
#Columnella width 
plot_CW <- ggplot(raw_calice, aes(x=Treatment, y=mean_CW, color=Species)) +
  geom_boxplot() +
  labs(y= "Mean Width", x = "Treatment")
#combine widths 
plots_W <- plot_grid(plot_W, plot_CW, labels=c("A", "B"), ncol = 2, nrow = 1) 
#save 
ggsave("plot_calice_W.jpg", plot = plots_W, path = '~/Desktop/GITHUB/TLPR21/Morphology/TL_Trans_Calice_Plots')


#Height 
plot_H <- ggplot(raw_calice, aes(x=Treatment, y=mean_H, color=Species)) +
  geom_boxplot() +
  labs(y= "Mean Height", x = "Treatment")
#Columnella height
plot_CH <- ggplot(raw_calice, aes(x=Treatment, y=mean_CH, color=Species)) +
  geom_boxplot() +
  labs(y= "Mean Height", x = "Treatment")
#combine widths 
plots_H <- plot_grid(plot_H, plot_CH, labels=c("A", "B"), ncol = 2, nrow = 1) 
#save 
ggsave("plot_calice_H.jpg", plot = plots_H, path = '~/Desktop/GITHUB/TLPR21/Morphology/TL_Trans_Calice_Plots')

#Area
plot_A <- ggplot(raw_calice, aes(x=Treatment, y=mean_A, color=Species)) +
  geom_boxplot() +
  labs(y= "Mean Area", x = "Treatment")
#Columnella area
plot_CA <- ggplot(raw_calice, aes(x=Treatment, y=mean_CA, color=Species)) +
  geom_boxplot() +
  labs(y= "Mean Area", x = "Treatment")
#combine widths 
plots_A <- plot_grid(plot_A, plot_CA, labels=c("A", "B"), ncol = 2, nrow = 1) 
#save 
ggsave("plot_calice_A.jpg", plot = plots_A, path = '~/Desktop/GITHUB/TLPR21/Morphology/TL_Trans_Calice_Plots')

#Density
plot_D <- ggplot(raw_calice, aes(x=Treatment, y=Calice_Density, color=Species)) +
  geom_boxplot() +
  labs(y= "Calice Density", x = "Treatment")
#save 
ggsave("plot_calice_D.jpg", plot = plot_D, path = '~/Desktop/GITHUB/TLPR21/Morphology/TL_Trans_Calice_Plots')

# TL_Trans OFRA Graphs -------------------------------------------------------------

#filter to just have OFRA data 
raw_OFRA <- raw_calice %>%
  filter(Species == "OFRA") %>%
  filter(Treatment == c("PP", "PS"))

#make dataset long 
raw_OFRA_long <- pivot_longer(raw_OFRA,cols = c(mean_W, mean_CW, mean_H, mean_CH, mean_A, mean_CA),
                       names_to = "measurement")

# replace col names to be more appropriate for graph 
raw_OFRA_long$measurement <- raw_OFRA_long$measurement %>%
  gsub("mean_W","W", .) %>%
  gsub("mean_H","H", .) %>%  
  gsub("mean_A","A", .) %>%
  gsub("mean_CW","CW", .) %>%
  gsub("mean_CH","CH", .) %>%
  gsub("mean_CA","CA", .) 

measurement_order <- c('A', 'CA', 'H', 'CH', 'W', 'CW') 

#make graph of calice measurements 
plots_OFRA <- ggplot(raw_OFRA_long, aes(x=factor(measurement, level = measurement_order), y=value, fill = Treatment)) +
  geom_boxplot() +
  labs(y= "Mean Width", x = "Treatment")+
  theme_bw() +
  scale_fill_manual(values = c("cyan4",  "goldenrod"))

plots_OFRA

#save graph
ggsave("plot_OFRA_Calice.jpg", plot = plots_OFRA, path = '~/Desktop/GITHUB/TLPR21/Morphology/TL_Trans_Calice_Plots')

# compare means
plots_OFRA_pvalues <- plots_OFRA + stat_compare_means(method = "t.test", size = 2)
ggsave("plot_OFRA_pvalues.jpg", plot = plots_OFRA_pvalues, path = '~/Desktop/GITHUB/TLPR21/Morphology/TL_Trans_Calice_Plots')

#compare Density of OFRA
plot_D_OFRA <- ggplot(raw_OFRA, aes(x=Treatment, y=Calice_Density)) +
  geom_boxplot() +
  labs(y= "Calice Density", x = "Treatment")
plots_OFRA_D_pvalues <- plot_D_OFRA + stat_compare_means(method = "t.test", size = 2)
ggsave("plot_OFRA_D_pvalues.jpg", plot = plots_OFRA_D_pvalues, path = '~/Desktop/GITHUB/TLPR21/Morphology/TL_Trans_Calice_Plots')



# TL_Trans OFAV Control Graphs -----------------------------------------------------------

#filter to just have OFAV data 
raw_OFAV_control <- raw_calice %>%
  filter(Species == "OFAV") %>%
  filter(Treatment == c("PP", "SS"))

#make dataset long 
raw_OFAV_control_long <- pivot_longer(raw_OFAV_control,cols = c(mean_W, mean_CW, mean_H, mean_CH, mean_A, mean_CA),
                              names_to = "measurement")

# replace col names to be more appropriate for graph 
raw_OFAV_control_long$measurement <- raw_OFAV_control_long$measurement %>%
  gsub("mean_W","W", .) %>%
  gsub("mean_H","H", .) %>%  
  gsub("mean_A","A", .) %>%
  gsub("mean_CW","CW", .) %>%
  gsub("mean_CH","CH", .) %>%
  gsub("mean_CA","CA", .) 

#make graph of calice measurements 
plots_OFAV_control <- ggplot(raw_OFAV_control_long, aes(x=factor(measurement, level = measurement_order), y=value, fill=Treatment)) +
  geom_boxplot() +
  labs(y= "Mean Width", x = "Treatment") +
  theme_bw() +
  scale_fill_manual(values = c("cyan4", "goldenrod"))

plots_OFAV_control

# compare means
plots_OFAV_control_pvalues <- plots_OFAV_control + stat_compare_means(method = "t.test", size = 2)
ggsave("plot_OFAV_control_pvalues.jpg", plot = plots_OFAV_control_pvalues, path = '~/Desktop/GITHUB/TLPR21/Morphology/TL_Trans_Calice_Plots')

#compare Density of OFAV Controls
plot_D_OFAV_control <- ggplot(raw_OFAV_control, aes(x=Treatment, y=Calice_Density)) +
  geom_boxplot() +
  labs(y= "Calice Density", x = "Treatment")
plot_OFAV_control_D_pvalues <- plot_D_OFAV_control + stat_compare_means(method = "t.test", size = 2)
ggsave("plot_OFAV_control_D_pvalues.jpg", plot = plots_OFAV_control_D_pvalues, path = '~/Desktop/GITHUB/TLPR21/Morphology/TL_Trans_Calice_Plots')





# TL_Trans OFAV PP PS --------------------------------------------------------------

#filter to just have OFAV data 
raw_OFAV_PPvPS <- raw_calice %>%
  filter(Species == "OFAV") %>%
  filter(Treatment == c("PP", "PS"))

#make dataset long 
raw_OFAV_PPvPS_long <- pivot_longer(raw_OFAV_PPvPS,cols = c(mean_W, mean_CW, mean_H, mean_CH, mean_A, mean_CA),
                                      names_to = "measurement")

# replace col names to be more appropriate for graph 
raw_OFAV_PPvPS_long$measurement <- raw_OFAV_PPvPS_long$measurement %>%
  gsub("mean_W","W", .) %>%
  gsub("mean_H","H", .) %>%  
  gsub("mean_A","A", .) %>%
  gsub("mean_CW","CW", .) %>%
  gsub("mean_CH","CH", .) %>%
  gsub("mean_CA","CA", .) 

#make graph of calice measurements 
plots_OFAV_PPvPS <- ggplot(raw_OFAV_PPvPS_long, aes(x=factor(measurement, level = measurement_order), y=value, fill=Treatment)) +
  geom_boxplot() +
  labs(y= "Mean Width", x = "Treatment")+
  theme_bw() +
  scale_fill_manual(values = c("cyan4", "goldenrod"))




# compare means
plots_OFAV_PPvPS_pvalues <- plots_OFAV_PPvPS + stat_compare_means(method = "t.test", size = 2)
ggsave("plot_OFAV_PPvPS_pvalues.jpg", plot = plots_OFAV_PPvPS_pvalues, path = '~/Desktop/GITHUB/TLPR21/Morphology/TL_Trans_Calice_Plots')


#compare Density of OFAV Controls
plot_D_OFAV_PPvPS <- ggplot(raw_OFAV_PPvPS, aes(x=Treatment, y=Calice_Density)) +
  geom_boxplot() +
  labs(y= "Calice Density", x = "Treatment")
plot_D_OFAV_PPvPS_pvalues <- plot_D_OFAV_PPvPS + stat_compare_means(method = "t.test", size = 2)
ggsave("plot_OFAV_PPvPS_D_pvalues.jpg", plot = plot_D_OFAV_PPvPS_pvalues, path = '~/Desktop/GITHUB/TLPR21/Morphology/TL_Trans_Calice_Plots')



# TL_Trans OFAV SS SP --------------------------------------------------------------

#filter to just have OFAV data 
raw_OFAV_SSvSP <- raw_calice %>%
  filter(Species == "OFAV") %>%
  filter(Treatment == c("SS", "SP"))

#make dataset long 
raw_OFAV_SSvSP_long <- pivot_longer(raw_OFAV_SSvSP,cols = c(mean_W, mean_CW, mean_H, mean_CH, mean_A, mean_CA),
                                    names_to = "measurement")

# replace col names to be more appropriate for graph 
raw_OFAV_SSvSP_long$measurement <- raw_OFAV_SSvSP_long$measurement %>%
  gsub("mean_W","W", .) %>%
  gsub("mean_H","H", .) %>%  
  gsub("mean_A","A", .) %>%
  gsub("mean_CW","CW", .) %>%
  gsub("mean_CH","CH", .) %>%
  gsub("mean_CA","CA", .) 

#make graph of calice measurements 
plots_OFAV_SSvSP <- ggplot(raw_OFAV_SSvSP_long, aes(x=factor(measurement, level = measurement_order), y=value, fill=Treatment)) +
  geom_boxplot() +
  labs(y= "Mean Width", x = "Treatment")+
  theme_bw() +
  scale_fill_manual(values = c("cyan4", "goldenrod"))

# compare means
plots_OFAV_SSvSP_pvalues <- plots_OFAV_SSvSP + stat_compare_means(method = "t.test", size = 2)
ggsave("plot_OFAV_SSvSP_pvalues.jpg", plot = plots_OFAV_SSvSP_pvalues, path = '~/Desktop/GITHUB/TLPR21/Morphology/TL_Trans_Calice_Plots')

#compare Density of OFAV Controls
plot_D_OFAV_SSvSP <- ggplot(raw_OFAV_SSvSP, aes(x=Treatment, y=Calice_Density)) +
  geom_boxplot() +
  labs(y= "Calice Density", x = "Treatment")
plot_D_OFAV_SSvSP_pvalues <- plot_D_OFAV_SSvSP + stat_compare_means(method = "t.test", size = 2)
ggsave("plot_OFAV_SSvSP_D_pvalues.jpg", plot = plot_D_OFAV_SSvSP_pvalues, path = '~/Desktop/GITHUB/TLPR21/Morphology/TL_Trans_Calice_Plots')







# TLPR21 data import & manipulation ------------------------------------------------------------------







# Import data 
TLPR21_calice_merged<- read.csv('~/Desktop/GITHUB/TLPR21/Morphology/TLPR21_Data/TLPR21_Calice_Merged.csv') 

# 
TLPR21_OFAV <- TLPR21_calice_merged %>%
  filter(Species == "OFAV")

ggplot(TLPR21_OFAV, aes(x=Act_Depth,y=value)) +
  geom_point() +
  facet_wrap( ~ MEASUREMENT)+
  geom_smooth(method=lm)

model <- lm(value ~ Act_Depth, TLPR21_OFAV)
summary (model)

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
