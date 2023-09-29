# Title: PAM_Prep.R
# Author: Taylor Lindsay
# Date: 08.31.2023
# Input files:
#  PAM_Prep.R
# Output files: 
# 
# Notes 


# Packages & Data Import --------------------------------------------------

# Install Packages
library(tidyverse)
library(dplyr)
library(ggpubr)
library(ggplot2)

#AFDW data
PAM <- read.csv('~/Desktop/GITHUB/TLPR21/PAM/TL_TRANS_PAM_Aug04_13.csv')

ggplot(PAM, aes(x=Treatment, y=Qm)) + 
  geom_boxplot() 

# load all the lists for graphs 
measurement_order <- c('OFAV_S','OFAV_P','OFRA_P') 
x_order <- c('OFAV_SP','OFAV_SS','OFAV_PP', 'OFAV_PS', 'OFRA_PP', 'OFRA_PS') 
pj <- position_jitterdodge(jitter.width=0.1, seed=9,
                           jitter.height = 0)
treatment_comparisons <- list( c("OFAV_PS","OFAV_PP"), c("OFAV_SP","OFAV_SS"), c("OFRA_PP","OFRA_PS"), c("OFRA_PP","OFAV_PP"), c("OFAV_PP","OFAV_SS"))

# make a nice graph for Qm
ggplot(PAM, aes(x=factor(Treatment, level=x_order), y=Qm, fill=factor(group, level=measurement_order))) +
  geom_boxplot(alpha=0.5, outlier.size=0) +
  geom_point(aes(color=factor(group, level=measurement_order)), position = pj, size=1, show.legend = FALSE)+
  scale_fill_manual(values = c("#EBB134", "#ED6B5F", "#6060A8"), labels=c('OFAV Shallow', 'OFAV Deep', 'OFRA Deep')) +  
  scale_color_manual(values = c("#EBB134", "#ED6B5F", "#6060A8")) +
  labs(y= "Qm", x = "Depth Treatment", fill='Origin') + 
  scale_x_discrete(labels=c('Deep', 'Shallow', 'Deep', 'Shallow', 'Deep', 'Shallow')) + 
  theme_classic() +
  stat_compare_means(comparisons = treatment_comparisons, method = "wilcox.test", 
                     symnum.args = list(cutpoints = c(0, 0.001 ,0.01, 0.05, Inf), symbols = c("***", "**", "*", "")))


# make a nice graph for Fv/Fm
ggplot(PAM, aes(x=factor(Treatment, level=x_order), y=Fv.Fm, fill=factor(group, level=measurement_order))) +
  geom_boxplot(alpha=0.5, outlier.size=0) +
  geom_point(aes(color=factor(group, level=measurement_order)), position = pj, size=1, show.legend = FALSE)+
  scale_fill_manual(values = c("#EBB134", "#ED6B5F", "#6060A8"), labels=c('OFAV Shallow', 'OFAV Deep', 'OFRA Deep')) +  
  scale_color_manual(values = c("#EBB134", "#ED6B5F", "#6060A8")) +
  labs(y= "Fv/Fm", x = "Depth Treatment", fill='Origin') + 
  scale_x_discrete(labels=c('Deep', 'Shallow', 'Deep', 'Shallow', 'Deep', 'Shallow')) + 
  theme_classic() +
  stat_compare_means(comparisons = treatment_comparisons, method = "wilcox.test", 
                     symnum.args = list(cutpoints = c(0, 0.001 ,0.01, 0.05, Inf), symbols = c("***", "**", "*", "")))

