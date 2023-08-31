# Title: TL_Trans_PCA_Results.R
# Author: Taylor Lindsay
# Date: 04.10.2023
# Input files:
#  
# Output files: 
# 
# Notes 


# Packages & Data ---------------------------------------------------------


library(ggfortify)   # needed to make pca plots 
library(tidyverse)
library(ggplot2)
library(vegan)       # needed for PERMANOVA 

raw <- read.csv('~/Desktop/GITHUB/TLPR21/TL_Trans_Results.csv')

# remove all NAs from all data, and morph and phys seperately 
na_all <- raw %>%
  .[c(2:12,16:21)] %>%
  .[complete.cases(.),]

na_morph <- raw %>%
  .[c(2:12)] %>%
  .[complete.cases(.),]

na_phys <- raw %>%
  .[c(2:5,16:21)] %>%
  .[complete.cases(.),]


# Species -----------------------------------------------------------------

### All 

# filter for just the species controls 
species_all <- na_all %>%
  filter(full_treatment==c("OFAV_PP","OFRA_PP"))
# PCA 
species_all_pca <- prcomp(species_all[c(5:17)], scale. = TRUE, center = TRUE)
autoplot(species_all_pca, data=species_all, color = 'full_treatment',
         frame = TRUE, frame.type = 't')
# Scale data to prepare for permanova 
species_all_scaled <- scale(species_all[,5:17],center = TRUE, scale = TRUE)
# PerMANOVA - partitioning the euclidean distance matrix by species
species_all_PERMANOVA <- adonis2(species_all_scaled ~ full_treatment, data = species_all, method='eu')
    # RESULTS 
    # p =
    # R2 = 

### MORPH 

# filter for just the species controls 
species_morph <- na_morph %>%
  filter(full_treatment==c("OFAV_PP","OFRA_PP"))
# PCA 
species_morph_pca <- prcomp(species_morph[c(5:11)], scale. = TRUE, center = TRUE)
autoplot(species_morph_pca, data=species_morph, color = 'full_treatment',
         frame = TRUE, frame.type = 't')
# Scale data to prepare for permanova 
species_morph_scaled <- scale(species_morph[,5:11],center = TRUE, scale = TRUE)
# PerMANOVA - partitioning the euclidean distance matrix by species
species_morph_PERMANOVA <- adonis2(species_morph_scaled ~ full_treatment, data = species_morph, method='eu')
    # RESULTS 
    # p = 
    # R2 = 

### PHYS 

# filter for just the species controls 
species_phys <- na_phys %>%
  filter(full_treatment==c("OFAV_PP","OFRA_PP"))
# PCA 
species_phys_pca <- prcomp(species_phys[,5:10], scale. = TRUE, center = TRUE)
autoplot(species_phys_pca, data=species_phys, color = 'full_treatment',
         frame = TRUE, frame.type = 't')
# Scale data to prepare for permanova 
species_phys_scaled <- scale(species_phys[,5:10],center = TRUE, scale = TRUE)
# PerMANOVA - partitioning the euclidean distance matrix by species
species_phys_PERMANOVA <- adonis2(species_phys_scaled ~ full_treatment, data = species_phys, method='eu')
    # RESULTS 
    # p = 
    # R2 = 


# Depth -------------------------------------------------------------------

### All 

# filter for just the species controls 
depth_all <- na_all %>%
  filter(full_treatment==c("OFAV_PP","OFAV_SS"))
# PCA 
depth_all_pca <- prcomp(depth_all[c(5:17)], scale. = TRUE, center = TRUE)
autoplot(depth_all_pca, data=depth_all, color = 'full_treatment',
         frame = TRUE, frame.type = 't')
# Scale data to prepare for permanova 
depth_all_scaled <- scale(depth_all[,5:17],center = TRUE, scale = TRUE)
# PerMANOVA - partitioning the euclidean distance matrix by species
depth_all_PERMANOVA <- adonis2(depth_all_scaled ~ full_treatment, data = depth_all, method='eu')
# RESULTS 
# p =
# R2 = 

### MORPH 

# filter for just the species controls 
depth_morph <- na_morph %>%
  filter(full_treatment==c("OFAV_PP","OFAV_SS"))
# PCA 
depth_morph_pca <- prcomp(depth_morph[c(5:11)], scale. = TRUE, center = TRUE)
autoplot(depth_morph_pca, data=depth_morph, color = 'full_treatment',
         frame = TRUE, frame.type = 't')
# Scale data to prepare for permanova 
depth_morph_scaled <- scale(depth_morph[,5:11],center = TRUE, scale = TRUE)
# PerMANOVA - partitioning the euclidean distance matrix by species
depth_morph_PERMANOVA <- adonis2(depth_morph_scaled ~ full_treatment, data = depth_morph, method='eu')
# RESULTS 
# p = 
# R2 = 

### PHYS 

# filter for just the species controls 
depth_phys <- na_phys %>%
  filter(full_treatment==c("OFAV_PP","OFAV_SS"))
# PCA 
depth_phys_pca <- prcomp(depth_phys[,5:10], scale. = TRUE, center = TRUE)
autoplot(depth_phys_pca, data=depth_phys, color = 'full_treatment',
         frame = TRUE, frame.type = 't')
# Scale data to prepare for permanova 
depth_phys_scaled <- scale(depth_phys[,5:10],center = TRUE, scale = TRUE)
# PerMANOVA - partitioning the euclidean distance matrix by species
depth_phys_PERMANOVA <- adonis2(depth_phys_scaled ~ full_treatment, data = depth_phys, method='eu')
# RESULTS 
# p = 
# R2 = 

# OFRAs  ------------------------------------------------------------------

### All 

# filter for just the species controls 
OFRA_all <- na_all %>%
  filter(full_treatment==c("OFRA_PP","OFRA_PS"))
# PCA 
OFRA_all_pca <- prcomp(OFRA_all[c(5:17)], scale. = TRUE, center = TRUE)
autoplot(OFRA_all_pca, data=OFRA_all, color = 'full_treatment',
         frame = TRUE, frame.type = 't')
# Scale data to prepare for permanova 
OFRA_all_scaled <- scale(OFRA_all[,5:17],center = TRUE, scale = TRUE)
# PerMANOVA - partitioning the euclidean distance matrix by species
OFRA_all_PERMANOVA <- adonis2(OFRA_all_scaled ~ full_treatment, data = OFRA_all, method='eu')
# RESULTS 
# p =
# R2 = 

### MORPH 

# filter for just the species controls 
OFRA_morph <- na_morph %>%
  filter(full_treatment==c("OFRA_PP","OFRA_PS"))
# PCA 
OFRA_morph_pca <- prcomp(OFRA_morph[c(5:11)], scale. = TRUE, center = TRUE)
autoplot(OFRA_morph_pca, data=OFRA_morph, color = 'full_treatment',
         frame = TRUE, frame.type = 't')
ggsave("~/Desktop/plot_OFRA_morph.jpg", width = 5, height = 3, units = "in")
# Scale data to prepare for permanova 
OFRA_morph_scaled <- scale(OFRA_morph[,5:11],center = TRUE, scale = TRUE)
# PerMANOVA - partitioning the euclidean distance matrix by species
OFRA_morph_PERMANOVA <- adonis2(OFRA_morph_scaled ~ full_treatment, data = OFRA_morph, method='eu')
# RESULTS 
# p = 
# R2 = 

### PHYS 

# filter for just the species controls 
OFRA_phys <- na_phys %>%
  filter(full_treatment==c("OFRA_PP","OFRA_PS"))
# PCA 
OFRA_phys_pca <- prcomp(OFRA_phys[,5:10], scale. = TRUE, center = TRUE)
autoplot(OFRA_phys_pca, data=OFRA_phys, color = 'full_treatment',
         frame = TRUE, frame.type = 't')
ggsave("~/Desktop/plot_OFRA_phys.jpg", width = 5, height = 3, units = "in")
# Scale data to prepare for permanova 
OFRA_phys_scaled <- scale(OFRA_phys[,5:10],center = TRUE, scale = TRUE)
# PerMANOVA - partitioning the euclidean distance matrix by species
OFRA_phys_PERMANOVA <- adonis2(OFRA_phys_scaled ~ full_treatment, data = OFRA_phys, method='eu')
# RESULTS 
# p = 
# R2 = 


# OFAV PP vs PS  ----------------------------------------------------------

### All 

# filter for just the species controls 
OFAV_P_all <- na_all %>%
  filter(full_treatment==c("OFAV_PP","OFAV_PS"))
# PCA 
OFAV_P_all_pca <- prcomp(OFAV_P_all[c(5:17)], scale. = TRUE, center = TRUE)
autoplot(OFAV_P_all_pca, data=OFAV_P_all, color = 'full_treatment',
         frame = TRUE, frame.type = 't')
# Scale data to prepare for permanova 
OFAV_P_all_scaled <- scale(OFAV_P_all[,5:17],center = TRUE, scale = TRUE)
# PerMANOVA - partitioning the euclidean distance matrix by species
OFAV_P_all_PERMANOVA <- adonis2(OFAV_P_all_scaled ~ full_treatment, data = OFAV_P_all, method='eu')
# RESULTS 
# p =
# R2 = 

### MORPH 

# filter for just the species controls 
OFAV_P_morph <- na_morph %>%
  filter(full_treatment==c("OFAV_PP","OFAV_PS"))
# PCA 
OFAV_P_morph_pca <- prcomp(OFAV_P_morph[c(5:11)], scale. = TRUE, center = TRUE)
autoplot(OFAV_P_morph_pca, data=OFAV_P_morph, color = 'full_treatment',
         frame = TRUE, frame.type = 't')
ggsave("~/Desktop/plot_OFAV_P_morph.jpg", width = 5, height = 3, units = "in")
# Scale data to prepare for permanova 
OFAV_P_morph_scaled <- scale(OFAV_P_morph[,5:11],center = TRUE, scale = TRUE)
# PerMANOVA - partitioning the euclidean distance matrix by species
OFAV_P_morph_PERMANOVA <- adonis2(OFAV_P_morph_scaled ~ full_treatment, data = OFAV_P_morph, method='eu')
# RESULTS 
# p = 
# R2 = 

### PHYS 

# filter for just the species controls 
OFAV_P_phys <- na_phys %>%
  filter(full_treatment==c("OFAV_PP","OFAV_PS"))
# PCA 
OFAV_P_phys_pca <- prcomp(OFAV_P_phys[,5:10], scale. = TRUE, center = TRUE)
autoplot(OFAV_P_phys_pca, data=OFAV_P_phys, color = 'full_treatment',
         frame = TRUE, frame.type = 't')
ggsave("~/Desktop/plot_OFAV_P_phys.jpg", width = 5, height = 3, units = "in")
# Scale data to prepare for permanova 
OFAV_P_phys_scaled <- scale(OFAV_P_phys[,5:10],center = TRUE, scale = TRUE)
# PerMANOVA - partitioning the euclidean distance matrix by species
OFAV_P_phys_PERMANOVA <- adonis2(OFAV_P_phys_scaled ~ full_treatment, data = OFAV_P_phys, method='eu')
# RESULTS 
# p = 
# R2 = 


# OFAV SS v SP ------------------------------------------------------------

### All 

# filter for just the species controls 
OFAV_S_all <- na_all %>%
  filter(full_treatment==c("OFAV_SS","OFAV_SP"))
# PCA 
OFAV_S_all_pca <- prcomp(OFAV_S_all[c(5:17)], scale. = TRUE, center = TRUE)
autoplot(OFAV_S_all_pca, data=OFAV_S_all, color = 'full_treatment',
         frame = TRUE, frame.type = 't')
ggsave("~/Desktop/plot_OFAV_S_morph.jpg", width = 5, height = 3, units = "in")
# Scale data to prepare for permanova 
OFAV_S_all_scaled <- scale(OFAV_S_all[,5:17],center = TRUE, scale = TRUE)
# PerMANOVA - partitioning the euclidean distance matrix by species
OFAV_S_all_PERMANOVA <- adonis2(OFAV_S_all_scaled ~ full_treatment, data = OFAV_S_all, method='eu')
# RESULTS 
# p =
# R2 = 

### MORPH 

# filter for just the species controls 
OFAV_S_morph <- na_morph %>%
  filter(full_treatment==c("OFAV_SS","OFAV_SP"))
# PCA 
OFAV_S_morph_pca <- prcomp(OFAV_S_morph[c(5:11)], scale. = TRUE, center = TRUE)
autoplot(OFAV_S_morph_pca, data=OFAV_S_morph, color = 'full_treatment',
         frame = TRUE, frame.type = 't')
ggsave("~/Desktop/plot_OFAV_S_phys.jpg", width = 5, height = 3, units = "in")
# Scale data to prepare for permanova 
OFAV_S_morph_scaled <- scale(OFAV_S_morph[,5:11],center = TRUE, scale = TRUE)
# PerMANOVA - partitioning the euclidean distance matrix by species
OFAV_S_morph_PERMANOVA <- adonis2(OFAV_S_morph_scaled ~ full_treatment, data = OFAV_S_morph, method='eu')
# RESULTS 
# p = 
# R2 = 

### PHYS 

# filter for just the species controls 
OFAV_S_phys <- na_phys %>%
  filter(full_treatment==c("OFAV_SS","OFAV_SP"))
# PCA 
OFAV_S_phys_pca <- prcomp(OFAV_S_phys[,5:10], scale. = TRUE, center = TRUE)
autoplot(OFAV_S_phys_pca, data=OFAV_S_phys, color = 'full_treatment',
         frame = TRUE, frame.type = 't')
# Scale data to prepare for permanova 
OFAV_S_phys_scaled <- scale(OFAV_S_phys[,5:10],center = TRUE, scale = TRUE)
# PerMANOVA - partitioning the euclidean distance matrix by species
OFAV_S_phys_PERMANOVA <- adonis2(OFAV_S_phys_scaled ~ full_treatment, data = OFAV_S_phys, method='eu')
# RESULTS 
# p = 
# R2 = 


# T-tests ----------------------------------------------------------

na_all
