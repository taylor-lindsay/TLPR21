## # Title: TL_Trans_Data_Analysis.R
# Author: TL
# Date: 03.06.2023
# Input files: 
#  TL_Trans_Results.csv
# Output files: 
#  
# Notes

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(ggpubr)
library(ggplot2)

# Data -------------------------------------------------------------------

raw <- read.csv('~/Desktop/GITHUB/TLPR21/TL_Trans_Results.csv')

# make a 'group column' that allows us to facet wrap 
raw <- raw %>%
  mutate(group = str_sub(full_treatment,1,6))


# make a table of means & SD 
means <- raw %>%
  group_by(full_treatment) %>%
  summarise(across(everything(), .f = list(mean = mean, sd = sd), na.rm = TRUE))
  
# Define my groups for stat compare means 
treatment_comparisons <- list( c("OFAV_PS","OFAV_PP"), c("OFAV_SP","OFAV_SS"), c("OFRA_PP","OFRA_PS"), c("OFRA_PP", "OFAV_PP"), c("OFAV_PP","OFAV_SS") )


# protein -------------------------------------------------------------------

protein <- ggplot(raw, aes(x=full_treatment, y=prot_mg.cm2, color=group)) +
  geom_boxplot() +
  labs(y= "Protein (mg/cm2)", x = "Treatment")
protein_pvalue <- protein + stat_compare_means(comparisons = treatment_comparisons, method = "t.test", size = 3)
ggsave("treatments_protein_pvalue.jpg", plot = protein_pvalue , path = '~/Desktop/GITHUB/TLPR21/TL_Trans_Results/', width = 6,
       height = 6)

# CHLA -------------------------------------------------------------------

chla <- ggplot(raw, aes(x=full_treatment, y=chla.ug.cm2, color=group)) +
  geom_boxplot() +
  labs(y= "Chlorophyl a (ug/cm2)", x = "Treatment")
chla_pvalue <- chla + stat_compare_means(comparisons = treatment_comparisons, method = "t.test", size = 3)
ggsave("treatments_chla_pvalue.jpg", plot = chla_pvalue , path = '~/Desktop/GITHUB/TLPR21/TL_Trans_Results/', width = 6,
       height = 6)

# CHLC -------------------------------------------------------------------

chlc2 <- ggplot(raw, aes(x=full_treatment, y=chlc2.ug.cm2, color=group)) +
  geom_boxplot() +
  labs(y= "Chlorophyl C2 (ug/cm2)", x = "Treatment")
chlc2_pvalue <- chlc2 + stat_compare_means(comparisons = treatment_comparisons, method = "t.test", size = 3)
ggsave("treatments_chlc2_pvalue.jpg", plot = chlc2_pvalue , path = '~/Desktop/GITHUB/TLPR21/TL_Trans_Results/', width = 6,
       height = 6)

# CHLA / sym -------------------------------------------------------------------

chla_sym <- ggplot(raw, aes(x=full_treatment, y=chla.sym, color=group)) +
  geom_boxplot() +
  labs(y= "Chlorophyl a (ug/cm2)", x = "Treatment")
chla_sym_pvalue <- chla_sym + stat_compare_means(comparisons = treatment_comparisons, method = "t.test", size = 3)
ggsave("treatments_chla_sym_pvalue.jpg", plot = chla_sym_pvalue , path = '~/Desktop/GITHUB/TLPR21/TL_Trans_Results/', width = 6,
       height = 6)

# sym counts -------------------------------------------------------------------

sym <- ggplot(raw, aes(x=full_treatment, y=sym.cm2, color=group)) +
  geom_boxplot() +
  labs(y= "Symbiont Density (Cells/cm2)", x = "Treatment")
sym_pvalue <- sym + stat_compare_means(comparisons = treatment_comparisons, method = "t.test", size = 3)
ggsave("treatments_sym_pvalue.jpg", plot = sym_pvalue , path = '~/Desktop/GITHUB/TLPR21/TL_Trans_Results/', width = 6,
       height = 6) 



# Morphology -------------------------------------------------------------------

# filter only 
morph_raw <- raw[,c(2:13,21)]

#make dataset long 
morph_long <- morph_raw %>%
  pivot_longer(.,cols = c(W, CW, H, CH, A, CA, D, CA.A),
               names_to = "measurement")

## Morphology Density Only 
morph_density <- morph_long %>%
  filter(measurement == "D")

# graph calice density 
morph_D <- ggplot(morph_density, aes(x=full_treatment, y=value, color=group)) +
  geom_boxplot() +
  labs(y= "Calice Density (calices/cm2)", x = "Treatment")
morph_D_pvalue <- morph_D  + stat_compare_means(comparisons = treatment_comparisons, method = "t.test", size = 3)
ggsave("morph_D_pvalue.jpg", plot = morph_D_pvalue , path = '~/Desktop/GITHUB/TLPR21/TL_Trans_Results/',width = 6,
       height = 6) 


## Morphology CA/A Ratio
morph_ratio <- morph_long %>%
  filter(measurement == "CA.A")

morph_ratio <- ggplot(morph_ratio, aes(x=full_treatment, y=value, color=group)) +
  geom_boxplot() +
  labs(y= "CA:C (cm)", x = "Treatment")
morph_ratio_pvalue <- morph_D  + stat_compare_means(comparisons = treatment_comparisons, method = "t.test", size = 3)
ggsave("morph_CA_A_pvalue.jpg", plot = morph_ratio_pvalue , path = '~/Desktop/GITHUB/TLPR21/TL_Trans_Results/',width = 6,
       height = 6) 



# Non-density morphology --------------------------------------------------

# set the measurement order 
measurement_order <- c('A', 'CA', 'H', 'CH', 'W', 'CW') 
# set the groups to compare 
morph_comparisons <- list( c("A","CA"), c("H","CH"),  c("W","CW") )

# OFAV PP vs. OFRA PP 
morph_species <- morph_long %>%
  filter(measurement != "CA.A") %>%
  filter(measurement != "D") %>%
  filter(full_treatment == "OFAV_PP" | full_treatment == "OFRA_PP")


plot_morph_species <- ggplot(morph_species, aes(x=factor(measurement, level=measurement_order), y=value, fill=full_treatment)) +
  geom_boxplot() +
  labs(y= "Mean Length (cm or cm2)", x = "Measurement")
morph_species_pvalue <- plot_morph_species  + stat_compare_means(method = "t.test", size = 2)
ggsave("morph_species_pvalue.jpg", plot = morph_species_pvalue , path = '~/Desktop/GITHUB/TLPR21/TL_Trans_Results/',width = 7,
       height = 6) 

# OFAV PP vs SS
morph_control <- morph_long %>%
  filter(measurement != "D") %>%
  filter(measurement != "CA.A") %>%
  filter(full_treatment == "OFAV_PP" | full_treatment == "OFAV_SS")
plot_morph_control <- ggplot(morph_control, aes(x=factor(measurement, level=measurement_order), y=value, fill=full_treatment)) +
  geom_boxplot() +
  labs(y= "Mean Length (cm or cm2)", x = "Measurement")
morph_control_pvalue <- plot_morph_control  + stat_compare_means(method = "t.test", size = 2)
ggsave("morph_control_pvalue.jpg", plot = morph_control_pvalue , path = '~/Desktop/GITHUB/TLPR21/TL_Trans_Results/',width = 7,
       height = 6) 

# OFAV PP vs PS
morph_OFAV_PP_PS <- morph_long %>%
  filter(measurement != "D") %>%
  filter(measurement != "CA.A") %>%
  filter(full_treatment == "OFAV_PP" | full_treatment == "OFAV_PS")
plot_morph_OFAV_PP_PS <- ggplot(morph_OFAV_PP_PS, aes(x=factor(measurement, level=measurement_order), y=value, fill=full_treatment)) +
  geom_boxplot() +
  labs(y= "Mean Length (cm or cm2)", x = "Measurement")
morph_OFAV_PP_PS_pvalue <- plot_morph_OFAV_PP_PS  + stat_compare_means(method = "t.test", size = 2)
ggsave("morph_OFAV_PP_PS_pvalue.jpg", plot = morph_OFAV_PP_PS_pvalue , path = '~/Desktop/GITHUB/TLPR21/TL_Trans_Results/',width = 7,
       height = 6) 

# OFAV SS vs SP
morph_OFAV_SS_SP <- morph_long %>%
  filter(measurement != "D") %>%
  filter(measurement != "CA.A") %>%
  filter(full_treatment == "OFAV_SS" | full_treatment == "OFAV_SP")
plot_morph_OFAV_SS_SP <- ggplot(morph_OFAV_SS_SP, aes(x=factor(measurement, level=measurement_order), y=value, fill=full_treatment)) +
  geom_boxplot() +
  labs(y= "Mean Length (cm or cm2)", x = "Measurement")
morph_OFAV_SS_SP_pvalue <- plot_morph_OFAV_SS_SP  + stat_compare_means(method = "t.test", size = 2)
ggsave("morph_OFAV_SS_SP_pvalue.jpg", plot = morph_OFAV_SS_SP_pvalue , path = '~/Desktop/GITHUB/TLPR21/TL_Trans_Results/',width = 7,
       height = 6) 

# OFRA PP vs PS 
morph_OFRA_PP_PS <- morph_long %>%
  filter(measurement != "D") %>%
  filter(measurement != "CA.A") %>%
  filter(full_treatment == "OFRA_PP" | full_treatment == "OFRA_PS")
plot_morph_OFRA_PP_PS <- ggplot(morph_OFRA_PP_PS, aes(x=factor(measurement, level=measurement_order), y=value, fill=full_treatment)) +
  geom_boxplot() +
  labs(y= "Mean Length (cm or cm2)", x = "Measurement")
morph_OFRA_PP_PS_pvalue <- plot_morph_OFRA_PP_PS  + stat_compare_means(method = "t.test", size = 2)
ggsave("morph_OFRA_PP_PS_pvalue.jpg", plot = morph_OFRA_PP_PS_pvalue , path = '~/Desktop/GITHUB/TLPR21/TL_Trans_Results/',width = 7,
       height = 6) 

