## # Title: TLPR21_Data_Analysis.R
# Author: TL
# Date: 08.29.2023
# Input files: 
#  TLPR21_Results.csv
# Output files: 
#  
# Notes

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(ggpmisc)


# Data -------------------------------------------------------------------

raw <- read.csv('~/Desktop/GITHUB/TLPR21/TLPR21_Results.csv') %>%
  select(depth, act_depth, species, site, colony_id, Host_AFDW_mg.cm2, Sym_AFDW_mg.cm2, sym.cm2, A,	CA,	di,	Cdi,D, chla.ug.cm2, chlc2.ug.cm2)

# make a table of means & SD 
means <- raw %>%
  group_by(species,depth) %>%
  summarise(across(everything(), .f = list(mean = mean, sd = sd), na.rm = TRUE))

# convert ft to m 
raw <- mutate(raw, depth_m = act_depth*0.3048)
  
# Define my groups for stat compare means 
#treatment_comparisons <- list( c("OFAV_PS","OFAV_PP"), c("OFAV_SP","OFAV_SS"), c("OFRA_PP","OFRA_PS"))

# define formula
formula <- y ~ x   


# summary stats table by kind ---------------------------------------------

means_chl <- means %>%
  select(species, depth_m, chla.ug.cm2_mean) %>%
  filter(chla.ug.cm2_mean != 'NaN') %>%
  pivot_wider(.,names_from = depth, values_from = chla.ug.cm2_mean) 
  


# protein -------------------------------------------------------------------

# CHLA -------------------------------------------------------------------
plot_chla <- ggplot(raw[!(is.na(raw$chla.ug.cm2)),], aes(x=depth_m, y=chla.ug.cm2)) +
  geom_point() +
  facet_wrap(~species, scales = "free") + 
  geom_smooth(method=lm) +
  labs(y= "Chlorophyll a (ug/cm2)", x = "Depth (m)") + 
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = formula),
                  geom = 'text',
                  aes(label = paste("P = ", signif(..p.value.., digits = 4), sep = "")), size = 2)

ggsave("TLPR21_chla_plot.jpg", plot_chla, path = '~/Desktop/GITHUB/TLPR21/TLPR21_Results/', width = 8,
       height = 4)

# CHLC2 -------------------------------------------------------------------
plot_chlc2 <- ggplot(raw[!(is.na(raw$chlc2.ug.cm2)),], aes(x=depth_m, y=chlc2.ug.cm2)) +
  geom_point() +
  facet_wrap(~species,scales = "free") + 
  geom_smooth(method=lm) +
  labs(y= "Chlorophyll c2 (ug/cm2)", x = "Depth (m)") + 
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = formula),
                  geom = 'text',
                  aes(label = paste("P = ", signif(..p.value.., digits = 4), sep = "")), size = 2)

ggsave("TLPR21_chlc2_plot.jpg", plot_chlc2, path = '~/Desktop/GITHUB/TLPR21/TLPR21_Results/', width = 8,
       height = 4)

# sym counts -------------------------------------------------------------------

plot_sym <- ggplot(raw[!(is.na(raw$sym.cm2)),], aes(x=depth_m, y=sym.cm2)) +
  geom_point() +
  facet_wrap(~species,scales = "free") + 
  geom_smooth(method=lm) +
  labs(y= "Symbiont Density (cells/cm2)", x = "Depth (m)") + 
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = formula),
                  geom = 'text',
                  aes(label = paste("P = ", signif(..p.value.., digits = 4), sep = "")), size = 2)

ggsave("TLPR21_sym_plot.jpg", plot_sym, path = '~/Desktop/GITHUB/TLPR21/TLPR21_Results/', width = 8,
       height = 4)

# Host AFDW --------------------------------------------------------------------

plot_AFDW_host <- ggplot(raw[!(is.na(raw$Host_AFDW_mg.cm2)),], aes(x=depth_m, y=Host_AFDW_mg.cm2)) +
  geom_point() +
  facet_wrap(~species,scales = "free") + 
  geom_smooth(method=lm) +
  labs(y= "Host Biomass (mg/cm2)", x = "Depth (m)") + 
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = formula),
                  geom = 'text',
                  aes(label = paste("P = ", signif(..p.value.., digits = 4), sep = "")), size = 2)

ggsave("TLPR21_AFDW_host_plot.jpg", plot_AFDW_host, path = '~/Desktop/GITHUB/TLPR21/TLPR21_Results/', width = 8,
       height = 4)

# Sym AFDW ----------------------------------------------------------------

plot_AFDW_sym <- ggplot(raw[!(is.na(raw$Sym_AFDW_mg.cm2)),], aes(x=depth_m, y=Sym_AFDW_mg.cm2)) +
  geom_point() +
  facet_wrap(~species,scales = "free") + 
  geom_smooth(method=lm) +
  labs(y= "Symbiont Biomass (mg/cm2)", x = "Depth (m)") + 
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = formula),
                  geom = 'text',
                  aes(label = paste("P = ", signif(..p.value.., digits = 4), sep = "")), size = 2)

ggsave("TLPR21_AFDW_sym_plot.jpg", plot_AFDW_sym, path = '~/Desktop/GITHUB/TLPR21/TLPR21_Results/', width = 8,
       height = 4)

# Morphology -------------------------------------------------------------------

plot_di <- ggplot(raw[!(is.na(raw$di)),], aes(x=depth_m, y=di)) +
  geom_point() +
  facet_wrap(~species,scales = "free") + 
  geom_smooth(method=lm) +
  labs(y= "Columella diameter (cm)", x = "Depth (m)") + 
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = formula),
                  geom = 'text',
                  aes(label = paste("P = ", signif(..p.value.., digits = 4), sep = "")), size = 2)

ggsave("TLPR21_columella_diameter_plot.jpg", plot_di, path = '~/Desktop/GITHUB/TLPR21/TLPR21_Results/', width = 8,
       height = 4)

plot_Cdi <- ggplot(raw[!(is.na(raw$Cdi)),], aes(x=depth_m, y=Cdi)) +
  geom_point() +
  facet_wrap(~species,scales = "free") + 
  geom_smooth(method=lm) +
  labs(y= "Calice diameter (cm)", x = "Depth (m)") + 
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = formula),
                  geom = 'text',
                  aes(label = paste("P = ", signif(..p.value.., digits = 4), sep = "")), size = 2)

ggsave("TLPR21_calice_diameter_plot.jpg", plot_Cdi, path = '~/Desktop/GITHUB/TLPR21/TLPR21_Results/', width = 8,
       height = 4)

plot_A <- ggplot(raw[!(is.na(raw$A)),], aes(x=depth_m, y=A)) +
  geom_point() +
  facet_wrap(~species,scales = "free") + 
  geom_smooth(method=lm) +
  labs(y= "Columella area (cm2)", x = "Depth (m)") + 
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = formula),
                  geom = 'text',
                  aes(label = paste("P = ", signif(..p.value.., digits = 4), sep = "")), size = 2)

ggsave("TLPR21_columella_area_plot.jpg", plot_A, path = '~/Desktop/GITHUB/TLPR21/TLPR21_Results/', width = 8,
       height = 4)

plot_CA <- ggplot(raw[!(is.na(raw$CA)),], aes(x=depth_m, y=CA)) +
  geom_point() +
  facet_wrap(~species,scales = "free") + 
  geom_smooth(method=lm) +
  labs(y= "Calice area (cm2)", x = "Depth (m)") + 
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = formula),
                  geom = 'text',
                  aes(label = paste("P = ", signif(..p.value.., digits = 4), sep = "")), size = 2)

ggsave("TLPR21_calice_area_plot.jpg", plot_CA, path = '~/Desktop/GITHUB/TLPR21/TLPR21_Results/', width = 8,
       height = 4)

plot_D <- ggplot(raw[!(is.na(raw$D)),], aes(x=depth_m, y=D)) +
  geom_point() +
  facet_wrap(~species,scales = "free") + 
  geom_smooth(method=lm) +
  labs(y= "Calice density (calices/cm2)", x = "Depth (m)") + 
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = formula),
                  geom = 'text',
                  aes(label = paste("P = ", signif(..p.value.., digits = 4), sep = "")), size = 2)

ggsave("TLPR21_calice_density_plot.jpg", plot_D, path = '~/Desktop/GITHUB/TLPR21/TLPR21_Results/', width = 8,
       height = 4)

