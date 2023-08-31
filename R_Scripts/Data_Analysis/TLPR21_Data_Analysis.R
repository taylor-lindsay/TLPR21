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
  select(depth, act_depth, species, site, colony_id, Host_AFDW_mg.cm2, Sym_AFDW_mg.cm2, sym.cm2, A,	CA,	di,	Cdi, A.CA,	D, chla.ug.cm2, chlc2.ug.cm2)

# make a table of means & SD 
means <- raw %>%
  group_by(species,site,depth) %>%
  summarise(across(everything(), .f = list(mean = mean, sd = sd), na.rm = TRUE))
  
# Define my groups for stat compare means 
#treatment_comparisons <- list( c("OFAV_PS","OFAV_PP"), c("OFAV_SP","OFAV_SS"), c("OFRA_PP","OFRA_PS"))

# define formula
formula <- y ~ x   

# protein -------------------------------------------------------------------
measurement_order <- c('OFAV_S','OFAV_P','OFRA_P') 
x_order <- c('OFAV_SP','OFAV_SS','OFAV_PP', 'OFAV_PS', 'OFRA_PP', 'OFRA_PS') 
pj <- position_jitterdodge(jitter.width=0.1, seed=9,
                           jitter.height = 0)

protein <- ggplot(raw, aes(x=factor(full_treatment, level=x_order), y=prot_mg.cm2, fill=factor(group, level=measurement_order))) +
  geom_boxplot(alpha=0.5, outlier.size=0) +
  geom_point(aes(color=factor(group, level=measurement_order)), position = pj, size=1, show.legend = FALSE)+
  scale_fill_manual(values = c("#EBB134", "#ED6B5F", "#6060A8"), labels=c('OFAV Shallow', 'OFAV Deep', 'OFRA Deep')) + 
  scale_color_manual(values = c("#EBB134", "#ED6B5F", "#6060A8")) + 
  labs(y= "Protein (mg/cm2)", x = "Depth Treatment", fill='Origin') + 
  scale_x_discrete(labels=c('Deep', 'Shallow', 'Deep', 'Shallow', 'Deep', 'Shallow')) + 
  theme_classic() +
  stat_compare_means(comparisons = treatment_comparisons, method = "t.test", 
                     symnum.args = list(cutpoints = c(0, 0.001 ,0.01, 0.05, Inf), symbols = c("***", "**", "*", "")))
protein
ggsave("treatments_protein_pvalue.jpg", plot = protein , path = '~/Desktop/GITHUB/TLPR21/TL_Trans_Results/', width = 6,
       height = 6)

# CHLA -------------------------------------------------------------------
ggplot(raw, aes(x=act_depth, y=chla.ug.cm2)) +
  geom_point() +
  facet_wrap(~species, scales = "free") + 
  geom_smooth(method=lm) +
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = formula),
                  geom = 'text',
                  aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")), size = 2)


# CHLC -------------------------------------------------------------------
ggplot(raw, aes(x=act_depth, y=chlc2.ug.cm2)) +
  geom_point() +
  facet_wrap(~species,scales = "free") + 
  geom_smooth(method=lm) +
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = formula),
                  geom = 'text',
                  aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")), size = 2)

# sym counts -------------------------------------------------------------------

ggplot(raw, aes(x=act_depth, y=sym.cm2)) +
  geom_point() +
  facet_wrap(~species,scales = "free") + 
  geom_smooth(method=lm) +
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = formula),
                  geom = 'text',
                  aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")), size = 2)

# Host AFDW --------------------------------------------------------------------

ggplot(raw, aes(x=act_depth, y=Host_AFDW_mg.cm2)) +
  geom_point() +
  facet_wrap(~species,scales = "free") + 
  geom_smooth(method=lm) +
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = formula),
                  geom = 'text',
                  aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")), size = 2)


# Sym AFDW ----------------------------------------------------------------

ggplot(raw, aes(x=act_depth, y=Sym_AFDW_mg.cm2)) +
  geom_point() +
  facet_wrap(~species,scales = "free") + 
  geom_smooth(method=lm) +
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = formula),
                  geom = 'text',
                  aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")), size = 2)

# Morphology -------------------------------------------------------------------

ggplot(raw, aes(x=act_depth, y=di)) +
  geom_point() +
  facet_wrap(~species,scales = "free") + 
  geom_smooth(method=lm) +
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = formula),
                  geom = 'text',
                  aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")), size = 2)

ggplot(raw, aes(x=act_depth, y=Cdi)) +
  geom_point() +
  facet_wrap(~species,scales = "free") + 
  geom_smooth(method=lm) +
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = formula),
                  geom = 'text',
                  aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")), size = 2)

ggplot(raw, aes(x=act_depth, y=A)) +
  geom_point() +
  facet_wrap(~species,scales = "free") + 
  geom_smooth(method=lm) +
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = formula),
                  geom = 'text',
                  aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")), size = 2)

ggplot(raw, aes(x=act_depth, y=CA)) +
  geom_point() +
  facet_wrap(~species,scales = "free") + 
  geom_smooth(method=lm) +
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = formula),
                  geom = 'text',
                  aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")), size = 2)

ggplot(raw, aes(x=act_depth, y=D)) +
  geom_point() +
  facet_wrap(~species,scales = "free") + 
  geom_smooth(method=lm) +
  stat_fit_glance(method = 'lm',
                  method.args = list(formula = formula),
                  geom = 'text',
                  aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")), size = 2)
