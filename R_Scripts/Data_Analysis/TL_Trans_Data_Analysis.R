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

#raw2 <- raw %>% filter(., full_treatment == c("OFAV_SS", "OFAV_SP"))
#ggplot(raw2, aes(x=colony_id,y=chla.ug.cm2 )) + geom_point()

# make a table of means & SD 
means <- raw %>%
  group_by(full_treatment) %>%
  summarise(across(everything(), .f = list(mean = mean, sd = sd), na.rm = TRUE))
  
# Define my groups for stat compare means 
treatment_comparisons <- list( c("OFAV_PS","OFAV_PP"), c("OFAV_SP","OFAV_SS"), c("OFRA_PP","OFRA_PS"))


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

chla <- ggplot(raw, aes(x=factor(full_treatment, level=x_order), y=chla.ug.cm2, fill=factor(group, level=measurement_order))) +
  geom_boxplot(alpha=0.5, outlier.size=0) +
  geom_point(aes(color=factor(group, level=measurement_order)), position = pj, size=1, show.legend = FALSE)+
  scale_fill_manual(values = c("#EBB134", "#ED6B5F", "#6060A8"), labels=c('OFAV Shallow', 'OFAV Deep', 'OFRA Deep')) + 
  scale_color_manual(values = c("#EBB134", "#ED6B5F", "#6060A8")) + 
  labs(y= "Chlorophyl a (ug/cm2)", x = "Depth Treatment", fill='Origin') + 
  scale_x_discrete(labels=c('Deep', 'Shallow', 'Deep', 'Shallow', 'Deep', 'Shallow')) + 
  theme_classic() +
  stat_compare_means(comparisons = treatment_comparisons, method = "t.test", 
                     symnum.args = list(cutpoints = c(0, 0.001 ,0.01, 0.05, Inf), symbols = c("***", "**", "*", "")))
chla

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

# Host AFDW --------------------------------------------------------------------

afdw <- ggplot(raw, aes(x=full_treatment, y=Host_AFDW_mg.cm2, color=group)) +
  geom_boxplot() +
  labs(y= " (mg/cm2)", x = "Treatment")
afdw_pvalue <- afdw + stat_compare_means(comparisons = treatment_comparisons, method = "t.test", size = 3)
ggsave("treatments_H_afdw_pvalue.jpg", plot = afdw_pvalue , path = '~/Desktop/GITHUB/TLPR21/TL_Trans_Results/', width = 6,
       height = 6) 
afdw_pvalue 

# Sym AFDW ----------------------------------------------------------------
S_afdw <- ggplot(raw, aes(x=full_treatment, y=Sym_AFDW_mg.cm2, color=group)) +
  geom_boxplot() +
  labs(y= " (mg/cm2)", x = "Treatment")
S_afdw_pvalue <- S_afdw + stat_compare_means(comparisons = treatment_comparisons, method = "t.test", size = 3)
ggsave("treatments_S_afdw_pvalue.jpg", plot = S_afdw_pvalue , path = '~/Desktop/GITHUB/TLPR21/TL_Trans_Results/', width = 6,
       height = 6) 
S_afdw_pvalue

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
       height = 9) 


# OFRA PP vs PS just height and width 
morph_OFRA_PP_PS <- morph_long %>%
  filter(measurement != "D") %>%
  filter(measurement != "CA.A") %>%
  filter(measurement != "A") %>%
  filter(measurement != "CA") %>%
  filter(full_treatment == "OFRA_PP" | full_treatment == "OFRA_PS")

plot_morph_OFRA_PP_PS <- ggplot(morph_OFRA_PP_PS, aes(x=factor(measurement, level=measurement_order), y=value, fill=full_treatment)) +
  geom_boxplot(alpha=0.7) +
  labs(y= "Mean Length (cm or cm2)", x = "Trait", fill = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", 
                     symnum.args = list(c(0, 0.0001, 0.001, 0.01, 0.05, 1),
                                        symbols = c("****", "***", "**", "*", ""))) +
  scale_fill_manual(values = c("#464694", "#A4A4DB"), labels=c('Deep to Deep', 'Deep to Shallow'))+
  geom_point(aes(color = full_treatment), position = pj, size=1, show.legend = FALSE)+
  scale_color_manual(values = c("#464694", "#A4A4DB")) +
  scale_x_discrete(labels=c('Calice Height', 'Columella Height', 'Calice Width', 'Columella Width')) + 
  theme_classic() 


plot_morph_OFRA_PP_PS 

ggsave("morph_OFRA_PP_PS_HW_pvalue.jpg", plot = plot_morph_OFRA_PP_PS  , path = '~/Desktop/GITHUB/TLPR21/TL_Trans_Results/',width = 6,
       height = 6) 


# Graphs for Carlos  ---------------------------------------------------
#morphology - diameter
# graph calice density 
diameter <- ggplot(raw, aes(x=factor(full_treatment, level=x_order), y=di, fill=factor(group, level=measurement_order))) +
  geom_boxplot(alpha=0.5, outlier.size=0) +
  geom_point(aes(color=factor(group, level=measurement_order)), position = pj, size=1, show.legend = FALSE)+
  scale_fill_manual(values = c("#EBB134", "#ED6B5F", "#6060A8"), labels=c('OFAV Shallow', 'OFAV Deep', 'OFRA Deep')) + 
  scale_color_manual(values = c("#EBB134", "#ED6B5F", "#6060A8")) + 
  labs(y= "Diameter (cm)", x = "Depth Treatment", fill='Origin') + 
  scale_x_discrete(labels=c('Deep', 'Shallow', 'Deep', 'Shallow', 'Deep', 'Shallow')) + 
  theme_classic() +
  stat_compare_means(comparisons = treatment_comparisons, method = "wilcox.test", 
                     symnum.args = list(cutpoints = c(0, 0.001 ,0.01, 0.05, Inf), symbols = c("***", "**", "*", "")))
Cdiameter <- ggplot(raw, aes(x=factor(full_treatment, level=x_order), y=Cdi, fill=factor(group, level=measurement_order))) +
  geom_boxplot(alpha=0.5, outlier.size=0) +
  geom_point(aes(color=factor(group, level=measurement_order)), position = pj, size=1, show.legend = FALSE)+
  scale_fill_manual(values = c("#EBB134", "#ED6B5F", "#6060A8"), labels=c('OFAV Shallow', 'OFAV Deep', 'OFRA Deep')) + 
  scale_color_manual(values = c("#EBB134", "#ED6B5F", "#6060A8")) + 
  labs(y= "Diameter (cm)", x = "Depth Treatment", fill='Origin') + 
  scale_x_discrete(labels=c('Deep', 'Shallow', 'Deep', 'Shallow', 'Deep', 'Shallow')) + 
  theme_classic() +
  stat_compare_means(comparisons = treatment_comparisons, method = "wilcox.test", 
                     symnum.args = list(cutpoints = c(0, 0.001 ,0.01, 0.05, Inf), symbols = c("***", "**", "*", "")))
diameter
Cdiameter
pairwise.wilcox.test(raw$di, raw$full_treatment, p.adjust.method="none")
pairwise.wilcox.test(raw$Cdi, raw$full_treatment, p.adjust.method="none")

both_diameters <- ggarrange(diameter,Cdiameter + rremove("ylab"), 
          labels = c("A", "B"),
          ncol = 2, nrow = 1, 
          common.legend = TRUE,
          legend="right")
ggsave("TL_Trans_combined_diameter_boxplot.jpg", both_diameters, path = '~/Desktop/GITHUB/TLPR21/TL_Trans_Results/', width = 8,
       height = 4)


### JUST CONTROLS
measurement_order2 <- c('OFAV','OFRA') 
x_order <- c('OFAV_SS','OFAV_PP','OFRA_PP') 
pj <- position_jitterdodge(jitter.width=0.1, seed=9,
                           jitter.height = 0)
treatment_comparisons2 <- list( c("OFAV_PP","OFRA_PP"), c("OFAV_SS","OFAV_PP"))
controls <- raw %>%
  filter(., full_treatment == "OFAV_PP" | full_treatment == "OFAV_SS" | full_treatment == "OFRA_PP")

diameter2 <- ggplot(controls, aes(x=factor(full_treatment, level=x_order), y=di, fill=factor(species, level=measurement_order2))) +
  geom_boxplot(alpha=0.5, outlier.size=0) +
  geom_point(aes(color=factor(group, level=measurement_order)), position = pj, size=1, show.legend = FALSE)+
  scale_fill_manual(values = c("#EBB134", "#6060A8"), labels=c('OFAV', 'OFRA')) + 
  scale_color_manual(values = c("#EBB134", "#EBB134", "#6060A8")) + 
  labs(y= "Calice Diameter (cm)", x = "Depth Treatment", fill='Species') + 
  scale_x_discrete(labels=c('Shallow', 'Deep', 'Deep')) + 
  theme_classic() +
  stat_compare_means(comparisons = treatment_comparisons2, method = "wilcox.test", 
                     symnum.args = list(cutpoints = c(0, 0.001 ,0.01, 0.05, Inf), symbols = c("***", "**", "*", "")))

Cdiameter2 <- ggplot(controls, aes(x=factor(full_treatment, level=x_order), y=Cdi, fill=factor(species, level=measurement_order2))) +
  geom_boxplot(alpha=0.5, outlier.size=0) +
  geom_point(aes(color=factor(group, level=measurement_order)), position = pj, size=1, show.legend = FALSE)+
  scale_fill_manual(values = c("#EBB134", "#6060A8"), labels=c('OFAV', 'OFRA')) + 
  scale_color_manual(values = c("#EBB134", "#EBB134", "#6060A8")) + 
  labs(y= "Columella Diameter (cm)", x = "Depth Treatment", fill='Species') + 
  scale_x_discrete(labels=c('Shallow', 'Deep', 'Deep')) + 
  theme_classic() +
  stat_compare_means(comparisons = treatment_comparisons2, method = "wilcox.test", 
                     symnum.args = list(cutpoints = c(0, 0.001 ,0.01, 0.05, Inf), symbols = c("***", "**", "*", "")))
Cdiameter2

both_diameters2 <- ggarrange(diameter2,Cdiameter2, 
                            labels = c("A", "B"),
                            ncol = 2, nrow = 1, 
                            common.legend = TRUE,
                            legend="right")
both_diameters2
ggsave("TL_Trans_combined_diameter_boxplot_controls.jpg", both_diameters2, path = '~/Desktop/GITHUB/TLPR21/TL_Trans_Results/', width = 8,
       height = 4)

## OFAV ONLY CORRECTED COLORS
measurement_order2 <- c('OFAV_SS','OFAV_PP') 
x_order3 <- c('Cdi','di')
pj <- position_jitterdodge(jitter.width=0.1, seed=9,
                           jitter.height = 0)
#treatment_comparisons3 <- list( c("OFAV_PP","OFAV_PP"), c("OFAV_SS","OFAV_SS"))

#filter just control OFAVs
control_OFAV <- raw %>%
  filter(., full_treatment == "OFAV_PP" | full_treatment == "OFAV_SS") %>%
  select(c(species,full_treatment,di,Cdi)) %>%
  pivot_longer(.,c(Cdi,di), names_to = "measurement", values_to = "values")

OFAV_controls_diameter <- ggplot(control_OFAV, aes(x=factor(measurement, level=x_order3), y=values, fill=factor(full_treatment, level=measurement_order2)))+ 
  geom_boxplot(alpha=0.5, outlier.size=0) +
  geom_point(aes(color=full_treatment), position = pj, size=1, show.legend = FALSE) +
  scale_fill_manual(values = c("#a11d13","#6591ad"), labels=c('Shallow', 'Deep')) + 
  scale_color_manual(values = c("#a11d13","#6591ad")) + 
  labs(y= "Diameter (cm)", x = "", fill='Origin') + 
  scale_x_discrete(labels=c('Columella','Calice')) + 
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent'),
    axis.line.x = element_line(colour = "grey50"),
    axis.line.y = element_line(colour = "grey50"))+ 
  stat_compare_means(aes(group = full_treatment), method = "wilcox.test", label = "p.signif")


OFAV_controls_diameter

ggsave("TL_Trans_OFAV_controls_diameter.png", OFAV_controls_diameter, path = '~/Desktop/GITHUB/TLPR21/TL_Trans_Results/', width = 8,
       height = 4, bg = "transparent")

