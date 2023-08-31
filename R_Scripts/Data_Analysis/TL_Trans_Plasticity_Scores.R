# Title: TL_Trans_Plasticity_Scores.R
# Author: Taylor Lindsay
# Date: 04.18.2023
# Input files:
#  
# Output files: 
# 
# Notes 


# Packages & Data ---------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(ggfortify)   # needed to make pca plots 
library(vegan)       # needed for PERMANOVA 
library(ggpubr)      # needed for ggarrange 
library(plotrix)     # needed for std.error()

# read in data 
raw <- read.csv('~/Desktop/GITHUB/TLPR21/TL_Trans_Results.csv')

means <- raw %>%
  .[complete.cases(.),] %>%              
  group_by(full_treatment) %>%
  summarise(across(everything(), mean)) %>%
  .[c(1,6:12,16:21)]  

data2 <- means %>%
  mutate(Genotype=(substring(.$full_treatment, 1, 6)))   %>%         # make a column that will group the pairs 
  mutate(Type= ifelse(full_treatment == "OFAV_PP", "Control",      # make a column that will dictate control or treatment 
                    ifelse(full_treatment == "OFAV_SS", "Control",
                           ifelse(full_treatment == "OFRA_PP", "Control",
                                  "Treatment")))) %>%
  .[c(15:16,2:14)] 

data_full <- raw %>%
  .[complete.cases(.),] %>%     
  mutate(Genotype=(substring(.$full_treatment, 1, 6)))   %>%         # make a column that will group the pairs 
  mutate(Type= ifelse(full_treatment == "OFAV_PP", "Control",      # make a column that will dictate control or treatment 
                      ifelse(full_treatment == "OFAV_SS", "Control",
                             ifelse(full_treatment == "OFRA_PP", "Control",
                                    "Treatment")))) 

  
# Plasticity Scores using 6 mean values -------------------------------------------------------

#Scale and center datasets
data_scaled <- scale(data2[3:15], center = T, scale = T) # scaled variables

#Identify Factors 
fac <- data2[1:2]

# PCA of all variables
pca.out <- prcomp(data_scaled, center=FALSE, scale=FALSE) #run PCA
summary(pca.out) #view results

#combine PC1 and PC2 into dataframe for plotting and calculation of PCA distance for plasticity section below
PCs <- as.data.frame(cbind(pca.out$x[,1], pca.out$x[,2]))
PCs.meta <- cbind(fac, PCs)

#convert to wide format to enable initial and subsequent comparisons
PCs.meta.wide <- pivot_wider(PCs.meta, values_from = c(V1, V2), names_from =Type) %>%
  .[complete.cases(.),]

#Creating PCA Plot from October to January Comparison
fig1 <- ggplot(PCs.meta.wide,aes(x=V1_Control,y=V2_Control))+
  geom_point(aes(), size=2, color="black")+ 
  geom_segment(aes(x=V1_Control, y=V2_Control, xend=V1_Treatment, yend=V2_Treatment, color=Genotype))+
  xlim(-6,5)+
  ylim(-6,5)+
  xlab(label = "PC1")+
  ylab(label = "PC2")+
  theme_classic() 

fig1

#Calculate distance between origin and transplant points
#plasticity (amount of change in X,Y space) between the genotypes for each time and site relative to the origin at TP0

PCA.dist <- as.data.frame(as.matrix(dist(PCs)))

#convert PCs.meta.wide back to long format (this is to fix the formatting issues where we want to replicate nursery values)  - AH come back to this after the nursery values can be copied in the code
#join with metadata
PCA.dist <- cbind(fac, PCA.dist)

PCA.dist <- PCA.dist %>%
  .[c(1:4)] 
colnames(PCA.dist)[4] <- "Distance"

#Plast.Data$group <- paste0(Plast.Data$Genotype, Plast.Data$site)
#Plast.Data$groupTS <- paste0(Plast.Data$timepoint, Plast.Data$site)

# Test for differences in plasticity
plast.mod <-aov(Distance ~ Genotype, data=PCA.dist)
summary(plast.mod)
hist(plast.mod$residuals)
qqnorm(plast.mod$residuals)
qqline(plast.mod$residuals)

#Posthoc Tests for Plasticity Score (not really needed since trend is very clear and only tp significant)
TukeyHSD(plast.mod)

#try kruskal wallis tests for low sample size
kruskal.test(Distance ~ Genotype, data=PCA.dist)
kruskal.test(Distance ~ Type, data=PCA.dist)


# Plast.Data %>%
#   ggplot(aes(timepoint,Distance)) +
#   geom_boxplot(aes(fill=site)) +
#   geom_point(aes(group=groupTS, shape=Genotype), color="black", position=position_dodge(width=0.75))+
#   geom_line(aes(group=group, color=site), alpha = .4) +
#   theme_classic() 


pj <- position_jitterdodge(jitter.width=0.05, seed=9,
                           jitter.height = 0,
                           dodge.width = 0.3)

#create group mean dataset 
gd <- PCA.dist %>%
  group_by(Genotype, Type)%>%
  summarise(mean=mean(Distance),
            sem=std.error(Distance))

#Create final plot for Reaction Norms

PCA.dist %>%
  ggplot(aes(x=Type, y=Distance, fill=Genotype))+
  #geom_boxplot(outlier.colour = NA, width =0.75, alpha=0.2) +
  lemon::geom_pointpath(aes(colour=Genotype,group=interaction(Genotype)),position = pj, alpha=0.4)+
  geom_point(aes(colour=Genotype, x=Type, y=mean), data=gd, position = pj, size=2)+
  #geom_line(aes(colour=site, group=site, x=timepoint, y=mean), data=gd, size=1, alpha=1, position = pj)+
  #geom_errorbar(aes(y=mean, ymin = mean-sem, ymax = mean+sem), data=gd, position = pj, width = 0)+
  theme_classic()

Plasticity <- ggarrange(RNorms,  common.legend = F)
Plasticity
#ggsave("output/ReactionNorms.pdf", width = 4, height = 4, units = "in")


# Plasticity Scores with full data -------------------------------------------

data_full <- raw %>%
  .[complete.cases(.),] %>%     
  mutate(Genotype=(substring(.$full_treatment, 1, 6)))   %>%         # make a column that will group the pairs 
  mutate(Type= ifelse(full_treatment == "OFAV_PP", "Control",      # make a column that will dictate control or treatment 
                      ifelse(full_treatment == "OFAV_SS", "Control",
                             ifelse(full_treatment == "OFRA_PP", "Control",
                                    "Treatment")))) %>%
  .[c(2:12,16:21,23:24)] 
  
#Scale and center datasets
data_scaled <- scale(data_full[5:17], center = T, scale = T) # scaled variables

#Identify Factors 
fac <- data_full[c(1:4,18:19)]

# PCA of all variables
pca.out <- prcomp(data_scaled, center=FALSE, scale=FALSE) #run PCA
summary(pca.out) #view results

#combine PC1 and PC2 into dataframe for plotting and calculation of PCA distance for plasticity section below
PCs <- as.data.frame(cbind(pca.out$x[,1], pca.out$x[,2]))
PCs.meta <- cbind(fac, PCs)

#convert to wide format to enable initial and subsequent comparisons
PCs.meta.wide <- pivot_wider(PCs.meta, values_from = c(V1, V2), names_from =Type) 

#Creating PCA Plot from October to January Comparison
fig1 <- ggplot(PCs.meta.wide,aes(x=V1_Control,y=V2_Control))+
  geom_point(aes(), size=2, color="black")+ 
  geom_segment(aes(x=V1_Control, y=V2_Control, xend=V1_Treatment, yend=V2_Treatment, color=Genotype))+
  xlim(-6,5)+
  ylim(-6,5)+
  xlab(label = "PC1")+
  ylab(label = "PC2")+
  theme_classic() 

fig1

#Calculate distance between origin and transplant points
#plasticity (amount of change in X,Y space) between the genotypes for each time and site relative to the origin at TP0

PCA.dist <- as.data.frame(as.matrix(dist(PCs)))

#convert PCs.meta.wide back to long format (this is to fix the formatting issues where we want to replicate nursery values)  - AH come back to this after the nursery values can be copied in the code
#join with metadata
PCA.dist <- cbind(fac, PCA.dist)

PCA.dist <- PCA.dist %>%
  .[c(1:8)] 
colnames(PCA.dist)[8] <- "Distance"

#Plast.Data$group <- paste0(Plast.Data$Genotype, Plast.Data$site)
#Plast.Data$groupTS <- paste0(Plast.Data$timepoint, Plast.Data$site)

# Test for differences in plasticity
plast.mod <-aov(Distance ~ Genotype*Type, data=PCA.dist)
summary(plast.mod)
hist(plast.mod$residuals)
qqnorm(plast.mod$residuals)
qqline(plast.mod$residuals)

#Posthoc Tests for Plasticity Score (not really needed since trend is very clear and only tp significant)
TukeyHSD(plast.mod)

#OFAVP Control:Treatment p= 0.99999
#OFAVS Control:Treatment p= 0.0008775
#OFRAp Control:Treatment p= 0.6344


#try kruskal wallis tests for low sample size
kruskal.test(Distance ~ Genotype, data=PCA.dist)       # p=0.7495
kruskal.test(Distance ~ Type, data=PCA.dist)           # p=0.01581


# Plast.Data %>%
#   ggplot(aes(timepoint,Distance)) +
#   geom_boxplot(aes(fill=site)) +
#   geom_point(aes(group=groupTS, shape=Genotype), color="black", position=position_dodge(width=0.75))+
#   geom_line(aes(group=group, color=site), alpha = .4) +
#   theme_classic() 


pj <- position_jitterdodge(jitter.width=0.05, seed=9,
                           jitter.height = 0,
                           dodge.width = 0.3)

#create group mean dataset 
gd <- PCA.dist %>%
  group_by(Genotype, Type)%>%
  summarise(mean=mean(Distance),
            sem=std.error(Distance))

#Create final plot for Reaction Norms

plastic_box <- PCA.dist %>%
  ggplot(aes(x=Type, y=Distance, fill=Genotype))+
  geom_boxplot(outlier.colour = NA, width =0.75, alpha=0.2) +
  #lemon::geom_pointpath(aes(colour=Genotype,group=interaction(Genotype)),position = pj, alpha=0.4)+
  #geom_point(aes(colour=Genotype, x=Type, y=mean), data=gd, position = pj, size=2)+
  #geom_line(aes(group=Genotype, x=Type, y=mean), data=gd, size=0.5, alpha=1, position = pj)+
  #geom_errorbar(aes(y=mean, ymin = mean-sem, ymax = mean+sem), data=gd, position = pj, width = 0)+
  theme_classic()

ggsave('~/Desktop/TL_Trans_Plasticity_Boxplot.jpg', plot=plastic_box, width = 4, height = 4, units = "in")

plastic_line <- PCA.dist %>%
  ggplot(aes(x=Type, y=Distance, fill=Genotype))+
  #geom_boxplot(outlier.colour = NA, width =0.75, alpha=0.2) +
  #lemon::geom_pointpath(aes(colour=Genotype,group=interaction(Genotype)),position = pj, alpha=0.4)+
  geom_line(aes(group=Genotype, x=Type, y=mean), data=gd, size=0.5, alpha=1)+
  geom_point(aes(colour=Genotype, x=Type, y=mean), data=gd, size=2)+
  #geom_errorbar(aes(y=mean, ymin = mean-sem, ymax = mean+sem), data=gd, position = pj, width = 0)+
  theme_classic()
plastic_line

ggsave('~/Desktop/TL_Trans_Plasticity_Lines.jpg',plot=plastic_line, width = 4, height = 4, units = "in")


# Plasticity Scores with just morph ---------------------------------------

data_morph <- raw %>%
  .[complete.cases(.),] %>%     
  mutate(Genotype=(substring(.$full_treatment, 1, 6)))   %>%         # make a column that will group the pairs 
  mutate(Type= ifelse(full_treatment == "OFAV_PP", "Control",      # make a column that will dictate control or treatment 
                      ifelse(full_treatment == "OFAV_SS", "Control",
                             ifelse(full_treatment == "OFRA_PP", "Control",
                                    "Treatment")))) %>%
  .[c(2:12,23:24)] 

#Scale and center datasets
data_scaled <- scale(data_morph[5:11], center = T, scale = T) # scaled variables

#Identify Factors 
fac <- data_morph[c(1:4,12:13)]

# PCA of all variables
pca.out <- prcomp(data_scaled, center=FALSE, scale=FALSE) #run PCA
summary(pca.out) #view results

#combine PC1 and PC2 into dataframe for plotting and calculation of PCA distance for plasticity section below
PCs <- as.data.frame(cbind(pca.out$x[,1], pca.out$x[,2]))
PCs.meta <- cbind(fac, PCs)

#convert to wide format to enable initial and subsequent comparisons
PCs.meta.wide <- pivot_wider(PCs.meta, values_from = c(V1, V2), names_from =Type) 

#Creating PCA Plot from October to January Comparison
fig1 <- ggplot(PCs.meta.wide,aes(x=V1_Control,y=V2_Control, color=Genotype))+
  geom_point(aes(), size=2)+ 
  geom_segment(aes(x=V1_Control, y=V2_Control, xend=V1_Treatment, yend=V2_Treatment, color=Genotype))+
  xlim(-6,5)+
  ylim(-6,5)+
  xlab(label = "PC1")+
  ylab(label = "PC2")+
  theme_classic() 

fig1

#Calculate distance between origin and transplant points
#plasticity (amount of change in X,Y space) between the genotypes for each time and site relative to the origin at TP0

PCA.dist <- as.data.frame(as.matrix(dist(PCs)))

#convert PCs.meta.wide back to long format (this is to fix the formatting issues where we want to replicate nursery values)  - AH come back to this after the nursery values can be copied in the code
#join with metadata
PCA.dist <- cbind(fac, PCA.dist)

PCA.dist <- PCA.dist %>%
  .[c(1:8)] 
colnames(PCA.dist)[8] <- "Distance"

#Plast.Data$group <- paste0(Plast.Data$Genotype, Plast.Data$site)
#Plast.Data$groupTS <- paste0(Plast.Data$timepoint, Plast.Data$site)

# Test for differences in plasticity
plast.mod <-aov(Distance ~ Genotype*Type, data=PCA.dist)
summary(plast.mod)
hist(plast.mod$residuals)
qqnorm(plast.mod$residuals)
qqline(plast.mod$residuals)

#Posthoc Tests for Plasticity Score (not really needed since trend is very clear and only tp significant)
TukeyHSD(plast.mod)

#OFAVP Control:Treatment p= 0.9859897
#OFAVS Control:Treatment p= 0.1264513
#OFRAp Control:Treatment p= 0.0336895

#try kruskal wallis tests for low sample size
kruskal.test(Distance ~ Genotype, data=PCA.dist)       # p=0.7495
kruskal.test(Distance ~ Type, data=PCA.dist)           # p=0.01581


# Plast.Data %>%
#   ggplot(aes(timepoint,Distance)) +
#   geom_boxplot(aes(fill=site)) +
#   geom_point(aes(group=groupTS, shape=Genotype), color="black", position=position_dodge(width=0.75))+
#   geom_line(aes(group=group, color=site), alpha = .4) +
#   theme_classic() 


pj <- position_jitterdodge(jitter.width=0.05, seed=9,
                           jitter.height = 0,
                           dodge.width = 0.3)

#create group mean dataset 
gd <- PCA.dist %>%
  group_by(Genotype, Type)%>%
  summarise(mean=mean(Distance),
            sem=std.error(Distance))



#Create final plot for Reaction Norms

plastic_box <- PCA.dist %>%
  ggplot(aes(x=Type, y=Distance, fill=Genotype))+
  geom_boxplot(outlier.colour = NA, width =0.75, alpha=0.2) +
  #lemon::geom_pointpath(aes(colour=Genotype,group=interaction(Genotype)),position = pj, alpha=0.4)+
  #geom_point(aes(colour=Genotype, x=Type, y=mean), data=gd, position = pj, size=2)+
  #geom_line(aes(group=Genotype, x=Type, y=mean), data=gd, size=0.5, alpha=1, position = pj)+
  #geom_errorbar(aes(y=mean, ymin = mean-sem, ymax = mean+sem), data=gd, position = pj, width = 0)+
  theme_classic()
plastic_box

ggsave('~/Desktop/TL_Trans_Plasticity_Boxplot_morph.jpg', plot=plastic_box, width = 4, height = 4, units = "in")

plastic_line <- PCA.dist %>%
  ggplot(aes(x=Type, y=Distance, fill=Genotype))+
  #geom_boxplot(outlier.colour = NA, width =0.75, alpha=0.2) +
  #lemon::geom_pointpath(aes(colour=Genotype,group=interaction(Genotype)),position = pj, alpha=0.4)+
  geom_line(aes(group=Genotype, x=Type, y=mean), data=gd, size=0.5, alpha=1)+
  geom_point(aes(color=Genotype, x=Type, y=mean), data=gd, position = pj, size=2)+
  scale_color_manual(values = c("#ED6B5F","#EBB134", "#6060A8")) + 
  geom_errorbar(aes(y=mean, ymin = mean-sem, ymax = mean+sem), data=gd,  position = pj, width = 0)+
  theme_classic()
plastic_line

ggsave('~/Desktop/TL_Trans_Plasticity_Lines_morph.jpg',plot=plastic_line, width = 4, height = 4, units = "in")


# Plasticity Scores with just phys ---------------------------------------

data_phys <- raw %>%
  .[complete.cases(.),] %>%     
  mutate(Genotype=(substring(.$full_treatment, 1, 6)))   %>%         # make a column that will group the pairs 
  mutate(Type= ifelse(full_treatment == "OFAV_PP", "Control",      # make a column that will dictate control or treatment 
                      ifelse(full_treatment == "OFAV_SS", "Control",
                             ifelse(full_treatment == "OFRA_PP", "Control",
                                    "Treatment")))) %>%
  .[c(2:5,16:21,23:24)] 

#Scale and center datasets
data_scaled <- scale(data_phys[5:10], center = T, scale = T) # scaled variables

#Identify Factors 
fac <- data_phys[c(1:4,11:12)]

# PCA of all variables
pca.out <- prcomp(data_scaled, center=FALSE, scale=FALSE) #run PCA
summary(pca.out) #view results

#combine PC1 and PC2 into dataframe for plotting and calculation of PCA distance for plasticity section below
PCs <- as.data.frame(cbind(pca.out$x[,1], pca.out$x[,2]))
PCs.meta <- cbind(fac, PCs)

#convert to wide format to enable initial and subsequent comparisons
PCs.meta.wide <- pivot_wider(PCs.meta, values_from = c(V1, V2), names_from =Type) 

#Creating PCA Plot from October to January Comparison
fig1 <- ggplot(PCs.meta.wide,aes(x=V1_Control,y=V2_Control, color=Genotype))+
  geom_point(aes(), size=2)+ 
  geom_segment(aes(x=V1_Control, y=V2_Control, xend=V1_Treatment, yend=V2_Treatment, color=Genotype))+
  xlim(-6,5)+
  ylim(-6,5)+
  xlab(label = "PC1")+
  ylab(label = "PC2")+
  theme_classic() 

fig1

#Calculate distance between origin and transplant points
#plasticity (amount of change in X,Y space) between the genotypes for each time and site relative to the origin at TP0

PCA.dist <- as.data.frame(as.matrix(dist(PCs)))

#convert PCs.meta.wide back to long format (this is to fix the formatting issues where we want to replicate nursery values)  - AH come back to this after the nursery values can be copied in the code
#join with metadata
PCA.dist <- cbind(fac, PCA.dist)

PCA.dist <- PCA.dist %>%
  .[c(1:8)] 
colnames(PCA.dist)[8] <- "Distance"

#Plast.Data$group <- paste0(Plast.Data$Genotype, Plast.Data$site)
#Plast.Data$groupTS <- paste0(Plast.Data$timepoint, Plast.Data$site)

# Test for differences in plasticity
plast.mod <-aov(Distance ~ Genotype*Type, data=PCA.dist)
summary(plast.mod)
hist(plast.mod$residuals)
qqnorm(plast.mod$residuals)
qqline(plast.mod$residuals)

#Posthoc Tests for Plasticity Score (not really needed since trend is very clear and only tp significant)
TukeyHSD(plast.mod)

#OFAVP Control:Treatment p= 0.9999372
#OFAVS Control:Treatment p= 0.0098468
#OFRAp Control:Treatment p= 1.0000000

#try kruskal wallis tests for low sample size
kruskal.test(Distance ~ Genotype, data=PCA.dist)       # p=0.7495
kruskal.test(Distance ~ Type, data=PCA.dist)           # p=0.01581


# Plast.Data %>%
#   ggplot(aes(timepoint,Distance)) +
#   geom_boxplot(aes(fill=site)) +
#   geom_point(aes(group=groupTS, shape=Genotype), color="black", position=position_dodge(width=0.75))+
#   geom_line(aes(group=group, color=site), alpha = .4) +
#   theme_classic() 


pj <- position_jitterdodge(jitter.width=0.05, seed=9,
                           jitter.height = 0,
                           dodge.width = 0.3)

#create group mean dataset 
gd <- PCA.dist %>%
  group_by(Genotype, Type)%>%
  summarise(mean=mean(Distance),
            sem=std.error(Distance))

#library("cowplot")
library(ggpubr)
library("RColorBrewer")

#Create final plot for Reaction Norms
measurement_order <- c('OFAV_S','OFAV_P','OFRA_P') 

plastic_box <- PCA.dist %>%
  ggplot(aes(x=Type, y=Distance, fill=factor(Genotype, level=measurement_order)))+
  geom_boxplot(outlier.colour = NA, width =0.75, alpha=0.9) +
  scale_fill_manual(values = c("#EBB134", "#ED6B5F", "#6060A8")) + 
  #lemon::geom_pointpath(aes(colour=Genotype,group=interaction(Genotype)),position = pj, alpha=0.4)+
  #geom_point(aes(colour=Genotype, x=Type, y=mean), data=gd, position = pj, size=2)+
  #geom_line(aes(group=Genotype, x=Type, y=mean), data=gd, size=0.5, alpha=1, position = pj)+
  #geom_errorbar(aes(y=mean, ymin = mean-sem, ymax = mean+sem), data=gd, position = pj, width = 0)+
  theme_classic() 
  
  
plastic_box

ggsave('~/Desktop/TL_Trans_Plasticity_Boxplot_phys.jpg', plot=plastic_box, width = 4, height = 4, units = "in")

plastic_line <- PCA.dist %>%
  ggplot(aes(x=Type, y=Distance, fill=Genotype))+
  #geom_boxplot(outlier.colour = NA, width =0.75, alpha=0.2) +
  #lemon::geom_pointpath(aes(colour=Genotype,group=interaction(Genotype)),position = pj, alpha=0.4)+
  geom_line(aes(group=Genotype, x=Type, y=mean), data=gd, size=0.5, alpha=1)+
  geom_point(aes(colour=Genotype, x=Type, y=mean), data=gd, size=2)+
  #geom_errorbar(aes(y=mean, ymin = mean-sem, ymax = mean+sem), data=gd, position = pj, width = 0)+
  theme_classic()
plastic_line

ggsave('~/Desktop/TL_Trans_Plasticity_Lines_phys.jpg',plot=plastic_line, width = 4, height = 4, units = "in")
