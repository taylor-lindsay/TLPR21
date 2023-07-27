## # Title: TL_Trans_Correlation_Plots.R
# Author: TL
# Date: 07.20.2023
# Input files: 
#  TL_Trans_Results.csv
# Output files: 
#  
# Notes

install.packages("ggpmisc", repos = "https://cran.rstudio.com")

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(ggpmisc)

# Data -------------------------------------------------------------------

raw <- read.csv('~/Desktop/GITHUB/TLPR21/TL_Trans_Results.csv')

# make a 'group column' that allows us to facet wrap 
raw <- raw %>%
  mutate(group = str_sub(full_treatment,1,6))

# Morphology  -------------------------------------------------------------------

ggplot(raw, aes(A,CA)) + 
  stat_poly_line() +
  stat_poly_eq() +
  geom_point()
