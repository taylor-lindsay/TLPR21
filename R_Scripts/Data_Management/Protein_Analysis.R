# Title: Protein Analysis Script 
# Author: Taylor Lindsay
# Date: 03.05.2023
# Input files: 
# Platemap, 
# Output files: 
# 
# Notes 


# Libraries ---------------------------------------------------------------

# install packages if you dont already have them
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("broom")) install.packages("broom")

# load packages
library(tidyverse)
library(broom)
library(plotrix)


# Data prep ---------------------------------------------------------------


# Define function to read in protein data
read_prot <- function(file) {
  prot_data <- read_csv(file, skip = 30) %>%       #
    #select(-1) %>%     
    magrittr::set_colnames(c("row", 1:12, "wavelength")) %>%                # 
    #fill(row) %>%
    gather("col", "absorbance", -row) %>%
    unite("well", c(row, col), sep = "")
}

# List chlorophyll data files
prot_path <- "~/Desktop/GitHub/TLPR21/Protein/TL_Trans/"                  # Path to chlorophyll data directory     #####
all_prot_files <- list.files(path = prot_path, pattern = "*.csv")          # List all files in directory
prot_platemaps <- list.files(path = prot_path, pattern = "platemap")       # List platemap files
prot_data_files <- setdiff(all_prot_files, prot_platemaps)                  # List absorbance data files

# Read in all files into tibble
df <- tibble(file = prot_data_files) %>%
  mutate(platemap = map(file, ~ read_csv(paste0(prot_path, tools::file_path_sans_ext(.), "_platemap.csv"))),
         prot_data = map(file, ~ read_prot(paste0(prot_path, .))))

# Merge platemap and data for each plate
df <- df %>%
  mutate(merged = map2(platemap, prot_data, ~ right_join(.x, .y)))

# Load homogenate volume
homog_vols <- read_csv("~/Desktop/GITHUB/TLPR21/TL_Trans_Raw_Master.csv") %>%                                              #####
select(colony_id, airbrush_volume) %>%
  filter(!is.na(airbrush_volume))

# Load surface area
sa <- read_csv("~/Desktop/GITHUB/TLPR21/Surface_Area/TL_Trans_Surface_Area.csv") %>%                                #####
select(colony_id, surface_area) %>%
  filter(!is.na(surface_area))

# average all technical replicates for each plate/sample/wavelength, including all acetone blanks together (per plate)
df <- df %>%
  unnest(merged) %>%
  filter(!is.na(colony_id)) %>%                         # remove empty wells (colony_id is NA)
  group_by(file, colony_id) %>%
  summarise(n = n(), mean_abs = mean(absorbance)) 


# Plot standard curve?  ---------------------------------------------------

# Create standard curve following kit instructions
standards <- tribble(
  ~std, ~BSA_ug.mL,
  "A",        2000,
  "B",        1500,
  "C",        1000,
  "D",         750,
  "E",         500,
  "F",         250,
  "G",         125,
  "H",          25,
  "I",           0
)

# separate standards data from files and combine with standards above. 
std_curve <- df %>%
  filter(grepl("Standard", colony_id)) %>%                    # select only the standards 
  select(file, colony_id, abs562 = `mean_abs`) %>%            # select only a few columns 
  rename(std = colony_id) %>%                                 # rename the colony id column to std 
  mutate(std = str_sub(std, 10, 10)) %>%                      # name the column "str" with only the standard letter name in it (item 10
  left_join(standards)                                        # join with the standards tibble above 

## Fit linear model for standard curve
mod <- lm(BSA_ug.mL ~ abs562, data = std_curve)
coef(mod)

## Fit nonlinear model for standard curve
## mod <- nls(formula = BSA_ug.mL ~ z + a * exp(b * abs562), start = list(z = 0, a = 1, b = 1), data = std_curve)

fitted <- mod %>% broom::augment()

# Plot standard curve
std_curve_plot <- std_curve %>%
  ggplot(aes(x = abs562, y = BSA_ug.mL)) +
  geom_point(color = "red", size = 3) 
std_curve_plot + 
  geom_line(data = fitted, aes(x = abs562, y = .fitted)) +
  labs(title = "Standard curve")


# Calculate protein -------------------------------------------------------

# Calculate protein concentration for all samples using standard curve
prot <- df %>%
  #unnest(merged) %>%
  filter(!grepl("Standard", colony_id)) %>%                     # Get just samples (not standards)
  select(file, colony_id, abs562 = `mean_abs`) %>%        # Select only needed columns
  filter(!is.na(colony_id)) %>%                                 # Filter out empty wells
  filter(colony_id != "BK") %>%                                 # Filter out blank wells
  mutate(prot_ug.mL = map_dbl(abs562, ~ predict(mod, newdata = data.frame(abs562 = .))))    # Use standard curve to convert absorbance to protein

std_curve_plot + 
  geom_point(data = prot, aes(x = abs562, y = prot_ug.mL), pch = "X", cex = 5, alpha = 0.3) +
  labs(title = "All samples projected on standard curve")




# Standardize to vol & Surface area  --------------------------------------

# Join homogenate volumes and surface area with sample metadata
metadata <- full_join(sa, homog_vols)

# Join prot data with metadata
prot <- left_join(prot, metadata) %>%
  mutate(prot_ug = prot_ug.mL * airbrush_volume,
         prot_ug.cm2 = prot_ug / surface_area,
         prot_mg.cm2 = prot_ug.cm2 / 1000)

protein <- prot[, c(2,4,8,9)]

write_csv(protein, '~/Desktop/GitHub/TLPR21/Protein/TL_Trans_Protein_Results.csv')
