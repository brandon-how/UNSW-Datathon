## Title: UNSW Datathon
## Author: Brandon Hao
## Date: 18 May 2023

## 0. Initialisation ------------------------------------------------------------------------------------------------

# Set working directory
setwd("C:/Users/Brandon/OneDrive - The University of Melbourne/Desktop/Health Data Science/Datathon/UNSW-Datathon")

# Install and load libraries 
packages <- c('tidyverse', 'forecast', 'DataExplorer')
installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

invisible(lapply(packages, require, character.only = TRUE))

# Set file paths  
output_path <- file.path(getwd(), 'output')

if (!dir.exists(output_path)) {
  dir.create(file.path(output_path))
}

# Read HIV data
hiv <- read.csv(file.path('data', 'HealthGymV2_CbdrhDatathon_ART4HIV.csv'))

## 1. Inspect and transform data -------------------------------------------------------------------------------------

str(hiv)

# Change some names coz they suck
names(hiv) <- c('vl', 'cd4', 'relcd4', 'gender', 'ethnic', 'base_drug_comb', 'comp_ini', 'comp_nnrti', 'extra_pi', 'extra_pk', 'vl_m', 'cd4_m', 'drug_m', 'id', 'time')

# Change patient ID to start with 1
hiv$id <- hiv$id + 1

# Transform columns into Boolean
boolean_cols <- hiv %>% select(extra_pk:drug_m) %>% names()
hiv[, boolean_cols] <-  lapply(hiv[, boolean_cols], as.logical)

# Factorise certain columns and give sensible levels
factor_cols <- hiv %>% select(gender:extra_pi) %>% names()
hiv[, factor_cols] <-  lapply(hiv[, factor_cols], factor)

levels(hiv$gender) <- c('male', 'female')
levels(hiv$ethnic) <- c('asian', 'afro', 'caucasian', 'other')
levels(hiv$base_drug_comb) <- c('ftc_tdf','3tc_abc', 'ftc_taf', 'drv_ftc_tdf', 'ftc_rtvb_tdf', 'other') # ftc_taf & 3tc_abc & ftc_tdf is NRTI backbone
levels(hiv$comp_ini) <- c('dtg', 'ral', 'evg', 'not_applied')
levels(hiv$comp_nnrti) <- c('nvp', 'efv', 'rpv', 'not_applied')
levels(hiv$extra_pi) <- c('drv', 'rtvb', 'lpv', 'rtv', 'atv', 'not_applied')

str(hiv)
summary(hiv)

## 2. Quick EDA --------------------------------------------------------------------------------------------------

# Automated EDA
hiv %>% create_report(
  #output_file = paste("Report", format(Sys.time(), "%d-%m-%Y %H:%M:%S %Z"), sep = ' - '),
  report_title = 'EDA Report - HIV Dataset',
  output_dir = output_path
)

# Plotting distribution of discrete variables
hiv %>% plot_bar(by = 'ethnic', nrow = 4, title = 'EDA - Ethnicity')
hiv %>% plot_bar(by = 'gender', nrow = 4, title = 'EDA - Gender')
hiv %>% plot_bar(by = 'base_drug_comb', nrow = 4, title = 'EDA - Drug Combo')

## 3. Quick feature engineering -----------------------------------------------------------------------------
  
# 1. Add indicator for detectable viral load using Kirby cut off of <200
# 2. Give CD4 status from slides
hiv_eng <- hiv %>% 
  mutate(vl_detect = ifelse(vl >= 200, TRUE, FALSE),
         immunosupp = ifelse(cd4 < 50, TRUE, FALSE)



# hiv %>% filter(base_drug_comb == '')

hiv %>% filter(comp_ini == 'not_applied') %>% head(10) %>% select(base_drug_comb)












