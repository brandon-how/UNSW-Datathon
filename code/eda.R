## Title: UNSW Datathon
## Author: Brandon Hao
## Date: 18 May 2023

## 0. Initialisation ------------------------------------------------------------------------------------------------

# Set working directory
setwd("C:/Users/brand/OneDrive/Desktop/Health Data Science/Datathon")

# Install and load libraries 
packages <- c('tidyverse', 'forecast', 'DataExplorer')
installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

invisible(lapply(packages, require, character.only = TRUE))

# Set file paths  
project_path <- r'{C:\Users\brand\OneDrive\Desktop\Health Data Science\Datathon\}'
output_path <- file.path(project_path, 'output')

if (!dir.exists(file.path(project_path, 'output'))) {
  dir.create(file.path(project_path, 'output'))
}

# Read HIV data
hiv <- read.csv(file.path(project_path, 'data', 'HealthGymV2_CbdrhDatathon_ART4HIV.csv'))

## 1. Inspect and transform data -------------------------------------------------------------------------------------

str(hiv)

# Change some names coz they suck
names(hiv) <- c('vl', 'cd4', 'relcd4', 'gender', 'ethnic', 'base_drug_comb', 'comp_ini', 'comp_nnrti', 'extra_pi', 'extra_pl', 'vl_m', 'cd4_m', 'drug_m', 'id', 'time')

# Factorise certain columns
factor_cols <- hiv %>% select(gender:drug_m) %>% names()
hiv[, factor_cols] <-  lapply(hiv[, factor_cols], factor)
assd
str(hiv)
summary(hiv)

# Now, give levels sensible names
levels(hiv$gender) <- c('male', 'female')
levels(hiv$ethnic) <- c('asian', 'afro', 'caucasian', 'other')
levels(hiv$base_drug_comb) <- c(1, 2, 3, 4, 5, 6)
levels(hiv$comp_ini) <- c('dtg', 'ral', 'evg', 'not_applied')
levels(hiv$comp_nnrti) <- c('nvp', 'efv', 'rov', 'not_applied')
levels(hiv$extra_pi) <- c(1, 2, 3, 4, 5, 6)
hiv$id <- hiv$id + 1

str(hiv)

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

# Pairwise EDA 

# Do some plotting stratified by patients
top10id <- hiv %>% filter(id <= 10)

top10id %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = log(vl), color = factor(id)))
  



log(hiv$vl)


















