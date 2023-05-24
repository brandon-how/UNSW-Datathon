## Title: UNSW Datathon
## Author: Brandon Hao
## Date: 18 May 2023

## 0. Initialisation ------------------------------------------------------------------------------------------------

# Set working directory
setwd("C:/Users/brand/OneDrive/Desktop/Health Data Science/Datathon")

# Install and load libraries 
packages <- c('tidyverse', 'forecast', 'DataExplorer', 'data.table')
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
hiv <- fread(file.path('data', 'HealthGymV2_CbdrhDatathon_ART4HIV.csv'))

## 1. Inspect and transform data -------------------------------------------------------------------------------------

str(hiv)

# Change some names coz they suck
names(hiv) <- c('vl', 'cd4', 'relcd4', 'gender', 'ethnic', 'base_drug_comb', 'comp_ini', 'comp_nnrti', 'extra_pi', 'extra_pk', 'vl_m', 'cd4_m', 'drug_m', 'id', 'time')

# Change patient ID to start with 1
hiv$id <- hiv$id + 1

# Transform columns into Boolean
boolean_cols <- hiv %>% select(extra_pk:drug_m) %>% names()
hiv[, (boolean_cols) := lapply(.SD, as.logical), .SDcols = boolean_cols]

# Factorise certain columns and give sensible levels
factor_cols <- hiv %>% select(gender:extra_pi) %>% names()
hiv[, (factor_cols) := lapply(.SD, as.factor), .SDcols = factor_cols]

levels(hiv$gender) <- c('male', 'female')
levels(hiv$ethnic) <- c('asian', 'afro', 'caucasian', 'other')
levels(hiv$base_drug_comb) <- c('ftc_tdf','3tc_abc', 'ftc_taf', 'drv_ftc_tdf', 'ftc_rtvb_tdf', 'other') 
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
         immunosupp = ifelse(cd4 < 50, TRUE, FALSE),
         immun_recover = ifelse(cd4 > 500, TRUE, FALSE))

# 3. Give backbone info
hiv_eng[, ':=' (backbone = fifelse(base_drug_comb %like% 'ftc_(.*?)tdf', 'ftc_tdf', fifelse(base_drug_comb %like% '3tc_(.*?)abc', '3tc_abc',  fifelse(base_drug_comb %like% 'ftc(.*?)taf', 'ftc_taf', 'other'))))]
hiv_eng

# Select first and last observation for each patient ID
hiv[, .SD[c(1, .N)], by = id] %>% head(6)

hiv[, .SD[c(1, .N)], by = id]  %>% pivot_wider(names_from = cd4, values_from = vl)

hiv[, mean(vl_m), by = id]
hiv[, mean(cd4_m), by = id]
hiv[, mean(drug_m), by = id]

hiv[, unique(base_drug_comb), by = id]


hiv[id <= 5] %>% mutate(new_vl = log10(vl)) %>% 
  ggplot(aes(x = time, y = new_vl, colour = factor(id))) + 
  geom_line()



hiv[, .SD[c(1, .N)], by = id]