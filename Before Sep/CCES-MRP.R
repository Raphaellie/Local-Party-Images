# load packages
library(ccesMRPprep)
library(tidyverse)
library(haven)
library(dataverse)

# Update 'cumulative' filenames to '2006-2021'
ids <- cces_dv_ids
ids$filename[ids$cces_name == 'cumulative'] <- 'cumulative_2006-2021.dta'

# Get data from dataverse
ccc <- get_cces_dataverse('2012')

# Cleaning CCES data
# ccc_std <- ccc_std_demographics(ccc)
# count(ccc_std, age)
# rm(ccc_std)

# Set brm model
fm_brm <- response ~ age + gender + educ + pct_trump + (1|cd)

# Generate Post-stratification info
acs_tab <- get_acs_cces(
  varlist = acscodes_age_sex_educ,
  varlab_df = acscodes_df,
  year = 2018)

poststrat <-  get_poststrat(
  cleaned_acs = acs_tab, 
  dist_data = cd_info_2018, 
  formula = fm_brm)



