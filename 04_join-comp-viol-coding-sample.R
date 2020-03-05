# Header ------------------------------------------------------------------
# 
# PROJECT: Implied Warranty of Habitability 
# AUTHOR: Mili Chapado
# DATE: 2018-07-06

# PURPOSE: Join violations data and complaints data to sample of 745 housing court 
# cases / Nicole's spreadsheet.

# DETAILS: 

# Setup -------------------------------------------------------------------

library(tidyverse) # dplyr, readr, stringr, etc.

iwh_dir <- ""


# Load Cases, Violations, and Complaints-----------------------------------------------

cases <- str_glue("{iwh_dir}/Coding/Housing Court random sample FINAL 7.31.18.csv") %>% 
  read_csv(col_types = cols(.default = "c")) %>%
  # drop columns without data
  select(-matches("X\\d+")) %>%
  # rename first column  for caseID
  rename(formattedindexnumber = 1) %>%
  drop_na(formattedindexnumber)

viols <- str_glue("{iwh_dir}/Data/Clean/Sample Cases/sample_cases_viol_info.csv") %>%
  read_csv(col_types = cols(.default = "c")) 
  
comp <- str_glue("{iwh_dir}/Data/Clean/Sample Cases/sample_cases_comp_info.csv") %>%
  read_csv(col_types = cols(.default = "c")) %>%
  drop_na()

# join violations and complaints data to Nicole's spreadsheet of cases by caseID

cases_all_info <- cases %>% 
  left_join(viols, by = "formattedindexnumber") %>%
  left_join(comp, by = "formattedindexnumber")

write_csv(cases_all_info, str_glue("{iwh_dir}/Data/Clean/Sample Cases/cases_info_violations_complaints_summary.csv"), na = "")

