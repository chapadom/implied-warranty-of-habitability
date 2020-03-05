# Header ------------------------------------------------------------------
# 
# PROJECT: Implied Warranty of Habitability 
# AUTHOR: Mili Chapado
# DATE: 2018-07-06

# PURPOSE: Perform matching of all cases to HPD complaints data.

# DETAILS: 
# 1) Match HPD complaints during 2016 to housing court cases (bbl and unit).
# 2) Identify only complaints made within one year of case filing.
# 3) Join collapsed results to final cases spreadsheet.


# Setup -------------------------------------------------------------------


library(tidyverse) # dplyr, readr, stringr, etc.
library(lubridate) # working with dates (eg. as_date)

iwh_dir <- ""


# Import Sample Cases and Complaints ---------------------------------------------

# HPD complaints (extracted from DB in 00_db-extracts-comp-viol.R)
comp <- str_glue("{iwh_dir}/Data/Clean/all_complaints.csv") %>% 
  read_csv(col_types = cols(bbl = "c"))

# Random sample of 745 Housing Court Cases (geocoded with BBL and address
# (including unit)) from "02_case-random-samples.R". (these already have the 5
# ungeocoded cases fixed)
cases_geo <- str_glue("{iwh_dir}/Data/Raw/Cases/new_sample_all_cases_745.csv") %>% 
  read_csv(col_types = cols(bbl = "c")) %>% 
  mutate(file_date = ymd(file_date)) %>%
  # separate addresses from apartments into two columns
  extract("std_addr", c("std_addr", "std_apt"), "(.*)(APT.+)") %>% 
  # remove "APT", keep only unit number
  mutate(std_apt = str_replace(std_apt, "APT ", ""))


# to join back into matched cases*comp
case_ids <- cases_geo %>% select(formattedindexnumber)


# Sample Cases and All Complaints --------------------------------------------

cases_comp <- cases_geo %>% 
  # match complaints to cases by bbl, keeping only cases with complaints
  inner_join(comp, by = "bbl") %>% 
  # match complaints on bbls to specific units("cases")
  mutate(unitmatch = if_else(std_apt == apartment, 1L, 0L)) %>% 
  filter(unitmatch == 1L) %>% 
  select(-unitmatch, -apartment) %>% 
  # categorize complaints by case filed date (see below)
  mutate(
    # Date Received (rec_dt) (date that complaint was made)

    # Category 1 (complaints made up to one year before case filing):
    # rec_dt (up to 1 yr) before filing date 
    cat1 = ((rec_dt) <= (file_date)) & ((rec_dt) >= (file_date - 365)),

    # Category 2 (complaints made up to one year after case filing):
    # rec_dt (up to 1 yr) after filing date 
    cat2 = ((rec_dt) >= (file_date)) & ((rec_dt) <= (file_date + 365))
    ) %>% 
  # remove complaints made too long before or too long after file date
  filter(!(cat1 == FALSE & cat2 == FALSE)) %>%
  # join back with all cases to we have cases that don't have complaints too
  right_join(case_ids, by = "formattedindexnumber")


# Full dataset of all cases and each complaint matched (unique by case and complaint ID)
write_csv(cases_comp, str_glue("{iwh_dir}/Data/Clean/Sample Cases/sample_cases_complaints_detailed.csv"), na = "")



# Complaint info ----------------------------------------------------------

cases_comp %>% 
  group_by(formattedindexnumber) %>% 
  summarise(any_comp = sum(!is.na(complaint_id)) > 0) %>% 
  count(any_comp)

# 745 total cases, 149 with complaints, 596 without


# Case-level complaint Info -----------------------------------------------

cases_comp_info <- cases_comp %>%
  # collapse by summarizing, stats into one row per case ID
  group_by(formattedindexnumber) %>% 
  summarise(
    n_comp_before_fd = sum(cat1),
    n_comp_after_fd = sum(cat2),
    n_total_comp = sum(cat1, cat2)
    )
  
# save case-level data to later join with Nicole's spreadsheet
write_csv(cases_comp_info, str_glue("{iwh_dir}/Data/Clean/Sample Cases/sample_cases_comp_info.csv"), na = "")


# Sample-wide stats -------------------------------------------------------
 
# start with the case-level data above (cases_comp_info) and aggregate further
# to get stats about the sample of cases

sample_cases_comp_stats <- cases_comp %>%
  group_by(formattedindexnumber) %>% 
  summarise(
    all_comp = sum(cat1, cat2, na.rm = TRUE),
    comp_before_fd = sum(cat1, na.rm = TRUE),
    comp_after_fd = sum(cat2, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  summarise(
    n_cases = n(),
    n_with_comp = sum(comp_before_fd > 0 | comp_after_fd > 0),
    n_comp_before_fd = sum(comp_before_fd > 0),
    pct_comp_before_fd = mean(comp_before_fd > 0),
    n_comp_after_fd = sum(comp_after_fd > 0),
    pct_comp_after_fd = mean(comp_after_fd > 0)
  )

write_csv(sample_cases_comp_stats, str_glue("{iwh_dir}/Data/Clean/Sample Cases/sample_cases_comp_stats.csv"), na = "")



