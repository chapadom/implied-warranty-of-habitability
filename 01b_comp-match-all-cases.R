
# Header ------------------------------------------------------------------
# 
# PROJECT: Implied Warranty of Habitability 
# AUTHOR: Mili Chapado
# DATE: 2018-07-06

# PURPOSE: Perform matching of all cases to HPD complaints data.

# DETAILS: 

# Setup -------------------------------------------------------------------


library(tidyverse) # dplyr, readr, stringr, etc.
library(lubridate) # working with dates (eg. as_date)

iwh_dir <- ""


# Load Cases and Violations -----------------------------------------------

allcases <- str_glue("{iwh_dir}/Data/Raw/Cases/all_2016_cases.csv") %>%
  read_csv(col_types = cols(bbl = "c"))

# HPD complaints
comp <- str_glue("{iwh_dir}/Data/Clean/all_complaints.csv") %>% 
  read_csv(col_types = cols(bbl = "c"))



# Matching all cases to all 2016 HPD complaints  --------------------------

# There are a few cases that didn't geocode properly but were included in an
# initial sample, so for consistency these are added new via manual look ups and
# hardcoding.
fixed_cases <- tribble(
  ~formattedindexnumber, ~fixed_bbl, ~fixed_std_addr,
  "LT-051279-16/QU", "4008930030", "2017 19TH ST APT 3A",
  "LT-051913-16/KI", "3017107501", "1825 ATLANTIC AVE APT 3T",
  "LT-052542-16/RI", "5023700234", "7 BOGOTA ST APT BSMT",
  "LT-064210-16/BX", "2026910122", "1022 REV J POLITE AVE APT 5P",
  "LT-801768-16/BX", "2049050001", "1189 229TH DR NORTH APT 2C"
)

geocoded_cases <- allcases %>%
  # hardcode geocoding fixes for 5 cases
  left_join(fixed_cases, by = "formattedindexnumber") %>% 
  mutate(
    bbl = coalesce(fixed_bbl, bbl), 
    std_addr = coalesce(fixed_std_addr, std_addr) 
  ) %>% 
  select(-starts_with("fixed_")) %>% 
  # separate addresses from apartments into two columns
  extract("std_addr", c("std_addr", "std_apt"), "(.*)(APT.+)") %>% 
  # remove "APT", keep only unit number
  mutate(std_apt = str_replace(std_apt, "APT ", "")) %>% 
  # drop cases without standard address or bbl - 84k cases remain
  drop_na(std_addr, bbl)

# case info columns
case_cols <- c("formattedindexnumber", "court", "propertytype", "classification", "causeofaction")

geocoded_case_info <- geocoded_cases %>% 
  select(one_of(case_cols))

# There are 84,600 cases with full address, unit, BBL info

cases_comp <- geocoded_cases %>%  
  # match 2016 complaints to all cases by bbl [1000 cases did not have bbls]
  inner_join(comp, by = "bbl") %>% 
  # match open complaints on bbls to specific units ("cases")
  mutate(unitmatch = if_else(std_apt == apartment, 1L, 0L)) %>% 
  filter(unitmatch == 1L) %>% 
  # drop extra columns
  select(-unitmatch, -propertyaddress, -apartment) %>% 
  # fix date formatting
  mutate(
    ans_date = ymd(ans_date),
    file_date = ymd(file_date) ) %>% 
  # categorize complaints by case filed date and complaint received date (see below)
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
  # join back with all cases to we have cases that don't have open complaints too
  right_join(geocoded_case_info, by = case_cols)


# Full dataset of all cases and each complaint matched (unique by case and complaint ID)
write_csv(cases_comp, str_glue("{iwh_dir}/Data/Clean/All Cases/all_cases_complaints_detailed.csv"), na = "")


# Complaint info ----------------------------------------------------------

cases_comp %>% 
  group_by(formattedindexnumber) %>% 
  summarise(any_comp = sum(!is.na(complaint_id)) > 0) %>% 
  count(any_comp)

# 84,600 total cases, 20,427 with complaints, 64,121 without


# Cases with Complaints Before and After Filing ------------------------------------

cases_comp_info <- cases_comp %>%
  # collapse by summarizing, stats into one row per case ID
  group_by(formattedindexnumber) %>% 
  summarise(
    n_comp_before_fd = sum(cat1),
    n_comp_after_fd = sum(cat2)
  )
# data set of unique cases (dropped bbls and non-std addresses) matched to complaints before/after fd
write_csv(cases_comp_info, str_glue("{iwh_dir}/Data/Clean/All Cases/all_cases_comp_info.csv"), na = "")


# All cases -wide stats -------------------------------------------------------

all_cases_comp_stats <- cases_comp %>%
  group_by(formattedindexnumber) %>% 
  summarise(
    all_comp = sum(cat1 | cat2, na.rm = TRUE),
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

write_csv(all_cases_comp_stats, str_glue("{iwh_dir}/Data/Clean/All Cases/all_cases_complaints_stats.csv"), na = "")


