# Header ------------------------------------------------------------------
#
# PROJECT: Implied Warranty of Habitability
# AUTHOR: Mili Chapado
# DATE: 2018-06-14

# PURPOSE: Match open HPD violations to sample of 486+259 housing court cases.

# DETAILS:
# 1) Match HPD open violations during 2016 to housing court cases (bbl and unit).
# 2) Determine overlap btwn case filed date and open violation windows.
# 3) Join collapsed results to final cases spreadsheet.

# Setup -------------------------------------------------------------------

library(tidyverse) # dplyr, readr, stringr, etc.
library(lubridate) # working with dates (eg. as_date)

iwh_dir <- "J:/DEPT/REUP/Projects/IWH project"


# Import Cases and Violations ---------------------------------------------

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


# to join back into matched cases*viols
case_ids <- cases_geo %>% select(formattedindexnumber)


# Import violations (extracted from DB in 00_db-extracts-comp-viol.R)
viol <- str_glue("{iwh_dir}/Data/Clean/all_violations.csv") %>% 
  read_csv(col_types = cols(bbl = "c"))


# All Cases and All Violations --------------------------------------------

cases_viols <- cases_geo %>% 
  # match 2016 violations to cases by bbl, keeping only cases with violations
  inner_join(viol, by = "bbl") %>% 
  # match open violations on bbls to specific units("cases")
  mutate(unitmatch = if_else(std_apt == apartment, 1L, 0L)) %>% 
  filter(unitmatch == 1L) %>% 
  select(-unitmatch, -apartment) %>% 
  # categorize violations by case filed date and violation open window (see below)
  mutate(
    # Notice of Violation (NOV) (start of violation's open window)
    # Certification (cert) (end of violation's open window)
    
    # Category 1 (violations open at time of case filing):
    # NOV (anytime) before filing date AND cert (anytime) after filing date
    cat1 = ((nov_issued_dt) < (file_date)) & ((orig_cert_by_dt) > (file_date)),

    # Category 2 (violations open within one year prior to case filing):
    # NOV (anytime) before filing date AND cert (within one year) before filing date
    cat2 = ((nov_issued_dt) < (file_date)) & ((orig_cert_by_dt) < (file_date)) & ((orig_cert_by_dt) > (file_date - 365)),

    # Category 3 (violations open within one year after case filing):
    # NOV (within one year) after filing date AND cert (anytime) after filing date
    cat3 = ((nov_issued_dt) > (file_date)) & ((orig_cert_by_dt) > (file_date)) & ((nov_issued_dt) < (file_date + 365)),

    days_open = difftime(orig_cert_by_dt, nov_issued_dt, units = "days")
  ) %>% 
  # join back with all cases to we have cases that don't have open violations too
  right_join(case_ids, by = "formattedindexnumber")


# Full dataset of all cases and each violation matched (unique by case and violation ID)
write_csv(cases_viols, str_glue("{iwh_dir}/Data/Clean/Sample Cases/sample_cases_violations_detailed.csv"), na = "")



# Violation info ----------------------------------------------------------

cases_viols %>% 
  group_by(formattedindexnumber) %>% 
  summarise(any_viols = sum(!is.na(violation_id)) > 0) %>% 
  count(any_viols)

# 486 total cases, 52 with violations, 434 without
# 745 total cases, 83 with violations, 662 without


# Case-level Violation Info -----------------------------------------------

cases_viol_info <- cases_viols %>%
  mutate(
    is_a_viol = (violation_class == "A"),
    is_b_viol = (violation_class == "B"),
    is_c_viol = (violation_class == "C")
  ) %>% 
  # Keep only only b and c violations
  filter(violation_class %in% c("B", "C")) %>% 
  # collapse by summarizing, stats into one row per case ID
  group_by(formattedindexnumber) %>% 
  summarise(
    n_c_viols = sum(is_b_viol),
    n_b_viols = sum(is_c_viol),
    cat1 = sum(cat1),
    cat2 = sum(cat2),
    cat3 = sum(cat3),
    avg_days_open = mean(days_open)
  )

# save case-level data to later join with Nicole's spreadsheet
write_csv(cases_viol_info, str_glue("{iwh_dir}/Data/Clean/Sample Cases/sample_cases_viol_info.csv"), na = "")


# Sample-wide stats -------------------------------------------------------

# start with the case-level data above (cases_viol_info) and aggregate further
# to get stats about the sample fo cases

sample_cases_viol_stats <- cases_viols %>%
  # identify violation types
  mutate(
    is_a_viol = violation_class %in% c("A"),
    is_b_viol = violation_class %in% c("B"),
    is_c_viol = violation_class %in% c("C"),
    days_open_upto_fd = difftime(file_date, nov_issued_dt, units = "days")
  ) %>%
  group_by(formattedindexnumber) %>% 
  summarise(
    n_a_viols = sum(is_a_viol, na.rm = TRUE),
    n_c_viols = sum(is_b_viol, na.rm = TRUE),
    n_b_viols = sum(is_c_viol, na.rm = TRUE),
    avg_days_open = mean(days_open, na.rm = TRUE),
    avg_days_open_upto_fd = mean(days_open_upto_fd, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  summarise(
    n_cases = n(),
    n_with_viol = sum(n_a_viols | n_b_viols | n_c_viols),
    n_with_a = sum(n_a_viols > 0),
    pct_with_a = mean(n_a_viols > 0),
    n_with_b = sum(n_b_viols > 0),
    pct_with_b = mean(n_b_viols > 0),
    n_with_c = sum(n_c_viols > 0),
    pct_with_c = mean(n_c_viols > 0),
    avg_days_open = mean(avg_days_open, na.rm = TRUE),
    avg_days_open_upto_fd = mean(avg_days_open_upto_fd, na.rm = TRUE),
    # among cases with at least 1 violation what's the average number of violations
    avg_n_b_viol = mean(cases_viol_info$n_b_viols),
    avg_n_c_viol = mean(cases_viol_info$n_c_viols)
  )

write_csv(sample_cases_viol_stats, str_glue("{iwh_dir}/Data/Clean/Sample Cases/sample_cases_violation_stats.csv"), na = "")


#distribution of cases with at least 1 b or c violation
distr <- cases_viol_info %>%
  mutate(viols = n_c_viols + n_b_viols)

ggplot(distr, aes(distr$viols)) + geom_histogram()


