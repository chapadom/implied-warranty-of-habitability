# Header ------------------------------------------------------------------
# 
# PROJECT: Implied Warranty of Habitability
# AUTHOR: Mili Chapado
# DATE: 2018-06-25

# PURPOSE: Perform matching of all cases to violations data.

# DETAILS: Start with raw data on all (geocoded) cases and match all cases to
# violations and export this dataset for refernce. Then identify the set of
# cases with at least 1 open B or C violation open at filing. In
# "02_case-random-samles.R" we will draw a sample of 507 cases from this set to
# pull casse files and do analysis. We also will take the full set of cases and
# aggregate first by case then overall to get stats about presence of violations
# in the full universe of cases.

# Setup -------------------------------------------------------------------


library(tidyverse) # dplyr, readr, stringr, etc.
library(lubridate) # working with dates (eg. as_date)

iwh_dir <- ""


# Load Cases and Violations -----------------------------------------------

allcases <- str_glue("{iwh_dir}/Data/Raw/Cases/all_2016_cases.csv") %>%
  read_csv(col_types = cols(bbl = "c"))

viol <- str_glue("{iwh_dir}/Data/Clean/all_violations.csv") %>%
  read_csv(col_types = cols(bbl = "c"))


# Matching all cases to all 2016 HPD violations --------------------------

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

geocoded_case_info <- geocoded_cases %>% select(one_of(case_cols))

# There are 84,600 cases with full address, unit, BBL info

cases_viols <- geocoded_cases %>%  
  # match 2016 violations to all cases by bbl [1000 cases did not have bbls]
  inner_join(viol, by = "bbl") %>% 
  # match open violations on bbls to specific units ("cases")
  mutate(unitmatch = if_else(std_apt == apartment, 1L, 0L)) %>% 
  filter(unitmatch == 1L) %>% 
  # drop extra columns
  select(-unitmatch, -propertyaddress, -apartment) %>% 
  # fix date formatting
  mutate(
    ans_date = ymd(ans_date),
    file_date = ymd(file_date)
  ) %>% 
  # categorize violations by case filed date and violation open window
  mutate(
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
  right_join(geocoded_case_info, by = case_cols)

  
# data set of matched cases to violations
write_csv(cases_viols, str_glue("{iwh_dir}/Data/Clean/All Cases/all_cases_violations_detailed.csv"), na = "")


# Violation info ----------------------------------------------------------

cases_viols %>% 
  group_by(formattedindexnumber) %>% 
  summarise(any_viols = sum(!is.na(violation_id)) > 0) %>% 
  count(any_viols)

# 84,600 total cases, 10,077 with violations, 74,471 without


# Cases with Violations open at Filing ------------------------------------

cases_bc_viol_open_at_filing <- cases_viols %>% 
  # identify violation types
  mutate(
    is_a_viol = violation_class %in% c("A"),
    is_b_viol = violation_class %in% c("B"),
    is_c_viol = violation_class %in% c("C")
  ) %>%
  # only cases/rows that had an open violation at time of file date (cat1=TRUE)
  filter(cat1 == "TRUE") %>% 
  # remove other time category columns and rename cat1
  select(-cat2, -cat3) %>% 
  rename(open_on_fd = cat1) %>% 
  # add column, n of days open from NOV to FD
  mutate(days_open_upto_fd = difftime(file_date, nov_issued_dt, units = "days")) %>% 
  # collapse by summarizing, stats into one row per case ID this data set
  # contains just case ID and violation type info
  group_by(formattedindexnumber) %>% 
  summarise(
    n_a_viols = sum(is_a_viol, na.rm = TRUE),
    n_c_viols = sum(is_b_viol, na.rm = TRUE),
    n_b_viols = sum(is_c_viol, na.rm = TRUE),
    avg_days_open = mean(days_open, na.rm = TRUE),
    avg_days_open_upto_fd = mean(days_open_upto_fd, na.rm = TRUE)
  ) %>% 
  # only keep cases with at least 1 B or C violation open at filing
  filter(n_b_viols > 0 | n_c_viols > 0)


# Data set of unique cases (dropped bbls and non-std addresses) matched to open
# B or C violations at time of filing date. These will be using in
# 02_case-rando-samples.R to draw sample of cases with open violations.

write_csv(cases_bc_viol_open_at_filing, str_glue("{iwh_dir}/Data/Clean/All Cases/all_cases_bc_viol_open_at_filing.csv"), na = "")


# Violations Descriptives ------------------------------------------------

# aggregate, total num of cases, total num of violations at fd, share of violations

# filter cases_viols, only cases with violations for analysis below
# run this before viol_stats to get only_viols info that will be used next
only_viols <- cases_viols %>%
  drop_na(violation_class) %>%
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
    total_viols = sum(is_a_viol, is_b_viol, is_c_viol, na.rm = TRUE),
    avg_days_open = mean(days_open, na.rm = TRUE),
    avg_days_open_upto_fd = mean(days_open_upto_fd, na.rm = TRUE)
  ) 

# distribution of violations among cases with at least 1 a/b/c violation
ggplot(only_viols, aes(x = total_viols)) + 
  geom_histogram(binwidth = 5) + 
  theme_classic() +
  labs(
    title = "Distribution of Violations",
    x = "violations", 
    y = "cases"
  )

ggsave("all_cases_viol_distribution.png", width = 2 , height = 2, units = "in")

# Start with all cases (with/without viols), aggregate by case, then futher
# aggregate over all cases.
all_cases_viol_stats <- cases_viols %>%
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
    n_cases_with_viol = sum(n_a_viols | n_b_viols | n_c_viols),
    n_with_a = sum(n_a_viols > 0),
    pct_with_a = mean(n_a_viols > 0),
    n_with_b = sum(n_b_viols > 0),
    pct_with_b = mean(n_b_viols > 0),
    n_with_c = sum(n_c_viols > 0),
    pct_with_c = mean(n_c_viols > 0),
    avg_days_open = mean(avg_days_open, na.rm = TRUE),
    avg_days_open_upto_fd = mean(avg_days_open_upto_fd, na.rm = TRUE),
# among cases with at least 1 violation what's the average number of violations
# use only_viols from above
    avg_viol_per_caseviol = mean(only_viols$total_viols),
    avg_a_viol = mean(only_viols$n_a_viols),
    avg_b_viol = mean(only_viols$n_b_viols),
    avg_c_viol = mean(only_viols$n_c_viols)
    )

write_csv(all_cases_viol_stats, str_glue("{iwh_dir}/Data/Clean/All Cases/all_cases_viol_stats.csv"), na = "")

