# Header ------------------------------------------------------------------

# GitHub/iwh/00_db-extracts-viol-comp.R

# PROJECT: Implied Warranty of Habitability
# AUTHOR: Mili Chapado
# DATE: 2018-06-25

# PURPOSE: Extract HPD violations and complaints from database

# DETAILS: Extract viols where: cert by date is between Jan 1 2016 and Dec 31 2017, 
# notice of viol date is between Jan 1 2015 and Dec 31 2016, and complaints where:
# rec dt is between Jan 1 2015 and Dec 31 2017.

# Setup -------------------------------------------------------------------


library(tidyverse) # dplyr, readr, stringr, etc.
library(lubridate) # working with dates (eg. as_date)
library(DBI) # database connection

iwh_dir <- ""

con <- dbConnect(
  RPostgres::Postgres(),
  host = "",
  dbname = "",
  user = "",
  port = ,
  password = getPass::getPass("Database password")
)


# Extract Violations ---------------------------------------------

# Extract of HPD violations from database (table="hpdviol")

dbListTables(con)

dbListFields(con, "hpdviol")

viol <- tbl(con, "hpdviol")

# viol_lookup <- tbl(con, "hpdviol_lookup") %>% collect()

viol %>%
  filter(
    (orig_cert_by_dt >= ("2016-01-01") & orig_cert_by_dt <= ("2017-12-31")), 
    (nov_issued_dt >= ("2015-01-01") & nov_issued_dt <= ("2016-12-31"))
  ) %>%
  collect() %>% 
  write_csv(str_glue("{iwh_dir}/Data/Clean/all_violations.csv"), na = "")


# Extract Complaints ---------------------------------------------

# Extract of HPD complaints from FC's database (table="hpdcomplaints")

dbListTables(con)

dbListFields(con, "hpdcomplaints")

comp <- tbl(con, "hpdcomplaints") %>% 
  collect()

comp %>%
  mutate(rec_dt = mdy(rec_dt)) %>%
  filter(rec_dt >= ("2015-01-01") & rec_dt <= ("2017-12-31")) %>%
  write_csv(str_glue("{iwh_dir}/Data/Clean/all_complaints.csv"), na = "")
