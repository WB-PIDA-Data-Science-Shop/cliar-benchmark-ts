## code to prepare 'ims_adoption' dataset for benchmarking analysis goes here
# GovTech Dataset downloaded direcltly from the WB Development Data Hub
# available at: https://datacatalog.worldbank.org/int/search/dataset/0037889/govtech-dataset
# Last access: 10/09/26


# set-up -----------------------------------------------------------------

library(readxl)
library(dplyr)
library(janitor)
library(here)
library(httr)
library(tidyr)

# Install cliaretl package from GitHub if not already installed

# Step 1. Install the packages 'pak' or 'remotes' if you don't have them:

# install.packages("pak")
# install.packages("remotes")


# Step 2. Install cliaretl from GitHub:
# remotes::install_github("WB-PIDA-Data-Science-Shop/cliaretl")

library(cliaretl)


# load data --------------------------------------------------------------


url_2022 <- "https://datacatalogfiles.worldbank.org/ddh-published/0037889/DR0089805/WBG_GovTech%20Dataset_Oct2022.xlsx"


temp_file22 <- tempfile(fileext = ".xlsx")
download.file(url_2022, destfile = temp_file22, mode = "wb")


govtech_raw_22 <- read_excel(temp_file22, sheet = "CG_GTMI_Data") |>
  # Drop fully empty columns first so clean_names() doesn't generate
  remove_empty("cols") |>
  clean_names() |> 
  # Filter only year 2025 rows
  filter(year == 2022) |>
  # Remove any rows that are completely NA (e.g., empty rows in the sheet)
  filter(if_any(everything(), ~ !is.na(.)))


# Keep only country identifiers and the 9 MIS indicators from the GTMI 2025 data.
# Each MIS has two columns:
govtech_subset_22 <- govtech_raw_22 |>
  select(
    # Country identifiers
    country_code = code,
    country_name = economy,

    # Financial Management Information System (FMIS)
    i_5,     # Operational FMIS in place to support core PFM functions
    i_5_5,   # Year FMIS was established

    # Tax Management Information System (TMIS)
    i_7,     # Tax Management Information System in place
    i_7_5,   # Year TMIS was established

    # Customs Management Information System (CMIS)
    i_8,     # Customs Management Information System in place
    i_8_5,   # Year CMIS was established

    # Human Resource and Payroll System (HRPS)
    i_9,     # Human Resource and Payroll System in place
    i_9_3,   # Year HRPS was established

    # Human Resource Management Information System (PAYROLL)
    i_10,    # Human Resource Management Information System in place
    i_10_3,  # Year PAYROLL was established

    # Social Safety Net Management Information System (SSNMIS)
    i_11,    # Social Safety Net Management Information System in place
    i_11_3,  # Year SSNMIS was established

    # E-Procurement (EGP)
    i_12,    # E-Procurement portal in place
    i_12_3,  # Year e-procurement portal was established

    # Debt Management System (DMS)
    i_13,    # Debt Management System in place
    i_13_3,  # Year DMS was established

    # Public Investment Management System (PIMS)
    i_14,    # Public Investment Management System in place
    i_14_3   # Year PIMS was established
  ) |>
  # Convert all indicator and year columns to numeric
  # (readxl often reads them as text because of blanks or notes in the sheet)
  mutate(
    across(starts_with("i_"), as.double)
  )

url_2025 <- "https://datacatalogfiles.worldbank.org/ddh-published/0037889/DR0095721/WBG_GovTech_Dataset_Dec2025.xlsx"

temp_file25 <- tempfile(fileext = ".xlsx")
download.file(url_2025, destfile = temp_file25, mode = "wb")

govtech_raw <- read_excel(temp_file25, sheet = "GTMI_Data") |>
  # Drop fully empty columns first so clean_names() doesn't generate
  # placeholder names like `...462`, `....463`, etc.
  remove_empty("cols") |>
  clean_names() |> 
  # Filter only year 2025 rows
  filter(year == 2025) |>
  # Remove any rows that are completely NA (e.g., empty rows in the sheet)
  filter(if_any(everything(), ~ !is.na(.)))

# Keep only country identifiers and the 9 MIS indicators from the GTMI 2025 data.
# Each MIS has two columns:
govtech_subset <- govtech_raw |>
  select(
    # Country identifiers
    country_code = code,
    country_name = economy,

    # Financial Management Information System (FMIS)
    i_5,     # Operational FMIS in place to support core PFM functions
    i_5_5,   # Year FMIS was established

    # Tax Management Information System (TMIS)
    i_7,     # Tax Management Information System in place
    i_7_5,   # Year TMIS was established

    # Customs Management Information System (CMIS)
    i_8,     # Customs Management Information System in place
    i_8_5,   # Year CMIS was established

    # Human Resource and Payroll System (HRPS)
    i_9,     # Human Resource and Payroll System in place
    i_9_3,   # Year HRPS was established

    # Human Resource Management Information System (PAYROLL)
    i_10,    # Human Resource Management Information System in place
    i_10_3,  # Year PAYROLL was established

    # Social Safety Net Management Information System (SSNMIS)
    i_11,    # Social Safety Net Management Information System in place
    i_11_3,  # Year SSNMIS was established

    # E-Procurement (EGP)
    i_12,    # E-Procurement portal in place
    i_12_3,  # Year e-procurement portal was established

    # Debt Management System (DMS)
    i_13,    # Debt Management System in place
    i_13_3,  # Year DMS was established

    # Public Investment Management System (PIMS)
    i_14,    # Public Investment Management System in place
    i_14_3   # Year PIMS was established
  ) |>
  # Convert all indicator and year columns to numeric
  # (readxl often reads them as text because of blanks or notes in the sheet)
  mutate(
    across(starts_with("i_"), as.double)
  )


# compare 22 vs 25 methodology -------------------------------------------
# Confirm that the 2022 and 2025 datasets are consistent in terms of column names and types and values

# compare 22 vs 25 methodology -------------------------------------------
# Confirm that the 2022 and 2025 datasets are consistent in terms of
# column names, column types and the coding of values.
# Result: one tidy tibble (one row per check x column) with a logical
# `is_equal` flag, so it can be summarised, counted and filtered.

library(purrr)   # map_chr()
library(stringr) # str_starts()

cols_22     <- names(govtech_subset_22)
cols_25     <- names(govtech_subset)
all_cols    <- union(cols_22, cols_25)
shared_cols <- intersect(cols_22, cols_25)
ind_cols    <- shared_cols[str_starts(shared_cols, "i_")]

# helper: collapse the distinct values of a column into one string
# (na.last = TRUE keeps NA in the comparison; sort() drops it by default)
collapse_values <- \(x) paste(sort(unique(x), na.last = TRUE), collapse = ", ")

# 1. Column names: present in both years and in the same position?
name_check <- tibble(
  check      = "name",
  column     = all_cols,
  value_2022 = as.character(match(all_cols, cols_22)),  # position in 2022
  value_2025 = as.character(match(all_cols, cols_25))   # position in 2025
) |>
  mutate(is_equal = !is.na(value_2022) & !is.na(value_2025) &
           value_2022 == value_2025)

# 2. Column types: same class for each shared column?
type_check <- tibble(
  check      = "type",
  column     = shared_cols,
  value_2022 = map_chr(shared_cols, \(col) class(govtech_subset_22[[col]])[1]),
  value_2025 = map_chr(shared_cols, \(col) class(govtech_subset[[col]])[1])
) |>
  mutate(is_equal = value_2022 == value_2025)

# 3. Values: same set of distinct values (coding scheme) per indicator?
value_check <- tibble(
  check  = "values",
  column = ind_cols
) |>
  mutate(
    value_2022   = map_chr(column, \(col) collapse_values(govtech_subset_22[[col]])),
    value_2025   = map_chr(column, \(col) collapse_values(govtech_subset[[col]])),
    is_equal     = value_2022 == value_2025,
    only_in_2022 = map_chr(column, \(col) paste(setdiff(govtech_subset_22[[col]], govtech_subset[[col]]), collapse = ", ")),
    only_in_2025 = map_chr(column, \(col) paste(setdiff(govtech_subset[[col]], govtech_subset_22[[col]]), collapse = ", "))
  )

# Consolidated tibble of the three checks
consistency_check_gtmi <- bind_rows(name_check, type_check, value_check) |>
  mutate(check = factor(check, levels = c("name", "type", "values")))

# Quick overview: rows per check and TRUE/FALSE counts of is_equal
summary(consistency_check)

# Count and proportion of equal vs different, by check
consistency_summary_gtmi <- consistency_check |>
  count(check, is_equal) |>
  complete(check, is_equal = c(TRUE, FALSE), fill = list(n = 0L)) |>
  mutate(
    status = if_else(is_equal, "equal", "different"),
    prop   = n / sum(n),
    .by    = check
  ) |>
  select(check, status, n, prop) |>
  arrange(check, desc(status))

consistency_summary_gtmi

# Inspect only the columns that differ between 2022 and 2025
consistency_check |>
  filter(!is_equal)


# compare at country_code level -----------------------------------------

# status indicators only (the i_* columns without a year suffix)
status_cols <- c("i_5", "i_7", "i_8", "i_9", "i_10",
                 "i_11", "i_12", "i_13", "i_14")

# long form, one row per country + indicator, for each year
long_22 <- govtech_subset_22 |>
  select(country_code, country_name, all_of(status_cols)) |>
  pivot_longer(all_of(status_cols), names_to = "indicator", values_to = "value_2022")

long_25 <- govtech_subset |>
  select(country_code, all_of(status_cols)) |>
  pivot_longer(all_of(status_cols), names_to = "indicator", values_to = "value_2025")

# join the two years and classify each country-indicator cell
country_compare <- long_22 |>
  full_join(long_25, by = c("country_code", "indicator")) |>
  mutate(
    status = case_when(
      is.na(value_2022) & !is.na(value_2025) ~ "new_in_2025",
      !is.na(value_2022) & is.na(value_2025) ~ "dropped_in_2025",
      value_2022 == value_2025               ~ "unchanged",
      TRUE                                   ~ "changed"
    )
  )

# summary: one row per country with counts of changed / unchanged indicators
country_summary <- country_compare |>
  summarize(
    n_indicators = n(),
    n_changed    = sum(status == "changed"),
    n_unchanged  = sum(status == "unchanged"),
    n_new        = sum(status == "new_in_2025"),
    n_dropped    = sum(status == "dropped_in_2025"),
    .by = c(country_code, country_name)
  ) |>
  arrange(desc(n_changed))

country_summary

country_summary |> 
  tabyl(n_changed)

# data-transformation ----------------------------------------------------

# load country list from cliaretl package
country_list <- cliaretl::wb_country_list |>
  # recode Kosovo country code to match the WB Development Data Hub dataset
  mutate(country_code = ifelse(country_code == "KOS", "XKX", country_code))

# filter raw data to only include country rows based on the country list,
# matching on country_code only (names differ across sources).
# Also drop NIC (Nicaragua), which is not comparable to the legacy counts.
govtech2025_clean <- govtech_subset |>
  filter(country_code != "NIC") |>
  semi_join(country_list, by = "country_code")

# clean data -----------------------------------------------------------

ims_df <- govtech2025_clean |>
  select(
    # Country identifiers
    country_code,
    country_name,
    # Financial Management Information System
    FMIS        = i_5,     # Operational FMIS in place to support core PFM functions
    FMIS_year   = i_5_5,   # Year FMIS was established

    # Tax Management Information System
    TMIS        = i_7,     # Tax Management Information System in place
    TMIS_year   = i_7_5,   # Year TMIS was established

    # Customs Management Information System
    CMIS        = i_8,     # Customs Management Information System in place
    CMIS_year   = i_8_5,   # Year CMIS was established

    # Human Resource and Payroll System
    HRSS        = i_9,     # Human Resource and Payroll System in place
    HRSS_year   = i_9_3,   # Year HRSS was established

    # Human Resource Management Information System
    PAYROLL       = i_10,    # Human Resource Management Information System in place
    PAYROLL_year  = i_10_3,  # Year PAYROLL was established

    # Social Safety Net Management Information System
    SSMIS       = i_11,    # Social Safety Net Management Information System in place
    SSMIS_year  = i_11_3,  # Year SSMIS was established

    # E-Procurement
    EPMIS       = i_12,    # E-Procurement portal in place
    EPMIS_year  = i_12_3,  # Year e-procurement portal was established

    # Debt Management System
    DMIS        = i_13,    # Debt Management System (DMS) in place
    DMIS_year   = i_13_3,  # Year DMS was established

    # Public Investment Management System
    PIMIS       = i_14,    # Public Investment Management System (PIMS) in place
    PIMIS_year  = i_14_3   # Year PIMS was established
  )


# pivot longer and coerce mis values to numeric. Have one row per country and columns that are: mis_type, mis_value and mis_year  
ims_df_long <- ims_df |>
  # Step 1: add "_value" to the score columns so every column follows MIS_part
  #         (FMIS -> FMIS_value, alongside FMIS_year)
  rename_with(
    \(x) paste0(x, "_value"),
    c(FMIS, TMIS, CMIS, HRSS, PAYROLL, SSMIS, EPMIS, DMIS, PIMIS)
  ) |>
  # Step 2: pivot once; text before "_" becomes mis_type,
  #         text after "_" ("value" or "year") becomes its own column
  pivot_longer(
    cols = -c(country_code, country_name),
    names_to = c("mis_type", ".value"),
    names_sep = "_"
  ) |>
  rename(mis_value = value, mis_year = year)

# Objective is to determine the proportion out of 100% (9 MIS) shere existent
# registering of course the year they where implemented if mis_value=2.
# The output should be a dataset with one row per country and  with timeline from 1990 to 2025,
# with columns for country_code, country_name, year and column for the proportion of MIS types each country has in place (0 = none, 1 = all).

ims_time_series <- ims_df_long |>
  # Step 3: expand each country + MIS type across the full 1990-2025 timeline
  cross_join(tibble(year = seq(1990, 2025))) |>
  # Step 4: an MIS counts as "in place" in a given year only if it is
  #         operational (mis_value == 2). Timing rule:
  #           - If we have a plausible establishment year (1960-2025), the system
  #             is in place from that year onward (a pre-1990 year means it is in
  #             place across the whole window).
  #           - If the establishment year is missing or implausible (NA, < 1960,
  #             or > 2025), we still know the system is operational as of 2025 but
  #             cannot date its start, so we treat it as in place across the whole
  #             window rather than silently dropping it to 0.
  mutate(
    mis_known_start = !is.na(mis_year) & between(mis_year, 1960, 2025),
    mis_in_place = if_else(
      !is.na(mis_value) & mis_value == 2  &
        if_else(mis_known_start, year >= mis_year, TRUE),
      2, 0 # 2 = in place, 0 = not in place
    )
  ) |>
  # Step 5: calculate the proportion of MIS types in place for each country and year.
  #         If ANY of the 9 systems has an unknown (NA) status, the overall
  #         proportion is NA rather than silently treating the gap as a 0.
  group_by(country_code, country_name, year) |>
  summarize(
    total_mis = n(),                                 # number of MIS types (9)
    mis_present = sum(mis_in_place == 2),            # Change to: number of MIS in place
    any_unknown = any(is.na(mis_value)),             # any system with no data
    mis_proportion = if_else(
      any_unknown | total_mis == 0,
      NA_real_,
      mis_present / total_mis
    ),
    .groups = "drop"
  ) |>
  select(-any_unknown)
 

# export-data ------------------------------------------------------------

ims_time_series |>
  readr::write_csv(here("raw-data", "ims_time_series_revised.csv"))




