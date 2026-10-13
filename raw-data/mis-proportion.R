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

url_2025 <- "https://datacatalogfiles.worldbank.org/ddh-published/0037889/DR0095721/WBG_GovTech_Dataset_Dec2025.xlsx"

temp_file <- tempfile(fileext = ".xlsx")
download.file(url_2025, destfile = temp_file, mode = "wb")

govtech_raw <- read_excel(temp_file, sheet = "GTMI_Data") |>
  clean_names() |> 
  # Filter only year 2025 rows
  filter(year == 2025) |>
  # Remove any rows that are completely NA (e.g., empty rows in the sheet)
  filter(if_any(everything(), ~ !is.na(.)))

# Keep only country identifiers and the 9 MIS indicators from the GTMI 2025 data.
# Each MIS has two columns:
govtech2025 <- govtech_raw |>
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

    # Human Resource Management Information System (HRMIS)
    i_10,    # Human Resource Management Information System in place
    i_10_3,  # Year HRMIS was established

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

# load country list from cliaretl package
country_list <- cliaretl::wb_country_list |>
  # recode Kosovo country code to match the WB Development Data Hub dataset
  mutate(country_code = ifelse(country_code == "KOS", "XKX", country_code))

# filter raw data to only include country rows based on the country list,
# conditioning on BOTH country_code and country_name
govtech2025_clean <- govtech2025 |>
  semi_join(country_list, by = c("country_code", "country_name"))

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
    HRMIS       = i_10,    # Human Resource Management Information System in place
    HRMIS_year  = i_10_3,  # Year HRMIS was established

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
    c(FMIS, TMIS, CMIS, HRSS, HRMIS, SSMIS, EPMIS, DMIS, PIMIS)
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
  # Step 4: an MIS is "in place" in a given year only if it exists (mis_value > 0)
  #         and the year is at/after its establishment year.
  #         If the establishment year is missing, treat it as present across all years.
  mutate(
    mis_in_place = if_else(
      !is.na(mis_value) & mis_value > 0 &
        (is.na(mis_year) | year >= mis_year),
      1, 0
    )
  ) |>
  # Step 5: calculate the proportion of MIS types in place for each country and year
  group_by(country_code, country_name, year) |>
  summarize(
    total_mis = n(),                                 # number of MIS types (9)
    mis_present = sum(mis_in_place, na.rm = TRUE),   # MIS types in place
    mis_proportion = if_else(total_mis > 0, mis_present / total_mis, NA_real_),
    .groups = "drop"
  )



# export-data ------------------------------------------------------------

ims_time_series |>
  readr::write_csv(here("raw-data", "ims_time_series.csv"))




