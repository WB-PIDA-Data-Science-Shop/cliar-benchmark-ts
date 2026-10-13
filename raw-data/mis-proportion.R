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
      !is.na(mis_value) & mis_value == 2 &
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
 

# quality-checks ---------------------------------------------------------

# QC1. Operational (==2) systems with a missing or out-of-window establishment
#      year. These are counted as in place across the whole 1990-2025 window
#      (we know they exist but cannot date the start), so review them to confirm
#      that full-window attribution is reasonable.
qc_op_bad_year <- ims_df_long |>
  filter(mis_value == 2) |>
  filter(is.na(mis_year) | mis_year > 2025 | mis_year < 1960) |>
  arrange(country_code, mis_type)

# QC2. Confirm "planned" (==1) systems never land inside the 1990-2025 window,
#      i.e. their establishment years are all in the future (or missing).
qc_planned_in_window <- ims_df_long |>
  filter(mis_value == 1, !is.na(mis_year), between(mis_year, 1990, 2025)) |>
  arrange(country_code, mis_type)

# QC3. Spot-check that a pre-1990 operational system is in place from 1990 on.
#      Picks one country/system with mis_year < 1990 and shows early years.
qc_pre1990_example <- ims_df_long |>
  filter(mis_value == 2, !is.na(mis_year), mis_year < 1990) |>
  slice(1) |>
  select(country_code, mis_type, mis_year) |>
  left_join(
    ims_time_series |> filter(year %in% c(1990, 1991)),
    by = "country_code"
  )

# QC4. Value domain: mis_value should only ever be NA, 0, 1, or 2.
qc_value_domain <- ims_df_long |>
  filter(!is.na(mis_value), !mis_value %in% c(0, 1, 2)) |>
  distinct(mis_value)

# QC5. Panel completeness: every country should have exactly 36 years (1990-2025)
#      and mis_proportion must stay within [0, 1] (or be NA).
qc_panel_shape <- ims_time_series |>
  group_by(country_code) |>
  summarise(
    n_years        = n(),
    min_year       = min(year),
    max_year       = max(year),
    prop_out_range = any(!is.na(mis_proportion) &
                           (mis_proportion < 0 | mis_proportion > 1)),
    .groups = "drop"
  ) |>
  filter(n_years != 36 | min_year != 1990 | max_year != 2025 | prop_out_range)

# QC6. Monotonicity: with ==2 and a fixed start year, a country's count should
#      never decrease over time. Flag any country where it drops.
qc_non_monotonic <- ims_time_series |>
  arrange(country_code, year) |>
  group_by(country_code) |>
  filter(any(diff(mis_present) < 0, na.rm = TRUE)) |>
  ungroup() |>
  distinct(country_code)

# Hard stops for the issues that should truly never happen
stopifnot(
  "mis_value has values outside {NA,0,1,2}" = nrow(qc_value_domain) == 0,
  "planned systems fall inside 1990-2025"   = nrow(qc_planned_in_window) == 0,
  "panel shape or proportion range invalid" = nrow(qc_panel_shape) == 0,
  "mis_present decreases over time"         = nrow(qc_non_monotonic) == 0
)


# export-data ------------------------------------------------------------

ims_time_series |>
  readr::write_csv(here("raw-data", "ims_time_series_revised.csv"))




