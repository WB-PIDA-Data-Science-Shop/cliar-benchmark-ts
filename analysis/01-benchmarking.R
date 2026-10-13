# set-up ------------------------------------------------------------------
#Load necesary libraries

library(haven)
library(dplyr)
library(here)
library(dplyr)
library(janitor)
library(stringr)
library(readr)
library(purrr)
library(tidyr)



# Install cliaretl package from GitHub if not already installed

# Step 1. Install the packages 'pak' or 'remotes' if you don't have them:

# install.packages("pak")
# install.packages("remotes")


# Step 2. Install cliaretl from GitHub:
# remotes::install_github("WB-PIDA-Data-Science-Shop/cliaretl")

library(cliaretl)



# data-load ---------------------------------------------------------------

# Metadata load
dictionary <- cliaretl::db_variables

# Load compiled indicators
cliar_indicators <- fs::path_package(
    "extdata", "compiled_indicators.rds", package = "cliaretl"
  ) |> 
  readr::read_rds() 

# Load country list
country_list <- cliaretl::wb_country_list

# Load the Digital data
mis_data <- readr::read_csv(
  here("raw-data", "ims_time_series_revised.csv")
) |> 
  select(country_code, year, mis_proportion)


# left join variable mis_proportion indicator to compiled indicators
cliar_indicators_full <- cliar_indicators |> 
  left_join(
    mis_data |> select(country_code, year, mis_proportion),
    by = c("country_code", "year")
  ) 

# data preparation -------------------------------------------------------------


# STEP 1. DEFINE VARIABLE SUBSETS (legacy)
#    Keys use the var_lists family names, so time_series_vars_by_family$vars_pol
#    lines up with var_lists$vars_pol. The Stata index name is kept as an
#    attribute so the output columns still match the old .dta.
#    Two Stata abbreviations are written out in full:
#      ctf_vdem_core_v2x_cspar -> vdem_core_v2x_cspart
#      vdem_core_v2pepwrge     -> vdem_core_v2pepwrgen
# -----------------------------------------------------------------------------
# Identify variables that are benchmarked for CTF calculations
vars_ctf <- db_variables |>
  filter(
    benchmarked_ctf == "Yes"
  ) |>
  pull(variable)


# Legacy names
time_series_vars_by_family <- list(
  vars_pol = c(
    "vdem_core_v2xlg_legcon", "fh_fiw_pr_rating", "vdem_core_v2pepwrses",
    "vdem_core_v2pepwrsoc", "vdem_core_v2pepwrgen", "vdem_core_v2lgqugen",
    "vdem_core_v2x_gender"
  ),
  vars_social = c(
    "fh_fiw_cl_rating", "vdem_core_v2cacamps", "vdem_core_v2x_cspart",
    "vdem_core_v2cseeorgs", "vdem_core_v2csreprss", "vdem_core_v2dlengage",
    "vdem_core_v2clacfree", "vdem_core_v2cldiscm", "vdem_core_v2cldiscw",
    "wb_wbl_social", "rwb_pfi_index"
  ),
  vars_anticorruption = c("vdem_core_v2x_corr", "vdem_core_v2exbribe"),
  vars_transp = c("wjp_rol_3_2", "wjp_rol_3_1", "wjp_rol_3_4"),
  vars_leg = c(
    "vdem_core_v2juaccnt", "vdem_core_v2juhcind", "wjp_rol_2_2",
    "vdem_core_v2juncind", "idea_gsod_v_21_05", "wjp_rol_6_6", "wjp_rol_4_3",
    "wjp_rol_7_7", "wjp_rol_7_1", "wjp_rol_7_6", "wjp_rol_7_5", "wjp_rol_8_2",
    "wjp_rol_8_1", "wjp_rol_8_4", "vdem_core_v2clacjstm", "vdem_core_v2clacjstw"
  ),
  vars_hrm = c(
    "vdem_core_v2peasjpol", "vdem_core_v2stcritrecadm",
    "vdem_core_v2peasjsoecon", "vdem_core_v2clrspct"
  ),
  vars_pfm     = c("ibp_obs_obi"),     # NB: var_lists files this under vars_transp
  vars_digital = c("mis_proportion"),  # added
  vars_climate = c(
    paste0("rise_ee_", 1:9),
    paste0("rise_re_", c(1, 2, 3, 4, 7))  # rise_ee_4_3 deliberately left out
  )
)


# STEP 2. PREPARE VARIABLES
# To compute the distance to frontier change, the use cliarertl::rescale_indicator()
# and cliaretl::compute_ctf() functions require the original raw indicator values
#  (before rescaling) to ensure consistency in the calculation.

# data preparation and rescaling
vars_ctf <- db_variables |>
  filter(
    benchmarked_ctf == "Yes"
  ) |>
  pull(variable) 

var_lists <- get_variable_lists(db_variables)

## Add the new variable to the list of dynamic CTF variables for rescaling and CTF computation
var_lists$vars_dynamic_ctf <- time_series_vars_by_family


# data-rescale -----------------------------------------------------------


# Rescale indicators and compute CTF for the dynamic set
cliar_indicators_rescaled <- cliar_indicators_full |>
  mutate(
    # Freedom House (1–7): reverse
    across(starts_with("fh_fiw"), ~ reverse_indicator(.x, min = 1, max = 7)),
    # V-DEM: flip so higher = stronger institutions
    across(c(vdem_core_v2x_pubcorr, vdem_core_v2x_execorr, vdem_core_v2cacamps, vdem_core_v2x_corr), flip_indicator)
    )
  

# ctf-transformations ----------------------------------------------------

# helper fns that don't warn on all-NA
safe_min <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

## Dynamic min max
# note: there are quite a few cases of Infinite warnings (due to missingness)
min_max_dynamic <- cliar_indicators_rescaled |>
  filter(between(year, 1990, 2025)) |>
  summarise(
    across(
      all_of(unlist(time_series_vars_by_family, use.names = FALSE)),
      list(
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE)
      ),
      .names = "{.col}-{.fn}"
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = c("variable", ".value"),
    names_pattern = "(.*)-(.*)"
  ) |>
  filter(!is.infinite(min) & !is.infinite(max))


## Dynamic CTF 
# Dynamic
ctf_dynamic_time_series <-
  cliaretl::compute_ctf(
    cliar_indicators_rescaled,
    (unlist(time_series_vars_by_family, use.names = FALSE)),
    min_max_dynamic,
    c("country_code","year"),
    zero_floor = 0.00)


group_ctf_dynamic <-
  country_list |>
  #join country list with ctf
  left_join(
    ctf_dynamic_time_series,
    by = "country_code",
    relationship = "many-to-many"
  ) |>
  # group by group_code, group_name, and year
  group_by(group_code, group, year) |>
  # take median
  summarise(across(all_of(unlist(time_series_vars_by_family, use.names = FALSE)),
                   ~ median(., na.rm = TRUE)),
            .groups = "drop") |>  # optionally drop grouping
  # filter out rows with missing group_name
  filter(!is.na(group)) |>
  # rename group_name to country_name and group_code to country_code
  rename(country_name = group,
         country_code = group_code)

# add group_ctf value to ctf dynamic dataset
ctf_dynamic <- tibble::add_column(ctf_dynamic_time_series,
                          country_group = 0,
                          .after = "country_code")

group_ctf_dynamic <- tibble::add_column(group_ctf_dynamic,
                                country_group = 1,
                                .after = "country_code")


## Prepare dynamic data ----
# Clean CTF dynamic data and incorporate logged GDP per capita, scorecard indicators
ctf_dynamic <-
  ctf_dynamic |>
  # add country codes and names
  left_join(
    country_list |> distinct(country_code, country_name),
    by = "country_code"
  ) |>
  select(
    country_code,
    country_name,
    everything()
  )

ctf_dynamic_long <-
  ctf_dynamic |>
  pivot_longer(
    all_of(unlist(time_series_vars_by_family, use.names = FALSE)),
    names_to = "variable"
  ) |>
  # select(-contains("gdp"), -starts_with("wb_csc")) |>
  left_join(
    db_variables |>
      select(variable, var_name, family_name, family_var)
  ) |>
  left_join(
    country_list |>
      select(country_code, country_name, country_name, group),
    relationship = "many-to-many",
    by = "country_code",
  )

# Clean CTF dynamic long data
ctf_dynamic_long_clean <-
  ctf_dynamic_long |>
  group_by(family_name, family_var, country_code, country_group, group, year) |>
  summarise(value = median(value, na.rm = TRUE)) |>
  ungroup() |>
  mutate(
    variable = family_var,
    var_name = family_name
  ) 




# family average --------------------------------------------------------

# NOTE: cliaretl::compute_family_average() assigns each indicator to a family
# using db_variables (family_var/family_name), NOT the time_series_vars_by_family
# list. That mislabels two indicators here:
#   - mis_proportion (vars_digital) has no family in db_variables -> NA_avg
#   - ibp_obs_obi    (vars_pfm)     is filed under vars_transp    -> no vars_pfm_avg
# So we build the family averages directly from time_series_vars_by_family, which
# is the intended source of truth for the dynamic families.

# variable -> family lookup taken straight from the list
family_lookup <-
  time_series_vars_by_family |>
  tibble::enframe(name = "family", value = "variable") |>
  unnest(variable)

# Dynamic families
ctf_dynamic_family <-
  ctf_dynamic |>
  select(
    country_code, country_group, year,
    all_of(unlist(time_series_vars_by_family, use.names = FALSE))
  ) |>
  pivot_longer(
    all_of(unlist(time_series_vars_by_family, use.names = FALSE)),
    names_to = "variable",
    values_to = "value"
  ) |>
  left_join(family_lookup, by = "variable") |>
  group_by(country_code, country_group, year, family) |> 
  # Following Legacy calculations:
  # A family score is now only calculated when every indicator in that family is present.
  # If any indicator is missing, the score is NA. 
  summarise(
    n_total = n(),                 # number of indicators in the family
    n_valid = sum(!is.na(value)),  # number of non-missing indicators
    value   = if (n_valid == n_total) mean(value) else NA_real_,
    .groups = "drop"
  ) |>
  select(-n_total, -n_valid) |>
  pivot_wider(
    names_from = family,
    values_from = value,
    names_glue = "{family}_avg"
  ) |>
  # drop 2026
  filter(year <= 2025) |> 
  # # drop country_group variable
  # select(-country_group) |>
  # add country names
  left_join(
    country_list |> distinct(country_code, country_name),
    by = "country_code"
  ) 

# Check for duplicates
family_lookup |> count(variable) |> filter(n > 1) # It should be empty


# Rename columns to match legacy output
ctf_dynamic_family_export <- ctf_dynamic_family |>
  rename(
    `Political Institutions`           = vars_pol_avg,
    `Social Institutions`              = vars_social_avg,
    `Corruption (lack of)`             = vars_anticorruption_avg,
    `Transparency Institutions`        = vars_transp_avg,
    `Justice Institutions`             = vars_leg_avg,
    `Public HR Institutions`           = vars_hrm_avg,
    `Public Finance Institutions`      = vars_pfm_avg,
    `Digital Institutions`             = vars_digital_avg,
    `Climate Change Institutions`      = vars_climate_avg
  )
  


# Export-data ------------------------------------------------------------


write_excel_csv(
  ctf_dynamic_family_export,
  here("output", "CLIAR_ts_benchmarking_10132026.csv")
)


# corr plot (optional) ----------------------------------------------------

legacy_panel <- readxl::read_xlsx(
  here("raw-data", "legacy_aggregates_2025.xlsx")
) 

# create a correlation plot between legacy_panel and ctf_dynamic_family as
# a check to see if the calculations are consistent

# map legacy family columns -> dynamic *_avg columns
family_crosswalk <- tibble::tribble(
  ~legacy_col,            ~family,
  "Political Inst.",      "vars_pol_avg",
  "Social Inst.",         "vars_social_avg",
  "Corruption (lack of)", "vars_anticorruption_avg",
  "Transparency Inst.",   "vars_transp_avg",
  "Justice Inst.",        "vars_leg_avg",
  "Public_HR_Inst",       "vars_hrm_avg",
  "Public_Finance_Inst",  "vars_pfm_avg",
  "Digital_Inst",         "vars_digital_avg",
  "Climate Change Inst.", "vars_climate_avg"
)

# legacy -> long
legacy_long <-
  legacy_panel |>
  pivot_longer(
    -c(country_code, country_name, year),
    names_to = "legacy_col",
    values_to = "legacy_value"
  ) |>
  left_join(family_crosswalk, by = "legacy_col") |>
  select(country_code, year, family, legacy_value)

# dynamic (country level only) -> long
dynamic_long <-
  ctf_dynamic_family |>
  filter(country_group == 0) |>
  pivot_longer(
    ends_with("_avg"),
    names_to = "family",
    values_to = "new_value"
  ) |>
  select(country_code, year, family, new_value)

# join and keep rows where both are present
compare_families <-
  inner_join(legacy_long, dynamic_long, by = c("country_code", "year", "family")) |>
  filter(!is.na(legacy_value), !is.na(new_value))

# per-family correlation labels
family_cor <-
  compare_families |>
  group_by(family) |>
  summarise(
    r = cor(legacy_value, new_value),
    label = paste0("r = ", round(r, 3)),
    .groups = "drop"
  )


library(ggplot2)

# faceted scatter: legacy vs new, with 45-degree reference line
ggplot(compare_families, aes(legacy_value, new_value)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey60", linetype = "dashed") +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_text(
    data = family_cor,
    aes(x = -Inf, y = Inf, label = label),
    hjust = -0.1, vjust = 1.3, size = 3, inherit.aes = FALSE
  ) +
  facet_wrap(~family, scales = "free") +
  labs(
    title = "Legacy vs. dynamic CTF family scores",
    subtitle = "Dashed line = perfect agreement (y = x) - final Digital revised",
    x = "Legacy aggregate",
    y = "Dynamic CTF family average"
  )
  
