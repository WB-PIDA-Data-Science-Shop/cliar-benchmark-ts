# Digital family benchmark -------------------------------------------------
# Tune the mis_proportion definition so the dynamic Digital series tracks the
# legacy "Digital_Inst" column as closely as possible.
#
# Prereqs (run first):
#   source("raw-data/mis-proportion.R")   # builds `mis_long` + `operational_code`
#   source("analysis/01-benchmarking.R")  # builds `legacy_panel`

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(here)


# candidate definitions ----------------------------------------------------
# Each candidate maps mis_long -> a (country_code, year, mis_proportion) panel.
# They differ in (a) what counts as "in place" and (b) the completeness rule.

build_series <- function(df, op_code, require_all) {
  df |>
    cross_join(tibble(year = 1990:2025)) |>
    mutate(
      in_place = case_when(
        is.na(mis_value)             ~ NA_real_,
        mis_value < op_code          ~ 0,
        is.na(mis_est_year)          ~ NA_real_,
        year >= mis_est_year         ~ 1,
        TRUE                         ~ 0
      )
    ) |>
    group_by(country_code, year) |>
    summarise(
      n_known        = sum(!is.na(in_place)),
      mis_present    = sum(in_place, na.rm = TRUE),
      mis_proportion = case_when(
        require_all & n_known == 9  ~ mis_present / 9,
        require_all                 ~ NA_real_,
        n_known > 0                 ~ mis_present / n_known,  # share of known
        TRUE                        ~ NA_real_
      ),
      .groups = "drop"
    ) |>
    select(country_code, year, mis_proportion)
}

candidates <- tribble(
  ~name,                        ~op_code, ~require_all,
  "operational_all9",           2,        TRUE,    # current definition
  "operational_shareKnown",     2,        FALSE,   # relax completeness
  "planned_all9",               1,        TRUE,    # count planned systems
  "planned_shareKnown",         1,        FALSE
)

digital_series <-
  candidates |>
  mutate(
    series = map2(op_code, require_all, \(oc, ra) build_series(mis_long, oc, ra))
  ) |>
  select(name, series) |>
  unnest(series)


# legacy Digital column ----------------------------------------------------

legacy_digital <-
  legacy_panel |>
  select(country_code, year, legacy_value = Digital_Inst) |>
  filter(!is.na(legacy_value))


# correlations -------------------------------------------------------------

digital_compare <-
  digital_series |>
  inner_join(legacy_digital, by = c("country_code", "year")) |>
  filter(!is.na(mis_proportion))

digital_cor <-
  digital_compare |>
  group_by(name) |>
  summarise(
    n           = n(),
    pearson     = cor(legacy_value, mis_proportion),
    spearman    = cor(legacy_value, mis_proportion, method = "spearman"),
    label       = paste0("r = ", round(pearson, 3)),
    .groups = "drop"
  ) |>
  arrange(desc(pearson))


# faceted scatter by candidate definition ----------------------------------

ggplot(digital_compare, aes(legacy_value, mis_proportion)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey60", linetype = "dashed") +
  geom_point(alpha = 0.25, size = 0.8) +
  geom_text(
    data = digital_cor,
    aes(x = -Inf, y = Inf, label = label),
    hjust = -0.1, vjust = 1.3, size = 3, inherit.aes = FALSE
  ) +
  facet_wrap(~name, scales = "free") +
  labs(
    title = "Legacy Digital vs. candidate mis_proportion definitions",
    subtitle = "Dashed line = perfect agreement (y = x)",
    x = "Legacy Digital_Inst",
    y = "Candidate mis_proportion"
  )

digital_cor
