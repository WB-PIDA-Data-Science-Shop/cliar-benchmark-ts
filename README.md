# cliar-benchmark-ts

Data analysis project.

## Project structure

- `R/` — reusable R functions sourced by analysis scripts.
- `data-raw/` — raw, unmodified input data (not tracked if large/sensitive).
- `data/` — processed/cleaned data ready for analysis.
- `output/` — generated figures, tables, and other artifacts (git-ignored).
- `reports/` — Quarto/R Markdown reports and notebooks.
- `renv/` — project-local R package library managed by `renv`.

## Setup

This project uses [`renv`](https://rstudio.github.io/renv/) to manage R package dependencies.

```r
# Restore the project library from renv.lock
renv::restore()
```

## Workflow

1. Place raw data in `data-raw/`.
2. Add cleaning/processing scripts (e.g., `data-raw/01_clean_data.R`) that write processed data to `data/`.
3. Add reusable functions to `R/`.
4. Write analysis reports in `reports/`.
