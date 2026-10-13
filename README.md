# cliar-benchmark-ts

Data analysis project.

## Project structure

- `R/` — reusable R functions sourced by analysis scripts.
- `data-raw/` — raw, unmodified input data (not tracked if large/sensitive).
- `output/` — generated data frames and other artifacts (git-ignored).
- `renv/` — project-local R package library managed by `renv`.

## Setup

This project uses [`renv`](https://rstudio.github.io/renv/) to manage R package dependencies.

```r
# Restore the project library from renv.lock
renv::restore()
```

## Reproducible Workflow

1. Raw Inputs: Place incoming datasets into the data-raw/ directory.
2. Processing: Source data processing scripts (e.g., data-raw/*.R). Processed outputs will be written directly to output/.
3. Dependencies: Pre-built lazy-loaded data objects from the cliaretl package are available in memory when working inside the analysis/ directory.
4. Benchmarking Execution: Run the primary benchmarking routine:

```r
source("analysis/01-benchmarking.R")
```

