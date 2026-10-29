# federal-fisheries-data-pipeline

R script to integrate raw data draws from Fisheries One Stop Shop (FOSS), Marine Recreational Information Program (MRIP), and Stock SMART federal databases in order to draw insights across platforms. The inspiration for this database-wrangling project was to draw novel visualizations and insights regarding the proportion of discards across regions, sectors, and stocks over time. 

The script standardizes species identifiers, harmonizes regional definitions, and produces time series outputs suitable for analysis and visualization.

This is an ongoing, WIP project.

## Quick Start

At the least, you will need to download `data-raw/`, `pipeline/`, `run-pipeline.R`, and `app.R` into your working directory. Once downloaded, you should only have to run `run-pipeline.R` and that should populate the data needed for the Shiny App to run. It will draw all of the functions from `pipeline/`, which itself uses the raw data pulled from MRIP, FOSS, Stock SMART, and our MRIP-to-FOSS key (from `data-raw/`). These functions are updated versions of the "static" pipes we had for producing results from 2020-2024. This makes the functionality reproducible if/when we want to add additional years of data. Instead of rebuilding the pipeline each time, it should only take a few lines of changed code to draw the correct filenames from `data-raw/`. Once that's done, open up `app.R` and run the app.

## Repository Contents

| File / Folder | Description |
|---|---|
| `data-raw/` | Houses FOSS landings data `FOSS_raw_2020_2024.csv`, MRIP harvest/release data `MRIP national (for BI).xlsx`, stock assessment reference points data `StockSmart Assessment_Summary_Data_all_years.csv`, and species crosswalks for matching MRIP data entries to Taxonomic Species Numbers `MRIP_TO_TSN.xlsx` (used in other data sources, namely FOSS). |
| `pipeline/` | The backend analysis that produces all of our cleaned data via the `run-pipeline.R` script, which calls the four constituent scripts housed in this folder, `build.R`, `foss.R`, `mrip.R`, and `status.R`. Each of these scripts basically makes our initial data wrangling steps into reproducible functions; e.g., standardizing FOSS and MRIP species names, matching them together using the MRIP-to-TSN key, pivoting to the correct formats for analysis, resolving stock status regions via our crosswalk tables, etc., etc. The end result is the same: cleaned files that allow us to analyze how stock status trends with harvest vs. discards on the recreational side, and with commercial v. recreational proportions of landings on the FOSS side. |
| `run-pipeline.R` | A short script that grabs all of our scripts and their associated functionality from `pipeline/` to produce the `output/` folder, including the `discards_data.rds` R-native file that we can use as the backend for our Shiny App. |
| `output/` | Once run-pipeline.R is run, this folder should generate and populate with an R file `discards_data.rds` warehousing all cleaned data for exploring MRIP, FOSS, and stock status trends over the period of time covered in your `data-raw/` files, as well as three cleaned .csv files `FOSS_sector_breakdown_2020-2024.csv`, `FOSS_stock_status_breakdown_2020-2024.csv`, `MRIP_stock_status_breakdown_2020-2024.csv` that can be slotted into our existing Power BI architecture to generate novel plots. As of now (10-29-26), I think the FOSS_sector_breakdown .csv is superfluous, given the FOSS_stock_status_breakdown .csv holds the same information but with stock status added. |
| `app.R` | An interactive Shiny App that we can use to explore stock status, rec v. commercial landings, and recreational harvest v. discards over time. |

## Notes and Assumptions
- Multi-source FOSS records (e.g. FOSS v. ACCSP) are collapsed, not treated as duplicates.
- Region mapping is rule-based and may require future refinement.
- Stock status is applied dynamically, wherein each "species" entry on FOSS or MRIP is matched to the stock assessment most recent to whatever year is being analyzed, e.g., if we view "Pacific Coast Cabezon" from 2020-2024, we'll be seeing stock status from the 2019 assessment. If, hypothetically, we were looking at "Pacific Coast Cabezon" from 2010-2024, the associated assessment would change pre- and post-2019.
- Some species remain unmatched due to naming inconsistencies and/or possible upstream data entry errors.

## Requirements

**R Version:** 4.4.3 (2025-02-28 ucrt)

### Key packages
- `shiny` 1.12.1
- `bslib` 0.12.0
- `plotly` 4.12.1
- `scales` 1.4.0
- `readxl` 1.4.5
- `tidyverse` 2.0.0 (includes `ggplot2`, `dplyr`, `tidyr`, `readr`, `tibble`, `stringr`, `forcats`, `purrr`)
- `janitor` 2.2.1
