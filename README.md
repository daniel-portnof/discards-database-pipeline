# federal-fisheries-data-pipeline

R script to integrate raw data draws from Fisheries One Stop Shop (FOSS), Marine Recreational Information Program (MRIP), and Stock SMART federal databases in order to draw insights across platforms. The inspiration for this database-wrangling project was to draw novel visualizations and insights regarding the proportion of discards across regions, sectors, and stocks over time. 

The script standardizes species identifiers, harmonizes regional definitions, and produces time series outputs suitable for analysis and visualization.

This is an ongoing, WIP project.

*NOTE:* You will need to download the `FOSS_raw_2020_2024.csv`, `MRIP national (for BI).xlsx`, `StockSmart Assessment_Summary_Data_all_years.csv`, and `MRIP_TO_TSN.xlsx` files into your working directory to run the main `discards-database-pipeline.R` scrip.

### Notes (04-26-2026):

## Repository Contents

| File / Folder | Description |
|---|---|
| `discards-database-pipeline.R` | Script to clean and standardize all datasets, create joining table keys to resolve species names across data sources, generate stock-region unique identifier labels, and append most recent stock status variables to each stock. The output is cleaned, aggregated datasets linking landings, discard proportions, and stock status across regions and time |
#### Data Inputs
| `FOSS_raw_2020_2024.csv` | FOSS landings data (commercial and recreational) |
| `MRIP national (for BI).xlsx` | Recreational sector harvest and release estimates |
| `StockSmart Assessment_Summary_Data_all_years.csv` | Stock assessment reference points |
| `MRIP_TO_TSN.xlsx` | Species crosswalks for matching MRIP data entries to Taxonomic Species Numbers (used in other data sources, namely FOSS) |

## Outputs

| `FOSS_sector_breakdown_2020-2024.csv` | FOSS sector breakdown |
| `FOSS_stock_status_breakdown_2020-2024.csv` | FOSS + stock status |
| `MRIP_stock_status_breakdown_2020-2024.csv` | MRIP + stock status + discard proportions |

## Notes and Assumptions
- Multi-source FOSS records (e.g. FOSS v. ACCSP) are collapsed, not treated as duplicates.
- Region mapping is rule-based and may require future refinement.
- Stock status is applied using most recent assessment only.
- Historical stock condition is not considered or reconstructed.
- Some species remain unmatched due to naming inconsistencies and/or possible upstream data entry errors.

## Requirements

**R Version:** 4.4.3 (2025-02-28 ucrt)

### Key packages
- `tidyverse` 2.0.0 (includes `ggplot2`, `dplyr`, `tidyr`, `readr`, `tibble`, `stringr`, `forcats`, `purrr`)
- `janitor` 2.2.1
