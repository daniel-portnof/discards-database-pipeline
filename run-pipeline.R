# ============================================================================ #
# Discards database pipeline ----
# ---------------------------------------------------------------------------- #
# Rebuilds every output from the four raw files:
#   output/FOSS_sector_breakdown_<years>.csv
#   output/FOSS_stock_status_breakdown_<years>.csv
#   output/MRIP_stock_status_breakdown_<years>.csv
#   output/discards_data.rds   (read by the Shiny app)
#
# The logic lives in pipeline/:
#   foss.R    build_tsn_lookup(), clean_foss(), summarize_foss(), foss_diagnostics()
#   mrip.R    mrip_tsn_overrides, clean_mrip(), build_mrip_lookup(),
#             summarize_mrip(), mrip_unmatched_catch()
#   status.R  map_stock_area(), build_status_history(), build_status_by_year(),
#             attach_status()
#   build.R   build_discards_data()  <- chains all of the above
#
# Edit those files to change the logic; this script just runs it.
# ============================================================================ #

library(tidyverse)
library(janitor)

for (f in list.files("pipeline", pattern = "\\.R$", full.names = TRUE)) source(f)


# Build ------------------------------------------------------------------------

d <- build_discards_data(
  foss_path       = "data-raw/FOSS_raw_2020_2024.csv",
  mrip_path       = "data-raw/MRIP national (for BI).xlsx",
  stocksmart_path = "data-raw/StockSmart Assessment_Summary_Data_all_years.csv",
  tsn_key_path    = "data-raw/MRIP_TO_TSN.xlsx",
  years           = NULL,        # e.g. 2022:2024 for a subset; NULL = all years
  out_dir         = "output"
)

# Diagnostics ------------------------------------------------------------------
# Replaces the inline checks from the old script. Review after each rebuild,
# especially when new raw data come in.

## FOSS TSNs not in the key (dropped by the inner join), largest first
d$diagnostics$foss$rejects

## FOSS records collapsed across sources (ACCSP + GULFFIN)
d$diagnostics$foss$multi_source

## FOSS rows dropped by the TSN join
d$diagnostics$foss$n_rows_dropped

## MRIP species with no TSN, ranked by catch. New names near the top are
## candidates for mrip_tsn_overrides in pipeline/mrip.R.
print(d$diagnostics$mrip_unmatched, n = 20)


# Step-through (optional) ------------------------------------------------------
# build_discards_data() runs these same steps internally. Uncomment to run them
# one at a time and inspect intermediate objects.

# foss_raw       <- read.csv("data-raw/FOSS_raw_2020_2024.csv")
# mrip_raw       <- readxl::read_xlsx("data-raw/MRIP national (for BI).xlsx")
# stocksmart_raw <- read.csv("data-raw/StockSmart Assessment_Summary_Data_all_years.csv")
# MRIP_TSN_key   <- readxl::read_xlsx("data-raw/MRIP_TO_TSN.xlsx")
#
# tsn_lookup     <- build_tsn_lookup(MRIP_TSN_key)
# foss_clean     <- clean_foss(foss_raw)
# foss           <- summarize_foss(foss_clean, tsn_lookup)
#
# mrip_clean     <- clean_mrip(mrip_raw)
# mrip_lookup    <- build_mrip_lookup(MRIP_TSN_key, mrip_clean)
# mrip           <- summarize_mrip(mrip_clean, mrip_lookup)
#
# data_years     <- sort(unique(c(foss$year, mrip$year)))
# status_history <- build_status_history(stocksmart_raw)
# status_by_year <- build_status_by_year(status_history, data_years)
#
# foss_out       <- attach_status(foss, status_by_year)
# mrip_out       <- attach_status(mrip, status_by_year)


# Exploratory plots ------------------------------------------------------------

mrip_plot_prep <- d$mrip

# Catch-weighted discard proportion by region
region_year_discard <- mrip_plot_prep %>%
  filter(!is.na(region_merged), !is.na(year), total_caught > 0) %>%
  group_by(region_merged, year) %>%
  summarise(
    released_total = sum(released_b2, na.rm = TRUE),
    caught_total   = sum(total_caught, na.rm = TRUE),
    discard_prop_weighted = released_total / caught_total,
    .groups = "drop"
  )

ggplot(region_year_discard, aes(year, discard_prop_weighted, color = region_merged)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(title = "Catch-weighted discard proportion over time by region",
       x = "Year", y = "Discard proportion") +
  theme_minimal()

# Catch-weighted discard proportion for poor-condition stocks, by region
region_year_poor_discard <- mrip_plot_prep %>%
  filter(poor_stock_condition %in% TRUE, !is.na(region_merged), total_caught > 0) %>%
  group_by(region_merged, year) %>%
  summarise(
    released_total = sum(released_b2, na.rm = TRUE),
    caught_total   = sum(total_caught, na.rm = TRUE),
    discard_prop_weighted = released_total / caught_total,
    n_stocks = n_distinct(tsn_region_unique),
    .groups = "drop"
  )

ggplot(region_year_poor_discard, aes(year, discard_prop_weighted, color = region_merged)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(title = "Catch-weighted discard proportion over time for poor-condition stocks",
       x = "Year", y = "Discard proportion") +
  theme_minimal()

# Stock-level discard proportions for poor-condition stocks
poor_stock_year_discard <- mrip_plot_prep %>%
  filter(poor_stock_condition %in% TRUE, total_caught > 0)

ggplot(poor_stock_year_discard,
       aes(year, discard_proportion, group = tsn_region_unique)) +
  geom_line(alpha = 0.35) +
  facet_wrap(~ region_merged) +
  labs(title = "Stock-level discard proportions over time for poor-condition stocks",
       x = "Year", y = "Discard proportion") +
  theme_minimal()

# Unweighted mean discard proportion for poor-condition stocks, by region
region_year_poor_discard_unweighted <- mrip_plot_prep %>%
  filter(poor_stock_condition %in% TRUE, !is.na(region_merged), total_caught > 0) %>%
  group_by(region_merged, year) %>%
  summarize(
    discard_prop_unweighted = mean(discard_proportion, na.rm = TRUE),
    n_stocks = n_distinct(tsn_region_unique),
    .groups = "drop"
  )

ggplot(region_year_poor_discard_unweighted,
       aes(year, discard_prop_unweighted, color = region_merged)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(title = "Unweighted mean discard proportion over time for poor-condition stocks",
       x = "Year", y = "Mean discard proportion (unweighted)") +
  theme_minimal()
