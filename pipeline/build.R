# ============================================================================#
# build_discards_data(): the whole pipeline in one call
# ----------------------------------------------------------------------------#
# Reads the four raw inputs, runs FOSS, MRIP and stock status, and writes:
#   - FOSS_sector_breakdown_<years>.csv        (Power BI)
#   - FOSS_stock_status_breakdown_<years>.csv  (Power BI)
#   - MRIP_stock_status_breakdown_<years>.csv  (Power BI)
#   - discards_data.rds                        (everything our Shiny app reads)
#
# Usage:
#   source("foss.R"); source("mrip.R")
#   source("status.R"); source("build.R")
#   d <- build_discards_data()                    # all years in the raw files
#   d <- build_discards_data(years = 2022:2024)   # a subset
# ============================================================================#

build_discards_data <- function(
    foss_path       = "FOSS_raw_2020_2024.csv",
    mrip_path       = "MRIP national (for BI).xlsx",
    stocksmart_path = "StockSmart Assessment_Summary_Data_all_years.csv",
    tsn_key_path    = "MRIP_TO_TSN.xlsx",
    years           = NULL,
    out_dir         = "output"
) {
  # Read
  foss_raw       <- read.csv(foss_path)
  mrip_raw       <- readxl::read_xlsx(mrip_path)
  stocksmart_raw <- read.csv(stocksmart_path)
  tsn_key        <- readxl::read_xlsx(tsn_key_path)

  # FOSS
  tsn_lookup <- build_tsn_lookup(tsn_key)
  foss_clean <- clean_foss(foss_raw, years)
  foss       <- summarize_foss(foss_clean, tsn_lookup)

  # MRIP
  mrip_clean  <- clean_mrip(mrip_raw, years)
  mrip_lookup <- build_mrip_lookup(tsn_key, mrip_clean)
  mrip        <- summarize_mrip(mrip_clean, mrip_lookup)

  # Stock status, as known in each data year
  data_years     <- sort(unique(c(foss$year, mrip$year)))
  status_history <- build_status_history(stocksmart_raw)
  status_by_year <- build_status_by_year(status_history, data_years)

  foss_out <- attach_status(foss, status_by_year)
  mrip_out <- attach_status(mrip, status_by_year) %>%
    mutate(
      total_caught       = released_b2 + total_harvest_a_b1,
      discard_proportion = if_else(total_caught > 0, released_b2 / total_caught, NA_real_)
    )

  # Write
  dir.create(out_dir, showWarnings = FALSE)
  yr <- paste(range(data_years), collapse = "-")

  readr::write_csv(select(foss, -tsn_region_unique),
                   file.path(out_dir, paste0("FOSS_sector_breakdown_", yr, ".csv")))
  readr::write_csv(foss_out, file.path(out_dir, paste0("FOSS_stock_status_breakdown_", yr, ".csv")))
  readr::write_csv(mrip_out, file.path(out_dir, paste0("MRIP_stock_status_breakdown_", yr, ".csv")))

  result <- list(
    foss           = foss_out,
    mrip           = mrip_out,
    status_history = status_history,
    diagnostics    = list(
      foss           = foss_diagnostics(foss_clean, tsn_lookup, tsn_key),
      mrip_unmatched = mrip_unmatched_catch(mrip_clean, mrip_lookup)
    ),
    years          = data_years,
    built_at       = Sys.time()
  )
  saveRDS(result, file.path(out_dir, "discards_data.rds"))

  message("Built ", yr, ": ", nrow(foss_out), " FOSS rows, ", nrow(mrip_out),
          " MRIP rows. Written to ", out_dir, "/")
  invisible(result)
}
