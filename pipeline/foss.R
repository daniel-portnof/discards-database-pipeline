# =============================================================================
# FOSS pipeline functions
# -----------------------------------------------------------------------------
# Same logic as the FOSS section of discards-database-pipeline.R, packaged as
# functions so the app's data build can run it over any year range.
#
#   tsn_lookup <- build_tsn_lookup(MRIP_TSN_key)
#   foss_clean <- clean_foss(foss_raw, years = 2020:2024)   # years optional
#   foss_out   <- summarize_foss(foss_clean, tsn_lookup)
#   diag       <- foss_diagnostics(foss_clean, tsn_lookup, MRIP_TSN_key)
# =============================================================================

library(tidyverse)

is_numeric_string <- function(x) grepl("^\\s*-?[0-9]+\\.?[0-9]*\\s*$", x)

# TSN -> display name. Identical to tsn_lookup and tsn_lookup_mrip in the
# original script, so MRIP can reuse this too.
build_tsn_lookup <- function(tsn_key) {
  tsn_key %>%
    group_by(TSN) %>%
    summarize(
      name_merged = {
        mrip_vals <- na.omit(MRIP_NAME)
        afs_vals  <- na.omit(TS_AFS_NAME)
        mrip_vals <- mrip_vals[!is_numeric_string(mrip_vals)]

        if (n_distinct(mrip_vals) == 1) {
          first(mrip_vals)
        } else if (n_distinct(afs_vals) == 1) {
          first(afs_vals)
        } else {
          NA_character_
        }
      },
      .groups = "drop"
    ) %>%
    rename(tsn = TSN)
}

# Standardize names/types, drop confidential records, optionally filter years.
clean_foss <- function(foss_raw, years = NULL) {
  out <- foss_raw %>%
    mutate(
      year             = as.integer(Year),
      region_merged    = str_to_lower(Region.Name) %>% str_replace_all("[ -]+", "_"),
      tsn              = suppressWarnings(as.numeric(Tsn)),
      pounds_num       = parse_number(Pounds),
      collection_clean = str_to_lower(Collection)
    ) %>%
    filter(Confidentiality != "Confidential")

  if (!is.null(years)) out <- filter(out, year %in% years)
  out
}

# Join to TSN lookup, aggregate by sector, pivot wide, compute rec proportion,
# add tsn_region_unique for the status join.
summarize_foss <- function(foss_clean, tsn_lookup) {
  foss_clean %>%
    inner_join(tsn_lookup, by = "tsn") %>%
    filter(collection_clean %in% c("commercial", "recreational")) %>%
    group_by(year, region_merged, tsn, name_merged, collection_clean) %>%
    summarize(pounds = sum(pounds_num, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      collection_clean = recode(collection_clean,
                                "recreational" = "recpounds",
                                "commercial"   = "compounds"),
      # Factor + names_expand guarantees both columns exist even if a year
      # range has no records for one sector
      collection_clean = factor(collection_clean, levels = c("recpounds", "compounds"))
    ) %>%
    pivot_wider(
      names_from   = collection_clean,
      values_from  = pounds,
      values_fill  = 0,
      names_expand = TRUE
    ) %>%
    mutate(
      rec_proportion_raw = if_else(
        (recpounds + compounds) > 0,
        recpounds / (recpounds + compounds),
        NA_real_
      ),
      recpounds      = na_if(recpounds, 0),
      rec_proportion = if_else(
        !is.na(rec_proportion_raw) & rec_proportion_raw == 0,
        NA_real_,
        rec_proportion_raw
      ),
      compounds      = na_if(compounds, 0)
    ) %>%
    select(-rec_proportion_raw) %>%
    arrange(year, region_merged, name_merged) %>%
    mutate(
      region_merged     = str_to_lower(str_squish(region_merged)),
      tsn_region_unique = paste(region_merged, tsn, sep = "_")
    )
}

# The checks from the original script, returned as tables instead of printed.
foss_diagnostics <- function(foss_clean, tsn_lookup, tsn_key) {
  rejects <- foss_clean %>%
    filter(!tsn %in% tsn_lookup$tsn) %>%
    left_join(
      tsn_key %>% select(TSN, MRIP_NAME, TS_AFS_NAME) %>% rename(tsn = TSN),
      by = "tsn"
    ) %>%
    group_by(tsn, NMFS.Name, Scientific.Name, MRIP_NAME, TS_AFS_NAME) %>%
    summarize(
      years        = paste(sort(unique(year)), collapse = ", "),
      regions      = paste(sort(unique(Region.Name)), collapse = ", "),
      collections  = paste(sort(unique(Collection)), collapse = ", "),
      n_rows       = n(),
      total_pounds = sum(pounds_num, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(total_pounds))

  multi_source <- foss_clean %>%
    inner_join(tsn_lookup, by = "tsn") %>%
    filter(collection_clean %in% c("commercial", "recreational")) %>%
    group_by(year, region_merged, tsn, name_merged, collection_clean) %>%
    filter(n() > 1) %>%
    summarize(
      sources = paste(sort(unique(Source)), collapse = " + "),
      n_rows  = n(),
      pounds  = sum(pounds_num, na.rm = TRUE),
      .groups = "drop"
    )

  list(
    rejects            = rejects,
    multi_source       = multi_source,
    n_rows_dropped     = nrow(foss_clean) - nrow(inner_join(foss_clean, tsn_lookup, by = "tsn")),
    missing_name       = sum(is.na(inner_join(foss_clean, tsn_lookup, by = "tsn")$name_merged))
  )
}
