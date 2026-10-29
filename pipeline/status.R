# =============================================================================
# Stock status functions (as known at the time, any_poor summary to flag if poor condition was found in ANY stocks under a species name)
# -----------------------------------------------------------------------------
# Function versions of status_as_of.R.
#
#   status_history <- build_status_history(stocksmart_raw)
#   status_by_year <- build_status_by_year(status_history, years = 2020:2024)
#   foss_out       <- attach_status(foss_summarized, status_by_year)
# =============================================================================

library(tidyverse)
library(janitor)

map_stock_area <- function(x) {
  x <- str_squish(str_to_lower(x))
  case_when(
    str_detect(x, "caribbean|puerto rico|st\\. croix|st\\. thomas|st\\. john") ~ "caribbean",
    str_detect(x, "gulf of mexico|eastern gulf of mexico|atlantic and gulf of mexico|texas") ~ "gulf",
    str_detect(x, "south atlantic|southern atlantic|carolinas|florida keys|east florida") ~ "south_atlantic",
    str_detect(x, "mid-atlantic|cape hatteras") ~ "middle_atlantic",
    str_detect(x, "gulf of maine|georges bank|cape cod|southern new england|north atlantic|northwestern atlantic") ~ "new_england",
    str_detect(x, "main hawaiian islands|hawaii|guam|american samoa|south pacific|western and central pacific|eastern north pacific far north migrating") ~ "hawaii",
    str_detect(x, "alaska|aleutian|bering|bristol bay|gulf of alaska|pribilof|norton sound|yakutat|saint matthew|bogoslof|auke creek|berners river|ford arm lake|hugh smith lake") ~ "alaska",
    str_detect(x, "california|oregon|washington|pacific coast|point conception|juan de fuca|puget sound|columbia river|klamath|willapa|grays harbor|quinault|quillayute|queets|hoh|hoko|pacific northwest|northern california|southern california") ~ "pacific_coast",
    x %in% c("pacific", "pacific coast", "eastern pacific", "north pacific",
             "northern pacific coast", "southern pacific coast",
             "northern subpopulation") ~ "pacific_coast", # northern anchovy
    x %in% c("atlantic", "atlantic coast", "atlantic coast / alaska", "western atlantic", "atlantic and gulf of mexico") ~ "middle_atlantic",
    TRUE ~ NA_character_
  )
}

# Every assessment for every stock, with region and tsn_region_unique. One record per stock per assessment year (whatever the later assessment_id is wins).
build_status_history <- function(stocksmart_raw) {
  status_all <- stocksmart_raw %>%
    clean_names() %>%
    mutate(
      across(
        c(jurisdiction, science_center, regional_ecosystem, stock_area),
        ~ .x %>%
          as.character() %>%
          str_squish() %>%
          str_to_lower() %>%
          str_replace_all("\\s*/\\s*", " / ")
      )
    ) %>%
    filter(
      !is.na(itis_taxon_serial_number),
      !is.na(assessment_year),
      !regional_ecosystem %in% c("pacific highly migratory", "caribbean sea"),
      jurisdiction != "atlantic hms"
    ) %>%
    mutate(
      merged_region     = map_stock_area(stock_area),
      tsn_region_unique = paste(merged_region, itis_taxon_serial_number, sep = "_")
    )

  unmapped <- status_all %>% filter(is.na(merged_region)) %>% distinct(stock_area) %>% pull()
  if (length(unmapped) > 0) {
    warning("Stock areas with no region (dropped); add them to map_stock_area(): ",
            paste(unmapped, collapse = "; "))
  }

  status_all %>%
    filter(!is.na(merged_region)) %>%
    select(
      tsn_region_unique, stock_id, stock_name, assessment_year, assessment_id,
      flimit, fmsy, f_flimit, b_blimit, blimit, bmsy
    ) %>%
    group_by(stock_id, assessment_year) %>%
    slice_max(assessment_id, n = 1, with_ties = FALSE) %>%
    ungroup()
}

# Per-stock as-of join for each year, summarized to species x region x year.
build_status_by_year <- function(status_history, years) {
  stock_years <- status_history %>%
    distinct(tsn_region_unique, stock_id) %>%
    cross_join(tibble(year = years))

  stock_years %>%
    left_join(
      status_history %>% select(-tsn_region_unique),
      by = join_by(stock_id, closest(year >= assessment_year)),
      relationship = "many-to-one"
    ) %>%
    filter(!is.na(assessment_id)) %>% # stock not yet assessed as of this year
    mutate(
      poor = case_when(
        (!is.na(f_flimit) & f_flimit > 1) | (!is.na(b_blimit) & b_blimit < 1) ~ TRUE,
        !is.na(f_flimit) | !is.na(b_blimit) ~ FALSE,
        TRUE ~ NA
      )
    ) %>%
    group_by(tsn_region_unique, year) %>%
    summarize(
      n_stocks               = n_distinct(stock_id),
      n_stocks_with_status   = sum(!is.na(poor)),
      n_poor                 = sum(poor, na.rm = TRUE),
      any_poor               = if (all(is.na(poor))) NA else any(poor, na.rm = TRUE),
      max_f_flimit           = suppressWarnings(max(f_flimit, na.rm = TRUE)),
      min_b_blimit           = suppressWarnings(min(b_blimit, na.rm = TRUE)),
      latest_assessment_year = max(assessment_year),
      oldest_assessment_year = min(assessment_year),
      stock_ids              = paste(sort(unique(stock_id)), collapse = ", "),
      .groups = "drop"
    ) %>%
    mutate(
      across(c(max_f_flimit, min_b_blimit), ~ if_else(is.infinite(.x), NA_real_, .x)),
      poor_stock_condition = any_poor # keeps existing plot code and Power BI fields working
    )
}

# Join status onto any table keyed by tsn_region_unique + year.
attach_status <- function(df, status_by_year) {
  out <- df %>%
    left_join(status_by_year, by = c("tsn_region_unique", "year"),
              relationship = "many-to-one")
  stopifnot(nrow(out) == nrow(df))
  out
}
