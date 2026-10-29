# =============================================================================
# MRIP pipeline functions
# -----------------------------------------------------------------------------
# Same logic as the MRIP section of discards-database-pipeline.R (including the 2026-10-26 TSN overrides), packaged so the app's data build can run it over any year range. Requires foss_functions.R (for build_tsn_lookup()).
#
#   mrip_clean  <- clean_mrip(mrip_raw, years = 2020:2024)   # years optional
#   mrip_lookup <- build_mrip_lookup(MRIP_TSN_key, mrip_clean)
#   mrip_out    <- summarize_mrip(mrip_clean, mrip_lookup)
#   unmatched   <- mrip_unmatched_catch(mrip_clean, mrip_lookup)
# =============================================================================

library(tidyverse)
library(janitor)

# Names that map to multiple TSNs, or that the TSN lookup misses.
# Resolved 2026-10-26. Add rows here when new unmatched species turn up.
mrip_tsn_overrides <- tribble(
  ~name_merged,                   ~tsn,
  # Names mapping to 2 TSNs
  "BUTTERFISH",                   172567,
  "UNIDENTIFIED SKATE OR RAY",    160806,
  "SPADEFISHES",                  553178,
  "SEABASS, WHITE",               169387,
  "TRUMPETFISH",                  166411,
  "ELONGATE FROSTFISH",           768169,
  # TSNs with multiple MRIP names (lookup fell back to the AFS name)
  "WINTER FLOUNDER",              172905,
  "YELLOWTAIL FLOUNDER",          172909,
  "DEACON ROCKFISH",              166730,
  "DEACON/BLUE ROCKFISH UNKNOWN", 166730,
  # Spelling/naming differences between MRIP data and the key
  "COWCOD",                       166754,  # key: COW ROCKFISH
  "GREY SNAPPER",                 168848,  # key: GRAY SNAPPER
  "BALLONFISH",                   173392   # typo in MRIP data
)

clean_mrip <- function(mrip_raw, years = NULL) {
  out <- mrip_raw %>% clean_names()
  if (!is.null(years)) out <- filter(out, year %in% years)
  out
}

# "RED SNAPPER" -> "SNAPPER, RED"
convert_to_afs_style <- function(x) {
  parts <- str_split(str_squish(x), "\\s+")
  vapply(parts, function(p) {
    if (length(p) < 2) p[1] else paste0(p[length(p)], ", ", paste(p[-length(p)], collapse = " "))
  }, character(1))
}

# Name -> TSN lookup for MRIP: base lookup, plus AFS-style recovery for names absent from the key's MRIP_NAME, plus manual overrides. Errors if any name still maps to more than one TSN (which would double-count catch).
build_mrip_lookup <- function(tsn_key, mrip_clean, overrides = mrip_tsn_overrides) {
  base_lookup <- build_tsn_lookup(tsn_key)

  key_mrip_names <- tsn_key %>% filter(!is.na(MRIP_NAME)) %>% distinct(MRIP_NAME) %>% pull()
  afs_key_names  <- tsn_key %>% filter(!is.na(TS_AFS_NAME)) %>% distinct(TS_AFS_NAME) %>% pull()

  afs_recovered <- mrip_clean %>%
    distinct(common_name) %>%
    filter(!is.na(common_name), !common_name %in% key_mrip_names) %>%
    mutate(afs_guess = convert_to_afs_style(common_name)) %>%
    filter(afs_guess %in% afs_key_names) %>%
    left_join(
      tsn_key %>% select(TSN, TS_AFS_NAME) %>% distinct(),
      by = c("afs_guess" = "TS_AFS_NAME")
    ) %>%
    transmute(tsn = TSN, name_merged = common_name) %>%
    distinct()

  lookup <- bind_rows(base_lookup, afs_recovered) %>%
    distinct(tsn, name_merged, .keep_all = TRUE) %>%
    anti_join(overrides, by = "name_merged") %>%
    bind_rows(overrides)

  dupes <- lookup %>% filter(!is.na(name_merged)) %>% count(name_merged) %>% filter(n > 1)
  if (nrow(dupes) > 0) {
    stop("These MRIP names map to more than one TSN; add them to mrip_tsn_overrides: ",
         paste(dupes$name_merged, collapse = ", "))
  }
  lookup
}

map_mrip_region <- function(region) {
  region_clean <- region %>% str_to_lower() %>% str_replace_all("\\s+", "_")
  case_when(
    region_clean == "hawaiian_islands" ~ "hawaii",
    region_clean %in% c("gulf_of_mexico", "texas") ~ "gulf",
    region_clean == "mid-atlantic" ~ "middle_atlantic",
    region_clean %in% c("northern_california", "southern_california", "pacific_northwest") ~ "pacific_coast",
    region_clean == "north_atlantic" ~ "new_england",
    TRUE ~ region_clean
  )
}

# Join to lookup, map regions, collapse to region x TSN x year, add tsn_region_unique for the status join.
summarize_mrip <- function(mrip_clean, mrip_lookup) {
  joined <- mrip_clean %>%
    left_join(mrip_lookup, by = c("common_name" = "name_merged"))

  if (nrow(joined) != nrow(mrip_clean)) {
    stop("TSN join added ", nrow(joined) - nrow(mrip_clean), " rows; check the lookup for duplicates.")
  }

  joined %>%
    mutate(region_merged = map_mrip_region(region)) %>%
    group_by(region_merged, tsn, year) %>%
    summarize(
      common_name        = first(na.omit(common_name)),
      total_harvest_a_b1 = sum(total_harvest_a_b1, na.rm = TRUE),
      released_b2        = sum(released_b2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      region_merged     = str_to_lower(str_squish(region_merged)),
      tsn_region_unique = paste(region_merged, tsn, sep = "_")
    )
}

# Species with no TSN, ranked by catch
mrip_unmatched_catch <- function(mrip_clean, mrip_lookup) {
  mrip_clean %>%
    left_join(mrip_lookup, by = c("common_name" = "name_merged")) %>%
    filter(is.na(tsn)) %>%
    group_by(common_name) %>%
    summarize(
      harvest  = sum(total_harvest_a_b1, na.rm = TRUE),
      released = sum(released_b2, na.rm = TRUE),
      regions  = paste(sort(unique(region)), collapse = ", "),
      .groups  = "drop"
    ) %>%
    mutate(total = harvest + released) %>%
    arrange(desc(total)) %>%
    mutate(cum_share = cumsum(total) / sum(total))
}
