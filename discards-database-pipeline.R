
# Data Import and Preparation load in -------------------------------------
### This code will pull in raw data from MRIP, FOSS, and Stock SMART for preparation.

library(tidyverse)
library(janitor)

foss_raw <- read.csv("FOSS_raw_2020_2024.csv")
mrip_raw <- readxl::read_xlsx("MRIP national (for BI).xlsx")
stocksmart_raw <- read.csv("StockSmart Assessment_Summary_Data_all_years.csv")
MRIP_TSN_key <- readxl::read_xlsx("MRIP_TO_TSN.xlsx")


# FOSS -----------------------------------------------------

# Clean some of the data names/categories first, filter out confidential data records
foss_clean <- foss_raw %>%
  mutate(
    year = as.integer(Year),
    region_merged = str_to_lower(Region.Name) %>%
      str_replace_all("[ -]+", "_"),
    tsn = suppressWarnings(as.numeric(Tsn)),
    pounds_num = parse_number(Pounds), # handles commas and blanks
    collection_clean = str_to_lower(Collection)
  ) %>%
  filter(Confidentiality != "Confidential")

# Build a TSN -> MRIP_NAME lookup so we can load identifiers on the front end
tsn_lookup <- MRIP_TSN_key %>%
  group_by(TSN) %>%
  summarize(
    name_merged = {
      mrip_vals <- na.omit(MRIP_NAME)
      afs_vals <- na.omit(TS_AFS_NAME)
      
      mrip_clean <- mrip_vals[!grepl("^\\s*-?[0-9]+\\.?[0-9]*\\s*$", mrip_vals)]
      
      if (n_distinct(mrip_clean) == 1) {
        first(mrip_clean)
      } else if (n_distinct(afs_vals) == 1) {
        first(afs_vals)
      } else {
        NA_character_
      }
    },
    .groups = "drop"
  ) %>%
  rename(tsn = TSN)

## How many unique TSNs in each?
n_distinct(tsn_lookup$tsn)
n_distinct(foss_clean$Tsn)

## Any TSNs missing from FOSS?
foss_clean %>%
  filter(!Tsn %in% tsn_lookup$tsn) %>%
  summarize(n_missing_tsns = n_distinct(Tsn))

## Which ones specifically?
foss_clean %>%
  filter(!Tsn %in% tsn_lookup$tsn) %>%
  distinct(Tsn) %>%
  arrange(Tsn)

## How many records have no TSN match? i.e., given we know some TSNs are missing from the above, what does that translate to in terms of observations across years, regions, etc.?
nrow(foss_clean %>% filter(!tsn %in% tsn_lookup$tsn))

## A table to house rejects from the tsn_lookup. This summarizes all tsn's across all years and regions that were not captured by the MRIP_TSN_key and therefore cannot be merged accurately into our analysis.
tsn_match_rejects_summary <- foss_clean %>%
  filter(!tsn %in% tsn_lookup$tsn) %>%
  arrange(tsn) %>%
  left_join(
    MRIP_TSN_key %>%
      select(TSN, MRIP_NAME, TS_AFS_NAME) %>%
      rename(tsn = TSN),
    by = "tsn"
  ) %>%
  group_by(tsn, NMFS.Name, Scientific.Name, MRIP_NAME, TS_AFS_NAME) %>%
  summarize(
    years = paste(sort(unique(year)), collapse = ", "),
    regions = paste(sort(unique(Region.Name)), collapse = ", "),
    collections = paste(sort(unique(Collection)), collapse = ", "),
    n_rows = n(),
    total_pounds = sum(pounds_num, na.rm = T),
    .groups = "drop"
  ) %>%
  arrange(desc(total_pounds))

## Check if any TSN values in the key table are duplicates, and if so how many of each.
MRIP_TSN_key %>%
  count(TSN, sort = TRUE) %>%
  filter(n > 1)

## If needed, we create a table showing which TSNs specifically were duplicated.
duplicate_tsn_rows <- MRIP_TSN_key %>%
  group_by(TSN) %>%
  filter(n() > 1) %>%
  arrange(TSN)
duplicate_tsn_rows

# Join FOSS to the tsn lookup
foss_tsn_joined <- foss_clean %>%
  inner_join(tsn_lookup, by = "tsn")
nrow(foss_clean) - nrow(foss_tsn_joined)

# Integrity checks

## Any missing TSNs in the joined table?
cat("Records with missing TSN:", sum(is.na(foss_tsn_joined$tsn)))

## Any missing name_merged values?
cat("Records with missing name_merged:", sum(is.na(foss_tsn_joined$name_merged)))

## Are all of our name_merged values strings?
numeric_names <- foss_tsn_joined %>%
  filter(grepl("^\\s*-?[0-9]+\\.?[0-9]*\\s*$", name_merged))
cat("Records with numeric name_merged:", nrow(numeric_names))

# Aggregate pounds by year / region / species / collection
foss_summarized <- foss_tsn_joined %>%
  filter(collection_clean %in% c("commercial", "recreational")) %>%
  group_by(year, region_merged, tsn, name_merged, collection_clean) %>%
  summarize(
    pounds = sum(pounds_num, na.rm = T),
    .groups = "drop"
  )

## If we dropped/collapsed some observations when aggregating, how many?
nrow(foss_tsn_joined) -
  nrow(
    foss_tsn_joined %>%
      group_by(year, region_merged, tsn, name_merged, collection_clean) %>%
      summarize(pounds = sum(pounds_num, na.rm = T), .groups = "drop")
  )

## Which combinations had multiple observations?
foss_tsn_joined %>%
  filter(collection_clean %in% c("commercial", "recreational")) %>%
  count(year, region_merged, tsn, name_merged, collection_clean, sort = T) %>%
  filter(n > 1)

## Looking at these results, I suspect it might be that the aggregation step is collapsing together the GULFFIN and ACCSP "Source" column for records that otherwise match. Let's double check
foss_tsn_joined %>%
  filter(collection_clean %in% c("commercial", "recreational")) %>%
  group_by(year, region_merged, tsn, name_merged, collection_clean) %>%
  filter(n() > 1) %>%
  arrange(year, region_merged, tsn, collection_clean) %>%
  select(year, region_merged, tsn, name_merged, collection_clean, Source, pounds_num)

## Yup, that does appear to be the issue. Let's add a step that flags this on the foss_tsn_joined table, since the aggregation step will collapse them together anyway.
## NOTE! For now, we're going to collapse multi-source records into one. We don't anticipate that this is double counting.
foss_tsn_joined <- foss_tsn_joined %>%
  group_by(year, region_merged, tsn, name_merged, collection_clean) %>%
  mutate(
    multi_source = n() > 1,
    sources = paste(sort(unique(Source)), collapse = " + ")
  ) %>%
  ungroup()

# Now that we've diagnosed, let's pivot the aggregated table to our wide format
foss_pivoted <- foss_summarized %>%
  mutate(collection_clean = recode(collection_clean,
                                   "recreational" = "recpounds",
                                   "commercial" = "compounds")) %>%
  pivot_wider(
    names_from = collection_clean,
    values_from = pounds,
    values_fill = 0
  ) %>%
  mutate(
    rec_proportion_raw = if_else(
      (recpounds + compounds) > 0,
      recpounds / (recpounds + compounds),
      NA_real_
    ),
    recpounds = na_if(recpounds, 0), # NAs for zero values
    rec_proportion = if_else(
      !is.na(rec_proportion_raw) & rec_proportion_raw == 0, # NAs for zero values
      NA_real_,
      rec_proportion_raw
    ),
    compounds = na_if(compounds, 0) # NAs for zero values
  ) %>%
  select(-rec_proportion_raw) %>%
  arrange(year, region_merged, name_merged)

foss_pivoted

# OUTPUT FROM THIS SECTION: time-series breakdown of FOSS by sector
readr::write_csv(foss_pivoted, "FOSS_sector_breakdown_2020-2024.csv")

# Output for a specific year only
#foss_pivoted_X <- foss_pivoted %>%
#  filter(year == X)

#foss_pivoted_2024 <- foss_pivoted %>%
#  filter(year == 2024)

# NOTE! Create function to pull out individual years in raw data for run below on stock status.
# Apply stock status to the most recent year pull in this loop^ and don't draw conclusions based on prior years.
## UNLESS we can find a way to pipe in historical stock status ...?

# Let's get a three-year map for this. 2022-2024?

## FOSS prep for stock status merge ----------------------------------------


foss_pivoted <- foss_pivoted %>%
  mutate(
    region_merged = str_to_lower(str_squish(region_merged)),
    tsn_region_unique = paste(region_merged, tsn, sep = "_")
  )

# 1. Keep ALL assessments, not just the latest ---------------------------------

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
  )

# 2. Stock area -> merged region -----------------------------------------------

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

status_history <- status_all %>%
  mutate(
    merged_region = map_stock_area(stock_area),
    tsn_region_unique = paste(merged_region, itis_taxon_serial_number, sep = "_")
  )

## Stock areas that didn't map (should be empty; recheck when new data come in)
status_history %>%
  filter(is.na(merged_region)) %>%
  distinct(stock_area) %>%
  arrange(stock_area)

status_history <- status_history %>%
  filter(!is.na(merged_region)) %>%
  select(
    tsn_region_unique, stock_id, stock_name, assessment_year, assessment_id,
    flimit, fmsy, f_flimit, b_blimit, blimit, bmsy
  )

## If one stock has two assessments in the same year, keep the later record
## (higher assessment_id). Placeholder rule; check how often it happens:
status_history %>%
  count(stock_id, assessment_year) %>%
  filter(n > 1)

status_history <- status_history %>%
  group_by(stock_id, assessment_year) %>%
  slice_max(assessment_id, n = 1, with_ties = FALSE) %>%
  ungroup()

# 3. Per-stock as-of join, then summarize to species x region x year -----------

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

data_years <- sort(unique(c(foss_pivoted$year, mrip_collapsed$year)))

status_by_year <- build_status_by_year(status_history, data_years)

# 4. Attach to FOSS and MRIP ---------------------------------------------------

FOSS_pivot_out <- foss_pivoted %>%
  left_join(status_by_year, by = c("tsn_region_unique", "year"),
            relationship = "many-to-one")

mrip_pivot_out <- mrip_collapsed %>%
  left_join(status_by_year, by = c("tsn_region_unique", "year"),
            relationship = "many-to-one") %>%
  mutate(
    total_caught = released_b2 + total_harvest_a_b1,
    discard_proportion = if_else(total_caught > 0, released_b2 / total_caught, NA_real_)
  )

## Row counts should be unchanged by the status joins
nrow(FOSS_pivot_out) - nrow(foss_pivoted)
nrow(mrip_pivot_out) - nrow(mrip_collapsed)

# NOTE: the plot-prep block that builds `status_lookup_mrip` and carries the
# latest status across all years is no longer needed. Every row now has its own
# year-appropriate status, so feed mrip_pivot_out straight into the summaries.

readr::write_csv(FOSS_pivot_out, "FOSS_stock_status_breakdown_2020-2024.csv")


# MRIP --------------------------------------------------------------------


str(mrip_raw)

mrip_clean <- mrip_raw %>%
  clean_names()

# Build a TSN -> MRIP_NAME lookup so we can load identifiers on the front end
tsn_lookup_mrip <- MRIP_TSN_key %>%
  group_by(TSN) %>%
  summarize(
    name_merged = {
      mrip_vals <- na.omit(MRIP_NAME)
      afs_vals <- na.omit(TS_AFS_NAME)
      
      mrip_clean <- mrip_vals[!grepl("^\\s*-?[0-9]+\\.?[0-9]*\\s*$", mrip_vals)]
      
      if (n_distinct(mrip_clean) == 1) {
        first(mrip_clean)
      } else if (n_distinct(afs_vals) == 1) {
        first(afs_vals)
      } else {
        NA_character_
      }
    },
    .groups = "drop"
  ) %>%
  rename(tsn = TSN)

## Define unique MRIP and key names
mrip_names <- mrip_clean %>%
  distinct(common_name) %>%
  filter(!is.na(common_name))

key_mrip_names <- MRIP_TSN_key %>%
  distinct(MRIP_NAME) %>%
  filter(!is.na(MRIP_NAME))

## Isolate species unmatched to the key
mrip_unmatched <- mrip_names %>%
  filter(!(common_name %in% key_mrip_names$MRIP_NAME))

## Convert to the style of TS_AFS_NAME (our second option for missing matches from MRIP_NAME)
convert_to_afs_style <- function(x) {
  x <- str_squish(x)
  parts <- str_split(x, "\\s+", simplify = F)
  
  sapply(parts, function(p) {
    if(length(p) < 2) {
      p[1]
    } else {
      paste0(p[length(p)], ", ", paste(p[-length(p)], collapse = " "))
    }
  })
}

mrip_unmatched_afs <- mrip_unmatched %>%
  mutate(afs_guess = convert_to_afs_style(common_name))

## Compare against TS_AFS_NAME in key table
afs_key_names <- MRIP_TSN_key %>%
  distinct(TS_AFS_NAME) %>%
  filter(!is.na(TS_AFS_NAME))

mrip_unmatched_afs <- mrip_unmatched_afs %>%
  mutate(matches_ts_afs = afs_guess %in% afs_key_names$TS_AFS_NAME)

## Summarize how many matches we got to TS_AFS_NAME
mrip_unmatched_afs %>%
  summarize(
    unmatched_names = n(),
    covered_by_ts_afs = sum(matches_ts_afs),
    still_uncovered = sum(!matches_ts_afs)
  )

## What are the matches?
mrip_unmatched_afs %>%
  filter(matches_ts_afs) %>%
  arrange(common_name) %>%
  print(n = Inf)

## What is still unmatched?
mrip_unmatched_afs %>%
  filter(!matches_ts_afs) %>%
  arrange(common_name) %>%
  print(n = Inf)

# At this point, visual inspection of the remaining unmatched species tells me that it's a mix of true missing values from the key and formatting/naming conventions. For example, "GREY SNAPPER" is reported in our MRIP data along with "GRAY SNAPPER," but the key only contains the latter. Another example is "BALLONFISH" in our MRIP data, whereas the key lists as "BALLOONFISH."

## Let's recover the new matches from the AFS matching
afs_recovered_lookup <- mrip_unmatched_afs %>%
  filter(matches_ts_afs) %>%
  left_join(
    MRIP_TSN_key %>%
      select(TSN, TS_AFS_NAME) %>%
      distinct(),
    by = c("afs_guess" = "TS_AFS_NAME")
  ) %>%
  transmute(
    tsn = TSN,
    name_merged = common_name
  ) %>%
  distinct()

## And add those matches to our tsn_lookup_mrip table
tsn_lookup_mrip_tfs <- bind_rows(
  tsn_lookup_mrip,
  afs_recovered_lookup
) %>%
  distinct(tsn, name_merged, .keep_all = T)

# Pin names that map to multiple TSNs, or that the TSN lookup misses (resolved 2026-10-26)
tsn_overrides <- tribble(
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

tsn_lookup_mrip_tfs <- tsn_lookup_mrip_tfs %>%
  anti_join(tsn_overrides, by = "name_merged") %>%
  bind_rows(tsn_overrides)

# Guard: stops the script if any name maps to more than one TSN in future data
stopifnot(!any(duplicated(na.omit(tsn_lookup_mrip_tfs$name_merged))))

## And join that into a single mrip table
mrip_tsn_joined <- mrip_clean %>%
  left_join(
    tsn_lookup_mrip_tfs,
    by = c("common_name" = "name_merged")
  )

# Now for the regions!

## What are the unique regions in this cut of MRIP data?
mrip_tsn_joined %>%
  distinct(region) %>%
  arrange(region) %>%
  print(n = Inf)

## Let's convert those to the FOSS format
mrip_tsn_joined <- mrip_tsn_joined %>%
  mutate(
    region_clean = region %>%
      str_to_lower() %>%
      str_replace_all("\\s+", "_"),
    
    region_merged = case_when(
      region_clean == "hawaiian_islands" ~ "hawaii",
      region_clean %in% c("gulf_of_mexico", "texas") ~ "gulf",
      region_clean == "mid-atlantic" ~ "middle_atlantic",
      region_clean %in% c("northern_california", "southern_california", "pacific_northwest") ~ "pacific_coast",
      region_clean == "north_atlantic" ~ "new_england",
      TRUE ~ region_clean
    )
  )

## Then, let's collapse all constituent values (e.g. Northern California, Southern California) into aggregate values (pacific_coast)
mrip_collapsed <- mrip_tsn_joined %>%
  group_by(region_merged, tsn, year) %>%
  summarize(
    common_name = first(na.omit(common_name)),
    total_harvest_a_b1 = sum(total_harvest_a_b1, na.rm = T),
    released_b2 = sum(released_b2, na.rm = T),
    .groups = "drop"
  )

## Any duplicates?
mrip_collapsed %>%
  count(region_merged, tsn, year) %>%
  filter(n > 1) %>%
  print(n = Inf)
## NO!


## MRIP prep for stock status merge ----------------------------------------
## NOTE! Currently, this section of code is contingent on having run the prior stock-status-FOSS merge. That's OK for now, but it's possible we may want a standalone MRIP-stock-status merge at some point. That will just be a case of adjusting the FOSS version.

## Generate the unique tsn_region identifier for a specific stock.
mrip_collapsed <- mrip_collapsed %>%
  mutate(
    region_merged = str_to_lower(str_squish(region_merged)),
    tsn_region_unique = paste(region_merged, tsn, sep = "_")
  )


# =============================================================================
# MRIP name-join check
# -----------------------------------------------------------------------------
# The "Any duplicates? NO!" check on mrip_collapsed can't catch this: if one
# common_name maps to two TSNs, the left join copies that species' catch into
# both TSN groups. Each group is unique on (region, tsn, year), so no duplicate
# shows up, but the catch is counted twice. These two checks catch it:
# =============================================================================

## Names in the lookup that point at more than one TSN
tsn_lookup_mrip_tfs %>%
  filter(!is.na(name_merged)) %>%
  count(name_merged) %>%
  filter(n > 1)

## Did the join add rows? Should be 0.
nrow(mrip_tsn_joined) - nrow(mrip_clean)

## Separate issue: catch from all unmatched species lands in one NA-TSN row per
## region-year. How much catch is that?
mrip_tsn_joined %>%
  filter(is.na(tsn)) %>%
  summarize(
    n_species = n_distinct(common_name),
    harvest = sum(total_harvest_a_b1, na.rm = TRUE),
    released = sum(released_b2, na.rm = TRUE)
  )



## Grab the most recent year of your MRIP data for comparison with the most recent stock assessment

readr::write_csv(mrip_pivot_out, "MRIP_stock_status_breakdown_2020-2024.csv")


# EXTRAS ------------------------------------------------------------------

# Exploratory plots?

library(dplyr)
library(ggplot2)
library(stringr)

mrip_plot_prep <- mrip_pivot_out %>%
  mutate(
    total_caught = released_b2 + total_harvest_a_b1
  )

mrip_plot_prep <- mrip_pivot_out

region_year_discard <- mrip_plot_prep %>%
  filter(!is.na(region_merged), !is.na(year), !is.na(total_caught), total_caught > 0) %>%
  group_by(region_merged, year) %>%
  summarise(
    released_total = sum(released_b2, na.rm = TRUE),
    caught_total = sum(total_caught, na.rm = TRUE),
    discard_prop_weighted = if_else(caught_total == 0, NA_real_, released_total / caught_total),
    .groups = "drop"
  )

# Catch-weighted discard proportion for stocks by region
ggplot(region_year_discard, aes(x = year, y = discard_prop_weighted, color = region_merged)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Catch-weighted discard proportion over time by region",
    x = "Year",
    y = "Discard proportion"
  ) +
  theme_minimal()

# Catch-weighted discard proportion for "poor-condition" stocks by region (i.e. either overfished or subject to overfishing)
region_year_poor_discard <- mrip_plot_prep %>%
  filter(
    poor_stock_condition == TRUE,
    !is.na(region_merged),
    !is.na(year),
    !is.na(total_caught),
    total_caught > 0
  ) %>%
  group_by(region_merged, year) %>%
  summarise(
    released_total = sum(released_b2, na.rm = TRUE),
    caught_total = sum(total_caught, na.rm = TRUE),
    discard_prop_weighted = if_else(caught_total == 0, NA_real_, released_total / caught_total),
    n_stocks = n_distinct(tsn_region_unique),
    .groups = "drop"
  )

ggplot(region_year_poor_discard, aes(x = year, y = discard_prop_weighted, color = region_merged)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Catch-weighted discard proportion over time for poor-condition stocks",
    x = "Year",
    y = "Discard proportion"
  ) +
  theme_minimal()

# Stock-level discard proportions for poor-condition stocks (unweighted)
poor_stock_year_discard <- mrip_plot_prep %>%
  filter(
    poor_stock_condition == TRUE,
    !is.na(year),
    !is.na(total_caught),
    total_caught > 0
  ) %>%
  mutate(
    discard_prop = released_b2 / total_caught
  )

ggplot(
  poor_stock_year_discard,
  aes(x = year, y = discard_prop, group = tsn_region_unique)
) +
  geom_line(alpha = 0.35) +
  facet_wrap(~ region_merged) +
  labs(
    title = "Stock-level discard proportions over time for poor-condition stocks",
    x = "Year",
    y = "Discard proportion"
  ) +
  theme_minimal()

# Unweighted discard proportions by region
region_year_poor_discard_unweighted <- mrip_plot_prep %>%
  filter(
    poor_stock_condition == T,
    !is.na(region_merged),
    !is.na(year),
    !is.na(total_caught),
    total_caught > 0
  ) %>%
  mutate(
    discard_prop = released_b2 / total_caught
  ) %>%
  group_by(region_merged, year) %>%
  summarize(
    discard_prop_unweighted = mean(discard_prop, na.rm = T),
    n_stocks = n_distinct(tsn_region_unique),
    .groups = "drop"
  )

ggplot(region_year_poor_discard_unweighted, aes(x = year, y = discard_prop_unweighted, color = region_merged)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Unweighted mean discard proportion over time for poor-condition stocks",
    x = "Year",
    y = "Mean discard proportion (unweighted)"
  ) +
  theme_minimal()



