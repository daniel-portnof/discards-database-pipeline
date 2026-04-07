
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

most_recent_stock_status <- stocksmart_raw %>%
  group_by(
    ITIS.Taxon.Serial.Number,
    Stock.ID,
    Regional.Ecosystem,
    Science.Center,
    FMP,
    Jurisdiction
  ) %>%
  filter(Assessment.Year == max(Assessment.Year, na.rm = T)) %>%
  ungroup()

most_recent_stock_status_clean <- most_recent_stock_status %>%
  clean_names() %>%
  mutate(
    across(
      c(jurisdiction, science_center, regional_ecosystem, stock_area),
      ~ .x %>%
        as.character() %>%
        str_squish() %>%
        str_to_lower()
    )
  )

most_recent_stock_status_clean <- most_recent_stock_status_clean %>%
  mutate(
    across(
      c(jurisdiction, science_center, regional_ecosystem, stock_area),
      ~ str_replace_all(.x, "\\s*/\\s*", " / ")
    )
  )

sc_xwalk <- tibble::tribble( # add texas to gulf filters; pacific northwest, northern california, southern california to pacific coast filters
  ~pattern, ~merged_region,
  "nefsc", "new england",
  "sefsc", "south atlantic",
  "gmfsc", "gulf",
  "afsc", "alaska",
  "pifsc", "hawaii",
  "nwfsc", "pacific coast",
  "swfsc", "pacific coast"
)

eco_xwalk <- tibble::tribble( # add texas to gulf filters; pacific northwest, northern california, southern california to pacific coast filters
  ~pattern, ~merged_region,
  "california current", "pacific coast",
  "gulf of mexico", "gulf",
  "northeast shelf", "new england",
  "pacific islands ecosystem complex", "hawaii",
  "southeast shelf", "south atlantic"
)

assign_region_from_patterns <- function(x, patterns, region) {
  # returns firts matching region; NA if none match
  out <- rep(NA_character_, length(x))
  for (i in seq_along(patterns)) {
    hits <- !is.na(x) & str_detect(x, fixed(patterns[i]))
    out[is.na(out) & hits] <- region[i]
  }
  out
}

status_with_region <- most_recent_stock_status_clean %>%
  mutate(
    region_sc = assign_region_from_patterns(science_center, sc_xwalk$pattern, sc_xwalk$merged_region),
    region_eco = assign_region_from_patterns(regional_ecosystem, eco_xwalk$pattern, eco_xwalk$merged_region),
    
    # choose final region by priority:
    merged_region = coalesce(region_sc, region_eco)
  )

status_with_region <- status_with_region %>%
  mutate(
    ambiguous_sc = str_detect(science_center, "/"),
    ambiguous_eco = str_detect(regional_ecosystem, "/")
  )


stock_area_xwalk <- most_recent_stock_status_clean %>%
  clean_names() %>%
  distinct(stock_area) %>%
  mutate(
    stock_area_std = stock_area %>%
      str_squish() %>%
      str_to_lower(),
    
    merged_region = case_when( # add texas to gulf filters; pacific northwest, northern california, southern california to pacific coast filters
      # --- CARIBBEAN ---
      str_detect(stock_area_std, "caribbean|puerto rico|st\\. croix|st\\. thomas|st\\. john") ~ "caribbean",
      
      # --- GULF OF MEXICO ---
      str_detect(stock_area_std, "gulf of mexico|eastern gulf of mexico|atlantic and gulf of mexico|gulf of mexico|texas") ~ "gulf",
      
      # --- SOUTH ATLANTIC ---
      str_detect(stock_area_std, "south atlantic|southern atlantic|carolinas|florida keys|east florida") ~ "south_atlantic",
      
      # --- MID-ATLANTIC (incl. Cape Hatteras transitions) ---
      str_detect(stock_area_std, "mid-atlantic|cape hatteras") ~ "middle_atlantic",
      
      # --- NEW ENGLAND / NEFSC-ish ---
      str_detect(stock_area_std,
                 "gulf of maine|georges bank|cape cod|southern new england|north atlantic|northwestern atlantic") ~ "new_england",
      
      # --- PACIFIC ISLANDS (tropical + central/western pacific management areas) ---
      str_detect(stock_area_std,
                 "main hawaiian islands|hawaii|guam|american samoa|south pacific|western and central pacific|eastern north pacific far north migrating") ~ "hawaii",
      
      # --- ALASKA ---
      str_detect(stock_area_std,
                 "alaska|aleutian|bering|bristol bay|gulf of alaska|pribilof|norton sound|yakutat|saint matthew|bogoslof|auke creek|berners river|ford arm lake|hugh smith lake") ~ "alaska",
      
      # --- WEST COAST (default for CONUS Pacific coast / river basins / Puget Sound etc.) ---
      str_detect(stock_area_std,
                 "california|oregon|washington|pacific coast|point conception|juan de fuca|puget sound|columbia river|klamath|willapa|grays harbor|quinault|quillayute|queets|hoh|hoko|pacific northwest|northern california|southern california") ~ "pacific_coast",
      
      # --- FALLBACKS / GENERICS ---
      stock_area_std %in% c("pacific", "pacific coast", "eastern pacific", "north pacific", "northern pacific coast", "southern pacific coast", "eastern pacific") ~ "pacific_coast",
      stock_area_std %in% c("atlantic", "atlantic coast", "atlantic coast / alaska", "western atlantic", "atlantic and gulf of mexico") ~ "middle_atlantic",
      
      TRUE ~ NA_character_
    )
  ) %>%
  select(stock_area, merged_region)

# Anything that didn't map?
stock_area_xwalk %>%
  filter(is.na(merged_region)) %>%
  arrange(stock_area)

# How many per region?
stock_area_xwalk %>%
  count(merged_region, sort = TRUE)

# Any stock areas that include "/" (often composite areas)?
most_recent_stock_status_clean %>%
  clean_names() %>%
  distinct(stock_area) %>%
  filter(str_detect(stock_area, "/"))

status_most_recent_with_region <- status_with_region %>%
  clean_names() %>%
  left_join(stock_area_xwalk, by = "stock_area")

status_most_recent_with_region_clean <- status_most_recent_with_region %>%
  filter(!is.na(itis_taxon_serial_number)) %>%
  filter(!regional_ecosystem %in% c("pacific highly migratory", "caribbean sea")) %>%
  filter(jurisdiction != "atlantic hms") %>%
  select(
    stock_id,
    stock_name,
    jurisdiction,
    science_center,
    regional_ecosystem,
    itis_taxon_serial_number,
    common_name,
    stock_area,
    assessment_id,
    assessment_year,
    flimit,
    fmsy,
    f_flimit,
    b_blimit,
    blimit,
    bmsy,
    region_sc,
    region_eco,
    merged_region.x,
    ambiguous_sc,
    ambiguous_eco,
    merged_region.y
  )

status_most_recent_with_region_clean <- status_most_recent_with_region_clean %>%
  mutate(
    tsn_region_unique = paste(merged_region.y, itis_taxon_serial_number, sep = "_")
  )

status_append <- status_most_recent_with_region_clean %>%
  select(tsn_region_unique, flimit, fmsy, f_flimit, b_blimit, blimit, bmsy, assessment_year, assessment_id,)

status_append <- status_append %>%
  group_by(tsn_region_unique) %>%
  slice_max(assessment_year, n = 1, with_ties = F) %>%
  ungroup()

most_recent_year <- max(foss_pivoted$year, na.rm = T)

FOSS_pivot_out <- foss_pivoted %>%
  left_join(
    foss_pivoted %>%
      filter(year == most_recent_year) %>%
      left_join(status_append, by = "tsn_region_unique") %>%
      select(tsn_region_unique, year, flimit, fmsy, f_flimit, b_blimit, blimit, bmsy, assessment_year, assessment_id),
    by = c("tsn_region_unique", "year")
  )

FOSS_pivot_out <- FOSS_pivot_out %>%
  mutate(
    poor_stock_condition = case_when(
      (!is.na(f_flimit) & f_flimit > 1) | (!is.na(b_blimit) & b_blimit < 1) ~ TRUE,
      (!is.na(f_flimit) | !is.na(b_blimit)) ~ FALSE,
      TRUE ~ NA
    )
  )

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

## Grab the most recent year of your MRIP data for comparison with the most recent stock assessment
most_recent_mrip_year <- max(mrip_collapsed$year, na.rm = T)

mrip_pivot_out <- mrip_collapsed %>%
  left_join(
    mrip_collapsed %>%
      filter(year == most_recent_mrip_year) %>%
      left_join(status_append, by = "tsn_region_unique") %>%
      select(
        tsn_region_unique,
        year,
        flimit,
        fmsy,
        f_flimit,
        b_blimit,
        blimit,
        bmsy,
        assessment_year,
        assessment_id
      ),
    by = c("tsn_region_unique", "year")
  )

## Generate discard proportions
mrip_pivot_out <- mrip_pivot_out %>%
  mutate(
    discard_proportion = case_when(
      is.na(released_b2) | is.na(total_harvest_a_b1) ~ NA_real_,
      (released_b2 + total_harvest_a_b1) == 0 ~ NA_real_,
      T ~ released_b2 / (released_b2 + total_harvest_a_b1)
    )
  )

mrip_pivot_out <- mrip_pivot_out %>%
  mutate(
    total_caught = released_b2 + total_harvest_a_b1,
    poor_stock_condition = case_when(
      (!is.na(f_flimit) & f_flimit > 1) | (!is.na(b_blimit) & b_blimit < 1) ~ TRUE,
      (!is.na(f_flimit) | !is.na(b_blimit)) ~ FALSE,
      TRUE ~ NA
    )
  )

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

# Pull stock status from the rows where it exists, then carry it across all years
status_lookup_mrip <- mrip_plot_prep %>%
  filter(!is.na(assessment_id)) %>%
  distinct(
    tsn_region_unique,
    f_flimit,
    b_blimit,
    flimit,
    fmsy,
    blimit,
    bmsy,
    assessment_year,
    assessment_id
  ) %>%
  mutate(
    poor_stock_condition = case_when(
      (!is.na(f_flimit) & f_flimit > 1) | (!is.na(b_blimit) & b_blimit < 1) ~ TRUE,
      (!is.na(f_flimit) | !is.na(b_blimit)) ~ FALSE,
      TRUE ~ NA
    )
  )

mrip_plot_prep <- mrip_plot_prep %>%
  select(-any_of(c(
    "f_flimit", "b_blimit", "flimit", "fmsy", "blimit", "bmsy",
    "assessment_year", "assessment_id", "poor_stock_condition"
  ))) %>%
  left_join(status_lookup_mrip, by = "tsn_region_unique")

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



