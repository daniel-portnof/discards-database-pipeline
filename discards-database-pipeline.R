
# Data Import and Preparation load in -------------------------------------
### This code will pull in raw data from MRIP, FOSS, and Stock SMART for preparation.

library(tidyverse)
library(janitor)

foss_raw <- read.csv("FOSS_raw_2020_2024.csv")
mrip_raw <- readxl::read_xlsx("MRIP national (for BI).xlsx")
stocksmat_raw <- read.csv("StockSmart Assessment_Summary_Data_all_years.csv")
MRIP_TSN_key <- readxl::read_xlsx("MRIP_TO_TSN.xlsx")


# Beginning with FOSS -----------------------------------------------------

# Clean some of the data names/categories first
foss_clean <- foss_raw %>%
  mutate(
    year = as.integer(Year),
    region_merged = str_to_lower(Region.Name) %>%
      str_replace_all("[ -]+", "_"),
    tsn = suppressWarnings(as.numeric(Tsn)),
    pounds_num = parse_number(Pounds), # handles commas and blanks
    collection_clean = str_to_lower(Collection)
  )

# Build a TSN -> MRIP_NAME lookup so we can load identifiers on the front end
tsn_lookup <- MRIP_TSN_key %>%
  group_by(TSN) %>%
  summarize(
    name_merged = if (n_distinct(MRIP_NAME, na.rm = T) == 1) {
      first(na.omit(MRIP_NAME))
    } else {
      NA_character_
    }, # add an ifelse step to take "TS_AFS_NAME" if MRIP_NAME returns a numeric value; that should yield an additional 42 species, including cabezon, copper rockfish, quillback rockfish, lingcod, etc.
    .groups = "drop"
  ) %>%
  rename(tsn = TSN)


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

## And how many TSNs in the lookup table were unresolved?
sum(is.na(tsn_lookup$name_merged))
unresolved_tsns <- tsn_lookup %>%
  group_by(tsn) %>%
  filter(is.na(tsn)) # FIX THIS CODE

# Join FOSS to the tsn lookup
foss_tsn_joined <- foss_clean %>%
  left_join(tsn_lookup, by = "tsn") # COME BACK TO THIS, CHECK HOW MANY OBSERVATIONS RETURN NA FOR TSN

## How many FOSS rows lost names after the join?
sum(is.na(foss_tsn_joined$name_merged))

## What were they?

# Aggregate pounds by year / region / species / collection
foss_summarized <- foss_tsn_joined %>%
  filter(collection_clean %in% c("commercial", "recreational")) %>%
  group_by(year, region_merged, tsn, name_merged, collection_clean) %>%
  summarize(
    pounds = sum(pounds_num, na.rm = T),
    .groups = "drop"
  ) # COME BACK TO THIS, WHY DID WE LOSE ~2000 OBSERVATIONS?

## If we dropped some observations when aggregating, how many?
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

# Pivot to our wide format
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

# Check for 2023 only
foss_pivoted_2023 <- foss_pivoted %>%
  filter(year == 2023)




# Data transformation load In -----------------------------------------------------------------
### This code will create cleaned tables matched with stock status.

#install.packages("janitor")
library(tidyverse)
library(janitor)

MRIP_TSN_key <- readxl::read_xlsx("MRIP_TO_TSN.xlsx")
FOSS_pivot_out <- read.csv("FOSSPIVOT_out.csv")
stock_status <- read.csv("StockSmart Assessment_Summary_Data_all_years.csv")
mrip_synthesis <- read.csv("mrip_2023_R_filter.csv")


# FOSS tsn + region unique identifier -------------------------------------

FOSS_pivot_out <- FOSS_pivot_out %>%
  mutate(
    region_merged = str_to_lower(str_squish(region_merged)),
    tsn_region_unique = paste(region_merged, tsn_num, sep = "_")
    )

most_recent_stock_status <- stock_status %>%
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
      str_detect(stock_area_std, "gulf of mexico|eastern gulf of mexico|atlantic and gulf of mexico|gulf of mexico") ~ "gulf",
      
      # --- SOUTH ATLANTIC ---
      str_detect(stock_area_std, "south atlantic|southern atlantic|carolinas|florida keys|east florida") ~ "south atlantic",
      
      # --- MID-ATLANTIC (incl. Cape Hatteras transitions) ---
      str_detect(stock_area_std, "mid-atlantic|cape hatteras") ~ "middle atlantic",
      
      # --- NEW ENGLAND / NEFSC-ish ---
      str_detect(stock_area_std,
                 "gulf of maine|georges bank|cape cod|southern new england|north atlantic|northwestern atlantic") ~ "new england",
      
      # --- PACIFIC ISLANDS (tropical + central/western pacific management areas) ---
      str_detect(stock_area_std,
                 "main hawaiian islands|hawaii|guam|american samoa|south pacific|western and central pacific|eastern north pacific far north migrating") ~ "hawaii",
      
      # --- ALASKA ---
      str_detect(stock_area_std,
                 "alaska|aleutian|bering|bristol bay|gulf of alaska|pribilof|norton sound|yakutat|saint matthew|bogoslof|auke creek|berners river|ford arm lake|hugh smith lake") ~ "alaska",
      
      # --- WEST COAST (default for CONUS Pacific coast / river basins / Puget Sound etc.) ---
      str_detect(stock_area_std,
                 "california|oregon|washington|pacific coast|point conception|juan de fuca|puget sound|columbia river|klamath|willapa|grays harbor|quinault|quillayute|queets|hoh|hoko") ~ "pacific coast",
      
      # --- FALLBACKS / GENERICS ---
      stock_area_std %in% c("pacific", "pacific coast", "eastern pacific", "north pacific", "northern pacific coast", "southern pacific coast", "eastern pacific") ~ "pacific coast",
      stock_area_std %in% c("atlantic", "atlantic coast", "atlantic coast / alaska", "western atlantic", "atlantic and gulf of mexico") ~ "middle atlantic",
      
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

FOSS_pivot_out2 <- FOSS_pivot_out %>%
  left_join(status_append, by = "tsn_region_unique")

readr::write_csv(FOSS_pivot_out2, "FOSS_stock_status_merge.csv")

# How many stocks in X region?

status_most_recent_with_region_clean %>%
  filter(merged_region.y == "gulf") %>%
  tally()




# MRIP + region unique identifier -----------------------------------------


key_collapsed <- MRIP_TSN_key %>%
  group_by(MRIP_NAME) %>%
  summarise(
    TSN = dplyr::first(na.omit(TSN)),     # pick a representative TSN
    n_tsn = n_distinct(TSN, na.rm = TRUE),    # how many TSNs per MRIP_NAME
    .groups = "drop"
  ) %>%
  mutate(ambiguous_name = n_tsn > 1)

mrip_with_tsn <- mrip_synthesis %>%
  left_join(key_collapsed %>% select(MRIP_NAME, TSN, ambiguous_name),
            by = c("common_name" = "MRIP_NAME"))

mrip_with_tsn <- mrip_with_tsn %>%
  mutate(
    region_merged = str_to_lower(str_squish(region_merged)),
    tsn_region_unique = paste(region_merged, TSN, sep = "_")
  )

#mrip_with_tsn2 <- mrip_with_tsn %>%
#  left_join(status_append, by = "tsn_region_unique")
#
#mrip_with_tsn2 <- mrip_with_tsn2 %>%
#  mutate(
#    discard_proportion = if_else(
#      released_b2 == 0 | total_harvest_a_b1 == 0,
#      NA_real_,
#      released_b2 / (released_b2 + total_harvest_a_b1)
#    )
#  )

mrip_with_tsn2 <- mrip_with_tsn %>%
  left_join(status_append, by = "tsn_region_unique") %>%
  group_by(year, tsn_region_unique) %>%
  summarize(
    released_b2 = sum(released_b2, na.rm = T),
    total_harvest_a_b1 = sum(total_harvest_a_b1, na.rm = T),
    
    common_name = dplyr::first(common_name),
    TSN = dplyr::first(TSN),
    region_merged = dplyr::first(region_merged),
    ambiguous_name = dplyr::first(ambiguous_name),
    
    .groups = "drop"
  ) %>%
  mutate(
    discard_proportion = case_when(
      is.na(released_b2) | is.na(total_harvest_a_b1) ~ NA_real_,
      (released_b2 + total_harvest_a_b1) == 0 ~ NA_real_,
      TRUE ~ released_b2 / (released_b2 + total_harvest_a_b1)
    )
  )

readr::write_csv(mrip_with_tsn2, "mrip_with_tsn.csv")
