# Title: Build Unified Annual Population Estimates (Geo & Sex)
# Input:  data-raw/pop_census_long.csv
# Output: data-processed/annual_population_estimates.qs

library(here)
library(tidyverse)
library(qs)

source(here("R", "functions_all.R"))

# Metadata -----------------------------------------------------

geo_meta <- get_official_geo_metadata()

# Create a master lookup to ensure metadata is consistent and never "lost"
meta_lookup <- bind_rows(
  
  # National
  tibble(
    geo = "national", 
    name = "kiribati", 
    geo_code = "0"
  ),
  
  # Divisions
  geo_meta$census_island %>%
    distinct(division_code, division_name, OI_ST) %>%
    mutate(
      geo = "division", 
      name = division_name, 
      geo_code = division_code,
      island_name = if_else(name == "south tarawa", "south tarawa", NA_character_),
      island_code = if_else(name == "south tarawa", "7", NA_character_)
    ),
  
  # OI_ST Regions
  tibble(
    geo = "oi_st", 
    name = c("st", "oi"),
    OI_ST = name
  ) %>%
    mutate(
      island_name   = if_else(name == "st", "south tarawa", NA_character_),
      island_code   = if_else(name == "st", "7", NA_character_),
      division_name = if_else(name == "st", "south tarawa", NA_character_),
      division_code = if_else(name == "st", "1", NA_character_)
    ),
  
  # Islands
  geo_meta$census_island %>%
    mutate(
      geo = "island", 
      name = island_name, 
      geo_code = island_code,
      council = if_else(name == "north tarawa", "etc", NA_character_)
    ),
  
  # Councils
  tibble(
    geo = "council", 
    name = c("btc", "tuc", "etc"),
    council = name,
    island_name   = c("south tarawa", "south tarawa", "north tarawa"),
    island_code   = c("7", "7", "6"),
    division_name = c("south tarawa", "south tarawa", "northern"),
    division_code = c("1", "1", "2"),
    OI_ST         = c("st", "st", "oi")
  ),
  
  # Villages (South Tarawa)
  geo_meta$census_village %>%
    mutate(
      geo = "village", 
      name = village_name, 
      geo_code = village_code
    )
  
) %>%
  mutate(across(where(is.character), ~str_trim(str_to_lower(.)))) %>%
  distinct(geo, name, .keep_all = TRUE)




# Raw data ---------------------------

pop_raw_path <- here("data-raw", "pop_census_long.csv")

pop_census_raw <- read_csv(pop_raw_path, show_col_types = FALSE) %>%
  mutate(across(c(name, geo, sex), ~str_trim(str_to_lower(.))),
         population = as.numeric(population)) %>%
  filter(!is.na(population)) %>% 
  left_join(meta_lookup, by = c("geo", "name"))

# Aggregate Census Data ----------------------------------------

# Helper: Returns TRUE only if all expected names for a geo-group are present
is_sex_complete <- function(current_names, expected_names) {
  expected_names <- na.omit(expected_names)
  length(intersect(current_names, expected_names)) == length(unique(expected_names))
}

census_aggregates <- bind_rows(
  # A. Divisions (Sum of Islands)
  pop_census_raw %>% 
    filter(geo == "island") %>%
    group_by(year, sex, division_name) %>%
    filter(is_sex_complete(name, meta_lookup$name[meta_lookup$geo == "island" & meta_lookup$division_name == first(division_name)])) %>%
    summarise(population = sum(population), .groups = "drop") %>% 
    mutate(geo = "division") %>% 
    rename(name = division_name),
  
  # B. Councils (BTC/TUC from Villages + ETC from Island)
  # BTC & TUC
  pop_census_raw %>% 
    filter(geo == "village", council %in% c("btc", "tuc")) %>%
    group_by(year, sex, council) %>% 
    filter(is_sex_complete(name, meta_lookup$name[meta_lookup$geo == "village" & meta_lookup$council == first(council)])) %>%
    summarise(population = sum(population), .groups = "drop") %>% 
    mutate(geo = "council") %>% 
    rename(name = council),
  
  # ETC (North Tarawa)
  pop_census_raw %>% 
    filter(geo == "island", name == "north tarawa") %>%
    mutate(name = "etc", geo = "council") %>%
    select(year, name, sex, population, geo),
  
  # C. OI_ST (Sum of Islands)
  pop_census_raw %>% 
    filter(geo == "island") %>%
    group_by(year, sex, OI_ST) %>%
    filter(is_sex_complete(name, meta_lookup$name[meta_lookup$geo == "island" & meta_lookup$OI_ST == first(OI_ST)])) %>%
    summarise(population = sum(population), .groups = "drop") %>% 
    mutate(geo = "oi_st") %>% 
    rename(name = OI_ST)
) %>% 
  mutate(data_type = "census") %>%
  left_join(meta_lookup, by = c("geo", "name"))

# Combine raw data with our computed census totals
pop_census_full <- bind_rows(
  pop_census_raw %>% mutate(data_type = "census"), 
  census_aggregates
)



# Estimation of interpolated population numbers ----------------------------------

# Get growth rates from census data

pop_rates <- pop_census_full %>%
  arrange(geo, name, sex, year) %>%
  group_by(geo, name, sex) %>%
  mutate(
    t_start = lag(year), 
    P_start = lag(population)) %>%
  filter(!is.na(t_start), P_start > 0) %>%
  
  # Formuala to derive rates
  mutate(
    r = log(population / P_start) / (year - t_start)
    ) %>%
  
  select(geo, name, sex, t_start, t_end = year, P_start, P_end = population, r) %>%
  ungroup()

# Create expanded grid for each year, geo, sex grouping
# Populate grid with census data, fill the gaps with estimates

annual_est <- expand_grid(
  meta_lookup %>% select(geo, name),
  sex = c("male", "female", "total"), 
  year = 1990:2025
) %>%
  
  # Join the census counts including aggregated counts
  left_join(
    pop_census_full %>% select(year, geo, name, sex, population), 
    by = c("year", "geo", "name", "sex")) %>%
  
  # Assign the correct Rate (r) to each year
  # Join all available rates for that geo/sex, then filter for the right one.
  left_join(
    pop_rates, 
    by = c("geo", "name", "sex"),
    relationship = "many-to-many") %>%
  
  # Filter to the correct rate for each year
  group_by(year, geo, name, sex) %>%
  
  # Use the rate where the interval start is <= the current year.
  # For years before the first census, we take the earliest available rate.
  filter(t_start <= year | t_start == min(t_start)) %>%
  filter(t_start == max(t_start)) %>% 
  ungroup() %>%
  
  # Fill the census values to use as anchors
  group_by(geo, name, sex) %>% 
  arrange(year) %>%
  mutate(anchor_year = if_else(!is.na(population), year, NA_integer_)) %>%
  
  # fill(down) identifies the most recent census. fill(up) handles pre-census years
  fill(anchor_year, population, .direction = "downup") %>%
  rename(pop_at_anchor = population) %>%
  
  # Interpolation
  mutate(
    
    # Exponential interpolation formula: P_t = P_anchor * e^(r * (t - t_anchor))
    # Use if_else() to retain census data if it exists
    pop_numeric = if_else(
      year == anchor_year, 
      pop_at_anchor, 
      pop_at_anchor * exp(r * (year - anchor_year))),
    data_type = if_else(year == anchor_year, "census", "estimate")
  ) %>% 
  
  ungroup() %>% 
  
  # Restore all metadata columns for the reconciliation step
  left_join(meta_lookup, by = c("geo", "name"))




# Reconciliation of estimates across aggregation levels ---------------------------

# Prioritise census data in all instances

# Sum of M and F must = total in every geo/year, etc

# Sum of villages must = South Tarawa population in every year, etc
# Principle is to use census data as the anchor point
# Apply growth to M and F populations separately then sum
# For South Tarawa villages, compare sum of village estimates to South Tarawa estimate
# Multiply the village level estimates by the ratio of these estimates

# Aggregated geos should = sum of parts (M+F in component geos)

# Convert to wide format for sex-disaggregated math
annual_pop_reconciled <- annual_est %>%
  select(year, name, geo, geo_code, division_code, division_name, OI_ST, council, sex, pop_numeric, data_type) %>%
  pivot_wider(names_from = sex, values_from = c(pop_numeric, data_type)) %>%
  group_by(year) %>%
  mutate(
    
    # Preserve the original interpolated values as 'draft'
    draft_pop_male   = pop_numeric_male,
    draft_pop_female = pop_numeric_female,
    draft_pop_total  = pop_numeric_total,
    
    # Pull the south tarawa populations
    anchor_m = sum(pop_numeric_male[geo == "island" & name == "south tarawa"], na.rm = TRUE),
    anchor_f = sum(pop_numeric_female[geo == "island" & name == "south tarawa"], na.rm = TRUE),
    
    # Sum the independent village estimates to find their total modeled weight
    st_v_sum_m = sum(pop_numeric_male[geo == "village" & council %in% c("btc", "tuc")], na.rm = TRUE),
    st_v_sum_f = sum(pop_numeric_female[geo == "village" & council %in% c("btc", "tuc")], na.rm = TRUE),
    
    # New Pop = (village share of sum) * south tarawa population
    pop_numeric_male = if_else(geo == "village" & council %in% c("btc", "tuc"), (pop_numeric_male / st_v_sum_m) * anchor_m, pop_numeric_male),
    pop_numeric_female = if_else(geo == "village" & council %in% c("btc", "tuc"), (pop_numeric_female / st_v_sum_f) * anchor_f, pop_numeric_female),
    
    # Bottom up reconciliation
    pop_numeric_male = case_when(
      geo == "council" & name == "btc" ~ sum(pop_numeric_male[geo == "village" & council == "btc"], na.rm = TRUE),
      geo == "council" & name == "tuc" ~ sum(pop_numeric_male[geo == "village" & council == "tuc"], na.rm = TRUE),
      geo == "council" & name == "etc" ~ sum(pop_numeric_male[geo == "island" & name == "north tarawa"], na.rm = TRUE),
      geo == "oi_st" & name == "st" ~ sum(pop_numeric_male[geo == "island" & name == "south tarawa"], na.rm = TRUE),
      geo == "oi_st" & name == "oi" ~ sum(pop_numeric_male[geo == "island" & name != "south tarawa"], na.rm = TRUE),
      geo == "national" ~ sum(pop_numeric_male[geo == "island"], na.rm = TRUE),
      TRUE ~ pop_numeric_male),
    pop_numeric_female = case_when(
      geo == "council" & name == "btc" ~ sum(pop_numeric_female[geo == "village" & council == "btc"], na.rm = TRUE),
      geo == "council" & name == "tuc" ~ sum(pop_numeric_female[geo == "village" & council == "tuc"], na.rm = TRUE),
      geo == "council" & name == "etc" ~ sum(pop_numeric_female[geo == "island" & name == "north tarawa"], na.rm = TRUE),
      geo == "oi_st" & name == "st" ~ sum(pop_numeric_female[geo == "island" & name == "south tarawa"], na.rm = TRUE),
      geo == "oi_st" & name == "oi" ~ sum(pop_numeric_female[geo == "island" & name != "south tarawa"], na.rm = TRUE),
      geo == "national" ~ sum(pop_numeric_female[geo == "island"], na.rm = TRUE),
      TRUE ~ pop_numeric_female),
    pop_numeric_male = if_else(geo == "division",
                               map_dbl(name, ~sum(pop_numeric_male[geo == "island" & division_name == .x], na.rm = TRUE)),
                               pop_numeric_male),
    
    pop_numeric_female = if_else(geo == "division",
                                 map_dbl(name, ~sum(pop_numeric_female[geo == "island" & division_name == .x], na.rm = TRUE)),
                                 pop_numeric_female),
    
    # Totals reconciliation
    # Use the census total if available; otherwise use the sum of estimates
    target_total = if_else(data_type_total == "census", draft_pop_total, pop_numeric_male + pop_numeric_female),
    
    # Calculate the sex ratio from our reconciled estimates
    sex_ratio_m = if_else(pop_numeric_male + pop_numeric_female > 0, 
                          pop_numeric_male / (pop_numeric_male + pop_numeric_female), 
                          0.5),
    
    # Finalise counts - Male = Total * Ratio; Female = Total - Male (ensures perfect sum)
    population_male = round(target_total * sex_ratio_m),
    population_female = round(target_total) - population_male,
    population_total = round(target_total),
    
    # Determine Data Type for Total
    data_type_total = if_else(
      (data_type_male == "census" & data_type_female == "census") | 
        (data_type_total == "census"), 
      "census", 
      "estimate"
    ),
    
    # Calculate drift per row, comparing the reconciled total with the draft direct interpolation
    drift_total = population_total - round(draft_pop_total)
  ) %>%
  
  ungroup()





# Output ---------------------------------------------------------------

# Save drift log for review
drift_log <- annual_pop_reconciled %>%
  filter(abs(drift_total) > 1) %>%
  select(year, geo, name, draft_pop_total, population_total, drift_total)

if(nrow(drift_log) > 0) write_csv(drift_log, here("data-processed", "audit_reconciliation_drift.csv"))

geo_order <- c("national", "division", "oi_st", "island", "council", "village")

# Prepare long format df for output
annual_pop_final <- annual_pop_reconciled %>%
  select(year, name, geo, 
         population_male, population_female, population_total,
         data_type_male, data_type_female, data_type_total) %>%
  pivot_longer(cols = -c(year, geo, name), 
               names_to = c(".value", "sex"), 
               names_pattern = "(.*)_(.*)") %>%
  left_join(meta_lookup, by = c("geo", "name")) %>% 
  arrange(
    match(geo, geo_order), 
    suppressWarnings(as.numeric(geo_code)), 
    name, year, sex
  )

qsave(annual_pop_final, here("data-processed", "annual_population_estimates.qs"))
write_csv(annual_pop_final, here("data-processed", "annual_population_estimates.csv"))

# Function to save wide format CSVs
save_wide <- function(df, sex_filter, prefix) {
  df %>% 
    filter(sex == sex_filter) %>% 
    select(geo, any_of("geo_code"), name, any_of("division_code"), year, population) %>%
    pivot_wider(names_from = year, values_from = population) %>%
    arrange(
      match(geo, geo_order), 
      suppressWarnings(as.numeric(geo_code)), 
      name
    ) %>%
    write_csv(here("data-processed", paste0(prefix, "_wide_", sex_filter, ".csv")))
}

walk(c("male", "female", "total"), ~save_wide(pop_census_raw, .x, "census"))
walk(c("male", "female", "total"), ~save_wide(annual_pop_final, .x, "annual"))



