# Title: Build Unified Annual Population Estimates (Geo & Sex)
# Input:  data-raw/pop_census_long.csv
# Output: data-processed/annual_population_estimates.qs

library(here)
library(tidyverse)
library(qs)

source(here("R", "functions_all.R"))

# Setup & Metadata -----------------------------------------------------

geo_meta <- get_official_geo_metadata()

pop_raw_path <- here("data-raw", "pop_census_long.csv")

pop_base_df <- read_csv(pop_raw_path, show_col_types = FALSE) %>%
  mutate(across(c(name, geo, sex), ~str_trim(str_to_lower(.))),
         population = as.numeric(population)) %>%
  filter(!is.na(population))

pop_census_raw <- bind_rows(
  # National level
  pop_base_df %>% filter(geo == "national") %>% mutate(geo_code = "0"),
  
  # Island level
  pop_base_df %>% filter(geo == "island") %>%
    left_join(geo_meta$census_island, by = c("name" = "island_name")) %>%
    mutate(geo_code = island_code),
  
  # Village level (South Tarawa only)
  pop_base_df %>% filter(geo == "village") %>%
    left_join(geo_meta$census_village %>% filter(island_name == "south tarawa"), by = c("name" = "village_name")) %>%
    mutate(geo_code = village_code)
)

# Aggregate Census Data ----------------------------------------

# Helper: Returns TRUE only if all expected names for a geo-group are present
is_sex_complete <- function(current_names, expected_names) {
  expected_names <- na.omit(expected_names)
  length(intersect(current_names, expected_names)) == length(unique(expected_names))
}

census_aggregates <- bind_rows(
  # A. Division totals from Islands
  pop_census_raw %>% 
    filter(geo == "island") %>%
    group_by(year, sex, division_name) %>%
    filter(is_sex_complete(name, geo_meta$census_island$island_name[geo_meta$census_island$division_name == first(division_name)])) %>%
    summarise(population = sum(population), geo = "division", .groups = "drop") %>%
    rename(name = division_name),
  
  # B. Council totals from Villages (BTC/TUC)
  pop_census_raw %>% 
    filter(geo == "village", council %in% c("btc", "tuc")) %>%
    group_by(year, sex, council) %>%
    filter(is_sex_complete(name, geo_meta$census_village$village_name[geo_meta$census_village$council == first(council)])) %>%
    summarise(population = sum(population), geo = "council", .groups = "drop") %>%
    rename(name = council),
  
  # C. ETC (North Tarawa) from Island level
  pop_census_raw %>% 
    filter(geo == "island", name == "north tarawa") %>%
    mutate(name = "etc", geo = "council") %>%
    select(year, name, sex, population, geo),
  
  # D. OI_ST Region totals from Islands
  pop_census_raw %>% 
    filter(geo == "island") %>%
    group_by(year, sex, OI_ST) %>%
    filter(is_sex_complete(name, geo_meta$census_island$island_name[geo_meta$census_island$OI_ST == first(OI_ST)])) %>%
    summarise(population = sum(population), geo = "oi_st", .groups = "drop") %>%
    rename(name = OI_ST)
) %>%
  mutate(data_type = "census")

pop_census_full <- bind_rows(pop_census_raw, census_aggregates)

# Estimation of interpolated population numbers ----------------------------------

#Get growth rates from census data

pop_rates <- pop_census_full %>%
  arrange(geo, name, sex, year) %>%
  group_by(geo, name, sex) %>%
  mutate(t0 = lag(year), P0 = lag(population)) %>%
  filter(!is.na(t0), P0 > 0) %>%
  
  # Formuala to derive rates
  mutate(r = log(population / P0) / (year - t0)) %>%
  
  select(geo, name, sex, t_start = t0, P0, r, 
         any_of(c("division_name", "OI_ST", "council", "geo_code"))) %>%
  ungroup()

# Create expanded grid for each year, geo, sex grouping

grid <- expand_grid(
  pop_census_full %>% 
    distinct(geo, name, sex, 
             across(any_of(c("division_name", "OI_ST", "council", "geo_code")))),
  year = seq(1990, 2025)
)

# Populate grid with census data, fill the gaps with estimates

annual_est <- grid %>%
  left_join(pop_census_full %>% select(year, geo, name, sex, population), 
            by = c("geo", "name", "sex", "year")) %>%
  left_join(pop_rates %>% select(geo, name, sex, year = t_start, P0, r), 
            by = c("geo", "name", "sex", "year")) %>%
  group_by(geo, name, sex) %>%
  arrange(year) %>%
  fill(P0, r, .direction = "downup") %>% 
  mutate(anchor_year = if_else(!is.na(population), year, NA_integer_)) %>%
  fill(anchor_year, .direction = "downup") %>%
  mutate(
    
    # Exponential interpolation formula
    # We keep 'pop_numeric' for high precision during pro-rating
    pop_numeric = if_else(!is.na(population), 
                          population, 
                          P0 * exp(r * (year - anchor_year))),
    
    data_type = if_else(!is.na(population), "census", "estimate")
  ) %>%
  ungroup()

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
  select(year, geo, name, council, sex, pop_numeric, data_type) %>%
  pivot_wider(names_from = sex, values_from = c(pop_numeric, data_type)) %>%
  group_by(year) %>%
  mutate(
    
    # Pull the south tarawa populations
    anchor_m = sum(pop_numeric_male[geo == "island" & name == "south tarawa"]),
    anchor_f = sum(pop_numeric_female[geo == "island" & name == "south tarawa"]),
    
    # Sum the independent village estimates to find their total modeled weight
    st_v_sum_m = sum(pop_numeric_male[geo == "village" & council %in% c("btc", "tuc")]),
    st_v_sum_f = sum(pop_numeric_female[geo == "village" & council %in% c("btc", "tuc")]),
    
    # New Pop = (Village share of sum) * south tarawa population
    pop_numeric_male = if_else(geo == "village" & council %in% c("btc", "tuc"),
                               (pop_numeric_male / st_v_sum_m) * anchor_m,
                               pop_numeric_male),
    pop_numeric_female = if_else(geo == "village" & council %in% c("btc", "tuc"),
                                 (pop_numeric_female / st_v_sum_f) * anchor_f,
                                 pop_numeric_female),
    
    # Bottom-Up estimates for each aggregated geo
    
    # 1. Council Reconciliation
    pop_numeric_male = case_when(
      geo == "council" & name == "btc" ~ sum(pop_numeric_male[geo == "village" & council == "btc"]),
      geo == "council" & name == "tuc" ~ sum(pop_numeric_male[geo == "village" & council == "tuc"]),
      geo == "council" & name == "etc" ~ sum(pop_numeric_male[geo == "island" & name == "north tarawa"]),
      TRUE ~ pop_numeric_male
    ),
    pop_numeric_female = case_when(
      geo == "council" & name == "btc" ~ sum(pop_numeric_female[geo == "village" & council == "btc"]),
      geo == "council" & name == "tuc" ~ sum(pop_numeric_female[geo == "village" & council == "tuc"]),
      geo == "council" & name == "etc" ~ sum(pop_numeric_female[geo == "island" & name == "north tarawa"]),
      TRUE ~ pop_numeric_female
    ),
    
    # 2. Island Reconciliation (South Tarawa)
    pop_numeric_male = if_else(geo == "island" & name == "south tarawa",
                               sum(pop_numeric_male[geo == "council" & name %in% c("btc", "tuc")]),
                               pop_numeric_male),
    pop_numeric_female = if_else(geo == "island" & name == "south tarawa",
                                 sum(pop_numeric_female[geo == "council" & name %in% c("btc", "tuc")]),
                                 pop_numeric_female),
    
    # 3. Division Reconciliation
    pop_numeric_male = if_else(geo == "division",
                               sum(pop_numeric_male[geo == "island" & division_name == name]),
                               pop_numeric_male),
    pop_numeric_female = if_else(geo == "division",
                                 sum(pop_numeric_female[geo == "island" & division_name == name]),
                                 pop_numeric_female),
    
    # 4. Region Reconciliation (OI_ST)
    pop_numeric_male = if_else(geo == "oi_st" & name == "st",
                               sum(pop_numeric_male[geo == "island" & name == "south tarawa"]),
                               pop_numeric_male),
    pop_numeric_male = if_else(geo == "oi_st" & name == "oi",
                               sum(pop_numeric_male[geo == "island" & name != "south tarawa"]),
                               pop_numeric_male),
    pop_numeric_female = if_else(geo == "oi_st" & name == "st",
                                 sum(pop_numeric_female[geo == "island" & name == "south tarawa"]),
                                 pop_numeric_female),
    pop_numeric_female = if_else(geo == "oi_st" & name == "oi",
                                 sum(pop_numeric_female[geo == "island" & name != "south tarawa"]),
                                 pop_numeric_female),
    
    # 5. National Reconciliation
    pop_numeric_male = if_else(geo == "national",
                               sum(pop_numeric_male[geo == "island"]),
                               pop_numeric_male),
    pop_numeric_female = if_else(geo == "national",
                                 sum(pop_numeric_female[geo == "island"]),
                                 pop_numeric_female),
    
    # Final cleanup and rounding
    population_male = round(pop_numeric_male),
    population_female = round(pop_numeric_female),
    population_total = population_male + population_female,
    data_type_total = if_else(data_type_male == "census" & data_type_female == "census", "census", "estimate")
  ) %>%
  ungroup()

# Output ---------------------------------------------------------------

# Prepare long format df for output
annual_pop_final <- annual_pop_reconciled %>%
  select(year, geo, name, population_male, population_female, population_total,
         data_type_male, data_type_female, data_type_total) %>%
  pivot_longer(cols = -c(year, geo, name), 
               names_to = c(".value", "sex"), 
               names_pattern = "(.*)_(.*)") %>%
  arrange(geo, name, year, sex)

qsave(annual_pop_final, here("data-processed", "annual_population_estimates.qs"))

# Function to save wide format CSVs
save_wide_csv <- function(data, sex_filter, prefix, folder = "data-processed") {
  data %>%
    filter(sex == sex_filter) %>%
    select(geo, name, year, population) %>%
    # Ensure years are sorted correctly across the top
    pivot_wider(names_from = year, values_from = population) %>%
    # Sort rows by geo level then name for readability
    arrange(match(geo, c("national", "division", "oi_st", "island", "council", "village")), name) %>%
    write_csv(here(folder, paste0(prefix, "_wide_", sex_filter, ".csv")))
}

# Export Census Snapshots (Census years only)
walk(c("male", "female", "total"), ~{
  save_wide_csv(pop_census_raw, .x, "census")
})

# Export Annual Estimates (Full 1990-2025 range, all geographic levels)
walk(c("male", "female", "total"), ~{
  save_wide_csv(annual_pop_final, .x, "annual")
})


