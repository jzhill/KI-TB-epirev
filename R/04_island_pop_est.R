# Title and Description --------------------------------------------
# Build annual island population estimates (interpolate/extrapolate)
# Input:  data-raw/pop_island_census.csv
# Output: data-processed/annual_pop_island_est.qs
# Author: Jeremy Hill
# Last modified: 2026-01-19

# Packages --------------------------------------------------------
library(here)
library(tidyverse)
library(lubridate)
library(qs)

# Parameters ------------------------------------------------------
end_year_default <- year(Sys.Date()) # extrapolate through current calendar year

# Load census data ------------------------------------------------
pop_path <- here("data-raw", "pop_island_census.csv")
if (!file.exists(pop_path)) stop("Population census file not found at: ", pop_path)

pop_raw <- read_csv(pop_path, show_col_types = FALSE)

# Load geo helper data --------------

geo_meta <- get_official_geo_metadata()


# Validate expected structure -------------------------------------

required_meta <- c("island", "island_code", "division", "division_code", "OI_ST")
missing_meta <- setdiff(required_meta, names(pop_raw))
if (length(missing_meta) > 0) stop("Missing expected columns in pop file: ", paste(missing_meta, collapse = ", "))

year_cols <- setdiff(names(pop_raw), required_meta)
if (!all(str_detect(year_cols, "^\\d{4}$"))) stop("Non-year columns detected outside metadata: ", paste(year_cols[!str_detect(year_cols, '^\\d{4}$')], collapse = ", "))

# Validate geo values in population file -----------------
# Expected to be the same official list of names as used for geo classification of reg_combined

validate_geo_values <- function(actual, reference, label) {
  
  invalid <- setdiff(actual, reference)
  
  if (length(invalid) > 0) {
    stop(paste0("! Invalid values in '", label, "': "), paste(invalid, collapse = ", "))
  }
}

# Checking Island names and codes
validate_geo_values(pop_raw$island, geo_meta$island_names, "island")
validate_geo_values(pop_raw$island_code, geo_meta$island_codes, "island_code")

# Checking Division names and codes
validate_geo_values(pop_raw$division, geo_meta$division_names, "division")
validate_geo_values(pop_raw$division_code, geo_meta$division_codes, "division_code")

# Check OI_ST specifically (must be strictly "OI" or "ST")
invalid_oi_st <- pop_raw %>%
  mutate(check = str_trim(str_to_upper(OI_ST))) %>%
  filter(!check %in% c("OI", "ST")) %>%
  pull(OI_ST) %>%
  unique()

if (length(invalid_oi_st) > 0) {
  stop("! Invalid values in 'OI_ST' (must be 'OI' or 'ST'): ", paste(invalid_oi_st, collapse = ", "))
}

message("Success: Raw population file geography validated against official metadata.")




# Tidy census data (wide -> long) ---------------------------------

pop_long <- pop_raw %>%
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "census_year",
    values_to = "population",
    names_transform = list(census_year = as.integer),
    values_transform = list(population = as.numeric)
  ) %>%
  select(
    island,
    island_code,
    division,
    division_code,
    OI_ST,
    census_year,
    population
  ) %>%
  filter(!is.na(population)) %>%
  arrange(island, census_year)

# Calculate inter-census exponential growth rates ------------------
# Based on the formula: $$P_t = P_0 e^{rt}$$
pop_intervals <- pop_long %>%
  group_by(island) %>%
  arrange(census_year) %>%
  mutate(
    t0 = lag(census_year),
    P0 = lag(population)
  ) %>%
  filter(!is.na(t0), !is.na(P0), P0 > 0, population > 0) %>%
  mutate(
    r = log(population / P0) / (census_year - t0)
  ) %>%
  select(island, t0, t1 = census_year, P0, r) %>%
  ungroup()

# Build annual grid (island x year) --------------------------------
start_year <- min(pop_long$census_year, na.rm = TRUE)
end_year <- max(end_year_default, max(pop_long$census_year, na.rm = TRUE))
years <- seq(start_year, end_year)

grid <- expand_grid(
  island = unique(pop_long$island),
  year = years
)

# Assign interval parameters and interpolate/extrapolate -----------
interp_base <- grid %>%
  left_join(
    pop_long %>% select(island, census_year, population),
    by = c("island", "year" = "census_year")
  ) %>%
  left_join(
    pop_intervals %>% select(island, t0, P0, r),
    by = c("island", "year" = "t0")
  ) %>%
  group_by(island) %>%
  arrange(year) %>%
  fill(P0, r, .direction = "downup") %>%
  mutate(
    t0 = if_else(!is.na(population), year, NA_integer_)
  ) %>%
  fill(t0, .direction = "down") %>%
  ungroup()

annual_pop_island_est <- interp_base %>%
  mutate(
    time_since_base = year - t0,
    interpolated_pop = if_else(
      !is.na(P0) & !is.na(r) & !is.na(time_since_base),
      P0 * exp(r * time_since_base),
      NA_real_
    ),
    estimated_population = if_else(!is.na(population), population, interpolated_pop),
    estimated_population = round(estimated_population)
  ) %>%
  filter(!is.na(estimated_population)) %>%
  select(island, year, estimated_population)

# Attach stable geo lookup columns --------------------------------
island_geo_lookup <- pop_long %>%
  distinct(island, island_code, division, division_code, OI_ST)

annual_pop_island_est <- annual_pop_island_est %>%
  select(island, year, estimated_population) %>%
  left_join(island_geo_lookup, by = "island") %>%
  mutate(
    division = factor(division, 
                      levels = geo_meta$division_names, 
                      labels = str_to_title(geo_meta$division_names) %>% 
                        # Deal with Line And Pheonix
                        str_replace(" And ", " and ")),
    island   = factor(island,   
                      levels = geo_meta$island_names, 
                      labels = str_to_title(geo_meta$island_names)),
    OI_ST    = factor(OI_ST,    
                      levels = c("ST", "OI"), 
                      labels = c("South Tarawa", "Outer Islands"))
  ) %>%
  relocate(island_code, division, division_code, OI_ST, .after = island) %>%
  arrange(division, island, year)

# Save outputs -----------------------------------------------------
qsave(annual_pop_island_est, here("data-processed", "annual_pop_island_est.qs"))
write_csv(annual_pop_island_est, here("data-processed", "annual_pop_island_est.csv"))


message("Saved annual population estimates")
message("Years covered: ", min(annual_pop_island_est$year), " to ", max(annual_pop_island_est$year))
message("Islands: ", n_distinct(annual_pop_island_est$island))
