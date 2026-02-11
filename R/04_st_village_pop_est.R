# Title and Description --------------------------------------------
# Build annual South Tarawa village population estimates (interpolate/extrapolate)
# Input:  data-raw/pop_st_village_census.csv
# Output: data-processed/annual_pop_st_village_est.qs
# Author: Jeremy Hill (via Assistant)
# Last modified: 2026-01-31

# Packages --------------------------------------------------------

library(here)
library(tidyverse)
library(lubridate)
library(qs)

# Source ---------------

source(here("R", "functions_all.R"))



# Parameters ------------------------------------------------------

end_year_default <- year(Sys.Date()) # extrapolate through current calendar year

# Load census data ------------------------------------------------
pop_path <- here("data-raw", "pop_st_village_census.csv")
if (!file.exists(pop_path)) stop("ST village population census file not found at: ", pop_path)

pop_raw <- read_csv(pop_path, show_col_types = FALSE)

# Load geo helper data --------------

geo_meta <- get_official_geo_metadata()

# Validate expected structure -------------------------------------

required_meta <- c("island", "village", "village_code", "BTC_TUC")
missing_meta <- setdiff(required_meta, names(pop_raw))
if (length(missing_meta) > 0) stop("Missing expected columns in pop file: ", paste(missing_meta, collapse = ", "))

year_cols <- setdiff(names(pop_raw), required_meta)
if (!all(str_detect(year_cols, "^\\d{4}$"))) stop("Non-year columns detected outside metadata: ", paste(year_cols[!str_detect(year_cols, '^\\d{4}$')], collapse = ", "))

# Validate geo values -----------------
# Ensure villages in the pop file match official South Tarawa village names


validate_geo_values(pop_raw$village, geo_meta$st_village_names, "village")
validate_geo_values(pop_raw$village_code, geo_meta$st_village_codes, "village_code")

# Check BTC_TUC specifically (must be strictly "BTC" or "TUC")
invalid_btc_tuc <- pop_raw %>%
  mutate(check = str_trim(str_to_upper(BTC_TUC))) %>%
  filter(!check %in% c("BTC", "TUC")) %>%
  pull(BTC_TUC) %>%
  unique()

if (length(invalid_btc_tuc) > 0) {
  stop("! Invalid values in 'BTC_TUC' (must be 'BTC' or 'TUC'): ", paste(invalid_btc_tuc, collapse = ", "))
}

message("Success: ST village population file geography validated.")




# Tidy census data (wide -> long) ---------------------------------

pop_long <- pop_raw %>%
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "census_year",
    values_to = "population",
    names_transform = list(census_year = as.integer),
    values_transform = list(population = as.numeric)
  ) %>%
  filter(!is.na(population)) %>%
  arrange(village, census_year)

# Calculate inter-census exponential growth rates ------------------
# Based on the formula: $$P_t = P_0 e^{rt}$$


pop_intervals <- pop_long %>%
  group_by(village) %>%
  arrange(census_year) %>%
  mutate(
    t0 = lag(census_year),
    P0 = lag(population)
  ) %>%
  filter(!is.na(t0), !is.na(P0), P0 > 0, population > 0) %>%
  mutate(
    r = log(population / P0) / (census_year - t0)
  ) %>%
  select(village, t0, t1 = census_year, P0, r) %>%
  ungroup()

# Build annual grid (village x year) --------------------------------

start_year <- 1998
end_year <- max(end_year_default, max(pop_long$census_year, na.rm = TRUE))
years <- seq(start_year, end_year)

grid <- expand_grid(
  village = unique(pop_long$village),
  year = years
)

# Assign interval parameters and interpolate/extrapolate -----------

interp_base <- grid %>%
  left_join(
    pop_long %>% select(village, census_year, population),
    by = c("village", "year" = "census_year")
  ) %>%
  left_join(
    pop_intervals %>% select(village, t0, P0, r),
    by = c("village", "year" = "t0")
  ) %>%
  group_by(village) %>%
  arrange(year) %>%
  fill(P0, r, .direction = "downup") %>%
  mutate(
    t0_anchor = if_else(!is.na(population), year, NA_integer_)
  ) %>%
  fill(t0_anchor, .direction = "downup") %>%
  ungroup()

annual_pop_st_village_est <- interp_base %>%
  mutate(
    time_since_base = year - t0_anchor,
    interpolated_pop = if_else(
      !is.na(P0) & !is.na(r) & !is.na(time_since_base),
      P0 * exp(r * time_since_base),
      NA_real_
    ),
    estimated_population = if_else(!is.na(population), population, interpolated_pop),
    estimated_population = round(estimated_population)
  ) %>%
  filter(!is.na(estimated_population))

# Attach stable geo metadata --------------------------------

village_meta <- pop_raw %>%
  distinct(island, village, village_code, BTC_TUC)

annual_pop_st_village_est <- annual_pop_st_village_est %>%
  left_join(village_meta, by = "village") %>%
  mutate(
    island = factor(island, levels = geo_meta$island_names, labels = str_to_title(geo_meta$island_names)),
    village = factor(village, levels = geo_meta$st_village_names, labels = str_to_title(geo_meta$st_village_names)),
    tarawa_council = factor(BTC_TUC, levels = c("BTC", "TUC"))
  ) %>%
  select(island, tarawa_council, village, village_code, year, estimated_population) %>%
  arrange(tarawa_council, village, year)

# Save outputs -----------------------------------------------------

qsave(annual_pop_st_village_est, here("data-processed", "annual_pop_st_village_est.qs"))
write_csv(annual_pop_st_village_est, here("data-processed", "annual_pop_st_village_est.csv"))

message("Saved annual ST village population estimates")
message("Years covered: ", min(annual_pop_st_village_est$year), " to ", max(annual_pop_st_village_est$year))