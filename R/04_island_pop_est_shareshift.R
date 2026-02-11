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
    distinct(division_code, division_name, oi_st) %>%
    mutate(
      geo = "division", 
      name = division_name, 
      geo_code = division_code,
      island_name = if_else(name == "south tarawa", "south tarawa", NA_character_),
      island_code = if_else(name == "south tarawa", "7", NA_character_)
    ),
  
  # oi_st Regions
  tibble(
    geo = "oi_st", 
    name = c("st", "oi"),
    oi_st = name
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
    oi_st         = c("st", "st", "oi")
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
  
  # C. oi_st (Sum of Islands)
  pop_census_raw %>% 
    filter(geo == "island") %>%
    group_by(year, sex, oi_st) %>%
    filter(is_sex_complete(name, meta_lookup$name[meta_lookup$geo == "island" & meta_lookup$oi_st == first(oi_st)])) %>%
    summarise(population = sum(population), .groups = "drop") %>% 
    mutate(geo = "oi_st") %>% 
    rename(name = oi_st)
) %>% 
  mutate(data_type = "census") %>%
  left_join(meta_lookup, by = c("geo", "name"))

# Combine raw data with our computed census totals
pop_census_full <- bind_rows(
  pop_census_raw %>% mutate(data_type = "census"), 
  census_aggregates
)



# 1. National Anchor (Global Exponential Regression) ----------------------

# Filter national totals
nat_census_data <- pop_census_full %>% 
  filter(geo == "national", sex == "total") %>%
  select(year, population)

# Fit the model: log(Pop) ~ Year
# This is equivalent to Pop = a * e^(r * year)
nat_model <- lm(log(population) ~ year, data = nat_census_data)

# Extract Statistics for your paper
r_squared <- summary(nat_model)$r.squared
growth_rate <- coef(nat_model)["year"]
intercept <- coef(nat_model)[1]

message(paste0("National Growth Model: R^2 = ", round(r_squared, 4), 
               ", Annual Rate (r) = ", round(growth_rate, 4)))

# Build National Annual Anchor
nat_anchor <- expand_grid(year = 1990:2025) %>%
  mutate(
    # The regression estimate: e^(intercept + r * year)
    nat_total = exp(intercept + growth_rate * year)
  )



## Hierarchical dampened shift-share allocation function ---------------

apply_shift_share <- function(df, col_name, dampen = 0.5, floor = 0.5, normalize = TRUE) {
  
  res <- df %>%
    group_by(geo, name) %>%
    arrange(year) %>%
    mutate(
      # Use .data[[col_name]] to handle the variable column name
      val = .data[[col_name]],
      
      # 1. Dynamic bounds
      t_min = min(year[!is.na(val)], na.rm = TRUE),
      t_max = max(year[!is.na(val)], na.rm = TRUE),
      
      # 2. Linear Interpolation (approx returns a list, we take $y)
      val_interp = approx(year, val, xout = year, rule = 2)$y,
      
      # 3. Long-Term Delta
      delta_long = (val[year == t_max] - val[year == t_min]) / (t_max - t_min)
    ) %>%
    fill(delta_long, .direction = "downup") %>%
    mutate(
      # 4. Extrapolation
      val_final = case_when(
        year < t_min ~ val[year == t_min] + (year - t_min) * delta_long * dampen,
        year > t_max ~ val[year == t_max] + (year - t_max) * delta_long * dampen,
        TRUE ~ val_interp
      ),
      # 5. Safety Floor
      val_final = pmax(val_final, val[year == t_min] * floor)
    )
  
  # 6. Optional Re-normalization (Standard for Shares, disabled for Sex Ratios)
  if (normalize) {
    res <- res %>%
      group_by(year) %>%
      mutate(val_final = val_final / sum(val_final, na.rm = TRUE))
  }
  
  res %>%
    ungroup() %>%
    # Dynamically rename the result column to [col_name]_final
    rename(!!paste0(col_name, "_final") := val_final) %>%
    select(-val, -t_min, -t_max, -val_interp, -delta_long)
}



# 2. Tier 1: Island Share Model (National -> Islands) --------------------

# Calculate Island shares of National total at census points
island_shares <- pop_census_raw %>%
  filter(geo == "island", sex == "total") %>%
  left_join(nat_census_data %>% select(year, nat_pop = population), by = "year") %>%
  mutate(share = population / nat_pop) %>%
  select(geo, name, year, share)

# Interpolate Island Shares
island_annual_shares <- expand_grid(
  meta_lookup %>% filter(geo == "island") %>% select(geo, name), 
  year = 1990:2025
) %>%
  left_join(island_shares, by = c("geo", "name", "year")) %>%
  apply_shift_share(col_name = "share")




# 3. Tier 2: Village Share Model (S. Tarawa Island -> Villages) -----------

# Calculate Village shares of South Tarawa Island total at census points
# We use pop_census_full to ensure we have the aggregated South Tarawa island total
st_total_census <- pop_census_raw %>% 
  filter(geo == "island", name == "south tarawa", sex == "total") %>% 
  select(year, st_pop = population)

village_shares <- pop_census_raw %>%
  filter(geo == "village", sex == "total") %>%
  left_join(st_total_census, by = "year") %>%
  mutate(share = population / st_pop) %>%
  select(geo, name, year, share)

# Interpolate and Extrapolate Village Shares
village_annual_shares <- expand_grid(
  meta_lookup %>% filter(geo == "village", island_name == "south tarawa") %>% select(geo, name), 
  year = 1990:2025
) %>%
  left_join(village_shares, by = c("geo", "name", "year")) %>%
  apply_shift_share(col_name = "share")




# 4. Sex Ratio Model (Male % of Total) ------------------------------------

sex_ratios <- pop_census_full %>%
  filter(sex %in% c("male", "total")) %>%
  select(geo, name, year, sex, population) %>%
  pivot_wider(names_from = sex, values_from = population) %>%
  mutate(male_pct = if_else(total > 0, male / total, NA_real_)) %>%
  select(geo, name, year, male_pct)

# Create a filtered grid: All National/Islands + ONLY South Tarawa Villages
target_meta <- bind_rows(
  meta_lookup %>% filter(geo %in% c("national", "island", "division", "oi_st", "council")),
  meta_lookup %>% filter(geo == "village", island_name == "south tarawa")
)

# Interpolate % Male linearly for every geo/name
annual_sex_ratios <- expand_grid(target_meta %>% select(geo, name), year = 1990:2025) %>%
  left_join(sex_ratios, by = c("geo", "name", "year")) %>%
  apply_shift_share(col_name = "male_pct", floor = 0.8, normalize = FALSE)






# 5. Final Assembly -------------------------------------------------------

# --- A. Construct Base Totals (National, Island, Village) ---

# 1. National
pop_nat <- nat_anchor %>%
  mutate(geo = "national", name = "kiribati") %>%
  rename(population_total = nat_total)

# 2. Islands (National * Island Share, rounded and balanced to national)
pop_islands <- island_annual_shares %>%
  left_join(pop_nat %>% select(year, nat_val = population_total), by = "year") %>%
  mutate(population_total = round(nat_val * share_final)) %>%
  group_by(year) %>%
  mutate(
    diff = first(nat_val) - sum(population_total),
    # Apply rounding difference to South Tarawa
    population_total = if_else(name == "south tarawa", population_total + diff, population_total)
  ) %>%
  ungroup() %>%
  mutate(geo = "island") %>%
  select(year, geo, name, population_total)

# 3. Villages (South Tarawa Total * Village Share, balanced to south tarawa total)
st_total_annual <- pop_islands %>% 
  filter(name == "south tarawa") %>% 
  select(year, st_val = population_total)

pop_villages <- village_annual_shares %>%
  left_join(st_total_annual, by = "year") %>%
  mutate(population_total = round(st_val * share_final)) %>%
  group_by(year) %>%
  mutate(
    diff = first(st_val) - sum(population_total),
    # Apply difference to Betio (the largest village)
    population_total = if_else(name == "betio", population_total + diff, population_total)
  ) %>%
  ungroup() %>%
  mutate(geo = "village") %>%
  select(year, geo, name, population_total)

# Combine national, island, village
annual_nivs <- bind_rows(pop_nat, pop_islands, pop_villages) %>%
  left_join(meta_lookup, by = c("geo", "name"))

# --- B. Roll-up Aggregates (Divisions, Councils, oi_st) ---

# This ensures that higher levels are ALWAYS the sum of their parts
annual_aggregates <- bind_rows(
  
  # Divisions
  annual_nivs %>% 
    filter(geo == "island") %>%
    group_by(year, division_name) %>%
    summarise(population_total = sum(population_total, na.rm = TRUE), .groups = "drop") %>%
    mutate(geo = "division") %>% 
    rename(name = division_name),
  
  # Councils
  bind_rows(
    
    # South Tarawa Councils
    annual_nivs %>% 
      filter(geo == "village", !is.na(council)) %>%
      group_by(year, council) %>% 
      summarise(population_total = sum(population_total, na.rm = TRUE), .groups = "drop") %>%
      mutate(geo = "council") %>% 
      rename(name = council),
    
    # North Tarawa Council (equivalent to the Island total)
    annual_nivs %>% 
      filter(geo == "island", name == "north tarawa") %>%
      mutate(geo = "council", name = "etc") %>% 
      select(year, name, geo, population_total)
  ),
  
  # oi_st Regions
  annual_nivs %>% 
    filter(geo == "island") %>%
    group_by(year, oi_st) %>%
    summarise(population_total = sum(population_total, na.rm = TRUE), .groups = "drop") %>%
    mutate(geo = "oi_st") %>% 
    rename(name = oi_st)
) %>%
  
  # Re-attach the metadata using both keys for safety
  left_join(meta_lookup, by = c("geo", "name"))

# --- C. Apply Sex Ratios & Finalize ---

annual_pop_final <- bind_rows(annual_nivs, annual_aggregates) %>%
  
  # Join with our Sex Ratio model (% Male)
  left_join(annual_sex_ratios %>% select(geo, name, year, male_pct_final), 
            by = c("geo", "name", "year")) %>%
  mutate(
    # Apply ratio to total
    pop_numeric_male   = population_total * male_pct_final,
    
    # Rounding strategy: ensure M + F = Total exactly
    population_male    = round(pop_numeric_male),
    population_total   = round(population_total),
    population_female  = population_total - population_male
  ) %>%
  
  # "balancing person" step to ensure Islands sum to National exactly
  group_by(year) %>%
  mutate(
    # Calculate the national discrepancy for this specific year
    nat_val = population_total[geo == "national"],
    isl_sum = sum(population_total[geo == "island"]),
    diff    = nat_val - isl_sum,
    
    # Apply the difference to South Tarawa (the largest island)
    population_total = if_else(geo == "island" & name == "south tarawa", 
                               population_total + diff, 
                               population_total),
    
    # Re-derive female for South Tarawa to keep gender balance intact
    population_female = if_else(geo == "island" & name == "south tarawa",
                                population_total - population_male,
                                population_female)
  ) %>%
  ungroup() %>%
  
  # Attach data_type flags based on original census presence
  left_join(pop_census_full %>% 
              filter(sex == "total") %>% 
              select(year, geo, name, data_type), 
            by = c("year", "geo", "name")) %>%
  mutate(data_type_total = if_else(!is.na(data_type), "census", "estimate")) %>%
  
  # Pivot to final long format
  select(year, name, geo, population_male, population_female, population_total, data_type_total) %>%
  pivot_longer(cols = starts_with("population_"), 
               names_to = "sex", 
               values_to = "population",
               names_prefix = "population_") %>%
  
  # Re-attach all metadata (geo_code, division_code, etc)
  left_join(meta_lookup, by = c("geo", "name")) %>%
  
  # Final Formatting & Sorting
  arrange(
    match(geo, c("national", "division", "oi_st", "island", "council", "village")), 
    suppressWarnings(as.numeric(geo_code)), 
    name, year, sex
  )






# 6. Output ---------------------------------------------------------------

# Save processed files
qsave(annual_pop_final, here("data-processed", "annual_population_estimates.qs"))
write_csv(annual_pop_final, here("data-processed", "annual_population_estimates.csv"))

# Helper for wide-format exports
save_wide_final <- function(df, sex_filter, prefix) {
  df %>% 
    filter(sex == sex_filter) %>% 
    distinct(geo, name, year, .keep_all = TRUE) %>%
    select(geo, any_of("geo_code"), name, any_of("division_code"), year, population) %>%
    pivot_wider(names_from = year, values_from = population) %>%
    arrange(
      match(geo, c("national", "division", "oi_st", "island", "council", "village")), 
      suppressWarnings(as.numeric(geo_code)), 
      name
    ) %>%
    write_csv(here("data-processed", paste0(prefix, "_wide_", sex_filter, ".csv")))
}

# Run exports
walk(c("male", "female", "total"), ~save_wide_final(annual_pop_final, .x, "annual_est"))

# Export original census comparison files for baseline check
walk(c("male", "female", "total"), ~save_wide_final(pop_census_raw %>% mutate(sex = str_to_lower(sex)), .x, "census"))

message("Success: Hierarchical Shift-Share Model complete and files exported.")