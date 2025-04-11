# Title and Description --------------------------------------------

# Analysis script: TB Case Notification Rates (CNR) by Geography
# Calculates interpolated population denominators and CNRs by Island, Division, ST/OI.
# Author: Jeremy Hill
# Date commenced: 11/4/2025 # Adjust date
# Last modified: 11/4/2025 # Adjust date

# Packages --------------------------------------------------------

library(here)
library(tidyverse)
library(lubridate)
library(ggplot2)  # For plotting
library(scales)   # For plot formatting
# library(zoo) # Might need this later for interpolation if not doing manually

# Load Cleaned TB Data (Check Environment First) -------------

message("Checking for 'tb_register_combined' in environment or loading from file...")

# Define object name we expect from cleaning script
cleaned_data_object_name <- "tb_register_combined" # Keep variable name consistent

# Check if the object already exists and is a data frame
if (exists(cleaned_data_object_name, where = .GlobalEnv) &&
    is.data.frame(get(cleaned_data_object_name, envir = .GlobalEnv))) {
  
  message("-> Found '", cleaned_data_object_name, "' in the current R environment. Using existing object.")
  # Assign to a standard name for this script if needed, or just use it directly
  tb_data <- get(cleaned_data_object_name, envir = .GlobalEnv)
  # Optionally ensure it's a tibble
  tb_data <- as_tibble(tb_data)
  
} else {
  message("-> '", cleaned_data_object_name, "' not found in environment. Loading from latest cleaned RDS file...")
  
  processed_data_dir <- here::here("data-processed")
  # Look for files matching the pattern from the saving step
  rds_files <- list.files(
    processed_data_dir,
    pattern = "^tb_register_cleaned_\\d{6}\\.rds$", # Matches 'tb_register_cleaned_'
    full.names = TRUE
  )
  
  if (length(rds_files) == 0) {
    stop("Error: No 'tb_register_cleaned_YYMMDD.rds' files found in ", processed_data_dir,
         "\nPlease run the cleaning script (02-clean-data.R) first.")
  }
  
  # Extract date strings and find the latest file
  date_strings_rds <- stringr::str_match(basename(rds_files), "_(\\d{6})\\.rds$")[, 2]
  file_dates_rds <- as.Date(date_strings_rds[!is.na(date_strings_rds)], format = "%y%m%d")
  if (length(file_dates_rds) == 0) stop("Error: No valid dates found in cleaned RDS filenames.")
  latest_rds_file <- rds_files[!is.na(date_strings_rds)][which.max(file_dates_rds)]
  
  message("-> Loading file: ", basename(latest_rds_file))
  
  # Load the RDS file
  tb_data <- readRDS(latest_rds_file)
  message("-> Cleaned TB data loaded successfully from file.")
}

# Final check: ensure the object exists and is usable
if (!exists("tb_data") || !is.data.frame(tb_data)) {
  stop("Error: Failed to load or find a valid cleaned TB data frame.")
}

# Glimpse the loaded TB data
# message("\nGlimpse of loaded cleaned TB data:")
# glimpse(tb_data)


# Load Population Census Data -------------------------------------

message("\nLoading population census data...")
pop_census_file <- here::here("data-raw", "pop_island_census.csv") # Use .csv as specified
if (!file.exists(pop_census_file)) {
  stop("Error: Population census file not found at: ", pop_census_file)
}
pop_census_raw <- readr::read_csv(pop_census_file, show_col_types = FALSE) # Use read_csv
message("-> Population data loaded successfully.")


# Prepare Population Data -----------------------------------------

## Tidy Census Data (Wide to Long) ----
message("\nTidying population census data...")

# Identify census year columns (assuming they are 4-digit numeric column names)
census_year_cols <- colnames(pop_census_raw)[str_detect(colnames(pop_census_raw), "^\\d{4}$")]

# Pivot, clean names, filter NAs, convert types
pop_census_long <- pop_census_raw %>%
  tidyr::pivot_longer(
    cols = all_of(census_year_cols),
    names_to = "census_year",
    values_to = "population",
    names_transform = list(census_year = as.integer),
    values_transform = list(population = as.integer) # Assuming integer populations
  ) %>%
  # Clean island name to lowercase for joining later
  dplyr::mutate(island = stringr::str_trim(stringr::str_to_lower(Island))) %>%
  # Keep relevant columns and rename for consistency
  dplyr::select(
    island, # Use cleaned name
    island_code = is_code,
    division = Division,
    division_code = div_code,
    OI_ST = OI_ST, # Renamed from OI_ST
    census_year,
    population
  ) %>%
  # Remove rows where population is NA (e.g., Kanton had gaps)
  dplyr::filter(!is.na(population)) %>%
  # Arrange for easier viewing and interpolation later
  dplyr::arrange(island, census_year)

message("-> Pivoted census data to long format.")
message("-> First few rows of tidy census data:")
print(head(pop_census_long))
# glimpse(pop_census_long)

# Interpolate Annual Population Estimates --------------------------

message("\nCalculating inter-census growth rates and interpolating annual population...")

## Calculate inter-census growth rates (r) and identify intervals ----
# Requires at least two census points per island
pop_intervals <- pop_census_long %>%
  # Ensure data is sorted by island and year
  dplyr::arrange(island, census_year) %>%
  # Group by island to calculate rates independently
  dplyr::group_by(island) %>%
  # Get previous census info (P0 at t0) using lag()
  dplyr::mutate(
    t0 = dplyr::lag(census_year),
    P0 = dplyr::lag(population)
  ) %>%
  # Keep only rows where we have a previous point to calculate from
  # Also filter out periods where pop might be 0 to avoid log(0) or division by zero
  dplyr::filter(!is.na(t0) & !is.na(P0) & P0 > 0 & population > 0) %>%
  # Calculate time difference and exponential growth rate 'r' for the interval [t0, census_year)
  dplyr::mutate(
    time_diff = census_year - t0,
    # r = log(P1/P0) / (t1-t0)
    r = log(population / P0) / time_diff
  ) %>%
  # Select the interval start (t0), end (census_year), base pop (P0), and rate (r)
  dplyr::select(island, t0, t1 = census_year, P0, r) %>%
  dplyr::ungroup()

message("-> Calculated inter-census growth rates and intervals.")
# glimpse(pop_intervals)


## Create complete grid of islands and years ----
# Determine the range of years needed for analysis (from TB data)
min_analysis_year <- min(as.integer(tb_data$notification_year), na.rm = TRUE)
max_analysis_year <- max(as.integer(tb_data$notification_year), na.rm = TRUE)
all_analysis_years <- seq(min_analysis_year, max_analysis_year)

# Get unique islands from the census data
unique_islands <- unique(pop_census_long$island)

# Create the grid
year_island_grid <- tidyr::expand_grid(
  island = unique_islands,
  year = all_analysis_years
)

message("-> Created grid for ", length(unique_islands), " islands from ",
        min_analysis_year, " to ", max_analysis_year, ".")


## Prepare data for interpolation by assigning interval data (t0, P0, r) to each year ----
# Start with the full grid, join actual census pops
pop_interpolation_base <- year_island_grid %>%
  dplyr::left_join(
    pop_census_long %>% select(island, census_year, population),
    by = c("island", "year" = "census_year")
  ) %>%
  # Join the *start* of each interval (t0, P0, r)
  dplyr::left_join(
    pop_intervals %>% select(island, t0, P0, r),
    by = c("island", "year" = "t0") # Join interval info where year is start of interval
  ) %>%
  # Group by island and fill values down/up
  dplyr::group_by(island) %>%
  dplyr::arrange(year) %>%
  # Fill P0 and r down to cover years within intervals
  # Fill the *last known* P0 and r *up* to cover years before the first calculated interval (extrapolation)
  tidyr::fill(P0, .direction = "downup") %>%
  # Fill the rate 'r' down and up for interpolation/extrapolation
  # Use the earliest rate for years before first interval, latest rate for years after last interval
  tidyr::fill(r, .direction = "downup") %>%
  # Determine the correct 't0' (base census year) for each year's calculation
  # This is the most recent census year *less than or equal to* the current year
  # We can achieve this by filling the actual census_year values downwards
  dplyr::mutate(t0 = ifelse(!is.na(population), year, NA_integer_)) %>% # Mark census years
  tidyr::fill(t0, .direction = "down") %>% # Fill census year down
  dplyr::ungroup()

message("-> Assigned interval data (t0, P0, r) to each year for interpolation.")
# glimpse(pop_interpolation_base)


## Interpolate/Extrapolate population ----
# Check if base contains needed columns before proceeding
required_interp_cols <- c("island", "year", "population", "t0", "P0", "r")
if (!all(required_interp_cols %in% colnames(pop_interpolation_base))) {
  stop("Error: Missing required columns in pop_interpolation_base for calculation.")
}

annual_population_estimates <- pop_interpolation_base %>%
  # Ensure columns are numeric for calculation
  dplyr::mutate(across(c(year, population, t0, P0, r), as.numeric)) %>%
  dplyr::mutate(
    # Calculate time difference from applicable base census year (t - t0)
    time_since_base_census = year - t0,
    # Calculate interpolated population P(t) = P0 * exp(r * (t - t0))
    # Ensure P0 is not zero or NA, and r is not NA before calculation
    interpolated_pop = dplyr::if_else(
      !is.na(P0) & P0 > 0 & !is.na(r) & !is.na(time_since_base_census),
      P0 * exp(r * time_since_base_census),
      NA_real_ # Assign NA if P0, r or t0 is missing for calculation
    ),
    # Final estimated population: use actual census value if available, otherwise use interpolated
    estimated_population = dplyr::if_else(
      !is.na(population), # If 'year' was a census year
      population,         # Use actual census population
      interpolated_pop    # Otherwise use interpolated value
    ),
    # Handle cases where interpolation failed but census pop was missing (should be rare)
    estimated_population = if_else(is.na(estimated_population) & !is.na(P0), P0, estimated_population), # Fallback to P0? Needs thought.
    # Round to integer
    estimated_population = round(estimated_population)
  ) %>%
  # Keep essential columns
  dplyr::select(island, year, estimated_population) %>%
  # Filter out any potential rows with NA population estimate if they occurred
  dplyr::filter(!is.na(estimated_population))

message("-> Calculated annual interpolated/extrapolated population estimates.")
message("-> Final population estimates data frame ('annual_population_estimates'):")
print(head(annual_population_estimates))
glimpse(annual_population_estimates)


# Aggregate Case Data ----------------------------------

message("\nAggregating annual TB case counts by geography...")

# Check required columns exist in tb_data
# Assuming 'island', 'division', 'OI_ST' were added/cleaned previously
# Assuming 'notification_year' exists and is integer
required_cols_cases <- c("notification_year", "island", "division", "OI_ST")
if (!all(required_cols_cases %in% colnames(tb_data))) {
  stop("Error: Missing required columns in tb_data for case aggregation: ",
       paste(required_cols_cases[!required_cols_cases %in% colnames(tb_data)], collapse=", "))
}

## Aggregate by Island and Year ----
case_counts_island <- tb_data %>%
  # Ensure year is integer just in case
  dplyr::mutate(year = as.integer(notification_year)) %>%
  # Exclude cases with missing year or island
  dplyr::filter(!is.na(year), !is.na(island)) %>%
  # Count cases per island-year
  dplyr::count(island, year, name = "cases") %>%
  dplyr::ungroup()

message("-> Calculated annual case counts per island.")

## Aggregate by Division and Year ----
case_counts_division <- tb_data %>%
  dplyr::mutate(year = as.integer(notification_year)) %>%
  dplyr::filter(!is.na(year), !is.na(division)) %>% # Exclude missing division
  dplyr::count(division, year, name = "cases") %>%
  dplyr::ungroup()

message("-> Calculated annual case counts per division.")

## Aggregate by Location Type (ST/OI) and Year ----
case_counts_OI_ST <- tb_data %>%
  dplyr::mutate(year = as.integer(notification_year)) %>%
  dplyr::filter(!is.na(year), !is.na(OI_ST)) %>% # Exclude missing location type
  dplyr::count(OI_ST, year, name = "cases") %>%
  dplyr::ungroup()

message("-> Calculated annual case counts per location type (ST/OI).")

# Optional: Inspect results
# message("\nCase counts per island (first few rows):")
# print(head(case_counts_island))
# message("\nCase counts per division:")
# print(head(case_counts_division))
# message("\nCase counts per location type:")
# print(head(case_counts_OI_ST))

# Calculate Case Notification Rates (CNR) --------------------------

message("\nCalculating Case Notification Rates (CNR)...")

# --- Prepare Geographic Lookup ---
# Create a simple lookup from island to division, OI_ST etc.
# Needed for aggregating population estimates later.
message("-> Creating island-to-geography lookup...")
island_geo_lookup <- pop_census_long %>%
  # Select distinct combinations of island and other geographic identifiers
  dplyr::distinct(island, island_code, division, division_code, OI_ST)


# --- Island Level CNR ----
message("-> Calculating Island level CNR...")

# Use full_join to keep all island-years from both population estimates and case counts
cnr_island_year <- dplyr::full_join(
  annual_population_estimates, # Contains island, year, estimated_population
  case_counts_island,          # Contains island, year, cases
  by = c("island", "year")
) %>%
  # Replace NA cases with 0 (if an island-year had population but zero reported cases)
  dplyr::mutate(cases = tidyr::replace_na(cases, 0)) %>%
  # Calculate CNR (per 100,000), handle division by zero or NA population
  dplyr::mutate(
    cnr = dplyr::if_else(
      !is.na(estimated_population) & estimated_population > 0, # Check denominator
      (cases / estimated_population) * 100000,
      NA_real_ # Result is NA if population is zero or NA
    )
  ) %>%
  # Join geographic info back for context / later aggregation
  dplyr::left_join(island_geo_lookup, by = "island") %>%
  # Arrange for clarity
  dplyr::arrange(island, year)

message("-> Island CNR calculation complete.")
# Optional check: glimpse(cnr_island_year)


# --- Division Level CNR ----
message("-> Calculating Division level CNR...")

# 1. Aggregate population estimates by division and year
# Use the island-level results which already have population and geo-codes joined
population_division_year <- cnr_island_year %>%
  # Filter out rows where division is NA before grouping
  dplyr::filter(!is.na(division)) %>%
  dplyr::group_by(division, division_code, year) %>% # Group by division code/name and year
  dplyr::summarise(
    # Sum population estimates across all islands in the division for that year
    estimated_population = sum(estimated_population, na.rm = TRUE),
    .groups = "drop" # Ungroup after summarising
  ) %>%
  # Ensure population is not zero before proceeding
  dplyr::filter(estimated_population > 0)

# 2. Join aggregated population with division case counts
cnr_division_year <- dplyr::full_join(
  population_division_year,
  case_counts_division,    # Contains division, year, cases
  by = c("division", "year") # Match on division name and year
) %>%
  # Replace NA cases with 0
  dplyr::mutate(cases = tidyr::replace_na(cases, 0)) %>%
  # Calculate CNR
  dplyr::mutate(
    cnr = dplyr::if_else(
      !is.na(estimated_population) & estimated_population > 0,
      (cases / estimated_population) * 100000,
      NA_real_
    )
  ) %>%
  # Arrange for clarity
  dplyr::arrange(division, year)

message("-> Division CNR calculation complete.")
# Optional check: glimpse(cnr_division_year)


# --- Location (ST/OI) Level CNR ----
message("-> Calculating Location (ST/OI) level CNR...")

# 1. Aggregate population estimates by OI_ST and year
population_location_year <- cnr_island_year %>%
  # Filter out rows where OI_ST is NA before grouping
  dplyr::filter(!is.na(OI_ST)) %>%
  dplyr::group_by(OI_ST, year) %>% # Group by location (ST/OI) and year
  dplyr::summarise(
    estimated_population = sum(estimated_population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::filter(estimated_population > 0)

# 2. Join aggregated population with location case counts
cnr_oi_st_year <- dplyr::full_join(
  population_location_year,
  case_counts_OI_ST,     # Contains OI_ST, year, cases
  by = c("OI_ST", "year")
) %>%
  # Replace NA cases with 0
  dplyr::mutate(cases = tidyr::replace_na(cases, 0)) %>%
  # Calculate CNR
  dplyr::mutate(
    cnr = dplyr::if_else(
      !is.na(estimated_population) & estimated_population > 0,
      (cases / estimated_population) * 100000,
      NA_real_
    )
  ) %>%
  # Arrange for clarity
  dplyr::arrange(OI_ST, year)

message("-> Location (ST/OI) CNR calculation complete.")
# Optional check: glimpse(cnr_oi_st_year)

message("\nFinished calculating CNRs at Island, Division, and Location (ST/OI) levels.")


# Create CNR Plots -----------------------------------------------

message("\nGenerating CNR plots (excluding current incomplete year)...")

# Define the current year to exclude dynamically
current_year_num <- lubridate::year(Sys.Date()) # Get current year from system time
# Or hardcode if preferred: current_year_num <- 2025

# Ensure the CNR data frames exist
if (!exists("cnr_island_year") || !exists("cnr_division_year") || !exists("cnr_oi_st_year")) {
  stop("Error: CNR data frames not found. Please run the calculation steps first.")
}

## Plot 1: CNR by Island ----
plot_cnr_island <- ggplot(
  data = cnr_island_year %>%
    dplyr::filter(island != "kanton", !is.na(cnr),
                  year < current_year_num), # <-- Exclude current year
  mapping = aes(x = year, y = cnr, color = island, group = island)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    title = "TB Case Notification Rate by Island (excluding Kanton)",
    subtitle = paste("Years up to", current_year_num - 1), # Indicate excluded year
    x = "Year",
    y = "CNR (per 100,000 population)",
    color = "Island"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

message("-> Created Island CNR plot object (Kanton & current year excluded).")


## Plot 2: CNR by Division ----
plot_cnr_division <- ggplot(
  data = cnr_division_year %>%
    dplyr::filter(!is.na(cnr),
                  year < current_year_num), # <-- Exclude current year
  mapping = aes(x = year, y = cnr, color = division, group = division)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    title = "TB Case Notification Rate by Division",
    subtitle = paste("Years up to", current_year_num - 1),
    x = "Year",
    y = "CNR (per 100,000 population)",
    color = "Division"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

message("-> Created Division CNR plot object (current year excluded).")


## Plot 3: CNR by Location (ST/OI) ----
plot_cnr_oi_st <- ggplot(
  data = cnr_oi_st_year %>%
    dplyr::filter(!is.na(cnr),
                  year < current_year_num), # <-- Exclude current year
  mapping = aes(x = year, y = cnr, color = OI_ST, group = OI_ST)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    title = "TB Case Notification Rate by Location (South Tarawa vs Outer Islands)",
    subtitle = paste("Years up to", current_year_num - 1),
    x = "Year",
    y = "CNR (per 100,000 population)",
    color = "Location" # Legend title
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

message("-> Created Location (ST/OI) CNR plot object (current year excluded).")


# Display the plots ---------------------------------------------
# (Display code remains the same)
message("\nDisplaying plots (excluding current year)...")
print(plot_cnr_island)
print(plot_cnr_division)
print(plot_cnr_oi_st)


# Display the plots ---------------------------------------------

message("\nDisplaying plots...")
print(plot_cnr_island)
print(plot_cnr_division)
print(plot_cnr_oi_st)

# Optional: Save the plots
# ggsave(here::here("outputs", "cnr_by_island.png"), plot = plot_cnr_island, width = 10, height = 8)
# ggsave(here::here("outputs", "cnr_by_division.png"), plot = plot_cnr_division, width = 8, height = 6)
# ggsave(here::here("outputs", "cnr_by_oi_st.png"), plot = plot_cnr_oi_st, width = 8, height = 6)
# message("\nPlots saved to 'outputs' folder.")