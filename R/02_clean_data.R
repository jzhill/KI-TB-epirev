# Title and Description --------------------------------------------

# Cleaning NTP register processed data
# Data are owned by Kiribati MHMS
# Author: Jeremy Hill
# Date commenced: 10/4/2025
# Last modified: 10/4/2025 # <<< Updated: Applied final formatting

# Packages --------------------------------------------------------

# Ensure packages installed via renv::install() / renv::restore()
library(here)
library(tidyverse) # includes readr, dplyr, stringr, etc.
library(lubridate)
library(skimr)
library(epikit)

# Load processed data -------------

message("Checking for 'tb_register_combined' in environment or loading from file...")

# Check if the object already exists in the global environment
if (exists("tb_register_combined", where = .GlobalEnv) &&
    is.data.frame(get("tb_register_combined", envir = .GlobalEnv))) {
  
  message("-> Found 'tb_register_combined' in the current R environment. Using existing object.")
  # Optional: Ensure it's a tibble if preferred downstream
  # tb_register_combined <- as_tibble(tb_register_combined)
  
} else {
  message("-> 'tb_register_combined' not found in environment or is not a data frame. Loading from latest RDS file...")
  
  processed_data_dir <- here::here("data-processed")
  rds_files <- list.files(
    processed_data_dir,
    pattern = "^tb_register_combined_\\d{6}\\.rds$", # Matches 'tb_register_combined_' followed by 6 digits '.rds'
    full.names = TRUE
  )
  
  if (length(rds_files) == 0) {
    stop("Error: No 'tb_register_combined_YYMMDD.rds' files found in ", processed_data_dir,
         "\nPlease run the loading script (01_load_data.R) first.")
  }
  
  # Extract date strings and find the latest file
  date_strings <- stringr::str_match(basename(rds_files), "_(\\d{6})\\.rds$")[, 2]
  file_dates <- as.Date(date_strings, format = "%y%m%d") # Handle potential NAs silently
  latest_index <- which.max(file_dates)
  
  if (length(latest_index) == 0) {
    stop("Error: Could not determine the latest RDS file based on date stamps.")
  }
  
  latest_rds_file <- rds_files[latest_index]
  message("-> Loading file: ", basename(latest_rds_file))
  
  # Load the RDS file into the global environment
  tb_register_combined <- readRDS(latest_rds_file)
  message("-> Data loaded successfully from file.")
}

# Final check: ensure the object exists and is usable
if (!exists("tb_register_combined") || !is.data.frame(tb_register_combined)) {
  stop("Error: Failed to load or find a valid 'tb_register_combined' data frame.")
}

skim(tb_register_combined)

# Convert date columns ------------------------------------------

message("\nConverting core date columns ('date_registered', 'date_started')...")

## Helper function to parse dates stored as Excel numbers (text) or date strings ----
parse_mixed_date <- function(date_vector) {
  numeric_dates <- suppressWarnings(as.numeric(as.character(date_vector)))
  excel_origin <- "1899-12-30" # Standard Excel origin
  is_excel_num <- !is.na(numeric_dates) & numeric_dates > 30000
  
  parsed_text_dates <- lubridate::parse_date_time(
    dplyr::if_else(is_excel_num, NA_character_, as.character(date_vector)),
    orders = c("dmy", "mdy", "ymd"),
    quiet = TRUE
  )
  
  final_dates <- dplyr::if_else(
    is_excel_num,
    as.Date(numeric_dates, origin = excel_origin),
    as.Date(parsed_text_dates)
  )
  
  # Optional: Report if some non-NA values failed conversion
  failed_indices <- which(!is.na(date_vector) & is.na(final_dates))
  if (length(failed_indices) > 0) {
    failed_vals <- unique(date_vector[failed_indices])
    message("  -> NOTE: Failed to parse some non-NA date values: ",
            paste(utils::head(failed_vals, 5), collapse=", "),
            if(length(failed_vals) > 5) "..." else "")
  }
  return(final_dates)
}

## Specify which columns and convert to date using helper function ----
# Define which date columns to convert
date_cols_to_convert <- c("date_registered",
                          "date_started")
present_date_cols <- date_cols_to_convert[date_cols_to_convert %in% colnames(tb_register_combined)]

# Check if any columns to convert actually exist
if(length(present_date_cols) == 0) {
  message("-> No target date columns found in the data frame. Skipping conversion. Check column names specified.")
} else {
  if(length(present_date_cols) < length(date_cols_to_convert)) {
    missing_cols <- date_cols_to_convert[!date_cols_to_convert %in% present_date_cols]
    message("  -> WARNING: Skipping conversion for missing columns: ", paste(missing_cols, collapse=", "))
  }
  
  # Apply the function to the existing relevant columns
  tb_register_combined <- tb_register_combined %>%
    dplyr::mutate(
      dplyr::across(tidyselect::all_of(present_date_cols), parse_mixed_date)
    )
  message("-> Finished attempting date conversion for: ", paste(present_date_cols, collapse=", "))
}

## Inspect the results ----

message("\nChecking types of converted date columns:")
# Use any_of to avoid errors if a column was missing and not converted
print(glimpse(dplyr::select(tb_register_combined, tidyselect::any_of(date_cols_to_convert))))

na_counts <- tb_register_combined %>%
  dplyr::summarise(
    na_date_registered = sum(is.na(date_registered)),
    na_date_started = sum(is.na(date_started))
    # Add other converted date columns here if needed
  )

print(na_counts)

# Clean address column -------------------------------------------

# Apply trimming and lowercasing
tb_register_combined <- tb_register_combined %>%
  dplyr::mutate(address_clean = str_trim(str_to_lower(address))) %>%
  dplyr::relocate(address_clean, .after = address) %>%
  dplyr::relocate(address_desc_2024, .after = address_clean) %>%
  dplyr::relocate(school_2024, .after = address_desc_2024)

message("-> Whitespace trimmed and address converted to lowercase.")

# Output helper file of new unique addresses in csv ------------------------------------

## Load Existing Lookup Table ----
lookup_file <- here::here("data-raw", "TBreg_address_lookup_table.csv")

if (!file.exists(lookup_file)) {
  stop("Error: Address lookup file not found at: ", lookup_file,
       "\nPlease ensure 'TBreg_address_lookup_table.csv' is in the 'data-raw' directory.")
}

address_lookup_table <- readr::read_csv(lookup_file, show_col_types = FALSE)

address_lookup_table <- address_lookup_table %>%
  dplyr::mutate(address_clean = str_trim(str_to_lower(address))) %>%
  dplyr::filter(!is.na(address_clean)) %>%
  dplyr::distinct() %>% 
  dplyr::select(`address_clean`, `island_coded`, `ST_village_coded`) 

message("-> Found ", nrow(address_lookup_table), " entries in existing lookup table.")

## Get current unique addresses from data ----
current_unique_addresses <- tb_register_combined %>%
  dplyr::select(address_clean) %>%
  dplyr::filter(!is.na(address_clean)) %>%
  dplyr::distinct()

message("-> Found ", nrow(current_unique_addresses), " unique non-NA addresses in current data.")

## Identify new addresses (not in lookup) ----
# Use anti_join: keeps rows from 'current_unique_addresses' that
# DO NOT have a match in 'address_lookup_table' based on the 'address_clean' column.
new_addresses_df <- dplyr::anti_join(
  current_unique_addresses,
  address_lookup_table,
  by = "address_clean"
) %>%
  dplyr::arrange(address_clean)

## Process and save new addresses (if any found) ----
if (nrow(new_addresses_df) == 0) {
  message("\n-> No new unique addresses found compared to the lookup table. No output file generated.")
} else {
  
  message("-> Found ", nrow(new_addresses_df), " new unique address(es) requiring review.")
  
  output_dir <- here::here("data-processed")
  output_path <- file.path(output_dir, "new_unique_addresses_for_lookup.csv")
  
  readr::write_csv(new_addresses_df, output_path)
  message("   -> Successfully saved new unique addresses.")
}

# Add Island and Village Codes via Lookup Table ---------------------

tb_register_combined <- dplyr::left_join(
  tb_register_combined,
  address_lookup_table,
  by = "address_clean"
) %>%
  dplyr::relocate(island_coded, .after = school_2024) %>% 
  dplyr::relocate(ST_village_coded, .after = island_coded)

message("-> Join complete. Added island_coded and ST_village_coded columns.")

# Clean Age Column ------------------------------------------------

## Apply cleaning rules using mutate and case_when ----
tb_register_combined <- tb_register_combined %>%
  dplyr::mutate(
    # Clean age input first (trim whitespace)
    age_trimmed = stringr::str_trim(as.character(age)),
    
    # Create age_clean column based on patterns in the trimmed age column
    age_clean = dplyr::case_when(
      # Handle NA inputs first
      is.na(age_trimmed) | age_trimmed == "" ~ NA_real_,
      
      # Rule 1: Already a whole number (integer years)
      stringr::str_detect(age_trimmed, "^\\d+$")
      ~ as.numeric(age_trimmed),
      
      # Rule 2: Weeks format (e.g., "3/52", "10 / 52") -> always round up to 1 year
      stringr::str_detect(age_trimmed, "^\\d+\\s*/\\s*52$")
      ~ 1.0,
      
      # Rule 3: Months format X/12 (e.g., "10/12", "18 / 12") -> round years UP
      stringr::str_detect(age_trimmed, "^\\d+\\s*/\\s*12$")
      ~ ceiling(as.numeric(stringr::str_extract(age_trimmed, "^\\d+")) / 12),
      
      # Rule 4: Months format (e.g., "9m", "10 mth", "6 mnths") -> round years UP
      stringr::str_detect(age_trimmed, "^\\d+\\s*m")
      ~ ceiling(as.numeric(stringr::str_extract(age_trimmed, "^\\d+")) / 12),
      
      # Rule 5: Handle "X+" format (e.g., "70+", "1+") -> return the value before the +
      stringr::str_detect(age_trimmed, "^\\d+\\+$")
      ~ as.numeric(stringr::str_extract(age_trimmed, "^\\d+")),
      
      # Default: If none of the above patterns match, result is NA (numeric NA)
      TRUE ~ NA_real_
    ),
    # Remove temporary trimmed column
    age_trimmed = NULL
  ) %>%
  # Optional: Move age_clean next to the original age column
  dplyr::relocate(age_clean, .after = age)

message("-> Created 'age_clean' column (handling X+ format).") # Updated message

## Inspect the results ---------------------------------------------
message("\nSummary of original 'age' and new 'age_clean':")

# Show summary statistics for the new numeric age column
print(summary(tb_register_combined$age_clean))

# Check how many NAs are in the new column
na_age_clean <- sum(is.na(tb_register_combined$age_clean))
original_na_age <- sum(is.na(tb_register_combined$age))
message("-> 'age_clean' contains ", na_age_clean, " NA values (original 'age' column had ", original_na_age, ").")

# View rows where age_clean is NA but original age was not
unparsed_ages <- tb_register_combined %>%
  dplyr::filter(is.na(age_clean) & !is.na(age)) %>%
  dplyr::count(age, sort = TRUE, name = "count_unparsed")

if(nrow(unparsed_ages) > 0) {
  message("-> Found ", sum(unparsed_ages$count_unparsed), " records (", nrow(unparsed_ages), " unique values) where non-NA 'age' resulted in NA 'age_clean':")
  print(head(unparsed_ages))
} else {
  message("-> All non-NA 'age' values appear to have been parsed successfully.")
}

# Create age group columns using epikit --------------------------

# Determine max age for setting upper limit for 10-year bins
max_age <- max(tb_register_combined$age_clean, na.rm = TRUE)

# Set upper limit for explicit 10-year bins (e.g., if max age is 89, upper is 80)
upper_limit_10yr <- floor(max_age / 10) * 10

# Define the specific breaks for WHO categories
age_breaks_who <- c(0, 5, 15, 25, 35, 45, 55, 65) # Lower bounds

# Add the age group columns using mutate and epikit::age_categories
tb_register_combined <- tb_register_combined %>%
  dplyr::mutate(
    
    # Standard 10-year bins using lower, upper, by
    # This creates groups like 0-9, 10-19, ..., up to upper_limit-upper_limit+9,
    # and then an upper_limit+ category
    age_group_10yr = epikit::age_categories(
      age_clean,
      lower = 0,
      upper = upper_limit_10yr, # Define upper boundary for explicit groups
      by = 10
    ),
    
    # WHO TB Programme standard bins using specific breakers
    # This creates groups 0-4, 5-14, ..., 55-64, and 65+
    age_group_who = epikit::age_categories(
      age_clean,
      breakers = age_breaks_who  # Default separator is "-", ceiling=FALSE gives "65+" for the last group
    )
  ) %>%
  dplyr::relocate(age_group_10yr, .after = age_clean) %>%
  dplyr::relocate(age_group_who, .after = age_group_10yr)

message("-> Created 'age_group_10yr' and 'age_group_who' columns using epikit.")

# Inspect the results ---------------------------------------------
message("\nCounts for epikit 10-year age groups:")
print(tb_register_combined %>% dplyr::count(age_group_10yr, .drop = FALSE))

message("\nCounts for epikit WHO age groups:")
print(tb_register_combined %>% dplyr::count(age_group_who, .drop = FALSE))

# Clean sex column ------------------------------------------------

# Use mutate with case_match for standardization into a new column
tb_register_combined <- tb_register_combined %>%
  dplyr::mutate(
    
    # Create intermediate standardized character column first
    # This avoids issues if factor conversion happened before case_match
    sex_standardized_char = dplyr::case_match(
      stringr::str_to_upper(sex), # Compare uppercase versions
      "M"  ~ "Male",             # Map "M" or "m" to "Male"
      "MN" ~ "Male",             # Map "MN" typo to "Male"
      "F"  ~ "Female",           # Map "F" or "f" to "Female"
      
      # All other values (original NA, any others) become NA_character_
      .default = NA_character_
    ),
    
    # Create the final 'sex_clean' column as a Factor from the standardized char
    sex_clean = factor(sex_standardized_char, levels = c("Male", "Female")),
    
    # Remove the intermediate character column - no longer needed
    sex_standardized_char = NULL
  ) %>%
  dplyr::relocate(sex_clean, .after = sex)

message("-> Created 'sex_clean' column (Factor: Male/Female/NA).")

# Inspect the results ---------------------------------------------
message("\nCounts of values in new 'sex_clean' column:")
# Use .drop = FALSE in count to include a count for NA values
print(tb_register_combined %>% dplyr::count(sex_clean, .drop = FALSE))