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

# Prepare lookup addresses (select the relevant column, ensure consistent naming for join)
known_addresses <- address_lookup_table %>%
  dplyr::mutate(address_clean = str_trim(str_to_lower(address))) %>%
  dplyr::filter(!is.na(address_clean)) %>%
  dplyr::distinct()

message("-> Found ", nrow(known_addresses), " entries in existing lookup table.")

## Get current unique addresses from data ----
current_unique_addresses <- tb_register_combined %>%
  dplyr::select(address_clean) %>%
  dplyr::filter(!is.na(address_clean)) %>%
  dplyr::distinct()

message("-> Found ", nrow(current_unique_addresses), " unique non-NA addresses in current data.")

## Identify new addresses (not in lookup) ----
# Use anti_join: keeps rows from 'current_unique_addresses' that
# DO NOT have a match in 'known_addresses' based on the 'address' column.
new_addresses_df <- dplyr::anti_join(
  current_unique_addresses,
  known_addresses,
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

# # Add Island and Village Codes via Lookup Table ---------------------
# 
# ## Prepare Lookup Table ----
# # Define expected column names
# addr_col_lookup <- "Address-unique" # The original messy address in the lookup
# island_col_lookup <- "island-coded"     # The island code column
# village_col_lookup <- "ST-village-coded" # The village code column
# required_lookup_cols <- c(addr_col_lookup, island_col_lookup, village_col_lookup)
# 
# # Check if required columns exist in the lookup table
# if (!all(required_lookup_cols %in% colnames(address_lookup_table))) {
#   stop("Error: Lookup table '", basename(lookup_file), "' is missing required columns. ",
#        "Expected: '", paste(required_lookup_cols, collapse="', '"), "'")
# }
# 
# # Clean the address column in the lookup table to match the main data's 'address_clean'
# # Select only needed columns, ensure address_clean is unique
# lookup_prepared <- address_lookup_table %>%
#   dplyr::select(
#     !!sym(addr_col_lookup), # Keep original for reference if needed, or remove
#     !!sym(island_col_lookup),
#     !!sym(village_col_lookup)
#   ) %>%
#   dplyr::mutate(
#     # Create the cleaned address column to join on
#     address_clean = stringr::str_trim(stringr::str_to_lower(.data[[addr_col_lookup]]))
#   ) %>%
#   # Keep only the cleaned address and the coded columns for joining
#   dplyr::select(
#     address_clean,
#     !!sym(island_col_lookup),
#     !!sym(village_col_lookup)
#   ) %>%
#   # Remove rows where the cleaned address is NA
#   dplyr::filter(!is.na(address_clean)) %>%
#   # Ensure address_clean is unique - keep only the first match if duplicates exist
#   dplyr::distinct(address_clean, .keep_all = TRUE)
# 
# n_distinct_lookup <- nrow(lookup_prepared)
# message("-> Prepared lookup table with ", n_distinct_lookup, " unique cleaned addresses.")
# if (n_distinct_lookup < nrow(address_lookup_table)) {
#   message("  -> NOTE: Removed ", nrow(address_lookup_table) - n_distinct_lookup,
#           " rows from lookup due to missing or duplicate cleaned addresses.")
# }
# 
# 
# # --- 3. Prepare Main Data Table ---
# # Check if tb_register_combined exists and has the cleaned address column
# if (!exists("tb_register_combined")) {
#   stop("Error: Data frame 'tb_register_combined' not found.")
# }
# if (!"address_clean" %in% colnames(tb_register_combined)) {
#   stop("Error: Column 'address_clean' not found in tb_register_combined. Please run the previous cleaning step.")
# }
# 
# 
# # --- 4. Perform Left Join ---
# message("-> Joining lookup table to main data based on 'address_clean'...")
# 
# tb_register_combined <- dplyr::left_join(
#   tb_register_combined,
#   lookup_prepared,
#   by = "address_clean" # Join key
# )
# 
# message("-> Join complete. Added '", island_col_lookup, "' and '", village_col_lookup, "' columns.")
# 
# 
# # --- 5. Inspect Results ---
# message("\nChecking results of address lookup join:")
# 
# # Calculate how many rows got a match
# match_rate <- tb_register_combined %>%
#   dplyr::summarise(
#     n_total = dplyr::n(),
#     n_island_na = sum(is.na(.data[[island_col_lookup]])),
#     n_village_na = sum(is.na(.data[[village_col_lookup]]))
#   ) %>%
#   dplyr::mutate(
#     perc_island_matched = scales::percent(1 - (n_island_na / n_total), accuracy = 0.1),
#     perc_village_matched = scales::percent(1 - (n_village_na / n_total), accuracy = 0.1)
#   )
# 
# message("-> Island codes added for ", match_rate$perc_island_matched, " of rows (",
#         match_rate$n_island_na, " missing).")
# message("-> Village codes added for ", match_rate$perc_village_matched, " of rows (",
#         match_rate$n_village_na, " missing).")
# 
# # Glimpse the new columns
# print(glimpse(dplyr::select(tb_register_combined,
#                             address, address_clean,
#                             tidyselect::all_of(c(island_col_lookup, village_col_lookup)))))