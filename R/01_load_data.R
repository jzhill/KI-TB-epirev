# Title and Description --------------------------------------------

# Loading and preparing NTP register data from raw data folder
# Data are owned by Kiribati MHMS
# Author: Jeremy Hill
# Date commenced: 10/4/2025
# Last modified: 10/4/2025 # <<< Updated: Applied final formatting

# Packages --------------------------------------------------------

# Ensure packages installed via renv::install() / renv::restore()
library(here)
library(readxl)
library(stringr)
library(lubridate)
library(tidyverse) # Includes purrr, dplyr, readr, lubridate
library(sf)

# Find Most Recent Data File --------------------------------------

message("Searching for the most recent TB Register file...")

data_dir <- here::here("data-raw", "register")

all_files <- list.files(
  data_dir,
  pattern = "\\.xlsx$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(all_files) == 0) {
  stop("Error: No .xlsx files found in ", data_dir)
}

# Extract date strings and parse dates
date_strings <- stringr::str_match(basename(all_files), "rec (\\d{6})")[, 2]
file_dates <- rep(as.Date(NA), length(all_files))
valid_indices <- !is.na(date_strings)
if (!any(valid_indices)) {
  stop("Error: No filenames with 'rec YYMMDD' pattern found in ", data_dir)
}
file_dates[valid_indices] <- as.Date(date_strings[valid_indices], format = "%y%m%d")

# Identify the latest file based on date
latest_index_relative <- which.max(file_dates[valid_indices])
original_indices <- which(valid_indices)
latest_file_index_in_all_files <- original_indices[latest_index_relative]
latest_file <- all_files[latest_file_index_in_all_files]

message("-> Using file: ", basename(latest_file))

# Load Column Name Metadata ---------------------------------------

message("Loading column name metadata...")

col_names_file <- here::here("data-raw", "TBreg_column_names.csv")

if (!file.exists(col_names_file)) {
  stop("Error: Column names CSV file not found at: ", col_names_file)
}

col_names_meta <- readr::read_csv(col_names_file, show_col_types = FALSE)

message("-> Metadata loaded from: ", basename(col_names_file))

# Define Processing Parameters ------------------------------------

rows_to_check_for_header <- 10 # How many initial rows to check for start row
empty_rows_threshold <- 5    # Consecutive empty rows in Col1 to signify end of data
max_cols_to_keep <- 80       # Max columns to read/keep per sheet

message(
  "INFO: Max columns: ", max_cols_to_keep,
  ". End row detection threshold: ", empty_rows_threshold, " blanks."
)

# Helper Functions ------------------------------------------------

## Function to find start and end rows of contiguous data block ----
find_data_row_indices <- function(df, rows_to_check = 10, empty_rows_threshold = 5) {
  message("  -> Finding data start and end rows...")
  start_row <- NA; end_row <- NA; n_rows_df <- nrow(df)
  rows_available_for_start_check <- min(n_rows_df, rows_to_check)
  
  # Find Start Row (Using Col 1 and Col 3)
  # message("    -> Checking for pattern: (Col1=Date-like OR Col1=ParsableDate) AND Col3=Positive Integer...") # Commented out
  if (rows_available_for_start_check > 0) {
    if (ncol(df) < 3) {
      message("    -> WARNING: Sheet has < 3 columns.")
      return(NULL)
    }
    for (i in 1:rows_available_for_start_check) {
      col1_txt <- as.character(df[[1]][i])
      col3_txt <- as.character(tryCatch(df[[3]][i], error = function(e) NA))
      col1_num <- suppressWarnings(as.numeric(col1_txt))
      col3_num <- suppressWarnings(as.numeric(col3_txt))
      
      is_excel_date_num <- (!is.na(col1_num) && col1_num > 30000)
      parsed_date <- suppressWarnings(lubridate::parse_date_time(col1_txt, orders = c("dmy", "mdy", "ymd")))
      is_parsable_date_str <- !is.na(parsed_date)
      is_date_like <- is_excel_date_num || is_parsable_date_str
      
      is_col3_int <- (!is.na(col3_num) && col3_num > 0 && round(col3_num) == col3_num)
      
      # message("DEBUG: Row ", i, ": is_date_like=", is_date_like, " (excel=", is_excel_date_num, ", str=", is_parsable_date_str, "), is_col3_int=", is_col3_int,
      #         " (Col1_txt: ", format(col1_txt), ", Col3_txt: ", format(col3_txt), ")") # Debug message commented
      
      if (is_date_like && is_col3_int) {
        start_row <- i
        break
      }
    }
  }
  
  if (is.na(start_row)) {
    message("    -> Data start pattern not found.")
    return(NULL)
  }
  message("    -> Data start detected at row: ", start_row)
  
  # Find End Row (Based on Col 1)
  # message("    -> Finding data end row...") # Commented out
  found_end = FALSE
  if (start_row < n_rows_df) {
    max_check_row = n_rows_df - (empty_rows_threshold - 1)
    if (start_row <= max_check_row) {
      is_empty <- function(x) is.na(x) | x == ""
      for (i in start_row:max_check_row) {
        all_empty_block = TRUE
        for (j in 0:(empty_rows_threshold - 1)) {
          if (!is_empty(df[[1]][i + j])) {
            all_empty_block = FALSE
            break
          }
        }
        if (all_empty_block) {
          end_row <- i - 1
          found_end = TRUE
          break
        }
      }
    }
  }
  
  if (!found_end) {
    last_non_empty_from_start <- tail(which(!is.na(df[[1]][start_row:n_rows_df]) & df[[1]][start_row:n_rows_df] != ""), 1)
    if (length(last_non_empty_from_start) > 0) {
      end_row <- (start_row - 1) + last_non_empty_from_start
    } else {
      end_row <- start_row
    }
    # message("    -> End detected at last non-empty row in Col 1: ", end_row) # Commented out
  } else {
    # message("    -> End detected at row: ", end_row) # Commented out
  }
  
  if (end_row < start_row) {
    message("    -> WARNING: Detected end_row is before start_row. Setting end_row = start_row.")
    end_row = start_row
  }
  message("    -> Data end detected at row: ", end_row)
  
  return(list(start = start_row, end = end_row))
}

## Function to get expected column names from metadata ----
get_metadata_colnames <- function(current_year_str, metadata_df) {
  # message("  -> Getting expected column names for year ", current_year_str, "...") # Commented out
  definition_years_str <- colnames(metadata_df)
  definition_years_num <- as.numeric(definition_years_str[stringr::str_detect(definition_years_str, "^\\d{4}$")])
  definition_years_num <- sort(definition_years_num[!is.na(definition_years_num)])
  current_year_num <- as.numeric(current_year_str)
  
  applicable_definition_year_num <- max(
    definition_years_num[definition_years_num <= current_year_num],
    na.rm = TRUE
  )
  
  if (is.infinite(applicable_definition_year_num) || is.na(applicable_definition_year_num)) {
    message("    -> No applicable definition found.")
    return(NULL)
  }
  
  applicable_definition_year_str <- as.character(applicable_definition_year_num)
  # message("    -> Using definition from metadata year: '", applicable_definition_year_str, "'") # Commented out
  
  year_colnames <- dplyr::pull(metadata_df, !!sym(applicable_definition_year_str))
  valid_colnames <- year_colnames[!is.na(year_colnames)]
  
  if (length(valid_colnames) == 0) {
    message("    -> No valid column names found for this definition.")
    return(NULL)
  }
  
  message("  -> Metadata definition expects ", length(valid_colnames), " columns (using '",
          applicable_definition_year_str, "' definition).")
  
  return(valid_colnames)
}

## Function to clean sheet data (subset rows using start/end, enforce cols, rename) ----
clean_sheet_data <- function(raw_df, row_indices, expected_colnames) {
  start_row <- row_indices$start
  end_row <- row_indices$end
  message("  -> Cleaning data...")
  
  # Subset rows using detected start and end
  if(start_row > end_row) {
    message("    -> Start row > end row. Returning empty.")
    empty_df <- as_tibble(
      matrix(NA, nrow = 0, ncol = length(expected_colnames)),
      .name_repair = "minimal"
    )
    colnames(empty_df) <- expected_colnames
    return(empty_df)
  }
  end_row_safe <- min(end_row, nrow(raw_df))
  df_subset <- raw_df[start_row:end_row_safe, , drop = FALSE]
  # message("    -> Subsetted to rows ", start_row, " to ", end_row_safe, ".") # Commented out
  
  # Enforce columns
  num_cols_read <- ncol(df_subset)
  num_cols_expected <- length(expected_colnames)
  temp_names <- paste0("...temp", seq_len(num_cols_read))
  colnames(df_subset) <- temp_names
  
  if (num_cols_read == num_cols_expected) {
    colnames(df_subset) <- expected_colnames
    # message("    -> Column count matches metadata.") # Commented out
  } else if (num_cols_read < num_cols_expected) {
    message("    -> Adding ", num_cols_expected - num_cols_read, " missing columns (as NA).")
    colnames(df_subset) <- expected_colnames[1:num_cols_read]
    for (k in (num_cols_read + 1):num_cols_expected) {
      # Use base R assignment which works well for adding columns
      df_subset[[expected_colnames[k]]] <- NA
    }
    # Ensure final column order matches metadata
    df_subset <- df_subset[, expected_colnames, drop = FALSE]
  } else { # num_cols_read > num_cols_expected
    message("    -> Truncating ", num_cols_read - num_cols_expected, " extra columns read.")
    df_subset <- df_subset[, 1:num_cols_expected, drop = FALSE]
    colnames(df_subset) <- expected_colnames
  }
  
  message("    -> Final dimensions: ", nrow(df_subset), " rows, ", ncol(df_subset), " cols.")
  return(df_subset)
}

# Load and Process Sheets -----------------------------------------

message("\nProcessing sheets one by one (reading, cleaning, storing)...")

# Get sheet names (original and trimmed)
all_sheet_names_original <- readxl::excel_sheets(path = latest_file)
all_sheet_names_trimmed <- stringr::str_trim(all_sheet_names_original)
year_sheet_indices <- stringr::str_detect(all_sheet_names_trimmed, "^\\d{4}$")
year_sheet_names <- all_sheet_names_trimmed[year_sheet_indices] # Cleaned names for logic
year_sheet_names_original_for_reading <- all_sheet_names_original[year_sheet_indices] # Original names for reading

if (length(year_sheet_names) == 0) {
  stop("Error: No sheets found with names matching a four-digit year (after trimming whitespace).")
}
message("Processing year sheets (identified by trimmed names): ", paste(year_sheet_names, collapse = ", "))

# Create lookup for original names based on cleaned names
names(year_sheet_names_original_for_reading) <- year_sheet_names

# Define read range once (limit columns, all rows initially)
initial_read_range <- readxl::cell_limits(ul = c(1, 1), lr = c(NA, max_cols_to_keep))

# Use map to loop through years. Inside map, perform all steps for one sheet.
tb_register_list <- purrr::map(year_sheet_names, ~{
  current_year <- .x # Cleaned name, e.g., "2006"
  current_year_original <- year_sheet_names_original_for_reading[[current_year]] # Original name, e.g., "2006 "
  
  message("\n--- Processing sheet: '", current_year_original, "' (as '", current_year, "') ---")
  
  # 1. Read Raw Data
  message("Step 1: Reading raw data (up to ", max_cols_to_keep, " cols)...")
  raw_df <- NULL
  tryCatch({
    raw_df <- read_excel(
      path = latest_file,
      sheet = current_year_original, # Use ORIGINAL name
      range = initial_read_range,
      col_names = FALSE,
      col_types = "text",         # Read as text initially
      .name_repair = "minimal"
    )
  }, error = function(e) {
    message("  -> ERROR reading sheet: ", e$message)
    # raw_df remains NULL
  })
  if(is.null(raw_df)) return(NULL) # Skip if read failed
  
  # 2. Get Metadata Colnames
  message("Step 2: Getting metadata definition...")
  expected_colnames <- get_metadata_colnames(current_year, col_names_meta)
  if (is.null(expected_colnames)) {
    message("  -> Skipping: Missing metadata definition.")
    rm(raw_df); return(NULL)
  }
  
  # 3. Find Data Start/End Rows
  message("Step 3: Finding data row indices...")
  row_indices <- find_data_row_indices(
    raw_df,
    rows_to_check = rows_to_check_for_header,
    empty_rows_threshold = empty_rows_threshold
  )
  if (is.null(row_indices)) {
    message("  -> Skipping: Start row not found.")
    rm(raw_df); return(NULL)
  }
  
  # 4. Clean Data
  message("Step 4: Cleaning data...")
  cleaned_df <- clean_sheet_data(
    raw_df,
    row_indices,
    expected_colnames
  )
  
  # Remove the large raw_df object now
  rm(raw_df)

  message("  -> Finished processing sheet '", current_year,
          "'. Final dimensions: ", nrow(cleaned_df), "r x ", ncol(cleaned_df), "c.")
  
  # Return the cleaned df (mostly character types)
  return(cleaned_df)
  
}) %>%
  # Name the final list using the cleaned year names
  purrr::set_names(year_sheet_names) %>%
  # Remove any NULLs from sheets that were skipped
  purrr::compact()


# Final Checks and Report -----------------------------------------

if (length(tb_register_list) == 0) {
  stop("Error: No data sheets were successfully processed.")
}

message("\nSuccessfully processed data from ", length(tb_register_list), " sheets into the list 'tb_register_list'.")
message("Years processed: ", paste(names(tb_register_list), collapse=", "))
message("Column structure enforced by metadata. Rows limited by dynamic end-row detection.")
message("Column definitions applied based on the latest applicable year in '", basename(col_names_file), "'.")


# Remove Non-Data Header/Separator Rows ----------------------

message("\nProcessing list: Removing non-data header/separator rows...")

tb_register_list <- purrr::imap(tb_register_list, ~{
  df <- .x  # .x is the current data frame (tibble)
  current_year <- .y  # .y is the name of the element in the list (the year string)
  
  # Optional message indicating which sheet is being processed
  # message("\n--- Filtering sheet: '", current_year, "' ---")
  
  rows_before <- nrow(df)
  
  # Define the condition to KEEP rows: keep if date_registered is NA OR if it's recognised as a date
  df_filtered <- dplyr::filter(
    df,
    is.na(date_registered) | # Keep rows where date is missing
      ( # OR keep rows where the value in date_registered is date-like:
        # Check 1: Is it like an Excel numeric date?
        (!is.na(suppressWarnings(as.numeric(as.character(date_registered)))) &
           suppressWarnings(as.numeric(as.character(date_registered))) > 30000) |
          # Check 2: Is it a parsable date string?
          (!is.na(suppressWarnings(lubridate::parse_date_time(as.character(date_registered),
                                                              orders = c("dmy", "mdy", "ymd")))))
      )
  )
  
  rows_after <- nrow(df_filtered)
  
  if (rows_before > rows_after) {
    message("  -> Sheet [", current_year, "]: Removed ", rows_before - rows_after, " potential header/separator rows.")
  } else {
    # message("  -> Sheet [", current_year, "]: No header/separator rows found to remove.") # Optional
  }
  
  return(df_filtered)
})

message("\nFinished removing non-data header/separator rows from all data frames.")

# Check tb_no column in each tibble for non-integer values -------------------------

message("\nChecking 'tb_no' column in each data frame for non-integer values...")

any_issues_found <- FALSE

purrr::iwalk(tb_register_list, ~{
  df <- .x
  year <- .y
  
  # Find indices where tb_no is NOT NA and does NOT consist only of digits
  # Convert to character and trim whitespace for robustness
  problematic_indices <- which(
    !is.na(df$tb_no) &
      !stringr::str_detect(stringr::str_trim(as.character(df$tb_no)), "^\\d+$")
  )
  
  # If any problematic rows are found for this year
  if (length(problematic_indices) > 0) {
    # Get the unique problematic values to report them
    unique_problems <- unique(df$tb_no[problematic_indices])
    
    # Print a warning message listing the first few unique issues
    message("  -> WARNING: Sheet [", year, "] 'tb_no' contains non-integer/non-NA values: ",
            paste(utils::head(unique_problems, 10), collapse=", "), # Show up to 10 unique problems
            if(length(unique_problems) > 10) "..." else "") # Add ellipsis if more exist
    
    # Set the overall flag to TRUE (using complex assignment <<- to modify parent environment)
    any_issues_found <<- TRUE
    
  } else {
    message("  -> Sheet [", year, "]: 'tb_no' column appears clean (only integers or NA).")
  }
  
  invisible(NULL)
})

# Report overall result
if (!any_issues_found) {
  message("\nCheck complete: No non-integer/non-NA values found in 'tb_no' columns.")
} else {
  message("\nCheck complete: Found non-integer/non-NA values in 'tb_no' columns (see warnings above).")
}

# Combine Data and Create Unique ID -----------------------------

message("\nCombining list and creating unique 'tb_no_year' identifier...")

# Check if the cleaned list exists and is not empty
if (!exists("tb_register_list") || length(tb_register_list) == 0) {
  stop("Error: 'tb_register_list' not found or is empty. Please ensure previous steps ran successfully.")
}

## Combine using dplyr::bind_rows() ----
# Data frames will have mostly character columns at this stage
tb_register_combined <- dplyr::bind_rows(tb_register_list, .id = "notification_year")

message("-> Combined data frames. Dimensions: ",
        nrow(tb_register_combined), " rows, ", ncol(tb_register_combined), " columns.")

## Create the tb_no_year identifier ----
# Assumes 'notification_year' and 'tb_no' columns exist and tb_no is integer/NA
if (!"notification_year" %in% colnames(tb_register_combined)) stop("Error: 'notification_year' missing.")
if (!"tb_no" %in% colnames(tb_register_combined)) stop("Error: 'tb_no' missing.")

# Perform mutate assuming columns exist and tb_no is clean
tb_register_combined <- tb_register_combined %>%
  dplyr::mutate(
    tb_no_formatted = sprintf("%04d", as.integer(tb_no)),  # Format tb_no (as.integer handles NA correctly)
    tb_no_year = stringr::str_c(notification_year, "_", tb_no_formatted),  # Combine year and formatted tb_no (str_c handles NAs correctly)
    tb_no_formatted = NULL # Remove intermediate column
  ) %>%
  dplyr::relocate(tb_no_year, .before = name) # Adjust '.before = name' as needed

message("-> Created 'tb_no_year' identifier.")

# Remove the list object if no longer needed
rm(tb_register_list)

# Inspect the combined data with the new ID -----------------------

message("\nInitial inspection of the combined data frame (types not yet converted):")

# Display structure - expect many <chr> types now
print(glimpse(tb_register_combined))

# Optional: Check unique values in a specific column (example)
# message("\nUnique values sample for 'sex' column:")
# if("sex" %in% colnames(tb_register_combined)) {
#   print(unique(tb_register_combined$sex))
# } else {
#   message("'sex' column not found.")
# }