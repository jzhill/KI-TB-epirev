# R/functions_all.R
# Consolidated helper functions for Kiribati NTP TB Register Analysis

# Dependency Imports ---------------------------------------

import::from(stringr, str_trim, str_extract, str_pad, str_match, str_replace_all, str_to_upper, str_to_lower, str_length, str_squish)
import::from(dplyr, filter, select, mutate, group_by, summarize, all_of, across, relocate, case_match, case_when, if_else, pull)
import::from(purrr, map_df, discard, map_chr)
import::from(rlang, sym)
import::from(lubridate, parse_date_time, year)
import::from(readxl, read_excel)
import::from(tools, file_path_sans_ext)
import::from(epikit, age_categories)
import::from(tidyr, pivot_longer)
import::from(tidyselect, all_of, everything)
import::from(readr, write_csv)

# I/O & File Management ------------------------------------

find_latest_file <- function(directory, pattern_regex) {
  all_files <- list.files(directory, pattern = pattern_regex, full.names = TRUE)
  if (length(all_files) == 0) return(NULL)
  date_strings <- str_match(basename(all_files), "(\\d{6})")[, 2]
  file_dates <- as.Date(date_strings, format = "%y%m%d")
  return(all_files[which.max(file_dates)])
}

# Standardizes a filename into a clean "slug" for folder naming
get_clean_ref <- function(path) {
  file_path_sans_ext(basename(path)) %>%
    tolower() %>%
    str_replace_all("[^a-z0-9]", "_") %>%
    str_replace_all("_+", "_")
}

get_metadata_colnames <- function(current_year_str, metadata_df) {
  all_years <- colnames(metadata_df)[str_detect(colnames(metadata_df), "^\\d{4}$")]
  def_years <- sort(as.numeric(all_years))
  target_year <- as.numeric(current_year_str)
  applicable_year <- max(def_years[def_years <= target_year], na.rm = TRUE)
  if (is.infinite(applicable_year)) return(NULL)
  return(metadata_df[[as.character(applicable_year)]] %>% discard(is.na))
}

import_and_name_sheet <- function(path, sheet_name, start_row, end_row, metadata_names) {
  
  # Calculate how many columns to read based on metadata length
  num_cols <- length(metadata_names)
  last_col_let <- get_excel_col_letter(num_cols)
  
  # Define the exact cell range (e.g., "A7:AH293")
  target_range <- paste0("A", start_row, ":", last_col_let, end_row)
  
  # Read data as character to avoid type conflicts during bind_rows
  df <- read_excel(
    path = path,
    sheet = sheet_name,
    range = target_range,
    col_names = FALSE,
    col_types = "text",
    .name_repair = "minimal"
  )
  
  # Apply the names from your CSV metadata
  colnames(df) <- metadata_names
  
  return(df)
}

# Structural Range Detection ------------------------------------

# Find start row: Col C (Int), Col D (Chr Name), Col E (Sex code)
find_register_start_row <- function(df, rows_to_check = 20) {
  start_row <- NA
  for (i in 1:min(nrow(df), rows_to_check)) {
    val_c <- suppressWarnings(as.numeric(as.character(df[[3]][i]))) # TB No
    val_d <- as.character(df[[4]][i])                             # Name
    val_e <- str_to_upper(str_trim(as.character(df[[5]][i])))
    
    is_c_int <- !is.na(val_c) && val_c > 0 && round(val_c) == val_c
    is_d_chr <- !is.na(val_d) && str_length(val_d) > 1 && is.na(suppressWarnings(as.numeric(val_d)))
    is_e_sex <- val_e %in% c("M", "F", "MALE", "FEMALE")
    
    if (is_c_int && is_d_chr && is_e_sex) {
      start_row <- i
      break
    }
  }
  return(start_row)
}

# Find end row: Row above the first instance of 3 consecutive blanks in Col C
find_register_end_row <- function(df, start_row) {
  if (is.na(start_row)) return(nrow(df))
  col_c <- as.character(df[[3]])
  is_blank <- is.na(col_c) | str_trim(col_c) == ""
  
  # Scan from start_row to find the first 3-blank streak
  res <- rle(is_blank[start_row:length(is_blank)])
  cum_indices <- cumsum(res$lengths)
  blank_runs <- which(res$values == TRUE & res$lengths >= 3)
  
  if (length(blank_runs) > 0) {
    first_blank_of_streak <- (start_row - 1) + (cum_indices[blank_runs[1]] - res$lengths[blank_runs[1]]) + 1
    return(first_blank_of_streak - 1)
  }
  return(nrow(df))
}

# Column and Header Logic ------------------------------------

get_excel_col_letter <- function(n) {
  ifelse(n <= 26, LETTERS[n], 
         paste0(get_excel_col_letter((n - 1) %/% 26), LETTERS[(n - 1) %% 26 + 1]))
}

# Finds the last column index that contains header text
find_last_header_column <- function(cells_df, start_row, max_scan = 100) {
  header_cells <- cells_df %>% 
    filter(row < start_row, col <= max_scan)
  
  if (nrow(header_cells) == 0) return(0)
  
  last_col <- header_cells %>%
    filter(!is.na(character), str_trim(character) != "") %>%
    pull(col) %>%
    max(na.rm = TRUE)
  
  return(last_col)
}

# Extracts raw headers by concatenating text found in each column
get_raw_header_concatenation <- function(cells_df, start_row, last_col) {
  header_data <- cells_df %>% 
    filter(row < start_row, col <= last_col) %>%
    filter(!is.na(character), str_trim(character) != "") %>%
    group_by(col) %>%
    summarize(full_header = paste(character, collapse = "_"), .groups = "drop") %>%
    mutate(full_header = str_squish(str_replace_all(full_header, " ", "_")))
  
  final_vec <- rep(NA_character_, last_col)
  final_vec[header_data$col] <- header_data$full_header
  return(final_vec)
}

# Cleaning & Data Logic ---------------------------------------------------

# Helper to log cleaning failures
log_cleaning_issue <- function(data, raw_col, clean_col, error_msg) {
  data %>%
    filter(!is.na(!!sym(raw_col)) & is.na(!!sym(clean_col))) %>%
    select(tb_id, name, value = !!sym(raw_col)) %>%
    mutate(
      field = raw_col,
      error = error_msg
    )
}

# Batch Auditor: Compares raw vs clean columns based on a validation map
generate_error_log <- function(data, validation_map) {
  
  # Use map_df to iterate through the map and combine results into one tibble
  map_df(1:nrow(validation_map), function(i) {
    row_spec <- validation_map[i, ]
    raw_col <- row_spec$raw
    clean_col <- row_spec$clean
    error_msg <- row_spec$msg
    
    data %>%
      # Find rows where original had data but clean version is NA
      filter(!is.na(!!sym(raw_col)) & is.na(!!sym(clean_col))) %>%
      select(
        tb_id, 
        name, 
        value = !!sym(raw_col)
      ) %>%
      mutate(
        field = raw_col,
        error = error_msg
      )
  })
}


# Generates a unique value audit CSV for a set of columns
export_unique_values_template <- function(data, target_cols, output_path,
                                          template_cols = NULL,
                                          lookup_data = NULL) {
  
  # 1. Identify and report which requested columns actually exist in this data version
  
  all_data_cols <- colnames(data)
  present_cols  <- intersect(target_cols, all_data_cols)
  missing_cols  <- setdiff(target_cols, all_data_cols)
  
  if (length(present_cols) > 0) {
    message("-> Auditing columns: ", paste(present_cols, collapse = ", "))
  }
  
  if (length(missing_cols) > 0) {
    message("! Note: Columns not found in this data version: ", paste(missing_cols, collapse = ", "))
  }
  
  if (length(present_cols) == 0) {
    message("! Warning: None of the specified columns found. Skipping export.")
    return(NULL)
  }
  
  # 2. Pivot and Clean
  current_uniques <- data %>%
    select(all_of(present_cols)) %>%
    pivot_longer(
      cols = everything(),
      names_to = "original_column",
      values_to = "raw_value",
      values_drop_na = TRUE
    ) %>%
    mutate(clean_value = str_squish(str_trim(str_to_lower(raw_value)))) %>%
    filter(clean_value != "") %>%
    distinct(original_column, clean_value, .keep_all = TRUE) %>%
    arrange(original_column, clean_value)
  
  # 3. If a lookup already exists, merge the old coding into the new uniques
  if (!is.null(lookup_data)) {
    message("-> Pre-filling audit with existing lookup data...")
    
    # We join based on the context (column + clean_value)
    current_uniques <- current_uniques %>%
      left_join(
        lookup_data %>% select(-any_of("raw_value")), # Don't duplicate raw_value
        by = c("original_column", "clean_value")
      )
  }
  
  # 4. Add blank template columns if requested
  if (!is.null(template_cols)) {
    for (col in template_cols) {
      if (!col %in% colnames(current_uniques)) {
        current_uniques[[col]] <- ""
      }
    }
  }
  
  # 5. Export
  write_csv(current_uniques, output_path, na = "")
  message("-> Audit template saved: ", basename(output_path), " (", nrow(current_uniques), " unique values)")
  
  return(current_uniques)
}

# Defensive TB number formatter (Extracts digits then pads)
format_tb_number <- function(tb_no_vector, width = 4) {
  # Extract only digits to handle messy entries like "12a" or "No. 5"
  num_only <- str_extract(as.character(tb_no_vector), "\\d+")
  formatted <- str_pad(num_only, width = width, side = "left", pad = "0")
  return(formatted)
}

# Improved mixed date parser with range validation
parse_mixed_date <- function(date_vector, min_year = 1990, max_year = 2030) {
  
  # 1. Try numeric (Excel serials)
  numeric_dates <- suppressWarnings(as.numeric(as.character(date_vector)))
  is_excel_num <- !is.na(numeric_dates) & numeric_dates > 30000
  
  # 2. Try text parsing
  parsed_text <- parse_date_time(
    if_else(is_excel_num, NA_character_, as.character(date_vector)),
    orders = c("dmy", "mdy", "ymd"), quiet = TRUE
  )
  
  # 3. Combine
  final_dates <- if_else(
    is_excel_num, 
    as.Date(numeric_dates, origin = "1899-12-30"), 
    as.Date(parsed_text)
  )
  
  # 4. Range Validation
  years_extracted <- year(final_dates)
  final_dates[which(years_extracted < min_year | years_extracted > max_year)] <- NA
  
  return(final_dates)
}

# Standardizes Age strings to numeric years
clean_age_column <- function(age_vector) {
  
  # Clean age input first (trim whitespace)
  age_trimmed <- str_trim(as.character(age_vector))
  
  case_when(
    # Handle NA inputs first
    is.na(age_trimmed) | age_trimmed == "" ~ NA_real_,
    
    # Rule 1: Already a whole number (integer years)
    str_detect(age_trimmed, "^\\d+$") ~ as.numeric(age_trimmed),
    
    # Rule 2: Weeks format (e.g., "3/52") -> always round up to 1 year
    str_detect(age_trimmed, "^\\d+\\s*/\\s*52$") ~ 1.0,
    
    # Rule 3: Months format X/12 -> round years UP
    str_detect(age_trimmed, "^\\d+\\s*/\\s*12$") 
    ~ ceiling(as.numeric(str_extract(age_trimmed, "^\\d+")) / 12),
    
    # Rule 4: Months format text (e.g., "9m") -> round years UP
    str_detect(age_trimmed, "^\\d+\\s*m") 
    ~ ceiling(as.numeric(str_extract(age_trimmed, "^\\d+")) / 12),
    
    # Rule 5: Handle "X+" format (e.g., "70+") -> return the value before the +
    str_detect(age_trimmed, "^\\d+\\+$") 
    ~ as.numeric(str_extract(age_trimmed, "^\\d+")),
    
    # Default: If none of the above patterns match, result is NA
    TRUE ~ NA_real_
  )
}

# Standardizes Sex strings into a factor (Male/Female)
clean_sex_column <- function(sex_vector) {
  standardized <- case_match(
    str_to_upper(str_trim(as.character(sex_vector))),
    c("M", "MALE", "MN") ~ "Male",
    c("F", "FEMALE")     ~ "Female",
    .default = NA_character_
  )
  
  # Return as a factor with explicit levels
  return(factor(standardized, levels = c("Male", "Female")))
}