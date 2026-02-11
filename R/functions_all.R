# R/functions_all.R
# Consolidated helper functions for Kiribati NTP TB Register Analysis

# Dependency Imports ---------------------------------------

import::from(dplyr, across, any_of, case_match, case_when, filter, group_by, if_else, mutate, pull, relocate, select, summarize)
import::from(epikit, age_categories)
import::from(lubridate, parse_date_time, year)
import::from(purrr, discard, map_chr, map_df)
import::from(readr, write_csv)
import::from(readxl, read_excel)
import::from(rlang, sym)
import::from(stringr, str_detect, str_extract, str_length, str_match, str_pad, str_replace_all, str_squish, str_to_lower, str_to_upper, str_trim)
import::from(tidyr, pivot_longer)
import::from(tidyselect, all_of, everything)
import::from(tools, file_path_sans_ext)

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
    val_c <- suppressWarnings(as.numeric(as.character(df[[3]][i]))) 
    val_d <- as.character(df[[4]][i]) # Name is a character string in column D
    val_e <- str_to_upper(str_trim(as.character(df[[5]][i])))
    
    # TB No is a number in column C
    is_c_int <- !is.na(val_c) && val_c > 0 && round(val_c) == val_c 
    # Name is a text string in column D
    is_d_chr <- !is.na(val_d) && str_length(val_d) > 1 && is.na(suppressWarnings(as.numeric(val_d)))
    # Sex is a string in the pattern below in column E
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


# Generate an error log comparing raw values with cleaned
# Captures if cleaned is left blank or "unclassified"

generate_error_log <- function(data, validation_map) {
  
  map_df(1:nrow(validation_map), function(i) {
    row_spec <- validation_map[i, ]
    raw_col <- row_spec$raw
    clean_col <- row_spec$clean
    error_msg <- row_spec$msg
    
    data %>%
      mutate(
        temp_raw = as.character(!!sym(raw_col)),
        # Standardize for logical check
        temp_clean_check = str_to_lower(as.character(!!sym(clean_col)))
      ) %>%
      
      # THE LOGIC GATE:
      filter(
        # 1. Does raw data exist?
        !is.na(temp_raw) & temp_raw != "" & 
          
          # 2. Is the clean result a failure?
          (is.na(temp_clean_check) | temp_clean_check == "unclassified" | temp_clean_check == "unknown")
      ) %>%
      
      transmute(
        tb_id,
        name,
        raw_value = temp_raw,
        # FORCE CHARACTER: This prevents the 'Can't combine <date> and <double>' error
        clean_state = as.character(!!sym(clean_col)), 
        field = raw_col,
        error = case_when(
          str_to_lower(clean_state) == "unclassified" ~ paste0("Unclassified: ", error_msg),
          TRUE ~ error_msg
        )
      )
  })
}



# categorise "unclassified" from age groups and then factorise
finalise_age_groups <- function(age_cat_vector, age_numeric_vector) {
  
  # Extract the original levels from the epikit factor
  orig_levels <- levels(age_cat_vector)
  
  # Create the character vector with our forensic logic
  cleaned_vec <- case_when(
    # Truly missing numeric age
    is.na(age_numeric_vector) ~ NA_character_,
    
    # Numeric age exists but was NOT binned by epikit (out of bounds)
    !is.na(age_numeric_vector) & is.na(age_cat_vector) ~ "unclassified",
    
    # Successful binning
    TRUE ~ as.character(age_cat_vector)
  )
  
  # Return as factor with the original bins first, then unclassified
  factor(cleaned_vec, levels = c(orig_levels, "unclassified"))
}



# Generates a unique value audit CSV for a set of columns
export_unique_values_template <- function(data, target_cols, output_path,
                                          template_cols = NULL,
                                          lookup_data = NULL,
                                          counts = FALSE,
                                          unique_scope = c("by_column", "global")) {
  
  unique_scope <- match.arg(unique_scope)
  
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
  current_long <- data %>%
    select(all_of(present_cols)) %>%
    pivot_longer(
      cols = everything(),
      names_to = "original_column",
      values_to = "raw_value",
      values_drop_na = TRUE
    ) %>%
    mutate(
      # CHANGE: now squish internal whitespace (must match 02_ join-key cleaning)
      clean_value = str_squish(str_trim(str_to_lower(raw_value)))
    ) %>%
    filter(clean_value != "")
  
  # 3. Collapse to uniques (+ optional counts), by chosen scope
  if (unique_scope == "by_column") {
    
    if (isTRUE(counts)) {
      current_uniques <- current_long %>%
        count(original_column, clean_value, name = "n") %>%
        arrange(desc(n), original_column, clean_value)
    } else {
      current_uniques <- current_long %>%
        distinct(original_column, clean_value, .keep_all = TRUE) %>%
        arrange(original_column, clean_value)
    }
    
    # 3b. If a lookup already exists, merge the old coding into the new uniques
    if (!is.null(lookup_data)) {
      message("-> Pre-filling audit with existing lookup data...")
      
      # This prevents "many-to-many" row explosions during the audit export
      lookup_safe <- lookup_data %>% 
        select(-any_of(c("raw_value", "n", "columns_present"))) %>%
        distinct(original_column, clean_value, .keep_all = TRUE)
      
      # We join based on the context (column + clean_value)
      current_uniques <- current_uniques %>%
        left_join(lookup_safe, by = c("original_column", "clean_value"))
    }
    
  } else {
    
    # Global uniques: one row per clean_value across all audited columns
    if (isTRUE(counts)) {
      current_uniques <- current_long %>%
        count(clean_value, name = "n") %>%
        arrange(desc(n), clean_value)
    } else {
      current_uniques <- current_long %>%
        distinct(clean_value, .keep_all = FALSE) %>%
        arrange(clean_value)
    }
    
    # Add context: which columns each value appears in
    columns_ctx <- current_long %>%
      distinct(clean_value, original_column) %>%
      group_by(clean_value) %>%
      summarise(
        columns_present = paste(sort(unique(original_column)), collapse = " | "),
        .groups = "drop"
      )
    
    current_uniques <- current_uniques %>%
      left_join(columns_ctx, by = "clean_value")
    
    # Lookup join in global mode only if lookup_data is also global (no original_column)
    if (!is.null(lookup_data)) {
      if (!"original_column" %in% colnames(lookup_data)) {
        message("-> Pre-filling audit with existing lookup data...")
        current_uniques <- current_uniques %>%
          left_join(
            lookup_data %>% select(-any_of(c("raw_value", "n", "columns_present"))),
            by = "clean_value"
          )
      } else {
        message("! Note: lookup_data contains original_column, so it cannot be safely joined in global mode. Skipping lookup join.")
      }
    }
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

# Pulls official geo names from geo_helper
get_official_geo_metadata <- function(geo_path = here("data-raw", "geo_helper.xlsx")) {
  
  if (!file.exists(geo_path)) stop("! Geo helper (xlsx) not found.")
  
  # Load the raw census mapping
  census_codes_df <- read_excel(geo_path, sheet = "census_codes") %>%
    mutate(across(everything(), ~str_trim(str_to_lower(as.character(.)))))
  
  # Universal helper to extract unique, ordered levels
  # Can filter by island_name (for villages) or be left NULL (for islands/divisions)
  get_levels <- function(name_col, code_col, filter_col = NULL, filter_val = NULL) {
    d <- census_codes_df
    
    if (!is.null(filter_col) & !is.null(filter_val)) {
      d <- d %>% filter(!!sym(filter_col) == filter_val)
    }
    
    d %>%
      distinct(!!sym(name_col), !!sym(code_col)) %>%
      arrange(as.numeric(!!sym(code_col))) %>%
      pull(!!sym(name_col)) %>%
      unique()
  }
  
  # Return structured metadata, names ordered by census code
  
  list(
    # Primary reporting levels (for 04_ and CNR plots)
    island_names   = get_levels("island_name", "island_code"),
    island_codes   = get_levels("island_code", "island_code"),
    division_names = get_levels("division_name", "division_code"),
    division_codes = get_levels("division_code", "division_code"),
    
    # Village levels (for 02_ address cleaning)
    st_village_names = get_levels("village_name", "village_code", "island_name", "south tarawa"),
    st_village_codes = get_levels("village_code", "village_code", "island_name", "south tarawa"),
    nt_village_names = get_levels("village_name", "village_code", "island_name", "north tarawa"),
    nt_village_codes = get_levels("village_code", "village_code", "island_name", "north tarawa"),
    
    # Council and Region groupings (oi_st)
    oi_st_names      = na.omit(unique(census_codes_df$oi_st)),
    council_names    = na.omit(unique(census_codes_df$council)),
    
    # Census code lookup tables at village and island level
    census_village = census_codes_df,
    census_island  = census_codes_df %>% 
      select(division_code, division_name, island_code, island_name, oi_st) %>% 
      distinct()
  )
}

# Validate geo values in population file
# Expected to be the same official list of names as used for geo classification of reg_combined

validate_geo_values <- function(actual, reference, label) {
  
  invalid <- setdiff(actual, reference)
  
  if (length(invalid) > 0) {
    stop(paste0("! Invalid values in '", label, "': "), paste(invalid, collapse = ", "))
  }
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



# Classify bacteriology blob 
# Input: blob (character scalar, already squished/trimmed/lower is fine; function is defensive)
# Output: pos / neg / nr / NA
classify_bac_blob <- function(blob) {
  
  if (is.na(blob)) return(NA_character_)
  x <- str_squish(str_trim(str_to_lower(blob)))
  if (x == "") return(NA_character_)
  
  # patterns
  
  # Positive evidence across tests:
  # - smear grades / scanty / AFB counts
  # - culture positive shorthand
  # - xpert trace/detected and RR language
  
  pos_patterns <- c(
    # smear
    "\\bscanty\\b", "\\bsc\\b", "(?<!\\d)[123]\\+(?!\\d)",
    "\\b\\d+\\s*afb\\b", "\\b\\d+\\s*/\\s*100\\s*field", "\\b\\d+\\s*/\\s*100\\s*fields", "ve\\+",
    # culture
    "\\bc/pos\\b", "\\bc/p\\b", "\\bculture\\s*/\\s*pos\\b", "\\bculture\\s*pos\\b",
    # generic positives
    "\\bpos\\b", "\\bpositive\\b", "\\bve\\+\\b", "ve\\+\\.",
    # xpert pos tokens
    "\\btx\\b", "\\btrace\\b", "\\btnr\\b", "\\bt\\(", "\\bt\\b", "\\btl\\)", "\\bti\\b",
    # detected (handled carefully to avoid not detected)
    "\\bdetected\\b",
    # RR language
    "\\brif\\s*resistant\\b", "\\brr\\s*tb\\s*detected\\b", "\\brr\\b", "rif\\s*resist"
  )
  
  # Negative evidence across tests
  
  neg_patterns <- c(
    "\\bneg\\b", "\\bneg(?=\\d)", "\\bneg(?=[a-z])", "\\bnegative\\b", "\\bn\\b",
    "\\bnd\\b", "\\bn\\.d\\b", "\\bn\\s*d\\b",
    "\\bnot\\s*detected\\b",
    "rpt\\s*-?\\s*\\d+\\s*days\\s*-\\s*neg"
  )
  
  # Unclassified markers (recorded but not interpretable)
  
  unc_patterns <- c(
    "\\bo/s\\b",
    "cartridge", "stock", "low\\s*cartridge", "low\\s*stock",
    "lab\\s*form\\s*lost",
    "no\\s*flight",
    "pus\\s*/\\s*swb", "pus/swb",
    "\\be/p\\b"
  )
  
  # classification logic
  
  # Avoid false pos from "detected" inside "not detected"
  x_no_notdet <- str_replace_all(x, "not\\s*detected", " ")
  
  has_pos <- any(str_detect(x_no_notdet, pos_patterns))
  if (has_pos) return("pos")
  
  has_neg <- any(str_detect(x, neg_patterns))
  if (has_neg) return("neg")
  
  # If text exists but no matches, still "unclassified"
  # Unclassified markers are not required; they just document known non-result tokens
  has_unc_marker <- any(str_detect(x, unc_patterns))
  if (has_unc_marker) return("unclassified")
  
  "unclassified"
}



