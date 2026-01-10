# 01_inspect_load_data.R

library(here)
library(tidyverse)
library(readxl)
library(tidyxl)
library(qs)

source(here("R", "functions_all.R"))

# Setup -----------------------

# Identify current source register file and create reference
raw_path <- find_latest_file(here("data-raw", "register"), "\\.xlsx$")
current_ref <- get_clean_ref(raw_path)

# Create folder based on source register file for processed data
current_dir <- here("data-processed", current_ref)
if (!dir.exists(current_dir)) dir.create(current_dir, recursive = TRUE)

# Save reference TXT to identify processed data related to current source register file
writeLines(current_ref, here("data-processed", "current_register_reference.txt"))

meta_csv <- read_csv(here("data-raw", "TBreg_column_names.csv"), show_col_types = FALSE)

tech_summary <- list()
header_verification <- list()
register_list <- list()

# Get original names and create a named vector for lookup
original_names <- excel_sheets(raw_path)
clean_names <- str_trim(original_names)

# Create a lookup: names are clean, values are original
sheet_lookup <- original_names
names(sheet_lookup) <- clean_names

# Filter for the sheets we want, where the cleaned sheet name is in YYYY format
years_to_process <- clean_names[str_detect(clean_names, "^\\d{4}$")]

# Annual worksheet loop --------------------------

# Loop through each register annual worksheet to perform analysis tasks

for (sh_clean in years_to_process) {
  
  # Find the original name (with spaces) for the Excel functions
  sh_original <- sheet_lookup[[sh_clean]]
  
  message("Analyzing: ", sh_clean)
  
  all_cells <- xlsx_cells(raw_path, sheets = sh_original)
  
  # Use sh_original for functions that touch the Excel file
  sample_df <- read_excel(
      raw_path,
      sheet = sh_original,
      n_max = 1000,
      col_names = FALSE,
      col_types = "text",
      .name_repair = "minimal"
  )
  
  # Identify register data range in the sheet --------------------
  
  # Starts after the header rows, finishes at last registered patient
  # Columns based on text in visual header block
  
  start_idx <- suppressWarnings(find_register_start_row(sample_df))
  if (is.na(start_idx)) { message("  -> Start row not found. Skipping."); next }
  
  end_idx  <- find_register_end_row(sample_df, start_idx)
  
  last_col_head <- find_last_header_column(all_cells, start_idx)  
  meta_fields <- get_metadata_colnames(sh_clean, meta_csv)
  
  # Create register range summary ---------------------------------
  
  # Tibble with register header+data range and column references for each year
  
  tech_summary[[sh_clean]] <- tibble(
    year = sh_clean,
    start_row = start_idx,
    end_row = end_idx,
    row_count = end_idx - start_idx + 1,
    col_count = last_col_head,
    last_col_head_ref = get_excel_col_letter(last_col_head)
  )
  
  # Header verification ------------------------
  
  # For each annual worksheet, match the raw concatenated header text to the proposed clean column names
  # Create a tibble that can be output as csv for review
  
  raw_heads <- get_raw_header_concatenation(all_cells, start_idx, last_col_head)
  max_len <- max(length(raw_heads), length(meta_fields))
  
  header_verification[[sh_clean]] <- tibble(
    year = sh_clean,
    col_idx = 1:max_len,
    excel_ref = map_chr(1:max_len, get_excel_col_letter),
    raw_excel_header = c(raw_heads, rep(NA, max_len - length(raw_heads))),
    metadata_fieldname = c(meta_fields, rep(NA, max_len - length(meta_fields)))
  )
  
  # Data loading ----------------------
  
  # This uses the metadata columns length and the register data rows start and end to define the range
  # Data in columns to the right of the metadata specified range is ignored in each sheet
  # Data below the register data rows is ignored
  # Visual header text is ignored
  
  sheet_data <- import_and_name_sheet(
    path = raw_path,
    sheet_name = sh_original,
    start_row = start_idx,
    end_row = end_idx,
    metadata_names = meta_fields
  )
  
  # Add the registration year
  sheet_data <- sheet_data %>% 
    mutate(reg_year = sh_clean) %>% 
    relocate(reg_year)
  
  register_list[[sh_clean]] <- sheet_data
  
}

# Combine sheets and check ------------------------

register_combined_raw <- bind_rows(register_list)

# Check for completely empty columns, may have been inadvertently created by bind_rows()

empty_cols <- register_combined_raw %>%
  summarise(across(everything(), ~all(is.na(.)))) %>%
  pivot_longer(everything()) %>%
  filter(value == TRUE)

if(nrow(empty_cols) > 0) {
  warning("The following columns are entirely empty. Check for typos in your metadata CSV: ", 
          paste(empty_cols$name, collapse = ", "))
} else {
  message("CHECK PASSED: No empty columns.")
}

# Check that the final number of rows in the combined register matches the sum of data rows in all the annual worksheets

total_intended_rows <- sum(bind_rows(tech_summary)$row_count)
actual_rows <- nrow(register_combined_raw)

if (total_intended_rows != actual_rows) {
  stop("CRITICAL: Row count mismatch! Intended: ", total_intended_rows, " | Actual: ", actual_rows)
} else {
  message("CHECK PASSED: Row counts match.")
}

# Check that name and TB reg number are filled in for all rows

missing_anchors <- register_combined_raw %>%
  filter(is.na(name) | is.na(tb_no)) %>%
  count(reg_year)

if (nrow(missing_anchors) > 0) {
  message("WARNING: Found rows with missing Name or TB Number in these years:")
  print(missing_anchors)
} else {
  message("CHECK PASSED: All rows have name and TB number.")
}

# Check that the combination of year and annual tb number is unique for each row

duplicates <- register_combined_raw %>%
  count(reg_year, tb_no) %>%
  filter(n > 1)

if (nrow(duplicates) > 0) {
  warning("CRITICAL: Duplicate TB Numbers found within the same year!")
  print(duplicates)
} else {
  message("CHECK PASSED: TB numbers are unique in each year")
}

# Save Outputs --------------------

# Data range summary and header verification as .csv files

write_csv(bind_rows(tech_summary), here(current_dir, "sheet_technical_summary.csv"))
write_csv(bind_rows(header_verification), here(current_dir, "header_verification.csv"))

# Raw register .qs and .csv files

qsave(register_list, file.path(current_dir, "register_raw_list.qs"))
qsave(register_combined_raw, file.path(current_dir, "register_combined_raw.qs"))
write_csv(register_combined_raw, file.path(current_dir, "register_combined_raw.csv"))

message("Inspection and loading complete. Current source register reference set to: ", current_ref)

