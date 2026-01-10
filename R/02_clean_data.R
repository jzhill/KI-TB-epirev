# Title and Description --------------------------------------------

# Cleaning NTP register processed data
# Data are owned by Kiribati MHMS
# Author: Jeremy Hill
# Date commenced: 10/4/2025
# Last modified: 9/1/2026 # <<< Refactor and improved accuracy

# Packages --------------------------------------------------------

# Ensure packages installed via renv::install() / renv::restore()
library(here)
library(tidyverse)
library(lubridate)
library(skimr)
library(epikit)
library(qs)

source(here("R", "functions_all.R"))


# Find and load processed register data -------------

# Read reference and validate environment

ref_file <- here("data-processed", "current_register_reference.txt")
if (!file.exists(ref_file)) stop("Reference file missing. Run Script 01 first.")

current_ref <- readLines(ref_file, n = 1)
current_dir <- here("data-processed", current_ref)
raw_path <- file.path(current_dir, "register_combined_raw.qs")

if (!file.exists(raw_path)) stop("Raw file not found in: ", current_dir)

reg_combined_raw <- qread(raw_path)

# Initial cleaning ------------------

# Initialize the error log collector
error_log <- list()

reg_combined <- reg_combined_raw %>%

  # Remove monthly divider rows and any other empty/invalid rows
  filter(!is.na(name), !is.na(tb_no)) %>%
  
  # Create unique TB reg number
  mutate(
    tb_no_clean = format_tb_number(tb_no),
    tb_id = paste0(reg_year, "_", tb_no_clean)
  ) %>%
  
  relocate(tb_no_clean, .after = tb_no) %>%
  relocate(tb_id, .before = 1)

# Output skim to .csv ------------

skim_results <- reg_combined %>% 
  skimr::skim() %>% 
  as_tibble()

write_csv(skim_results, file.path(current_dir, "data_dictionary_raw_skim.csv"))

# Clean dates age and sex ------------------------------------------

reg_combined <- reg_combined %>%
  mutate(
    # Dates
    date_reg_clean = parse_mixed_date(date_registered),
    date_start_clean = parse_mixed_date(date_started),
    
    # Age
    age_clean = clean_age_column(age),
    
    # Sex
    sex_clean = clean_sex_column(sex)
  ) %>%
  
  relocate(date_reg_clean, .after = date_registered) %>%
  relocate(date_start_clean, .after = date_started) %>%
  relocate(age_clean, .after = age) %>%
  relocate(sex_clean, .after = sex)

# Create age group columns ------------------------------------------

# Set parameters for grouping

max_age_val <- max(reg_combined$age_clean, na.rm = TRUE)
upper_limit_10yr <- floor(max_age_val / 10) * 10
age_breaks_who   <- c(0, 5, 15, 25, 35, 45, 55, 65)
age_breaks_new   <- c(0, 5, 10, 15, 20, 25, 35, 45, 55, 65)

# Apply categorization
reg_combined <- reg_combined %>%
  mutate(
    
    # Standard 10-year bins (0-9, 10-19... 80+)
    age_group_10yr = age_categories(
      age_clean,
      lower = 0,
      upper = upper_limit_10yr,
      by = 10
    ),
    
    # WHO TB Programme standard bins (0-4, 5-14... 65+)
    age_group_who = age_categories(
      age_clean,
      breakers = age_breaks_who
    ),
    
    # New WHO TB Programme bins (0-4, 5-9, 10-14, 15-19... 65+)
    age_group_new = age_categories(
      age_clean,
      breakers = age_breaks_new
    )
    
  ) %>%
  # Position next to the numeric age
  relocate(age_group_10yr, age_group_who, age_group_new, .after = age_clean)

# Geography & Address cleaning and mapping ----------------------------

## Use address lookup table to add coded island and ST village ----------------------

addr_lookup <- read_csv(here("data-raw", "TBreg_address_lookup_table.csv"), show_col_types = FALSE) %>%
  mutate(
    address_clean = str_trim(str_to_lower(address)),
    # If the lookup string exists but island/village is missing, mark as "NR"
    island_coded = if_else(is.na(island_coded) | island_coded == "Check", "NR", island_coded),
    ST_village_coded = if_else(is.na(ST_village_coded) | ST_village_coded == "Check", "NR", ST_village_coded)
  ) %>%
  distinct(address_clean, .keep_all = TRUE) %>%
  select(address_clean, island = island_coded, ST_village = ST_village_coded)

# Perform the Join
reg_combined <- reg_combined %>%
  mutate(address_clean = str_trim(str_to_lower(address)), .after = address) %>%
  select(-any_of(c("island", "ST_village"))) %>%
  left_join(addr_lookup, by = "address_clean") %>%
  relocate(island, ST_village, .after = address_clean)

## Add island and division names and codes from lookup table --------------------

# Load island census population table, which includes categories
pop_island_census_file <- here("data-raw", "pop_island_census.csv")
island_code_lookup <- read_csv(pop_island_census_file, show_col_types = FALSE)

# Clean the 'Island' column (lowercase, trim) and use it as the join key 'island'.
# Keep other required columns.

island_code_lookup <- island_code_lookup %>%
  mutate(
    island = str_trim(str_to_lower(Island))
  ) %>%
  select(
    island,
    is_code,
    division = Division,
    div_code,
    OI_ST
  )

## Perform left join using the cleaned island name ---------------------

cols_to_add <- c("is_code", "division", "div_code", "OI_ST")

reg_combined <- reg_combined %>%
  
  # Remove columns first to ensure idempotency
  select(-any_of(cols_to_add)) %>%
  left_join(island_code_lookup, by = "island") %>%
  mutate(division = if_else(island == "NR", "NR", division)) %>%
  mutate(across(
    c(island, ST_village, is_code, division, div_code, OI_ST), 
    as.factor
  )) %>%
  relocate(all_of(cols_to_add), .after = island)


# Coded disease types and sites from lookup ------------------------

## Define disease column sets ----------------------------------------------------

disease_cols <- c(
  "disease_site",
  "disease_site_pulm_2018",
  "disease_site_ep_2018"
)

# Define the blank columns you want for your future lookup table
disease_template <- c(
  # Primary WHO Classifications
  "ptb_eptb",        # PTB, EPTB

  # Clinical Types & Severity Flags
  "type_disseminated",
  "type_miliary",
  "type_severe_paed", # Critical for paediatric treatment guidelines
  "type_meningitis",
  "type_potts",       # Severe Spinal TB
  "type_abscess",
  "type_effusion",    # Pleural, Pericardial, or Peritoneal
  "type_lymphadenitis",
  
  # Anatomical Sites - Respiratory
  "site_pulmonary",
  "site_pleural",
  "site_hilar_lns", # Specific for paediatric/primary TB
  
  # Anatomical Sites - Lymphatic (Peripheral)
  "site_cervical_lns",
  "site_axillary_lns",
  "site_inguinal_lns",

  # Anatomical Sites - Organ Systems
  "site_abdominal",
  "site_anal",
  "site_pericardial",
  "site_bone_joint",
  "site_cns",         # Brain/Spinal Cord
  "site_gu",          # Kidney/Genitourinary
  "site_skin",
  "site_ento"         # Eye, Nose, Throat, Ophthal
)


## Audit Disease Sites ----------------------------------------------------

export_unique_values_template(
  data = reg_combined,
  target_cols = disease_cols,
  output_path = file.path(current_dir, "unique_disease_sites.csv"),
  template_cols = disease_template
)



## Load and prepare the disease lookup file -----------------------

disease_lookup_path <- here("data-raw", "disease_site_lookup_table.csv")
if (!file.exists(disease_lookup_path)) stop("! Disease lookup not found.")

disease_lookup <- read_csv(disease_lookup_path, show_col_types = FALSE) %>%
  mutate(clean_value = str_trim(str_to_lower(raw_value))) %>%
  select(original_column, clean_value, ptb_eptb, matches("^(type_|site_)"))


## Pivot register disease columns long ---------------------------------------


disease_long <- reg_combined %>%
  select(tb_id, any_of(disease_cols)) %>%
  pivot_longer(
    cols = -tb_id, 
    names_to = "original_column", 
    values_to = "raw_value",
    values_drop_na = TRUE
  ) %>%
  mutate(clean_value = str_trim(str_to_lower(raw_value)))

## Join to your Lookup Table -------------------------------------------

disease_classified_long <- disease_long %>%
  left_join(disease_lookup, by = c("original_column", "clean_value"))

## Summarise back to one row per patient -------------------------------

disease_final_summary <- disease_classified_long %>%
  group_by(tb_id) %>%
  summarize(
    
    has_any_disease_data = TRUE,
    
    # WHO PTB/EPTB classification
    ptb_eptb = case_when(
      any(ptb_eptb == "PTB", na.rm = TRUE)  ~ "PTB",
      any(ptb_eptb == "EPTB", na.rm = TRUE) ~ "EPTB",
      TRUE                                  ~ NA_character_
    ),
    
    # Site/type classification
    across(
      matches("^(type_|site_)"), 
      ~ any(.x == 1 | .x == "1" | .x == TRUE, na.rm = TRUE)
    ),
    
    .groups = "drop"
  )

## Join results back to main register -------------

reg_combined <- reg_combined %>%
  select(-any_of(colnames(disease_final_summary)[-1])) %>% 
  left_join(disease_final_summary, by = "tb_id") %>%
  mutate(ptb_eptb = factor(ptb_eptb, levels = c("PTB", "EPTB"))) %>%
  
  # Organize the new columns
  relocate(ptb_eptb, starts_with("type_"), starts_with("site_"), .after = disease_site)

message("-> Disease classification complete.")


# Batch Error Logging ------------------------------------

message("Batch auditing cleaning results...")

# Define what to check
cleaning_map <- tibble::tribble(
  ~raw,                     ~clean,             ~msg,
  "date_registered",        "date_reg_clean",   "Date invalid or outside range",
  "date_started",           "date_start_clean", "Date invalid or outside range",
  "age",                    "age_clean",        "Unrecognized age format",
  "sex",                    "sex_clean",        "Unknown sex code",
  
  # Geography Audits
  "address",                "island",           "Address string not found in lookup table",
  "island",                 "division",         "Island name not found in census lookup",
  
  # Disease Classification Audits
  "disease_site",           "ptb_eptb",         "Disease string missing from lookup (Site)",
  "disease_site_pulm_2018", "ptb_eptb",         "Disease string missing from lookup (Pulm 2018)",
  "disease_site_ep_2018",   "ptb_eptb",         "Disease string missing from lookup (EP 2018)"
)

# Generate the log in one command
data_error_log <- generate_error_log(reg_combined, cleaning_map)

# Outputs ------------------------------------

# Output Error Log
if (nrow(data_error_log) > 0) {
  write_csv(data_error_log, file.path(current_dir, "data_error_log.csv"))
  message("-> Error log saved with ", nrow(data_error_log), " issues.")
}

# Output Clean Combined Register Data
qsave(reg_combined, file.path(current_dir, "register_combined_clean.qs"))
write_csv(reg_combined, file.path(current_dir, "register_combined_clean.csv"))

message("Cleaning complete for: ", current_ref)



