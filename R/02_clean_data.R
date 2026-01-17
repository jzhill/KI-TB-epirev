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
  skim() %>% 
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

# Coded geography island/village from lookup ----------------------------

## Setup and Reference Data -----------------------------------------------

# File Paths
geo_lookup_path <- here("data-raw", "unique_geo_lookup.csv")
geo_helper_path <- here("data-raw", "geo_helper.xlsx")

if (!file.exists(geo_lookup_path)) stop("! Geo lookup (csv) not found.")
if (!file.exists(geo_helper_path)) stop("! Geo helper (xlsx) not found.")

# Load Census Reference
census_codes <- read_excel(geo_helper_path, sheet = "census_codes") %>%
  mutate(across(everything(), ~str_trim(str_to_lower(as.character(.)))))

# Load geo lookup table
geo_lookup <- read_csv(geo_lookup_path, show_col_types = FALSE) %>%
  mutate(clean_value = str_squish(str_trim(str_to_lower(raw_value)))) 

# Define Official Lists for Template
# Sorted by census code to preserve geographic/official order
island_names <- census_codes %>% 
  arrange(as.numeric(island_code)) %>% 
  pull(island_name) %>% 
  unique()

island_codes <- census_codes %>% 
  arrange(as.numeric(island_code)) %>% 
  pull(island_code) %>% 
  unique()

division_names <- census_codes %>% 
  arrange(as.numeric(division_code)) %>% 
  pull(division_name) %>% 
  unique()

division_codes <- census_codes %>% 
  arrange(as.numeric(division_code)) %>% 
  pull(division_code) %>% 
  unique()

st_village_names <- census_codes %>% 
  filter(island_name == "south tarawa") %>% 
  arrange(as.numeric(village_code)) %>% # Assuming you have village_code
  pull(village_name) %>% 
  unique()

nt_village_names <- census_codes %>% 
  filter(island_name == "north tarawa") %>% 
  arrange(as.numeric(village_code)) %>% 
  pull(village_name) %>% 
  unique()

# Define column sets
# ORDER MATTERS: The first column is the highest priority for resolution
geo_cols <- c("address")
# here are some other potential columns for later consideration
# geo_cols <- c("address", "address_desc_2024", "treatment_unit", "school_2024")

geo_template <- c(
  "island", "island_search", "island_manual", 
  "ST_village", "ST_village_search", "ST_village_manual", 
  "NT_village", "NT_village_search", "NT_village_manual",
  paste0("is_", island_names),
  paste0("st_v_", st_village_names),
  paste0("nt_v_", nt_village_names)
)



## Audit geo columns ----------------------------------------------------

export_unique_values_template(
  data = reg_combined,
  target_cols = geo_cols,
  output_path = file.path(current_dir, "unique_geo.csv"),
  template_cols = geo_template,
  lookup_data = geo_lookup
)


## Pivot register geo columns long ------------------------

geo_long <- reg_combined %>%
  select(tb_id, any_of(geo_cols)) %>%
  pivot_longer(
    cols = -tb_id,
    names_to = "original_column",
    values_to = "raw_value",
    values_drop_na = TRUE
    ) %>%
  mutate(
    clean_value = str_squish(str_trim(str_to_lower(raw_value))),
    col_priority = factor(original_column, levels = geo_cols, ordered = TRUE)
  )

## Join to lookup table and summarise -------------------

geo_classified_wide <- geo_long %>%
  left_join(
    geo_lookup %>% select(original_column, clean_value, island, ST_village, NT_village),
    by = c("original_column", "clean_value")
    ) %>%
  arrange(tb_id, col_priority) %>% 
  group_by(tb_id) %>%
  summarize(
    
    has_any_geo_data = TRUE,
    
    # Take the first non-NA result from the sorted priority list
    island = first(na.omit(island)),
    ST_village = first(na.omit(ST_village)),
    NT_village = first(na.omit(NT_village)),
    .groups = "drop"
  )



## Join results back to main register -------------

reg_combined <- reg_combined %>%
  
  select(-any_of(colnames(geo_classified_wide)[-1])) %>% 
  left_join(geo_classified_wide, by = "tb_id") %>%
  
  # Attach Official Census Codes and Divisions
  left_join(
    census_codes %>%
      select(island_name, island_code, division = division_name, division_code) %>%
      distinct(), 
    by = c("island" = "island_name")
  ) %>% 
  
  mutate(
    
    # If island is "nr", ensure division and codes are also "nr" instead of NA
    across(c(division, division_code, island_code), 
           ~if_else(island == "nr", "nr", as.character(.))),
    
    # island_ST_bin: TRUE only for South Tarawa
    island_ST_bin = !is.na(island) & island == "south tarawa",
    
    # island_NT_bin: TRUE only for North Tarawa
    island_NT_bin = !is.na(island) & island == "north tarawa",
    
    # ST_OI: "ST" (South Tarawa), "OI" (Outer Islands), or "NR" (Not Recorded)
    ST_OI = case_when(
      island == "south tarawa" ~ "ST",
      island == "nr"           ~ "NR",
      !is.na(island)           ~ "OI", # All other matched islands are Outer Islands
      TRUE                     ~ NA_character_
    )
  ) %>%
  
  # Organize the new columns after joining
  relocate(
    has_any_geo_data, ST_OI, 
    division, division_code,
    island, island_code,  
    ST_village, NT_village, 
    island_ST_bin, island_NT_bin, 
    .after = address
    ) %>% 
  
  # Ensure geo codes are factors using the census code ordering
  mutate(
    # 1. Divisions
    division = factor(division, levels = c(division_names, "nr")),
    division_code = factor(division_code, levels = c(division_codes, "nr")),
    
    # 2. Islands (Ordered North to South based on census codes)
    island = factor(island, levels = c(island_names, "nr")),
    island_code   = factor(island_code, levels = c(island_codes, "nr")),
    
    # 3. Villages
    ST_village = factor(ST_village, levels = c(st_village_names, "nr")),
    NT_village = factor(NT_village, levels = c(nt_village_names, "nr")),
    
    # 4. ST_OI Grouping
    ST_OI = factor(ST_OI, levels = c("ST", "OI", "NR"))
  )

message("-> Geography classification complete.")





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


## Load and prepare the disease lookup file -----------------------

disease_lookup_path <- here("data-raw", "unique_disease_sites_lookup.csv")
if (!file.exists(disease_lookup_path)) stop("! Disease lookup not found.")

disease_lookup <- read_csv(disease_lookup_path, show_col_types = FALSE) %>%
  mutate(clean_value = str_squish(str_trim(str_to_lower(raw_value)))) %>%
  select(original_column, clean_value, ptb_eptb, matches("^(type_|site_)"))


## Audit Disease Sites ----------------------------------------------------

export_unique_values_template(
  data = reg_combined,
  target_cols = disease_cols,
  output_path = file.path(current_dir, "unique_disease_sites.csv"),
  template_cols = disease_template,
  lookup_data = disease_lookup
)



## Pivot register disease columns long ---------------------------------------

disease_long <- reg_combined %>%
  select(tb_id, any_of(disease_cols)) %>%
  pivot_longer(
    cols = -tb_id, 
    names_to = "original_column", 
    values_to = "raw_value",
    values_drop_na = TRUE
  ) %>%
  mutate(clean_value = str_squish(str_trim(str_to_lower(raw_value))))

## Join to lookup table and summarise -------------------------------------------

disease_classified_wide <- disease_long %>%
  left_join(disease_lookup, by = c("original_column", "clean_value")) %>% 
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
  select(-any_of(colnames(disease_classified_wide)[-1])) %>% 
  left_join(disease_classified_wide, by = "tb_id") %>%
  
  # Make sure that ptb_eptb is a categorical factor with levels
  mutate(ptb_eptb = factor(ptb_eptb, levels = c("PTB", "EPTB"))) %>%
  
  # Organize the new columns after joining
  relocate(any_of(disease_cols), has_any_disease_data, ptb_eptb, starts_with("type_"), starts_with("site_"), .after = date_start_clean)

message("-> Disease classification complete.")



# Registration Category from lookup ---------------------------------------

## Define category column sets --------------------------------------------

cat_cols <- c(
  "cat_new", "cat_relapse", "cat_failure", 
  "cat_rad", "cat_tf_in", "cat_others",
  "cat_ltfu_2018", "cat_unknown_2018", "cat_failure_dup"
)

# These are the clean columns we want in our final register
cat_template <- c(
  "registration_category" # The final single-choice factor
)

# 1. Define the "Master List" of valid category codes
valid_cat_codes <- c("new", "relapse", "failure", "ltfu", "tf_in", "other", "unknown", "nr")

## Load and prepare the category lookup file -----------------------

cat_lookup_path <- here("data-raw", "unique_registration_category_lookup.csv")
if (!file.exists(cat_lookup_path)) stop("! Category lookup not found.")

cat_lookup <- read_csv(cat_lookup_path, show_col_types = FALSE) %>%
  mutate(
    clean_value = str_squish(str_trim(str_to_lower(raw_value))),
    registration_category = str_trim(str_to_lower(registration_category))
  ) %>% 
  mutate(registration_category = if_else(
    registration_category %in% valid_cat_codes, 
    registration_category, 
    NA_character_
  ))

## Audit Registration Categories -------------------------------------------

export_unique_values_template(
  data = reg_combined,
  target_cols = cat_cols,
  output_path = file.path(current_dir, "unique_registration_category.csv"),
  template_cols = cat_template,
  lookup_data = cat_lookup
)

## Pivot register category columns long ------------------------------------

cat_long <- reg_combined %>%
  select(tb_id, any_of(cat_cols)) %>%
  pivot_longer(
    cols = -tb_id, 
    names_to = "original_column", 
    values_to = "raw_value",
    values_drop_na = TRUE
  ) %>%
  mutate(clean_value = str_squish(str_trim(str_to_lower(raw_value))))

## Join and Resolve -------------------------------------------------------

cat_classified_wide <- cat_long %>%
  left_join(cat_lookup, by = c("original_column", "clean_value")) %>%
  filter(!is.na(registration_category)) %>%
  group_by(tb_id) %>%
  summarize(
    
    # How many categories have been matched for the patient
    cat_count = n_distinct(registration_category),
    
    # Only give a category if one matches
    registration_category = case_when(
      cat_count == 1 ~ first(registration_category),
      cat_count  > 1 ~ NA_character_, # Conflict!
      TRUE           ~ NA_character_
    ),
    .groups = "drop"
  )

## Join results back to main register -------------------------------------

reg_combined <- reg_combined %>%
  select(-any_of("registration_category")) %>% 
  left_join(cat_classified_wide, by = "tb_id") %>%
  
  # Convert to Factor with Proper Labels
  mutate(registration_category = factor(
    registration_category, 
    levels = c("new", "relapse", "failure", "ltfu", "tf_in", "other", "unknown", "nr"),
    labels = c("New", "Relapse", "Treatment after failure", "Treatment after LTFU", 
               "Transfer in", "Other", "Unknown", "Not recorded")
  )) %>%
  
  # Organize
  select(-any_of("cat_count")) %>%
  relocate(cat_ltfu_2018, cat_unknown_2018, cat_failure_dup, registration_category, .after = cat_others)

message("-> Registration category classification complete.")



# Bacteriology columns (smear / xpert / culture) audit and classification ------------------

## Audit unique values ------------------------


# Smear 
bac_smear_cols <- c(
  # pre-2018 separate result fields
  "sm_0_result", "sm_2_result", "sm_5_result", "sm_6_result", "sm_8_result",
  # 2023 split result fields
  "sm_0_result_2023", "sm_2_result_2023", "sm_5_result_2023", "sm_end_result_2023"
)

# Xpert 
bac_xpert_cols <- c(
  "gx_0_result_2023"
)

# Culture
bac_culture_cols <- c(
  # 2023 split result fields
  "c_0_result_2023", "c_2_result_2023", "c_5_result_2023", "c_end_result_2023"
)

# Template
bac_template <- c(
  "test", "result", "detail"
)

# Lookup paths
lookup_smear  <- read_csv(here("data-raw", "unique_bac_smear_lookup.csv"), show_col_types = FALSE)
lookup_xpert  <- read_csv(here("data-raw", "unique_bac_xpert_lookup.csv"), show_col_types = FALSE)
lookup_culture   <- read_csv(here("data-raw", "unique_bac_culture_lookup.csv"), show_col_types = FALSE)

# Output paths
out_smear  <- file.path(current_dir, "unique_bac_smear.csv")
out_xpert  <- file.path(current_dir, "unique_bac_xpert.csv")
out_culture   <- file.path(current_dir, "unique_bac_culture.csv")

# Export audit files

export_unique_values_template(
  data        = reg_combined,
  target_cols = bac_smear_cols,
  output_path = out_smear,
  template_cols = bac_template,
  lookup_data = lookup_smear,
  counts      = TRUE,
  unique_scope = "global"
)

export_unique_values_template(
  data        = reg_combined,
  target_cols = bac_xpert_cols,
  output_path = out_xpert,
  template_cols = bac_template,
  lookup_data = lookup_xpert,
  counts      = TRUE,
  unique_scope = "global"
)

export_unique_values_template(
  data        = reg_combined,
  target_cols = bac_culture_cols,
  output_path = out_culture,
  template_cols = bac_template,
  lookup_data = lookup_culture,
  counts      = TRUE,
  unique_scope = "global"
)

message("-> Bacteriology unique values audit exported (smear/xpert/culture).")



## Classify bacteriology based on regex strings function and register data -----------------

# Regex strings were determined from the unique values audit
# Validated against lookup files for years were clean result is available
# Screened to ensure bacteriology results during 2018-2022 are properly classified

# Bacteriology column names
bac_cols_all <- keep(names(reg_combined), ~ str_detect(.x, "^(sm|gx|c)_"))
bac_cols_dx <- keep(bac_cols_all, ~ str_detect(.x, "_0_"))
bac_cols_mon <- keep(bac_cols_all, ~ str_detect(.x, "_2_"))
bac_cols_eot <- keep(bac_cols_all, ~ str_detect(.x, "_5_|_6_|_8_|_end"))

reg_combined <- reg_combined %>%

    # Build blobs (row-wise concatenation)
  mutate(
    bac_dx_blob = apply(select(., any_of(bac_cols_dx)), 1, function(row) paste(na.omit(row), collapse = " | ")),
    bac_eot_blob = apply(select(., any_of(bac_cols_eot)), 1, function(row) paste(na.omit(row), collapse = " | ")),
    bac_all_blob = apply(select(., any_of(bac_cols_all)), 1, function(row) paste(na.omit(row), collapse = " | "))
  ) %>%
  mutate(
    across(
      c(bac_dx_blob, bac_eot_blob, bac_all_blob),
      ~ str_squish(str_trim(str_to_lower(.x)))
    ),
    across(
      c(bac_dx_blob, bac_eot_blob, bac_all_blob),
      ~ if_else(.x == "", NA_character_, .x)
    )
  ) %>%
  
  # Classify blobs using function
  mutate(
    bac_dx_cat  = vapply(bac_dx_blob,  classify_bac_blob, character(1)),
    bac_eot_cat = vapply(bac_eot_blob, classify_bac_blob, character(1)),
    bac_all_cat = vapply(bac_all_blob, classify_bac_blob, character(1))
  ) %>%
  
  
  mutate(
    bc_reg_clean = str_squish(str_trim(str_to_lower(disease_site_bc_2017))),
    cd_reg_clean = str_squish(str_trim(str_to_lower(disease_site_cd_2017))),
    
    # Categorical clean register interpretation for auditing
    bc_cd_reg = case_when(
      is.na(bc_reg_clean) & is.na(cd_reg_clean) ~ NA_character_,
      
      # One side present, other missing
      bc_reg_clean %in% c("bc", "bd") & is.na(cd_reg_clean) ~ "bc",
      cd_reg_clean == "cd" & is.na(bc_reg_clean)            ~ "cd",
      
      # Both present and concordant
      bc_reg_clean %in% c("bc", "bd") & cd_reg_clean == "bc" ~ "bc", # rare, but handle
      bc_reg_clean == "cd"            & cd_reg_clean == "cd" ~ "cd",
      
      # Both present but discordant (e.g., bc vs cd)
      !is.na(bc_reg_clean) & !is.na(cd_reg_clean) & (
        (bc_reg_clean %in% c("bc", "bd") & cd_reg_clean == "cd") |
          (bc_reg_clean == "cd"            & cd_reg_clean %in% c("bc", "bd"))
      ) ~ "disc",
      
      # Anything else unusual but present
      TRUE ~ "disc"
    ),
    bc_cd_reg = factor(bc_cd_reg, levels = c("bc", "cd", "disc")),
    
    # Registration-derived binary BC flag
    bc_reg_bin = case_when(
      bc_reg_clean %in% c("bc", "bd") ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # Derived from blobs (binary BC flags)
    bc_dx_bin  = (bac_dx_cat  == "pos"),
    bc_all_bin = (bac_all_cat == "pos"),
    
    # Composite rules (BC if any evidence)
    bc_dx_comp_bin  = bc_dx_bin  | bc_reg_bin,
    bc_all_comp_bin = bc_all_bin | bc_reg_bin
  ) %>% 
  
  # organise columns
  relocate(
    disease_site_bc_2017, bc_reg_clean,
    disease_site_cd_2017, cd_reg_clean,
    bc_cd_reg,
    any_of(bac_cols_dx), any_of(bac_cols_mon), any_of(bac_cols_eot),
    bac_dx_blob, bac_dx_cat,
    bac_eot_blob, bac_eot_cat,
    bac_all_blob, bac_all_cat,
    bc_reg_bin, bc_dx_bin, bc_all_bin,
    bc_dx_comp_bin, bc_all_comp_bin,
    .after = registration_category
    )


message("-> 2017 BC/CD cleaning and comparison complete.")






# Batch Error Logging ------------------------------------

message("Batch auditing cleaning results...")

# Define what to check
cleaning_map <- tribble(
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



