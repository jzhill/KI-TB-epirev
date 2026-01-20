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

raw_cols <- reg_combined_raw %>% colnames()


# ID and demographics ------------------

## Initial cleaning and tb_id -----------------------

reg_combined <- reg_combined_raw %>%

  # Remove monthly divider rows and any other empty/invalid rows
  filter(!is.na(name), !is.na(tb_no)) %>%
  
  # Ensure reg_year is numeric/integer for downstream joins/plots
  mutate(reg_year = suppressWarnings(as.integer(reg_year))) %>%
  
  # Create unique TB reg number
  mutate(
    tb_no_clean = format_tb_number(tb_no),
    tb_id = paste0(reg_year, "_", tb_no_clean)
  )
  
  
# Output skim to .csv

skim_results <- reg_combined %>% 
  skim() %>% 
  as_tibble()

write_csv(skim_results, file.path(current_dir, "data_dictionary_raw_skim.csv"))

## Dates using function ------------------------------------------

reg_combined <- reg_combined %>%
  mutate(
    date_reg_clean = parse_mixed_date(date_registered),
    date_start_clean = parse_mixed_date(date_started)
  ) 

## Sex ---------------

reg_combined <- reg_combined %>%
  mutate(

    # Standardize strings, then convert NA to "Not Recorded" level
    sex_clean = case_when(
      is.na(sex) | str_trim(as.character(sex)) == "" ~ NA_character_,
      str_to_upper(str_trim(as.character(sex))) %in% c("M", "MALE", "MN") ~ "Male",
      str_to_upper(str_trim(as.character(sex))) %in% c("F", "FEMALE")     ~ "Female",
      TRUE ~ "unclassified"
      ) %>% 
      factor(levels = c("Male", "Female", "unclassified"))
    )


## Age ------------------------

reg_combined <- reg_combined %>%
  mutate(
    age_trimmed = str_trim(as.character(age)),
    age_clean = case_when(
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
  ) %>% 
  
  select(-age_trimmed) # Clean up temporary helper column


## Age groups ------------------------------------------

# Set parameters for grouping

max_age_val <- max(reg_combined$age_clean, na.rm = TRUE)
upper_limit_10yr <- floor(max_age_val / 10) * 10
age_breaks_who   <- c(0, 5, 15, 25, 35, 45, 55, 65)
age_breaks_new   <- c(0, 5, 10, 15, 20, 25, 35, 45, 55, 65)

# Apply categorization
reg_combined <- reg_combined %>%
  mutate(
    # Create the raw epikit objects
    age_group_10yr_raw = age_categories(age_clean, lower = 0, upper = upper_limit_10yr, by = 10),
    age_group_who_raw  = age_categories(age_clean, breakers = age_breaks_who),
    age_group_new_raw  = age_categories(age_clean, breakers = age_breaks_new),
    
    # Apply the integrated cleaning and factorizing helper
    age_group_10yr = finalise_age_groups(age_group_10yr_raw, age_clean),
    age_group_who  = finalise_age_groups(age_group_who_raw, age_clean),
    age_group_new  = finalise_age_groups(age_group_new_raw, age_clean)
  ) %>%
  # Clean up the temporary raw columns
  select(-ends_with("_raw"))

## Demo columns ------------------

demo_cols <- c(
  "tb_id", "tb_no", "tb_no_clean", "reg_year", "month_ref",
  "date_registered", "date_reg_clean", "date_started", "date_start_clean",
  "name", "age", "age_clean", "age_group_10yr", "age_group_who", "age_group_new", "sex", "sex_clean"
)

demo_cols_clean <- c(
  "tb_id", "reg_year", "date_reg_clean", "date_start_clean", "name",
  "age_clean", "age_group_10yr", "age_group_who", "age_group_new", "sex_clean"
)




# Geography island/village ----------------------------

## Setup and Reference Data -----------------------------------------------

# File Paths
geo_lookup_path <- here("data-raw", "unique_geo_lookup.csv")
if (!file.exists(geo_lookup_path)) stop("! Geo lookup (csv) not found.")

# Load geo lookup table
geo_lookup <- read_csv(geo_lookup_path, show_col_types = FALSE) %>%
  mutate(clean_value = str_squish(str_trim(str_to_lower(raw_value)))) 

# Define Official Lists for Template
# Sorted by census code to preserve geographic/official order

geo_meta <- get_official_geo_metadata(here("data-raw", "geo_helper.xlsx"))

# Define column sets
# ORDER MATTERS: The first column is the highest priority for resolution
geo_cols_raw <- c("address")
# here are some other potential columns for later consideration
# geo_cols_raw <- c("address", "address_desc_2024", "treatment_unit", "school_2024", "card_location_2018")

geo_template <- c(
  "island", "island_search", "island_manual", 
  "ST_village", "ST_village_search", "ST_village_manual", 
  "NT_village", "NT_village_search", "NT_village_manual",
  paste0("is_", geo_meta$island_names),
  paste0("st_v_", geo_meta$st_village_names),
  paste0("nt_v_", geo_meta$nt_village_names)
)



## Audit geo columns ----------------------------------------------------

export_unique_values_template(
  data = reg_combined,
  target_cols = geo_cols_raw,
  output_path = file.path(current_dir, "unique_geo.csv"),
  template_cols = geo_template,
  lookup_data = geo_lookup
)


## Pivot register geo columns long ------------------------

geo_long <- reg_combined %>%
  select(tb_id, any_of(geo_cols_raw)) %>%
  pivot_longer(
    cols = -tb_id,
    names_to = "original_column",
    values_to = "raw_value",
    values_drop_na = TRUE
    ) %>%
  mutate(
    clean_value = str_squish(str_trim(str_to_lower(raw_value))),
    col_priority = factor(original_column, levels = geo_cols_raw, ordered = TRUE)
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
    geo_meta$census_codes %>%
      select(island_name, island_code, division = division_name, division_code) %>%
      distinct(), 
    by = c("island" = "island_name")
  ) %>% 
  
  mutate(
    
    # If island is "unclassified", ensure division and codes are also "unclassified" instead of NA
    across(c(division, division_code, island_code), 
           ~if_else(island == "unclassified", "unclassified", as.character(.))),
    
    # island_ST_bin: TRUE only for South Tarawa
    island_ST_bin = !is.na(island) & island == "south tarawa",
    
    # island_NT_bin: TRUE only for North Tarawa
    island_NT_bin = !is.na(island) & island == "north tarawa",
    
    # OI_ST: "ST" (South Tarawa), "OI" (Outer Islands), or "unclassified" (Unclassified)
    OI_ST = case_when(
      island == "south tarawa" ~ "ST",
      island == "unclassified" ~ "unclassified",
      !is.na(island)           ~ "OI", # All other matched islands are Outer Islands
      TRUE                     ~ NA_character_
    )
  ) %>%
  
  # Ensure geo codes are factors using the census code ordering
  mutate(
    division = factor(division, 
                      levels = c(geo_meta$division_names, "unclassified"),
                      # Deal with Line And Pheonix
                      labels = c(str_to_title(geo_meta$division_names) %>% str_replace(" And ", " and "), "Unclassified")),
    
    division_code = factor(division_code, levels = c(geo_meta$division_codes, "unclassified")),
    
    island = factor(island, 
                    levels = c(geo_meta$island_names, "unclassified"),
                    labels = c(str_to_title(geo_meta$island_names), "Unclassified")),
    
    island_code = factor(island_code, levels = c(geo_meta$island_codes, "unclassified")),
    
    ST_village = factor(ST_village, 
                        levels = c(geo_meta$st_village_names, "unclassified"),
                        labels = c(str_to_title(geo_meta$st_village_names), "Unclassified")),
    
    NT_village = factor(NT_village, 
                        levels = c(geo_meta$nt_village_names, "unclassified"),
                        labels = c(str_to_title(geo_meta$nt_village_names), "Unclassified")),
    
    OI_ST = factor(OI_ST, 
                   levels = c("ST", "OI", "unclassified"),
                   labels = c("South Tarawa", "Outer Islands", "Unclassified"))
  )

geo_cols <- c(
  "address", "treatment_unit", "card_location_2018", "address_desc_2024", "school_2024",
  "has_any_geo_data", "OI_ST", "division", "division_code", "island", "island_code",  
  "ST_village", "NT_village", "island_ST_bin", "island_NT_bin"
  )

geo_cols_clean <- c(
  "has_any_geo_data", "OI_ST", "division", "division_code", "island", "island_code",  
  "ST_village", "NT_village", "island_ST_bin", "island_NT_bin"
)




message("-> Geography classification complete.")





# PTB/EPTB and disease sites ------------------------

## Define disease column sets ----------------------------------------------------

disease_cols_raw <- c(
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
  target_cols = disease_cols_raw,
  output_path = file.path(current_dir, "unique_disease_sites.csv"),
  template_cols = disease_template,
  lookup_data = disease_lookup
)



## Pivot register disease columns long ---------------------------------------

disease_long <- reg_combined %>%
  select(tb_id, any_of(disease_cols_raw)) %>%
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
      any(ptb_eptb == "PTB", na.rm = TRUE)          ~ "PTB",
      any(ptb_eptb == "EPTB", na.rm = TRUE)         ~ "EPTB",
      any(ptb_eptb == "unclassified", na.rm = TRUE) ~ "unclassified",
      TRUE                                          ~ NA_character_
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
  mutate(ptb_eptb = factor(ptb_eptb, levels = c("PTB", "EPTB", "unclassified"))) 

disease_cols <- c(disease_cols_raw, "has_any_disease_data", disease_template)

disease_cols_clean <- c("has_any_disease_data", disease_template)




message("-> Disease classification complete.")



# Treatment Category ---------------------------------------

## Define category column sets --------------------------------------------

cat_cols_raw <- reg_combined_raw %>%
  select(starts_with("cat_")) %>%
  colnames()

# These are the clean columns we want in our final register
cat_template <- c(
  "cat_factor" # The final single-choice factor
)

# 1. Define the "Master List" of valid category codes
valid_cat_codes <- c("new", "relapse", "failure", "ltfu", "tf_in", "other", "unknown", "unclassified")

## Number of columns with data -------------------------

reg_combined <- reg_combined %>%
  
  # Binary flag showing if there is any category data across the columns
  mutate(
    has_any_cat_data = if_else(
      if_any(any_of(cat_cols_raw), ~ !is.na(.x) & .x != ""),
      TRUE,
      NA
    )
  ) %>% 
  
  # How many category columns have data
  mutate(
    cat_n = rowSums(
      !is.na(select(., any_of(cat_cols_raw))) &
        select(., any_of(cat_cols_raw)) != "",
      na.rm = TRUE
    ),
    
    # Replaces 0 counts with NA, so that we can use it for error logging
    cat_n = if_else(cat_n == 0, NA_integer_, cat_n)
  )


## Load and prepare the category lookup file -----------------------

cat_lookup_path <- here("data-raw", "unique_category_lookup.csv")
if (!file.exists(cat_lookup_path)) stop("! Category lookup not found.")

cat_lookup <- read_csv(cat_lookup_path, show_col_types = FALSE) %>%
  mutate(
    clean_value = str_squish(str_trim(str_to_lower(raw_value))),
    cat_factor = str_squish(str_trim(str_to_lower(cat_factor)))
  ) %>% 
  mutate(cat_factor = if_else(
    cat_factor %in% valid_cat_codes, 
    cat_factor, 
    NA_character_
  ))

## Audit Registration Categories -------------------------------------------

export_unique_values_template(
  data = reg_combined,
  target_cols = cat_cols_raw,
  output_path = file.path(current_dir, "unique_category.csv"),
  template_cols = cat_template,
  lookup_data = cat_lookup
)

## Pivot register category columns long ------------------------------------

cat_long <- reg_combined %>%
  select(tb_id, any_of(cat_cols_raw)) %>%
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
  filter(!is.na(cat_factor)) %>%
  group_by(tb_id) %>%
  summarize(
    
    # How many categories have been matched for the patient
    cat_n_mapped = n_distinct(cat_factor),
    
    # Only give a category if one matches
    cat_factor = case_when(
      cat_n_mapped == 1 ~ first(cat_factor),
      cat_n_mapped  > 1 ~ NA_character_, # Conflict!
      TRUE           ~ NA_character_
    ),
    .groups = "drop"
  )

## Join results back to main register -------------------------------------

reg_combined <- reg_combined %>%
  select(-any_of("cat_factor")) %>% 
  left_join(cat_classified_wide, by = "tb_id") %>%
  
  # Convert to Factor with Proper Labels
  mutate(cat_factor = factor(
    cat_factor, 
    levels = c("new", "relapse", "failure", "ltfu", "tf_in", "other", "unknown", "unclassified"),
    labels = c("New", "Relapse", "Treatment after failure", "Treatment after LTFU", 
               "Transfer in", "Other", "Unknown", "Unclassified")
  )) %>%
  
  # Remove
  select(-any_of("cat_n_mapped")) 


cat_cols <- c(cat_cols_raw, "has_any_cat_data", "cat_n", "cat_factor")

cat_cols_clean <- c("has_any_cat_data", "cat_n", "cat_factor")



message("-> Registration category classification complete.")



# Bacteriology ------------------

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
    bac_dx_cat  = factor(bac_dx_cat,  levels = c("pos", "neg", "unclassified")),
    bac_eot_cat = factor(bac_eot_cat, levels = c("pos", "neg", "unclassified")),
    bac_all_cat = factor(bac_all_cat, levels = c("pos", "neg", "unclassified"))
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
      ) ~ "unclassified",
      
      # Anything else unusual but present
      TRUE ~ "unclassified"
    ),
    bc_cd_reg = factor(bc_cd_reg, levels = c("bc", "cd", "unclassified")),
    
    # Registration-derived binary BC flag
    bc_reg_bin = case_when(
      bc_reg_clean %in% c("bc", "bd") ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # Derived from blobs (binary BC flags)
    bc_dx_bin  = coalesce(bac_dx_cat  == "pos", FALSE),
    bc_all_bin = coalesce(bac_all_cat == "pos", FALSE),
    
    # Composite rules (BC if any evidence)
    bc_dx_comp_bin  = bc_dx_bin  | bc_reg_bin,
    bc_all_comp_bin = bc_all_bin | bc_reg_bin,
    
    # Composite Factor Rules: Resolution of Diagnosis
    # Priority 1: Any BC evidence -> "bc"
    # Priority 2: Any "unclassified" signal (in either source) -> "unclassified"
    # Priority 3: CD evidence (and no BC/Unclassified signals) -> "cd"
    
    bc_cd_dx_comp = case_when(
      
      # If binary composite is TRUE, it's BC (Highest Priority)
      bc_dx_comp_bin == TRUE ~ "bc",
      
      # Check for "cd" or "neg" (If no BC and no Unclassified, it's CD)
      bc_cd_reg == "cd" | bac_dx_cat == "neg" ~ "cd",
      
      # Check for "unclassified" in either the register or the diagnostic blob
      bc_cd_reg == "unclassified" | bac_dx_cat == "unclassified" ~ "unclassified",
      
      # Default to NA if everything is blank
      TRUE ~ NA_character_
    ),
    
    bc_cd_all_comp = case_when(
      
      # Any BC evidence across the whole course of treatment
      bc_all_comp_bin == TRUE ~ "bc",
      
      # CD evidence
      bc_cd_reg == "cd" | bac_all_cat == "neg" ~ "cd",
      
      # Unclassified signals
      bc_cd_reg == "unclassified" | bac_all_cat == "unclassified" ~ "unclassified",
      
      TRUE ~ NA_character_
    )
  ) %>%
  
  # Convert to Factors with standard levels
  mutate(
    across(c(bc_cd_dx_comp, bc_cd_all_comp), 
           ~ factor(.x, levels = c("bc", "cd", "unclassified")))
  )


bac_cols <- c(
  "disease_site_bc_2017", "bc_reg_clean", "disease_site_cd_2017", "cd_reg_clean", "bc_cd_reg",
  bac_cols_dx, bac_cols_mon, bac_cols_eot,
  "bac_dx_blob", "bac_dx_cat", "bac_eot_blob", "bac_eot_cat", "bac_all_blob", "bac_all_cat",
  "bc_reg_bin", "bc_dx_bin", "bc_all_bin", "bc_dx_comp_bin", "bc_all_comp_bin",
  "bc_cd_dx_comp", "bc_cd_all_comp")

bac_cols_clean <- c(
  "bc_cd_reg", "bac_dx_cat", "bac_eot_cat", "bac_all_cat",
  "bc_reg_bin", "bc_dx_bin", "bc_all_bin", "bc_dx_comp_bin", "bc_all_comp_bin",
  "bc_cd_dx_comp", "bc_cd_all_comp")




message("-> 2017 BC/CD cleaning and comparison complete.")


# DRTB -------------------

## Classify based on raw column -------------------

reg_combined <- reg_combined %>%
  mutate(
    drtb_raw = str_squish(str_trim(str_to_lower(drtb_status_2018))),
    drtb_raw = na_if(drtb_raw, ""),
    
    # Strip trailing dates like "15/7/19" or "15-07-2019"
    drtb_raw = str_replace(
      drtb_raw,
      "\\s+\\d{1,2}[/\\-]\\d{1,2}[/\\-]\\d{2,4}$",
      ""
    ),
    
    # Classify
    drtb_factor = case_when(
      
      #NA is NA
      is.na(drtb_raw) ~ NA_character_,
      
      # Explicit DS-TB / no resistance
      drtb_raw %in% c("n", "no", "none", "nil", "neg", "negative", "dstb", "ds-tb", "ds") ~ "dstb",
      
      # Explicit unknown / unclear
      drtb_raw %in% c("u", "unknown", "nr", "not recorded") ~ "nr",
      
      # Any RR / MDR / XDR signal
      str_detect(drtb_raw, "\\b(rr|mdr|xdr)\\b|rr\\s*/\\s*mdr|rr\\s+mdr") ~ "drtb",
      
      # Catch common shorthand variants
      drtb_raw %in% c("(r/r)", "r/r") ~ "drtb",
      
      # Anything else mark as NA
      TRUE ~ "unclassified"
    ),
    
    # Stable factor
    drtb_factor = factor(
      drtb_factor,
      levels = c("dstb", "drtb", "nr", "unclassified")
    )
  )



## Search across raw columns and combine ----------------

# Regex: aims to catch RR/MDR-style signals while avoiding "Dr / Dr."
drtb_pattern <- paste(
  c(
    "\\bdrtb\\b",
    "\\bdrug\\s*resistan\\w*\\b",           # drug resistant / resistance / resistant
    "\\bmdr\\b", "\\bmdr\\s*tb\\b", "\\bmdr[-\\s]?tb\\b",
    "\\bxdr\\b", "\\bxdr[-\\s]?tb\\b",
    "\\brr\\b", "\\brr\\s*tb\\b", "\\brr[-\\s]?tb\\b",
    "\\brr\\s*/\\s*mdr\\b", "\\brr\\s+mdr\\b",
    "\\brif\\s*resistan\\w*\\b",            # rif resistant / resistance
    "\\brif\\s*resist\\b",                  # common shorthand
    "\\brifampicin\\s*resistan\\w*\\b"
  ),
  collapse = "|"
)

reg_combined <- reg_combined %>%
  tidyr::unite(
    col = "drtb_search_blob",
    dplyr::any_of(raw_cols),
    sep = " | ",
    na.rm = TRUE,
    remove = FALSE
  ) %>%
  mutate(
    drtb_search_blob = str_squish(str_trim(str_to_lower(drtb_search_blob))),
    drtb_search_blob = na_if(drtb_search_blob, ""),
    
    # binary flag
    drtb_search_bin = if_else(
      !is.na(drtb_search_blob) & str_detect(drtb_search_blob, drtb_pattern),
      TRUE,
      FALSE,
      missing = FALSE
    )
  ) %>%
  mutate(
    drtb_comp_bin = (drtb_factor == "drtb") | drtb_search_bin
  )

drtb_cols <- c("drtb_status_2018", "drtb_factor", "drtb_search_bin", "drtb_comp_bin")

drtb_cols_clean <- c("drtb_factor", "drtb_comp_bin")




# Regimen -----------------------------------

## Setup -----------------

reg_combined <- reg_combined %>%
  mutate(regimen_clean = str_squish(str_trim(str_to_lower(regimen))))


regimen_template <- c(
  "regimen_factor",  # cat1 / cat2 / cat3
  "regimen_note"    # modifiers, toxicity, free text
)

valid_regimen_codes <- c("cat1", "cat2", "cat3", "unclassified")

regimen_lookup_path <- here("data-raw", "unique_regimen_lookup.csv")
if (!file.exists(regimen_lookup_path)) stop("! Regimen lookup not found.")

regimen_lookup <- read_csv(regimen_lookup_path, show_col_types = FALSE) %>%
  mutate(
    regimen_factor = str_squish(str_trim(str_to_lower(regimen_factor))),
    regimen_factor = if_else(regimen_factor %in% valid_regimen_codes, regimen_factor, NA_character_)
  )

## Create audit file -----------------

export_unique_values_template(
  data = reg_combined,
  target_cols = c("regimen_clean"),
  output_path = file.path(current_dir, "unique_regimen.csv"),
  template_cols = regimen_template,
  lookup_data = regimen_lookup,
  counts = TRUE,
  unique_scope = "global"
)

message("-> Regimen audit exported.")

## Join from lookup and tidy up --------------------

reg_combined <- reg_combined %>%
  select(-any_of(regimen_template)) %>%  # idempotent
  left_join(
    regimen_lookup %>% select(clean_value, any_of(regimen_template)),
    by = c("regimen_clean" = "clean_value")
  ) %>% 
  mutate(

    # Regimen factor with labels
    regimen_factor = factor(
      regimen_factor,
      levels = valid_regimen_codes,
      labels = c("Category 1", "Category 2", "Category 3", "Unclassified")
    )
  )


regimen_cols <- c("regimen", "regimen_clean", regimen_template)

regimen_cols_clean <- regimen_template




# Treatment outcomes -----------------

# Identify outcome columns
outcome_cols_raw <- reg_combined_raw %>%
  select(starts_with("outcome_")) %>%
  colnames()

# Define valid outcome codes
valid_outcome_codes <- c("cured", "completed", "failed", "died", "ltfu", "tf_out", "ne", "sld", "unclassified")

# Flag: any outcome data present 
reg_combined <- reg_combined %>%
  mutate(
    has_any_outcome_data = if_else(
      if_any(any_of(outcome_cols_raw), ~ !is.na(.x) & .x != ""),
      TRUE,
      NA
    )
  )


## Load and prepare the outcome lookup file -----------------------

outcome_lookup_path <- here("data-raw", "unique_outcome_lookup.csv")
if (!file.exists(outcome_lookup_path)) stop("! Outcome lookup not found.")

outcome_lookup <- read_csv(outcome_lookup_path, show_col_types = FALSE) %>%
  mutate(
    outcome_factor_manual = str_squish(str_trim(str_to_lower(outcome_factor_manual))),
    outcome_factor_manual = if_else(outcome_factor_manual %in% valid_outcome_codes, outcome_factor_manual, NA_character_)
  )

## Pivot outcomes long --------------------------------------------

outcome_long <- reg_combined %>%
  select(tb_id, any_of(outcome_cols_raw)) %>%
  pivot_longer(
    cols = any_of(outcome_cols_raw),
    names_to = "outcome_col",
    values_to = "outcome_val",
    values_drop_na = TRUE
  ) %>%
  mutate(
    outcome_val = str_squish(str_trim(str_to_lower(as.character(outcome_val)))),
    outcome_val = na_if(outcome_val, ""),
    
    # outcome code from column name (raw)
    outcome_code_raw = str_remove(outcome_col, "^outcome_"),
    
    # harmonise codes to your valid set
    outcome_factor_from_col = case_when(
      outcome_code_raw == "cured"              ~ "cured",
      outcome_code_raw == "complete"           ~ "completed",
      outcome_code_raw == "failure"            ~ "failed",
      outcome_code_raw == "died"               ~ "died",
      outcome_code_raw == "ltfu"               ~ "ltfu",
      outcome_code_raw == "tf_out"             ~ "tf_out",
      outcome_code_raw == "not_evaluated_2017" ~ "ne",
      outcome_code_raw == "sld_2018"           ~ "sld",
      TRUE                                     ~ NA_character_
    ),
    
    # attempt to parse the cell content as a date
    outcome_date_parsed = parse_mixed_date(outcome_val),
    
    # for audit key construction
    outcome_part = paste0(outcome_col, ": ", outcome_val)
  ) %>%
  filter(!is.na(outcome_val))  # keep only non-blank values


## Build per-patient key for manual audit ------------------------------

outcome_key_df <- outcome_long %>%
  group_by(tb_id) %>%
  summarise(
    outcome_key = paste(sort(unique(outcome_part)), collapse = " | "),
    .groups = "drop"
  )

reg_combined <- reg_combined %>%
  select(-any_of("outcome_key")) %>%
  left_join(outcome_key_df, by = "tb_id")


## Auto-assign ONLY when: outcome_n == 1 AND date parses ----------------
## (and the column name implies the outcome type)

outcome_auto_df <- outcome_long %>%
  group_by(tb_id) %>%
  summarise(
    outcome_n = n(),  # number of non-blank outcome cells across outcome_* cols
    
    outcome_factor = if_else(
      outcome_n == 1 & !is.na(first(outcome_date_parsed)),
      first(outcome_factor_from_col),
      NA_character_
    ),
    
    outcome_date = if_else(
      outcome_n == 1 & !is.na(first(outcome_date_parsed)),
      as.Date(first(outcome_date_parsed)),
      as.Date(NA)
    ),
    .groups = "drop"
  )

reg_combined <- reg_combined %>%
  select(-any_of(c("outcome_n", "outcome_factor", "outcome_date"))) %>%  # idempotent
  left_join(outcome_auto_df, by = "tb_id")



## Unique keys that didn't parse to lookup ----------------

# - multiple outcomes recorded OR
# - single outcome but raw is not parseable as a date

outcome_manual_df <- reg_combined %>%
  filter(
    outcome_n > 1 |
      (outcome_n == 1 & !is.na(outcome_key) & is.na(outcome_date))
  )

export_unique_values_template(
  data = outcome_manual_df,
  target_cols = c("outcome_key"),
  output_path = file.path(current_dir, "unique_outcome.csv"),
  template_cols = c("outcome_factor_manual", "outcome_date_manual"),
  lookup_data = outcome_lookup,
  counts = TRUE,
  unique_scope = "global"
)

message("-> Outcome manual audit exported for rows with outcome_n > 1 or unparsed single-outcome text.")

## Join from lookup and tidy up --------------------

reg_combined <- reg_combined %>%
  select(-any_of(c("outcome_factor_manual", "outcome_date_manual"))) %>%  # idempotent
  left_join(
    outcome_lookup %>%
      select(
        clean_value,
        outcome_factor_manual,
        outcome_date_manual
      ),
    by = c("outcome_key" = "clean_value")
  ) %>% 
  mutate(
    outcome_factor = case_when(
      !is.na(outcome_factor_manual) ~ outcome_factor_manual,
      !is.na(outcome_factor)        ~ outcome_factor,
      !is.na(outcome_key)           ~ "unclassified",
      TRUE                          ~ NA_character_
    ),
    outcome_date = case_when(
      !is.na(outcome_date_manual)      ~ parse_mixed_date(outcome_date_manual),
      !is.na(outcome_date)             ~ outcome_date,
      TRUE                             ~ NA_Date_
    )
  ) %>%
  mutate(
    # Ensure Date type (idempotent)
    outcome_date = as.Date(outcome_date),
    
    # Outcome factor with labels
    outcome_factor = factor(
      outcome_factor,
      levels = valid_outcome_codes,
      labels = c(
        "Cured",
        "Treatment completed",
        "Treatment failed",
        "Died",
        "Lost to follow-up",
        "Transferred out",
        "Not evaluated",
        "Second-line drugs",
        "Unclassified"
      )
    )
  ) %>%
  select(-any_of(c("outcome_factor_manual", "outcome_date_manual"))) 


outcome_cols <- c(outcome_cols_raw, "has_any_outcome_data", "outcome_n", "outcome_key", "outcome_factor", "outcome_date")

outcome_cols_clean <- c("has_any_outcome_data", "outcome_n", "outcome_factor", "outcome_date")




# Organise columns ---------------------------



final_order <- c(
  demo_cols,
  geo_cols,
  disease_cols,
  regimen_cols,
  cat_cols,
  bac_cols,
  drtb_cols,
  outcome_cols
)

reg_combined <- reg_combined %>%
  select(any_of(final_order), everything())


ungrouped_cols <- setdiff(names(reg_combined), final_order)
ungrouped_cols

# Create clean register dataframe --------------------

reg_clean_cols <- c(
  demo_cols_clean,
  geo_cols_clean,
  disease_cols_clean,
  regimen_cols_clean,
  cat_cols_clean,
  bac_cols_clean,
  drtb_cols_clean,
  outcome_cols_clean
)

reg_clean <- reg_combined %>% 
  select(any_of(reg_clean_cols))


# Batch Error Logging ------------------------------------

message("Batch auditing cleaning results...")

# Define what to check
cleaning_map <- tribble(
  ~raw,                     ~clean,             ~msg,
  "date_registered",        "date_reg_clean",   "Date invalid or outside range",
  "date_started",           "date_start_clean", "Date invalid or outside range",
  "age",                    "age_clean",        "Unrecognized age format",
  "age_clean",              "age_group_new",    "Age category unassigned",
  "sex",                    "sex_clean",        "Unknown sex code",
  
  # Geography Audits
  "address",                "island",           "Address string not found in lookup table",
  "island",                 "division",         "Island name not found in census lookup",

  # Disease Classification Audits
  "disease_site",           "ptb_eptb",         "Disease string missing from lookup (Site)",
  "disease_site_pulm_2018", "ptb_eptb",         "Disease string missing from lookup (Pulm 2018)",
  "disease_site_ep_2018",   "ptb_eptb",         "Disease string missing from lookup (EP 2018)",
  
  "cat_n",                  "cat_factor",       "Treatment category not assigned from lookup",
  "outcome_key",            "outcome_factor",   "Outcome category not assigned from columns",
  "outcome_key",            "outcome_date",     "Outcome date not assigned from columns",
  "regimen",                "regimen_factor",   "Regimen not assigned from lookup",
  "drtb_status_2018",       "drtb_factor",      "DRTB status not assigned from column"
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
qsave(reg_clean, file.path(current_dir, "register_combined_clean.qs"))
write_csv(reg_clean, file.path(current_dir, "register_combined_clean.csv"))

message("Cleaning complete for: ", current_ref)



