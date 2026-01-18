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

# ID and demographics ------------------

## Initial cleaning and tb_id -----------------------

reg_combined <- reg_combined_raw %>%

  # Remove monthly divider rows and any other empty/invalid rows
  filter(!is.na(name), !is.na(tb_no)) %>%
  
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

## Dates age and sex ------------------------------------------

reg_combined <- reg_combined %>%
  mutate(
    # Dates
    date_reg_clean = parse_mixed_date(date_registered),
    date_start_clean = parse_mixed_date(date_started),
    
    # Age
    age_clean = clean_age_column(age),
    
    # Sex
    sex_clean = clean_sex_column(sex)
  ) 

## Age groups ------------------------------------------

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
    
  ) 

demo_cols <- c(
  "tb_id", "tb_no", "tb_no_clean", "reg_year", "month_ref",
  "date_registered", "date_reg_clean", "date_started", "date_start_clean",
  "name", "age", "age_clean", "age_group_10yr", "age_group_who", "age_group_new", "sex", "sex_clean"
)




# Geography island/village ----------------------------

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
geo_cols_raw <- c("address")
# here are some other potential columns for later consideration
# geo_cols_raw <- c("address", "address_desc_2024", "treatment_unit", "school_2024", "card_location_2018")

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

geo_cols <- c(
  "address", "treatment_unit", "card_location_2018", "address_desc_2024", "school_2024",
  "has_any_geo_data", "ST_OI", "division", "division_code", "island", "island_code",  
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
  mutate(ptb_eptb = factor(ptb_eptb, levels = c("PTB", "EPTB"))) 

disease_cols <- c(disease_cols_raw, "has_any_disease_data", disease_template)

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
valid_cat_codes <- c("new", "relapse", "failure", "ltfu", "tf_in", "other", "unknown", "nr")

## Number of columns with data -------------------------

reg_combined <- reg_combined %>%
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
    levels = c("new", "relapse", "failure", "ltfu", "tf_in", "other", "unknown", "nr"),
    labels = c("New", "Relapse", "Treatment after failure", "Treatment after LTFU", 
               "Transfer in", "Other", "Unknown", "Not recorded")
  )) %>%
  
  # Remove
  select(-any_of("cat_n_mapped")) 


cat_cols <- c(cat_cols_raw, "cat_n", "cat_factor")

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
  ) 

bac_cols <- c(
  "disease_site_bc_2017", "bc_reg_clean", "disease_site_cd_2017", "cd_reg_clean", "bc_cd_reg",
  bac_cols_dx, bac_cols_mon, bac_cols_eot,
  "bac_dx_blob", "bac_dx_cat", "bac_eot_blob", "bac_eot_cat", "bac_all_blob", "bac_all_cat",
  "bc_reg_bin", "bc_dx_bin", "bc_all_bin", "bc_dx_comp_bin", "bc_all_comp_bin")


message("-> 2017 BC/CD cleaning and comparison complete.")



# Regimen -----------------------------------

## Setup -----------------

reg_combined <- reg_combined %>%
  mutate(regimen_clean = str_squish(str_trim(str_to_lower(regimen))))


regimen_template <- c(
  "regimen_factor",  # cat1 / cat2 / cat3
  "regimen_note"    # modifiers, toxicity, free text
)

valid_regimen_codes <- c("cat1", "cat2", "cat3")

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
      labels = c("Category 1", "Category 2", "Category 3")
    )
  )


regimen_cols <- c("regimen", "regimen_clean", regimen_template)



# Treatment outcomes -----------------

# Identify outcome columns
outcome_cols_raw <- reg_combined_raw %>%
  select(starts_with("outcome_")) %>%
  colnames()

# Define valid outcome codes
valid_outcome_codes <- c("cured", "completed", "failed", "died", "ltfu", "tf_out", "ne", "sld")


## Load and prepare the outcome lookup file -----------------------

outcome_lookup_path <- here("data-raw", "unique_outcome_lookup.csv")
if (!file.exists(outcome_lookup_path)) stop("! Outcome lookup not found.")

outcome_lookup <- read_csv(outcome_lookup_path, show_col_types = FALSE) %>%
  mutate(
    outcome_factor_manual = str_squish(str_trim(str_to_lower(outcome_factor_manual))),
    outcome_factor_manual = if_else(outcome_factor_manual %in% valid_outcome_codes, outcome_factor_manual, NA_character_)
  )

## Counts and keys from multiple columns ---------------------------

# Counts
reg_combined <- reg_combined %>%
  mutate(
    outcome_n = rowSums(
      !is.na(select(., any_of(outcome_cols_raw))) &
        select(., any_of(outcome_cols_raw)) != "",
      na.rm = TRUE
    ),
    outcome_n = if_else(outcome_n == 0, NA_integer_, outcome_n)
  )

# Build per-patient keyed string: "outcome_x: <value> | outcome_y: <value>"
outcome_key_df <- reg_combined %>%
  select(tb_id, any_of(outcome_cols_raw)) %>%
  pivot_longer(
    cols = any_of(outcome_cols_raw),
    names_to = "outcome_col",
    values_to = "outcome_val",
    values_drop_na = TRUE
  ) %>%
  mutate(
    outcome_val = str_squish(str_trim(str_to_lower(outcome_val))),
    outcome_val = if_else(outcome_val == "", NA_character_, outcome_val),
    outcome_part = paste0(outcome_col, ": ", outcome_val)
  ) %>%
  filter(!is.na(outcome_val)) %>%
  group_by(tb_id) %>%
  summarise(
    outcome_key = paste(sort(unique(outcome_part)), collapse = " | "),
    .groups = "drop"
  )

# Join back to register
reg_combined <- reg_combined %>%
  select(-any_of("outcome_key")) %>%
  left_join(outcome_key_df, by = "tb_id")


## Assign outcome from correctly filled column ----------------------

# ONLY when outcome_n == 1 and cell parses as a date

reg_combined <- reg_combined %>%
  mutate(
    outcome_date = if_else(
      outcome_n == 1,
      apply(select(., any_of(outcome_cols_raw)), 1, function(row) {
        x <- row[!is.na(row) & row != ""]
        if (length(x) == 1) as.character(x) else NA_character_
      }),
      NA_character_
    ),
    outcome_factor = if_else(
      outcome_n == 1,
      apply(select(., any_of(outcome_cols_raw)), 1, function(row) {
        nm <- outcome_cols_raw[!is.na(row) & row != ""]
        if (length(nm) == 1) str_remove(nm, "^outcome_") else NA_character_
      }),
      NA_character_
    ),
    outcome_factor = case_when(
      outcome_factor == "cured" ~ "cured",
      outcome_factor == "complete" ~ "completed",
      outcome_factor == "failure" ~ "failed",
      outcome_factor == "died" ~ "died",
      outcome_factor == "ltfu" ~ "ltfu",
      outcome_factor == "tf_out" ~ "tf_out",
      outcome_factor == "not_evaluated_2017" ~ "ne",
      outcome_factor == "sld_2018" ~ "sld",
      TRUE ~ NA_character_
    ),
    outcome_date = parse_mixed_date(outcome_date),
    
    # Only keep the auto-assignment if date parsed successfully
    outcome_factor = if_else(outcome_n == 1 & !is.na(outcome_date), outcome_factor, NA_character_)
  )

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
        "Second-line drugs"
      )
    )
  ) %>%
  select(-any_of(c("outcome_factor_manual", "outcome_date_manual"))) 


outcome_cols <- c(outcome_cols_raw, "outcome_n", "outcome_key", "outcome_factor", "outcome_date")



# Organise columns ---------------------------

final_order <- c(
  demo_cols,
  geo_cols,
  disease_cols,
  regimen_cols,
  cat_cols,
  bac_cols,
  outcome_cols
)

reg_combined <- reg_combined %>%
  select(any_of(final_order), everything())


ungrouped_cols <- setdiff(names(reg_combined), final_order)
ungrouped_cols







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
  "disease_site_ep_2018",   "ptb_eptb",         "Disease string missing from lookup (EP 2018)",
  
  "cat_n",                  "cat_factor",       "Treatment category not assigned from lookup",
  "outcome_key",            "outcome_factor",   "Outcome category not assigned from columns",
  "outcome_key",            "outcome_date",     "Outcome date not assigned from columns"
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



