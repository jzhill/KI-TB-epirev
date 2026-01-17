# Geography Inference Helper Script
# Use this to auto-suggest islands/villages based on synonyms
# Optimization: Vectorized Pattern Matching

library(here)
library(tidyverse)
library(readxl)



# 1. Setup & Load ---------------------------------------------------------

ref_file <- here("data-processed", "current_register_reference.txt")
if (!file.exists(ref_file)) stop("Reference file missing. Run Script 01 first.")

current_ref <- readLines(ref_file, n = 1)
current_dir <- here("data-processed", current_ref)


# File Paths
geo_lookup_path <- here("data-raw", "unique_geo_lookup.csv")
geo_helper_path <- here("data-raw", "geo_helper.xlsx")

if (!file.exists(geo_lookup_path)) stop("! Geo lookup (csv) not found.")
if (!file.exists(geo_helper_path)) stop("! Geo helper (xlsx) not found.")

# Load geo lookup table
geo_lookup <- read_csv(geo_lookup_path, show_col_types = FALSE) %>%
  mutate(
    clean_value = str_trim(str_to_lower(raw_value)),
    across(contains("_manual"), ~str_trim(str_to_lower(as.character(.))))
    )

# Load string search table
string_lookup <- read_excel(geo_helper_path, sheet = "lookup_table") %>%
  rename(search_term = 1, official_name = 2, category = 3) %>%
  mutate(across(everything(), ~str_trim(str_to_lower(.))))

# Load Census Reference
census_codes <- read_excel(geo_helper_path, sheet = "census_codes") %>%
  mutate(across(everything(), ~str_trim(str_to_lower(as.character(.)))))



# 2. Search string processing using function -----------------------------

# Function to apply search strings to unique addresses in lookup file
process_binary_flags <- function(data, category_filter, prefix) {
  targets <- string_lookup %>% filter(category == category_filter)
  official_names <- unique(targets$official_name)
  
  for (name in official_names) {
    col_name <- paste0(prefix, name)
    syns <- targets %>% filter(official_name == name) %>% pull(search_term)
    
    # Vectorized detection (much faster than rowwise)
    pattern <- paste(str_escape(syns), collapse = "|")
    data[[col_name]] <- as.integer(str_detect(data$clean_value, pattern))
  }
  return(data)
}

# Implement function for each level of geography
geo_processed <- geo_lookup %>%
  process_binary_flags("island", "is_") %>%
  process_binary_flags("st village", "st_v_") %>%
  process_binary_flags("nt village", "nt_v_")



# 3. Fast Hyphenation & Smart Inference ----------------------------------

# Define binary columns
is_cols <- grep("^is_", names(geo_processed), value = TRUE)
st_cols <- grep("^st_v_", names(geo_processed), value = TRUE)
nt_cols <- grep("^nt_v_", names(geo_processed), value = TRUE)

# Define Official Lists for checking entries
island_names     <- sort(unique(census_codes$island_name))
st_village_names <- census_codes %>% filter(island_name == "south tarawa") %>% pull(village_name) %>% unique() %>% sort()
nt_village_names <- census_codes %>% filter(island_name == "north tarawa") %>% pull(village_name) %>% unique() %>% sort()

geo_processed <- geo_processed %>%
  rowwise() %>%
  mutate(
    
    # Concatenated from the binary flags found in the text
    island_search = paste(sort(str_remove(is_cols[c_across(all_of(is_cols)) == 1], "is_")), collapse = "-"),
    ST_village_search = paste(sort(str_remove(st_cols[c_across(all_of(st_cols)) == 1], "st_v_")), collapse = "-"),
    NT_village_search = paste(sort(str_remove(nt_cols[c_across(all_of(nt_cols)) == 1], "nt_v_")), collapse = "-")
  ) %>%
  ungroup() %>%
  
  mutate(
    
    across(ends_with("_search"), ~na_if(., "")),
  
    # Island allocation based on search and manual inputs
    
    island = case_when(
      # Manual entry (including "nr") is priority
      !is.na(island_manual) & (island_manual %in% c(island_names, "nr")) ~ as.character(island_manual),
      
      # Single direct match in search (no hyphens allowed)
      !is.na(island_search) & !str_detect(island_search, "-") ~ island_search,
      
      # Inference - Search is blank, but villages were found
      is.na(island_search) & !is.na(ST_village_search) & !is.na(NT_village_search)  ~ "",
      is.na(island_search) & !is.na(ST_village_search)  ~ "south tarawa",
      is.na(island_search) & !is.na(NT_village_search) ~ "north tarawa",
      
      # Default: Leave empty for manual review
      TRUE ~ ""
    ),
    
    # Village allocation based on search and manual inputs
    # Constraint: Villages only appear if the Island matches the parent group
    
    ST_village = case_when(
      island != "south tarawa" ~ "",
      !is.na(ST_village_manual) & (ST_village_manual %in% st_village_names) ~ as.character(ST_village_manual),
      !is.na(ST_village_search) & !str_detect(ST_village_search, "-") ~ ST_village_search,
      TRUE ~ ""
    ),
    
    NT_village = case_when(
      island != "north tarawa" ~ "" ,
      !is.na(NT_village_manual) & (NT_village_manual %in% nt_village_names) ~ as.character(NT_village_manual),
      !is.na(NT_village_search) & !str_detect(NT_village_search, "-") ~ NT_village_search,
      TRUE ~ ""
    )
  )

# 4. Save for Review ------------------------------------------------------
write_csv(
  geo_processed,
  file.path(current_dir, "unique_geo_lookup_processed.csv"),
  na = ""
  )



