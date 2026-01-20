# Title and Description --------------------------------------------
# Console runner for KI TB epi analysis outputs
# Purpose: run output functions interactively (no Quarto)
# Author: Jeremy Hill
# Last modified: 2026-01-19

# Packages --------------------------------------------------------
library(here)
library(tidyverse)
library(qs)

# Source output functions -----------------------------------------
source(here("R", "03_output_functions.R"))

# Load clean register ---------------------------------------------
ref_file <- here("data-processed", "current_register_reference.txt")
if (!file.exists(ref_file)) stop("Reference file missing. Run Script 01/02 first.")

current_ref <- readLines(ref_file, n = 1)
current_dir <- here("data-processed", current_ref)

clean_path <- file.path(current_dir, "register_combined_clean.qs")
if (!file.exists(clean_path)) stop("Clean register not found at: ", clean_path)

reg_clean <- qread(clean_path)

message("Loaded reg_clean: ", nrow(reg_clean), " rows, ", ncol(reg_clean), " cols")
message("reg_year range: ", paste(range(as.integer(reg_clean$reg_year), na.rm = TRUE), collapse = "–"))

# Load/build annual population denominators ------------------------
annual_pop_island_est <- get_annual_pop_island_est()

message("Loaded annual_pop_island_est: ", nrow(annual_pop_island_est), " rows")
message("year range: ", paste(range(as.integer(annual_pop_island_est$year), na.rm = TRUE), collapse = "–"))


# Demographic and disease table -----------------

# Build table (1998–2024, include NR in All)
demo_table_stoi <- build_demo_table_stoi(
  reg_clean = reg_clean,
  year_min  = 1998,
  year_max  = 2024
)

# Inspect in console
print(demo_table_stoi, n = Inf)

# Optional: view nicely in RStudio Viewer
if (requireNamespace("flextable", quietly = TRUE)) {
  flextable::flextable(demo_table_stoi)
}



# Build CNR tables -------------------------------------------------
cnr_tables <- build_cnr_tables(
  reg_clean = reg_clean,
  annual_pop_island_est = annual_pop_island_est,
  exclude_islands = c("kanton", "nr")
)

# Build CNR plots --------------------------------------------------
cnr_plots <- build_cnr_plots(
  cnr_tables = cnr_tables,
  exclude_current_year = TRUE
)

# Quick sanity checks ----------------------------------------------
message("CNR tables built:")
message(" - island:   ", nrow(cnr_tables$island))
message(" - division: ", nrow(cnr_tables$division))
message(" - st_oi:    ", nrow(cnr_tables$st_oi))

# Print plots to Viewer --------------------------------------------
print(cnr_plots$island)
print(cnr_plots$division)
print(cnr_plots$st_oi)

message("Done. Objects available: reg_clean, annual_pop_island_est, cnr_tables, cnr_plots")
