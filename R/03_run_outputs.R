# Title and Description --------------------------------------------
# Console runner for KI TB epi analysis outputs
# Purpose: Generate and save a full catalog of plots/tables
# Author: Jeremy Hill
# Last modified: 2026-02-08

# Packages --------------------------------------------------------
library(here)
library(tidyverse)
library(qs)
library(flextable)

# 1. Setup Environment --------------------------------------------

source(here("R", "functions_all.R"))
source(here("R", "03_output_functions.R"))

# Directory logic: Create a dated catalog folder
ref_file <- here("data-processed", "current_register_reference.txt")
if (!file.exists(ref_file)) stop("Reference file missing. Run Script 01 first.")

current_ref <- readLines(ref_file, n = 1)
dated_ref   <- paste0(format(Sys.Date(), "%y%m%d"), "_", current_ref)

current_dir  <- here("data-processed", current_ref)
outputs_dir  <- here("outputs", dated_ref)
if (!dir.exists(outputs_dir)) dir.create(outputs_dir, recursive = TRUE)

# 2. Load Data ----------------------------------------------------

# Load Cleaned Register
reg_clean <- qread(file.path(current_dir, "register_combined_clean.qs"))

# Load Master Denominators (The single hierarchical file from Goal 1)
pop_master <- get_annual_pop_est(
  path = here("data-processed", "annual_population_estimates.qs"),
  build_script = here("R", "04_annual_pop_est.R")
)

# Set global year range for the review
year_min <- 1998
year_max <- 2025

message(">>> Starting Catalog Generation for: ", dated_ref)

# 3. Demographics Table -------------------------------------------
message("-> Building Demographics Table...")
out_demo <- build_demo_table_stoi(reg_clean, year_min, year_max)

# Save to Word and PNG
save_as_docx(out_demo$ft, path = file.path(outputs_dir, "table_1_demographics.docx"))
# Use a white background for the image export
save_as_image(out_demo$ft %>% bg(bg = "white", part = "all"), 
              path = file.path(outputs_dir, "table_1_demographics.png"))

# 4. CNR Tables and Plots (The Master Loop) -----------------------
message("-> Building Master CNR Outputs...")

# Build all tables once (National, Division, OI_ST, Island, Council, Village)
cnr_master_list <- build_cnr_tables(
  reg_clean    = reg_clean, 
  pop_est_data = pop_master,
  geo_levels   = c("national", "division", "oi_st", "island", "council", "village")
)

# A. Generate Standard Plots (Total Rate, Facetted by Geo)
plots_total <- build_cnr_plots(cnr_master_list, sex_mode = "all", facet_by_geo = TRUE)

# B. Generate Sex-Split Plots (M vs F, National and Divisions only)
plots_sex <- build_cnr_plots(cnr_master_list, sex_mode = "split", facet_by_geo = TRUE)

# Save Plot Harvest
message("-> Saving CNR Plots...")
# Save Totals
walk2(names(plots_total), plots_total, ~{
  ggsave(filename = file.path(outputs_dir, paste0("plot_cnr_total_", .x, ".png")),
         plot = .y, width = 10, height = 7, dpi = 300, bg = "white")
})
# Save Sex-split
walk2(names(plots_sex), plots_sex, ~{
  ggsave(filename = file.path(outputs_dir, paste0("plot_cnr_sex_", .x, ".png")),
         plot = .y, width = 10, height = 7, dpi = 300, bg = "white")
})

# 5. Clinical Proportion Trends -----------------------------------
message("-> Building Proportion Trends (PTB, BC, Child)...")

prop_types <- c("ptb", "bc", "child")

# Use map() instead of walk() to return the plot objects into a list
proportion_plots <- map(set_names(prop_types), ~{
  
  # A. Generate the data and plot object
  data <- build_proportion_data(reg_clean, type = .x)
  p    <- plot_proportion_trends(data, type = .x)
  
  # B. Save to file (Side effect)
  ggsave(file.path(outputs_dir, paste0("plot_trend_", .x, ".png")), 
         plot = p, width = 10, height = 6, dpi = 300, bg = "white")
  
  # C. Return the plot object to the list
  return(p)
})

# 6. Monthly Notifications Analysis -------------------------------
message("-> Building Monthly Notification Trends...")

monthly_data <- build_monthly_trend_data(
  reg_clean, 
  start_date = paste0(year_min, "-01-01"), 
  end_date   = paste0(year_max, "-12-01")
)

# Monthly/Seasonal Plots
m_plot <- plot_monthly_trend(monthly_data)
s_plot <- plot_seasonal_subseries(monthly_data)

ggsave(file.path(outputs_dir, "plot_trend_monthly.png"), m_plot, width = 12, height = 6, dpi = 300, bg = "white")
ggsave(file.path(outputs_dir, "plot_trend_seasonal.png"), s_plot, width = 12, height = 6, dpi = 300, bg = "white")



# 7. Population Model Diagnostics -----------------------------------
message("-> Building Population Model Diagnostics...")

# Rederive the intermediate objects from the master population file
diag_island_shares  <- get_pop_diagnostics(pop_master, mode = "shares", target_geo = "island")
diag_village_shares <- get_pop_diagnostics(pop_master, mode = "shares", target_geo = "village")
diag_ratios         <- get_pop_diagnostics(pop_master, mode = "ratios", target_geo = "island")
diag_village_ratios <- get_pop_diagnostics(pop_master, mode = "ratios", target_geo = "village")

# A. Island Shares
p_island_share <- plot_pop_shares(diag_island_shares, level = "island", exclude_name = "south tarawa")
ggsave(file.path(outputs_dir, "diag_island_shares.png"), p_island_share, width = 12, height = 8)

# B. Village Shares
p_village_share <- plot_pop_shares(diag_village_shares, level = "village", exclude_name = "betio")
ggsave(file.path(outputs_dir, "diag_village_shares.png"), p_village_share, width = 12, height = 8)

# C. Sex Ratio Diagnostics (Combined call)
p_island_sex  <- plot_sex_ratio_diagnostics(diag_ratios, target_geo = "island")
p_village_sex <- plot_sex_ratio_diagnostics(diag_village_ratios, target_geo = "village")

ggsave(file.path(outputs_dir, "diag_island_sex_ratios.png"), p_island_sex, width = 12, height = 8)
ggsave(file.path(outputs_dir, "diag_village_sex_ratios.png"), p_village_sex, width = 12, height = 8)

# D. Capture objects for viewer
pop_diagnostics <- list(
  island_share = p_island_share,
  village_share = p_village_share,
  island_sex = p_island_sex,
  village_sex = p_village_sex
)


# 7. Finalize -----------------------------------------------------
message(">>> Catalog complete! Total outputs generated: ", length(list.files(outputs_dir)))
message(">>> Files saved to: ", outputs_dir)

# Open output folder for review
if (.Platform$OS.type == "windows") shell.exec(outputs_dir)