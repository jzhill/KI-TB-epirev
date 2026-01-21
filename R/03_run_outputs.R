# Title and Description --------------------------------------------
# Console runner for KI TB epi analysis outputs
# Purpose: Generate and save a full catalog of plots/tables
# Author: Jeremy Hill
# Last modified: 2026-01-20

# Packages --------------------------------------------------------
library(here)
library(tidyverse)
library(qs)
library(flextable)

# Setup Environment --------------------------------------------

source(here("R", "functions_all.R"))
source(here("R", "03_output_functions.R"))

# Directory logic: Create a dated catalog folder

ref_file <- here("data-processed", "current_register_reference.txt")
if (!file.exists(ref_file)) stop("Reference file missing. Run Script 01 first.")

current_ref <- readLines(ref_file, n = 1)
current_dir <- here("data-processed", current_ref)
outputs_dir <- here("outputs", current_ref)
if (!dir.exists(outputs_dir)) dir.create(outputs_dir, recursive = TRUE)



# Load Data
reg_clean <- qread(file.path(current_dir, "register_combined_clean.qs"))
annual_pop_island_est <- get_annual_pop_island_est()

year_min <- min(reg_clean$reg_year, na.rm = TRUE)
year_max <- max(reg_clean$reg_year, na.rm = TRUE)

message(">>> Starting Catalog Generation for ", current_ref)

# Demographics Table -------------------------------------------
message("-> Building Demographics Table...")
out_demo <- build_demo_table_stoi(reg_clean, year_min, year_max)

# Create a 'print-ready' version with a solid background for the PNG
ft_for_png <- out_demo$ft %>% 
  bg(bg = "white", part = "all")

save_as_docx(out_demo$ft, path = file.path(outputs_dir, "table_demographics.docx"))
save_as_image(ft_for_png, path = file.path(outputs_dir, "table_demographics.png"))

# CNR Tables and Plots -----------------------------------------
message("-> Building CNR Outputs...")
cnr_tables <- build_cnr_tables(reg_clean, annual_pop_island_est, exclude_geos = c("Kanton", "Not Recorded", "Unclassified"))
cnr_plots <- build_cnr_plots(cnr_tables, exclude_current_year = TRUE)

# Save CNR Plots
walk2(names(cnr_plots), cnr_plots, ~{
  ggsave(filename = file.path(outputs_dir, paste0("plot_cnr_", .x, ".png")),
         plot = .y, width = 10, height = 7, dpi = 300, bg = "white")
})

# Clinical & Demographic Proportions ----------------------
message("-> Building Proportion Trends...")

# PTB Proportions
ptb_data <- build_ptb_prop_tables(reg_clean)
ptb_plot <- plot_ptb_trends(ptb_data)

# BC Proportions
bc_data <- build_bc_prop_tables(reg_clean)
bc_plot <- plot_bc_trends(bc_data)

# Childhood (0-9) Proportions
child_data <- build_childhood_prop_tables(reg_clean)
child_plot <- plot_childhood_trends(child_data)

# Save
ggsave(file.path(outputs_dir, "plot_trend_ptb.png"), ptb_plot, width = 10, height = 6, dpi = 300, bg = "white")
ggsave(file.path(outputs_dir, "plot_trend_bc.png"), bc_plot, width = 10, height = 6, dpi = 300, bg = "white")
ggsave(file.path(outputs_dir, "plot_trend_childhood.png"), child_plot, width = 10, height = 6, dpi = 300, bg = "white")

# Monthly Notifications Analysis --------------------------
message("-> Building Monthly Notification Trends...")

# Define the window based on your study parameters
start_analysis <- paste0(year_min, "-01-01")
end_analysis   <- "2024-12-01" # Hard cap for current register year

# 1. Generate data
monthly_data <- build_monthly_trend_data(
  reg_clean, 
  start_date = start_analysis, 
  end_date = end_analysis
)

# 2. Build Plots
monthly_plot  <- plot_monthly_trend(monthly_data)
seasonal_plot <- plot_seasonal_subseries(monthly_data)

# 3. Save to catalog
ggsave(file.path(outputs_dir, "plot_trend_monthly_notifications.png"), monthly_plot, width = 12, height = 6, dpi = 300, bg = "white")
ggsave(file.path(outputs_dir, "plot_trend_seasonal.png"), seasonal_plot, width = 12, height = 6, dpi = 300, bg = "white")

# Display in console/viewer
print(monthly_plot)
print(seasonal_plot)

# 5. Finalize -----------------------------------------------------
message(">>> Catalog complete! Files saved to: ", outputs_dir)

# Open the folder (Windows specific - remove if on Mac/Linux)
shell.exec(outputs_dir)

