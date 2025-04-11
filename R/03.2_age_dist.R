# Title and Description --------------------------------------------

# Analysis script: Age Distribution of TB Cases Over Time
# Generates 100% stacked bar chart by 10-year age groups, faceted by location.
# Author: Jeremy Hill
# Date commenced: 11/4/2025 # Adjust date
# Last modified: 11/4/2025 # Adjust date

# Packages --------------------------------------------------------

library(here)
library(tidyverse)
library(ggplot2)
library(scales)

# Load Cleaned TB Data (Check Environment First) -------------

message("Checking for 'tb_register_combined' in environment or loading from file...")
cleaned_data_object_name <- "tb_register_combined"
if (exists(cleaned_data_object_name, where = .GlobalEnv) &&
    is.data.frame(get(cleaned_data_object_name, envir = .GlobalEnv))) {
  message("-> Found '", cleaned_data_object_name, "' in environment. Using existing object.")
  tb_data <- get(cleaned_data_object_name, envir = .GlobalEnv) %>% as_tibble()
} else {
  message("-> '", cleaned_data_object_name, "' not found. Loading from latest cleaned RDS file...")
  processed_data_dir <- here::here("data-processed")
  rds_files <- list.files(processed_data_dir, pattern = "^tb_register_cleaned_\\d{6}\\.rds$", full.names = TRUE)
  if (length(rds_files) == 0) stop("Error: No cleaned RDS file found.")
  date_strings_rds <- stringr::str_match(basename(rds_files), "_(\\d{6})\\.rds$")[, 2]
  file_dates_rds <- as.Date(date_strings_rds[!is.na(date_strings_rds)], format = "%y%m%d")
  if (length(file_dates_rds) == 0) stop("Error: No valid dates found in filenames.")
  latest_rds_file <- rds_files[!is.na(date_strings_rds)][which.max(file_dates_rds)]
  message("-> Loading file: ", basename(latest_rds_file))
  tb_data <- readRDS(latest_rds_file)
  message("-> Data loaded successfully.")
}
if (!exists("tb_data") || !is.data.frame(tb_data)) {
  stop("Error: Failed to load or find a valid cleaned TB data frame.")
}

# Prepare Data for Age Distribution Plot --------------------------

message("\nPreparing data for age distribution plot...")

# Check for required columns
required_cols_plot <- c("notification_year", "age_group_10yr", "OI_ST")
if (!all(required_cols_plot %in% colnames(tb_data))) {
  stop("Error: Missing required columns for plot: ",
       paste(required_cols_plot[!required_cols_plot %in% colnames(tb_data)], collapse=", "))
}

# Convert year to integer
tb_data <- tb_data %>% mutate(year = as.integer(notification_year))

# Create data subset with counts for ST and OI
plot_data_st_oi <- tb_data %>%
  # Keep only necessary columns and filter missing grouping values
  dplyr::select(year, age_group = age_group_10yr, location_plot = OI_ST) %>%
  dplyr::filter(!is.na(year), !is.na(age_group), !is.na(location_plot)) %>%
  # Count cases per group
  dplyr::count(year, location_plot, age_group, name = "n")

# Create data subset for "All" notifications
plot_data_all <- tb_data %>%
  dplyr::select(year, age_group = age_group_10yr) %>%
  dplyr::filter(!is.na(year), !is.na(age_group)) %>%
  # Assign "All" as the location
  dplyr::mutate(location_plot = "All Notifications") %>%
  dplyr::count(year, location_plot, age_group, name = "n")

# Combine "All", "ST", "OI" data
plot_data_combined <- dplyr::bind_rows(plot_data_all, plot_data_st_oi) %>%
  # Calculate proportion within each year and location
  dplyr::group_by(year, location_plot) %>%
  dplyr::mutate(prop = n / sum(n)) %>%
  dplyr::ungroup() %>%
  # Set factor levels for plotting order
  dplyr::mutate(
    location_plot = factor(location_plot, levels = c("All Notifications", "ST", "OI"))
  )

message("-> Calculated counts and proportions for plotting.")
# glimpse(plot_data_combined)


# Create 100% Stacked Bar Chart -----------------------------------

message("\nGenerating 100% stacked bar chart for age distribution...")

# Define a color palette (adjust colors as needed)
# Example using viridis palette
# age_colors <- scales::viridis_pal(option = "D")(length(levels(plot_data_combined$age_group)))
# Or specify manually: age_colors <- c("0-9" = "red", "10-19" = "blue", ...)

plot_age_dist <- ggplot(
  data = plot_data_combined,
  mapping = aes(x = year, y = prop, fill = age_group)
) +
  # Use geom_col for pre-calculated proportions
  geom_col(position = "stack", width = 0.9) +
  # Facet by location, vertically stacked
  facet_wrap(~ location_plot, ncol = 1, scales = "free_y") + # free_y not really needed for prop
  # Format y-axis as percentage
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  # scale_fill_viridis_d(option = "plasma") # Example color scale
  labs(
    title = "Proportional Age Distribution of TB Cases Over Time",
    subtitle = "Faceted by Location Category",
    x = "Notification Year",
    y = "Proportion of Cases",
    fill = "Age Group (10-Year Bins)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill="grey90", color=NA), # Facet title background
    strip.text = element_text(face="bold") # Facet title text
  )

message("-> Created age distribution plot object.")

# Display the plot ------------------------------------------------

message("\nDisplaying plot...")
print(plot_age_dist)

ggsave(here::here("figures", "age_distribution_stacked_bar.png"), plot = plot_age_dist, width = 8, height = 10)
message("\nPlot saved to 'figures' folder.")