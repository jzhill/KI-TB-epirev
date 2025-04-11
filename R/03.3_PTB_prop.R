# Title and Description --------------------------------------------

# Analysis script: Proportion of Pulmonary TB Cases Over Time
# Generates plot and table showing proportion PTB by year and location.
# Author: Jeremy Hill
# Date commenced: 11/4/2025 # Adjust date
# Last modified: 11/4/2025 # Adjust date

# Packages --------------------------------------------------------

library(here)
library(tidyverse)
library(ggplot2)
library(scales)    # For percentage formatting
library(flextable) # For creating formatted tables
library(officer)   # Often needed for flextable formatting/output

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

# Prepare Data for PTB Proportion Analysis ------------------------

message("\nPreparing data for PTB proportion analysis (treating NA PTB status as Non-PTB)...")

# Check for required columns
required_cols_prop <- c("notification_year", "disease_ptb", "OI_ST")
if (!all(required_cols_prop %in% colnames(tb_data))) {
  stop("Error: Missing required columns for plot: ",
       paste(required_cols_prop[!required_cols_prop %in% colnames(tb_data)], collapse=", "))
}

# Prepare base data: ensure year integer, recode NA PTB status to FALSE
ptb_prop_base <- tb_data %>%
  mutate(
    year = as.integer(notification_year),
    # Treat NA disease_ptb as FALSE (Non-PTB) based on user request
    disease_ptb_clean = if_else(is.na(disease_ptb), FALSE, disease_ptb)
  ) %>%
  # Exclude rows where location is unknown (cannot be grouped) or year is missing
  filter(!is.na(year), !is.na(OI_ST)) %>%
  # Select relevant columns for aggregation
  select(year, disease_ptb_clean, location_plot = OI_ST)

# Create the "All Notifications" category data
ptb_prop_all <- ptb_prop_base %>%
  mutate(location_plot = "All Notifications")

# Combine All, ST, OI data
ptb_prop_combined <- bind_rows(ptb_prop_all, ptb_prop_base) %>%
  # Set factor levels for plotting/tabling order
  mutate(
    location_plot = factor(location_plot, levels = c("All Notifications", "ST", "OI")),
    # Convert cleaned logical disease_ptb to factor for ggplot fill / grouping
    disease_ptb_cat = factor(if_else(disease_ptb_clean, "PTB", "Non-PTB"), levels = c("PTB", "Non-PTB"))
  ) %>%
  # Count cases per year, location, and cleaned PTB status
  count(year, location_plot, disease_ptb_cat, name = "n") %>%
  # Ensure all combinations exist, filling count with 0 if missing
  # (important for accurate proportions and complete plots/tables)
  tidyr::complete(year, location_plot, disease_ptb_cat, fill = list(n = 0))

message("-> Calculated counts by year, location, and PTB status (NA PTB treated as Non-PTB).")
# glimpse(ptb_prop_combined) # This data is used for the plot


# Create 100% Stacked Bar Chart -----------------------------------

message("\nGenerating 100% stacked bar chart for PTB proportion...")

plot_ptb_prop <- ggplot(
  data = ptb_prop_combined,
  mapping = aes(x = year, y = n, fill = disease_ptb_cat)
) +
  # geom_bar creates bars based on counts; position="fill" makes it 100%
  geom_bar(stat = "identity", position = "fill", width = 0.9) +
  # Facet by location, vertically stacked
  facet_wrap(~ location_plot, ncol = 1) +
  # Format y-axis as percentage
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  # Use a suitable color scale (e.g., simple contrasting colors)
  scale_fill_manual(values = c("PTB" = "steelblue", "Non-PTB" = "lightgrey")) +
  labs(
    title = "Proportion of Classified TB Cases with Pulmonary TB (PTB)",
    subtitle = "Faceted by Location Category",
    x = "Notification Year",
    y = "Proportion of Cases",
    fill = "Case Type" # Legend title
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill="grey90", color=NA),
    strip.text = element_text(face="bold")
  )

message("-> Created PTB proportion plot object.")

# Create Summary Table with flextable ----------------------------

message("\nGenerating reshaped summary table for PTB proportion using flextable...")

# Calculate proportions and totals grouped by year and location
ptb_table_data_long <- ptb_prop_combined %>% # Start with counts generated above
  # Calculate total cases per year/location group
  group_by(year, location_plot) %>%
  mutate(total_cases = sum(n)) %>%
  ungroup() %>%
  # Filter for only PTB cases and calculate proportion
  filter(disease_ptb_cat == "PTB") %>%
  mutate(prop_ptb = n / total_cases) %>%
  # Select relevant columns for the final table structure
  select(
    Year = year,
    Location = location_plot,
    `PTB Cases` = n,
    `Total Cases` = total_cases, # Renamed for clarity
    `Proportion PTB` = prop_ptb
  )

# --- Reshape data: years as rows, locations as columns ---
ptb_table_data_wide <- ptb_table_data_long %>%
  tidyr::pivot_wider(
    id_cols = Year, # Years will form the rows
    names_from = Location, # Values from Location will form part of column names
    # Specify which columns contain the values to spread
    values_from = c(`PTB Cases`, `Total Cases`, `Proportion PTB`),
    # Define how to create new column names (Location_Measure)
    names_glue = "{Location}_{.value}",
    # Ensure consistent column order by specifying names_vary and names_sort
    names_vary = "slowest", # Group by measure first, then location
    names_sort = TRUE     # Sort column names alphabetically (within measure)
  ) %>%
  # Arrange by Year
  arrange(Year)

message("-> Reshaped PTB proportion data for table.")
# glimpse(ptb_table_data_wide)


# --- Create the flextable object from the WIDE data ---
# Requires the 'flextable' package
ft <- flextable::flextable(ptb_table_data_wide)

# Define formatting for different column groups (using patterns)
prop_cols <- colnames(ptb_table_data_wide)[str_detect(colnames(ptb_table_data_wide), "Proportion PTB")]
case_cols <- colnames(ptb_table_data_wide)[str_detect(colnames(ptb_table_data_wide), "Cases")]

# Apply formatting
ft <- ft %>%
  # Format the proportion columns as percentage
  flextable::colformat_num(j = prop_cols, digits = 1, prefix = "", suffix = "%") %>%
  # Format case count columns with comma separator
  flextable::colformat_num(j = case_cols, digits = 0, big.mark = ",") %>%
  # Set custom headers (optional multi-level headers)
  # Example: Group columns by Location
  flextable::set_header_labels(
    `All Notifications_PTB Cases` = "PTB Cases", `All Notifications_Total Cases` = "Total Cases", `All Notifications_Proportion PTB` = "Proportion",
    `OI_PTB Cases` = "PTB Cases", `OI_Total Cases` = "Total Cases", `OI_Proportion PTB` = "Proportion",
    `ST_PTB Cases` = "PTB Cases", `ST_Total Cases` = "Total Cases", `ST_Proportion PTB` = "Proportion"
  ) %>%
  flextable::add_header_row(values = c("", "All Notifications", "Outer Islands (OI)", "South Tarawa (ST)"), colwidths = c(1, 3, 3, 3)) %>% # Adjust colwidths as needed
  # Add borders
  flextable::border_outer(part="all", border = fp_border(color="grey")) %>%
  flextable::border_inner_h(part="all", border = fp_border(color="grey")) %>%
  flextable::border_inner_v(part="all", border = fp_border(color="grey")) %>%
  # Align columns
  flextable::align(align = "center", part = "all") %>%
  flextable::align(j = "Year", align = "left", part = "body") %>%
  # Autofit columns
  flextable::autofit() %>%
  # Add a title
  flextable::set_caption(caption = "Annual Proportion of TB Cases Classified as Pulmonary TB (PTB) by Location (NA PTB treated as Non-PTB)") %>%
  # Bold headers
  flextable::bold(part = "header")

message("-> Created reshaped flextable object for PTB proportions.")

# Display Plot and Table -----------------------------------------

message("\nDisplaying plot and table...")

# Print the plot
print(plot_ptb_prop)

# Print the flextable (will render in RStudio Viewer or save to formats)
print(ft)

output_dir_tables <- here::here("figures")
flextable::save_as_docx(ft, path = file.path(output_dir_tables, "ptb_proportion_table.docx"))
flextable::save_as_html(ft, path = file.path(output_dir_tables, "ptb_proportion_table.html"))
message("\nTable saved to 'outputs/tables' folder.")