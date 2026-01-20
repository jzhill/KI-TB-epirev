# Title and Description --------------------------------------------
# Output functions for KI TB epi analysis
# Focus: Case Notification Rates (CNR) over time
# Author: Jeremy Hill
# Last modified: 2026-01-19
#
# Design:
# - Functions only (no file I/O unless you call a loader helper)
# - Safe to source from Quarto or scripts
# - Tidyverse-first

# Packages --------------------------------------------------------
library(here)
library(tidyverse)
library(lubridate)
library(scales)
library(qs)
library(flextable)
library(officer)


# Themes and Styling ---------------------------------------------------

## Standard ggplot theme for Kiribati TB Reports ----------------

# Standard ggplot theme for Kiribati TB Reports ----
theme_ki_tb <- function(base_size = 11) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      panel.grid.minor = element_blank(),
      
      # Add a light border to facets
      strip.background = element_rect(fill = "grey95", color = "grey80"),
      strip.text = element_text(face = "bold", size = rel(0.9)),
      
      # Legend at the bottom is standard for epi reports
      legend.position = "bottom",
      plot.title = element_text(face = "bold", hjust = 0),
      plot.subtitle = element_text(size = rel(0.9), color = "grey30"),
      axis.title = element_text(size = rel(0.9))
    )
}


## Standard Flextable styling for MS Word compatibility -----------

# This ensures a professional, publication-ready look for MS Word/PowerPoint

style_ki_table <- function(x, caption = NULL) {
  x <- x %>%
    font(fontname = "Arial", part = "all") %>%
    fontsize(size = 9, part = "all") %>%
    bold(part = "header") %>%
    border_remove() %>%
    hline_top(border = fp_border(width = 1.5), part = "header") %>%
    hline_bottom(border = fp_border(width = 0.75), part = "header") %>%
    hline_bottom(border = fp_border(width = 1.5), part = "body") %>%
    padding(padding = 2, part = "all") %>%
    autofit()
  
  if (!is.null(caption)) {
    x <- set_caption(x, caption = caption)
  }
  
  return(x)
}


# Helper functions -------------------

## Load annual island population data (helper) ---------------------------

get_annual_pop_island_est <- function(
    path = here("data-processed", "annual_pop_island_est.qs"),
    build_if_missing = TRUE,
    build_script = here("R", "04_island_pop_est.R")
) {
  if (!file.exists(path)) {
    if (!build_if_missing) {
      stop("Population estimate file not found: ", path)
    }
    message("annual_pop_island_est.qs not found — building population estimates via: ", build_script)
    source(build_script)
    if (!file.exists(path)) stop("Build script ran but output still missing: ", path)
  }
  qread(path)
}

## Filter register data according to a range of years -----------------

# Limit register data to only complete years for annualised outputs
# Good for removing current year notifications

get_complete_register_years <- function(
    reg_clean,
    min_year = 1998,
    max_year = 2024
) {
  reg_clean %>%
    mutate(reg_year = as.integer(reg_year)) %>%
    filter(!is.na(reg_year)) %>%
    pull(reg_year) %>%
    range(na.rm = TRUE) %>%
    {\(x) seq(max(x[1], min_year), min(x[2], max_year))}()
}



# Demographic summary table (ST / OI / All) -------------------

# Generates a standardized summary of the TB cohort
# Operates on the main reg_clean df
# Allows the user to specify whether the table will count empty values for each column or drop them
# Returns a list: $data (raw tibble), $ft (flextable), $metadata (counts/years)


build_demo_table_stoi <- function(
    reg_clean,
    year_min = 1998,
    year_max = 2024,
    include_unknown_rows = TRUE
) {
  
  # df_all is the analytic cohort
  # Includes: All registered TB cases within the specified year range.
  # Note: 'NR' (Not Recorded) values for geographic location are KEPT here 
  # to serve as the correct denominator for national-level proportions.
  
  df_all <- reg_clean %>%
    mutate(
      reg_year = suppressWarnings(as.integer(as.character(reg_year)))
    ) %>%
    filter(!is.na(reg_year), reg_year >= year_min, reg_year <= year_max)
  
  # df_stoi is the Geographic Comparison Cohort
  # Includes: Only cases with a confirmed location in South Tarawa (ST) or Outer Islands (OI).
  # Excludes: Cases marked 'NR' or 'Unknown' for location.
  # Usage: Used for the two columns comparing ST and OI
  
  df_stoi <- df_all %>%
    filter(!is.na(OI_ST), OI_ST %in% c("South Tarawa", "Outer Islands"))
  
  # Denominators for each geographic column
  
  denom_st <- nrow(df_stoi %>% filter(OI_ST == "South Tarawa"))
  denom_oi <- nrow(df_stoi %>% filter(OI_ST == "Outer Islands"))
  denom_all <- nrow(df_all)
  
  # Helper: format "n (x.x%)" with correct denominator
  
  format_cell <- function(n, denom) {
    if (is.na(denom) || denom <= 0) return(as.character(n))
    sprintf("%s (%.1f%%)", format(n, big.mark = ","), 100 * n / denom)
  }
  
  # Helper: build block table for one variable
  
  # This internal helper takes one variable and creates the summary rows for the table.
  # It handles the "All" cohort and the "ST/OI" cohort separately to ensure
  # we are using the correct denominators for each.
  # Allows us to specify levels and labels specifically for each column if needed
  
  tab_one <- function(var, levels = NULL, label_map = NULL, block) {
    
    # NSE Setup: Convert the column name (e.g., sex_clean) into a symbol 
    # so we can use it inside tidyverse functions with '!!'
    
    var_sym <- rlang::ensym(var)
    
    # Nested Helper: Standardize cleaning logic across both cohorts 
    # This prevents "logic drift" between the National and Regional columns.
    
    process_vals <- function(dat, use_stoi_filter = FALSE) {
      d <- dat %>%
        transmute(
          value = as.character(!!var_sym),
          
          # If we are processing the regional cohort, keep the ST/OI labels
          group = if(use_stoi_filter) as.character(OI_ST) else "All"
        ) %>%
        
        mutate(
          
          # Handling of NA, Unclassified, NR
          # Convert any "" to actual NAs
          value = case_when(
            is.na(value) | value == "" ~ "Not Recorded", # Truly blank
            value == "unclassified" ~ "Unclassified", # Algorithmic failure
            TRUE ~ value           # Valid clinical data
          )
        )
      
      # Unknowns: applies toggle to filter out missing values if parameter is FALSE
      if (!include_unknown_rows) {
        d <- filter(d, !(value %in% c("Not Recorded", "Unclassified")))
      }
      
      # Force "Unclassified" and "Not Recorded" to the end of the factor levels
      # but only if they weren't already explicitly ordered in 'levels'
      if (!is.null(levels)) {
        clean_levels <- setdiff(levels, c("Unclassified", "Not Recorded"))
        d <- mutate(d, value = factor(value, levels = c(clean_levels, "Unclassified", "Not Recorded")))
      }
      
      return(d)
    }
    
    # Execute the cleaning logic for both groups
    # 'vals_all' uses the full register (National)
    # 'vals_stoi' uses the filtered register (South Tarawa / Outer Islands)
    vals_all  <- process_vals(df_all)
    vals_stoi <- process_vals(df_stoi, use_stoi_filter = TRUE)
    
    # Aggregate counts
    n_all <- count(vals_all, value, name = "All_n")
    n_stoi <- vals_stoi %>%
      count(group, value, name = "n") %>%
      pivot_wider(names_from = group, values_from = n, values_fill = 0) %>%
      rename(
        ST_n = `South Tarawa`, 
        OI_n = `Outer Islands`
      )
    
    # Final row construction
    full_join(n_all, n_stoi, by = "value") %>%
      mutate(across(ends_with("_n"), ~replace_na(., 0L))) %>%
      transmute(
        Attribute = block,
        Group = paste0("  ", as.character(value)),
        ST  = format_cell(ST_n,  denom_st),
        OI  = format_cell(OI_n,  denom_oi),
        All = format_cell(All_n, denom_all)
      )
    
  }
  
  # Build table row counts, column by column
  
  # All cases row 
  all_cases <- tibble(
    Attribute = "All cases",
    Group = paste0("Total registered cases"),
    ST  = format(denom_st, big.mark = ","),
    OI  = format(denom_oi, big.mark = ","),
    All = format(denom_all, big.mark = ",")
  )
  
  # Sex
  sex_tab <- tab_one(
    sex_clean,
    levels = c("Female", "Male", "Unclassified", "Not Recorded"),
    block = "Sex"
  )
  
  # Age groups
  age_levels <- if (is.factor(df_all$age_group_new)) levels(df_all$age_group_new) else NULL
  age_tab <- tab_one(
    age_group_new,
    levels = age_levels,
    block = "Age group"
  )
  
  # Category
  cat_levels <- if (is.factor(df_all$cat_factor)) levels(df_all$cat_factor) else NULL
  cat_tab <- tab_one(
    cat_factor,
    levels = cat_levels,
    block = "Category"
  )
  
  # Disease type
  disease_tab <- tab_one(
    ptb_eptb,
    levels = c("PTB", "EPTB", "Unclassified", "Not Recorded"),
    block = "Disease type"
  )
  
  # Bacteriology and DRTB - process '_status' from binary columns in df_all
  df_all <- df_all %>%
    mutate(
      bc_status = case_when(
        bc_cd_dx_comp == "bc" ~ "BC",
        bc_cd_dx_comp == "cd" ~ "CD",
        bc_cd_dx_comp == "unclassified" ~ "Unclassified",
        TRUE ~ "Not Recorded"
      ),
      drtb_status = case_when(
        drtb_factor == "drtb" ~ "DR-TB",
        drtb_factor == "dstb" ~ "No DR-TB identified",
        drtb_factor == "unclassified" ~ "Unclassified",
        TRUE ~ "Not Recorded"
      )
    )
  
  # Bacteriology and DRTB - add '_status'  columns to df_stoi from df_all idempotently
  df_stoi <- df_stoi %>%
    select(-any_of(c("bc_status", "drtb_status"))) %>%
    left_join(select(df_all, tb_id, bc_status, drtb_status), by = "tb_id")
  
  # Bacteriology
  bac_tab <- tab_one(
    bc_status, 
    levels = c("BC", "CD"), 
    block = "Bacteriology"
    )
  
  
  # Drug resistance
  dr_tab  <- tab_one(
    drtb_status, 
    levels = c("DR-TB", "No DR-TB identified"), 
    block = "Drug Resistance"
    )

  
  # Outcome
  out_levels <- if (is.factor(df_all$outcome_factor)) levels(df_all$outcome_factor) else NULL
  outcome_tab <- tab_one(
    outcome_factor,
    levels = out_levels,
    block = "Outcome"
  )
  
  final_df <- bind_rows(
    all_cases,
    sex_tab,
    age_tab,
    cat_tab,
    disease_tab,
    bac_tab,
    dr_tab,
    outcome_tab
  )
  
  # Create Flextable with dynamic header
  ft_obj <- final_df %>%
    as_grouped_data(groups = "Attribute") %>%
    as_flextable(hide_grouplabel = TRUE) %>%
    set_header_labels(
      Group = "Attribute",
      ST = paste0("South Tarawa"),
      OI = paste0("Outer Islands"),
      All = paste0("Total")
    ) %>%
    style_ki_table(caption = paste("Demographic and clinical characteristics of TB cases,", year_min, "–", year_max)) %>% 
    align(j = 2:4, align = "center", part = "all") %>%
    bold(i = ~ !is.na(Attribute)) # Bold the block headers
  
  # Return structured list
  return(list(
    data = final_df,
    ft = ft_obj,
    metadata = list(
      years = c(year_min, year_max),
      n_total = denom_all,
      n_st = denom_st,
      n_oi = denom_oi
    )
  ))
}




# Case notification rate output functions ------------------------

## Numerator: aggregate TB cases from register --------------------------

# For use in build_cnr_tables()
# Creates the numerator for CNR
# Count cases from the register in year/geo groups
# Uses NSE (rlang::ensym) so you can pass column names freely

aggregate_tb_cases <- function(tb_data, geo_var) {
  geo_var_sym <- rlang::ensym(geo_var)
  
  tb_data %>%
    filter(!is.na(!!geo_var_sym)) %>%
    count(
      year = reg_year,
      geo  = !!geo_var_sym,
      name = "cases"
    )
}

## Denominator: sum population from estimates ------------

# For use in build_cnr_tables()
# Creates the denominator for CNR
# Sums population for year/geo groups from population estimate df

aggregate_population_to_geo <- function(pop_data, geo_var) {
  geo_var_sym <- rlang::ensym(geo_var)
  
  pop_data %>%
    group_by(geo = !!geo_var_sym, year) %>%
    summarise(
      pop = sum(estimated_population, na.rm = TRUE),
      .groups = "drop"
    )
}

## Calculate CNR tables in a list ---------------------------------

# Expects BOTH inputs in standard form:
# - population_estimates: geo, year, estimated_population
# - case_counts:          geo, year, cases
# For use in build_cnr_tables()

build_cnr_tables <- function(reg_clean, annual_pop_island_est, exclude_geos = c("Unclassified")) {
  
  # Sync year range across datasets
  years <- get_complete_register_years(reg_clean)
  
  # Filter out excluded islands from both datasets at the source
  reg_subset <- reg_clean %>% 
    filter(reg_year %in% years, !(island %in% exclude_geos))
  
  # Apply Factor Ordering based on Census Codes
  # Filter out excluded islands from both datasets at the source
  pop_subset <- annual_pop_island_est %>% 
    filter(year %in% years, !(island %in% exclude_geos))
  
  # Internal logic for calculating rates
  calc_layer <- function(geo_col) {
    geo_col_sym <- rlang::ensym(geo_col)
    col_str <- rlang::as_string(geo_col_sym)

    # Extract the correct levels from the factorized population data
    geo_levels <- levels(pop_subset[[col_str]])
    
    cases <- aggregate_tb_cases(reg_subset, !!geo_col_sym)
    pops  <- aggregate_population_to_geo(pop_subset, !!geo_col_sym)
    
    # Join and calculate
    full_join(pops, cases, by = c("geo", "year")) %>%
      mutate(
        cases = replace_na(cases, 0),
        cnr   = (cases / pop) * 100000,
        geo   = factor(geo, levels = geo_levels)
      ) %>%
      arrange(geo, year)
  }
  
  # National calculation remains the total (ignoring exclusions)
  national_df <- annual_pop_island_est %>%
    filter(year %in% years) %>%
    group_by(year) %>%
    summarise(pop = sum(estimated_population, na.rm = TRUE), .groups = "drop") %>%
    left_join(
      reg_clean %>% filter(reg_year %in% years) %>% count(year = reg_year, name = "cases"),
      by = "year"
    ) %>%
    mutate(geo = "National", cases = replace_na(cases, 0), cnr = (cases / pop) * 100000)
  
  # Return structured list
  list(
    national = national_df,
    OI_ST    = calc_layer(OI_ST),
    division = calc_layer(division),
    island   = calc_layer(island)
  )
}



## Build CNR plots and save in a list ---------------------------

# Uses the data list from the CNR tables
# It applies the Project Theme (theme_ki_tb) to ensure consistency.
# Returns a named list of ggplot objects: island, division, OI_ST

build_cnr_plots <- function(cnr_tables, exclude_current_year = TRUE) {
  
  max_year <- if(exclude_current_year) year(Sys.Date()) - 1 else 2099
  
  # Standardized plotting engine
  # 'National_ref' parameter to allow background reference lines
  
  # Standardized plotting engine
  make_plot <- function(df, title, facet = FALSE, show_nat_ref = TRUE) {
    
    # Isolate National data for the background "shadow" line
    nat_ref <- cnr_tables$national %>% 
      filter(year <= max_year) %>%
      select(-any_of("geo"))

    # Initialize the base plot (Note: No trailing '+' here)
    p <- df %>%
      filter(year <= max_year) %>%
      ggplot(aes(x = year, y = cnr)) 
    
    # Add National trend as a background reference line conditionally
    if (show_nat_ref) {
      # Use the 'geo-less' nat_ref here
      p <- p + 
        geom_line(
          data = nat_ref, 
          aes(y = cnr),
          color = "grey80", 
          linetype = "dashed", 
          linewidth = 0.8
          )
    }
    
    # Add the primary data layers and formatting
    p <- p + 
      geom_line(aes(color = geo, group = geo), linewidth = 1) +
      geom_point(aes(color = geo), size = 1.2)
    
    # Apply visuals and scales
    p <- p + 
      theme_ki_tb() +
      scale_y_continuous(labels = label_comma(), limits = c(0, NA)) +
      scale_x_continuous(breaks = breaks_pretty()) + 
      labs(
        title = title, 
        subtitle = if(show_nat_ref) "Dashed grey line = National CNR benchmark" else NULL,
        y = "CNR per 100,000", x = "Year", color = NULL
      )
    
    # Handle faceting
    if (facet) {
      p <- p + 
        facet_wrap(~geo, drop = FALSE) + 
        theme(legend.position = "none")
    }
    
    return(p)
  }
  
  list(
    national = make_plot(cnr_tables$national, "National TB Case Notification Rate", show_nat_ref = FALSE),
    OI_ST    = make_plot(cnr_tables$OI_ST, "CNR: South Tarawa vs Outer Islands"),
    division = make_plot(cnr_tables$division, "CNR by Division", facet = TRUE),
    island   = make_plot(cnr_tables$island, "CNR by Individual Island", facet = TRUE)
  )
}

# Proportion PTB output functions ------------------

## Numerator/Denominator: Calculate PTB Proportions --------------------------

# Creates the data frame for PTB proportions
# Returns: year, geo, cases (PTB), total (All), and proportion

calculate_ptb_trends <- function(reg_clean, geo_var) {
  geo_var_sym <- rlang::ensym(geo_var)
  
  reg_clean %>%
    # Filter for valid years
    filter(!is.na(reg_year), !is.na(!!geo_var_sym)) %>%
    group_by(year = reg_year, geo = !!geo_var_sym) %>%
    summarise(
      ptb_n = sum(ptb_eptb == "PTB", na.rm = TRUE),
      total_n = n(),
      prop = (ptb_n / total_n),
      .groups = "drop"
    )
}


## Build PTB Proportion Tables ---------------------------------

build_ptb_prop_tables <- function(reg_clean) {
  
  # 1. Sync years
  years <- get_complete_register_years(reg_clean)
  df <- reg_clean %>% filter(reg_year %in% years)
  
  # 2. Regional Analysis (ST/OI)
  # Filter to only the two levels we want for the comparison
  regional_df <- df %>% 
    filter(OI_ST %in% c("South Tarawa", "Outer Islands")) %>%
    calculate_ptb_trends(OI_ST)
  
  # 3. National Analysis (All)
  national_df <- df %>%
    group_by(year = reg_year) %>%
    summarise(
      ptb_n = sum(ptb_eptb == "PTB", na.rm = TRUE),
      total_n = n(),
      prop = (ptb_n / total_n),
      .groups = "drop"
    ) %>%
    mutate(geo = "National")
  
  # 4. Combine for a single plotting object
  combined_trends <- bind_rows(national_df, regional_df) %>%
    mutate(geo = factor(geo, levels = c("National", "South Tarawa", "Outer Islands")))
  
  return(combined_trends)
}


## Build PTB Proportion Plot -----------------------------------

plot_ptb_trends <- function(ptb_trends_df) {
  
  ggplot(ptb_trends_df, aes(x = year, y = prop, color = geo, group = geo)) +

    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    
    # Visuals
    theme_ki_tb() +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    scale_color_manual(values = c(
      "National" = "black", 
      "South Tarawa" = "#E69F00", # Specific color for ST
      "Outer Islands" = "#56B4E9"  # Specific color for OI
    )) +
    
    labs(
      title = "Proportion of Pulmonary TB (PTB) Cases Over Time",
      subtitle = "Denominator includes all registered cases (including unclassified)",
      x = "Year",
      y = "Percent of Total Notifications",
      color = NULL
    )
}



# Proportion BC output functions ----------------

## Build BC Proportion Tables ---------------------------------

build_bc_prop_tables <- function(reg_clean) {
  
  # 1. Sync years
  years <- get_complete_register_years(reg_clean)
  df <- reg_clean %>% filter(reg_year %in% years)
  
  # 2. Internal calculation logic (Numerator = "bc")
  calc_bc <- function(dat, geo_var) {
    geo_var_sym <- rlang::ensym(geo_var)
    dat %>%
      filter(!is.na(!!geo_var_sym)) %>%
      group_by(year = reg_year, geo = !!geo_var_sym) %>%
      summarise(
        bc_n = sum(bc_cd_dx_comp == "bc", na.rm = TRUE),
        total_n = n(),
        prop = (bc_n / total_n),
        .groups = "drop"
      )
  }
  
  # 3. National vs Regional
  national_df <- df %>%
    group_by(year = reg_year) %>%
    summarise(
      bc_n = sum(bc_cd_dx_comp == "bc", na.rm = TRUE),
      total_n = n(),
      prop = (bc_n / total_n),
      .groups = "drop"
    ) %>%
    mutate(geo = "National")
  
  regional_df <- calc_bc(df %>% filter(OI_ST %in% c("South Tarawa", "Outer Islands")), OI_ST)
  
  # 4. Combine
  bind_rows(national_df, regional_df) %>%
    mutate(geo = factor(geo, levels = c("National", "South Tarawa", "Outer Islands")))
}


## Build BC Proportion Plot -----------------------------------

plot_bc_trends <- function(bc_trends_df) {
  
  ggplot(bc_trends_df, aes(x = year, y = prop, color = geo, group = geo)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    
    # Visuals
    theme_ki_tb() +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    scale_color_manual(values = c(
      "National" = "black", 
      "South Tarawa" = "#E69F00", 
      "Outer Islands" = "#56B4E9"
    )) +
    
    labs(
      title = "Proportion of Bacteriologically Confirmed (BC) TB Cases",
      subtitle = "Based on composite diagnostic logic (Smear/Xpert/Culture and Register)",
      x = "Year",
      y = "Percent of Total Notifications",
      color = NULL
    )
}


# Proportion children 0-9 ----------------------

## Build Childhood TB Proportion Tables ---------------------------------

build_childhood_prop_tables <- function(reg_clean) {
  
  # 1. Sync years
  years <- get_complete_register_years(reg_clean)
  df <- reg_clean %>% filter(reg_year %in% years)
  
  # 2. Helper for regional calculation
  # Numerator = "0-9" bin
  calc_regional_child <- function(dat, geo_var) {
    geo_var_sym <- rlang::ensym(geo_var)
    dat %>%
      filter(!is.na(!!geo_var_sym)) %>%
      group_by(year = reg_year, geo = !!geo_var_sym) %>%
      summarise(
        child_n = sum(age_group_10yr == "0-9", na.rm = TRUE),
        total_n = n(),
        prop = (child_n / total_n),
        .groups = "drop"
      )
  }
  
  # 3. National Analysis
  national_df <- df %>%
    group_by(year = reg_year) %>%
    summarise(
      child_n = sum(age_group_10yr == "0-9", na.rm = TRUE),
      total_n = n(),
      prop = (child_n / total_n),
      .groups = "drop"
    ) %>%
    mutate(geo = "National")
  
  # 4. Regional Analysis (Fixed the function call here)
  regional_df <- calc_regional_child(
    df %>% filter(OI_ST %in% c("South Tarawa", "Outer Islands")), 
    OI_ST
  )
  
  # 5. Combine for a single plotting object
  bind_rows(national_df, regional_df) %>%
    mutate(geo = factor(geo, levels = c("National", "South Tarawa", "Outer Islands")))
}



## Build Childhood TB Proportion Plot -----------------------------------

plot_childhood_trends <- function(child_trends_df) {
  
  ggplot(child_trends_df, aes(x = year, y = prop, color = geo, group = geo)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    
    # Visuals
    theme_ki_tb() +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, NA)) + # Start at 0
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    scale_color_manual(values = c(
      "National" = "black", 
      "South Tarawa" = "#E69F00", 
      "Outer Islands" = "#56B4E9"
    )) +
    
    labs(
      title = "Proportion of Childhood TB Cases (Ages 0-9)",
      subtitle = "Denominator includes all registered cases (including those with missing age)",
      x = "Year",
      y = "Percent of Total Notifications",
      color = NULL
    )
}

