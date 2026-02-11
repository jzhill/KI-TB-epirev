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




# Label dictionary ---------------------------

# Use this to convert raw codes to report-ready labels
label_ki_tb <- function(x) {
  case_match(x,
             "st" ~ "South Tarawa",
             "oi" ~ "Outer Islands",
             "kiribati" ~ "Kiribati (National)",
             "male" ~ "Male",
             "female" ~ "Female",
             "total" ~ "Total",
             "ptb" ~ "Pulmonary TB",
             "eptb" ~ "Extrapulmonary TB",
             "bc" ~ "Bacteriologically Confirmed",
             "cd" ~ "Clinically Diagnosed",
             "dstb" ~ "No drug resistance identified",
             "drtb" ~ "Drug-resistant TB",
             "btc" ~ "Betio Town Council",
             "tuc" ~ "Teinainano Urban Council",
             "etc" ~ "Eutan Tarawa Council",
             "unclassified" ~ "Unclassified",
             "not recorded" ~ "Not Recorded",
             .default = str_to_title(x)
  )
}




# Themes and Styling ---------------------------------------------------


# Standard ggplot theme for Kiribati TB Reports
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



# Professional look for MS Word/PowerPoint
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


# Data Loaders -------------------


# Load annual population data (Harmonized Helper)

get_annual_pop_est <- function(path, build_script, build_if_missing = TRUE) {
  if (!file.exists(path)) {
    if (!build_if_missing) stop("File not found: ", path)
    message("Building denominators via: ", build_script)
    source(build_script)
  }
  return(qread(path))
}


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
    year_max = 2025,
    include_unknown_rows = TRUE
) {
  
  # Internal Helpers 
  
  get_st_oi_pvalue <- function(st_n, st_denom, oi_n, oi_denom) {
    if (is.na(st_denom) || is.na(oi_denom) || st_denom == 0 || oi_denom == 0) return(NA_real_)
    
    # Use prop.test for Chi-squared test of proportions
    res <- suppressWarnings(prop.test(x = c(st_n, oi_n), n = c(st_denom, oi_denom)))
    return(res$p.value)
  }
  
  # Creates stars for significance
  format_p_stars <- function(p) {
    if (is.na(p)) return(""); 
    if (p < 0.001) return("***"); 
    if (p < 0.01) return("**"); 
    if (p < 0.05) return("*"); return("")
  }
  
  # Helper: format "n (x.x%)" with correct denominator
  
  format_cell <- function(n, denom) {
    if (is.na(denom) || denom <= 0) return(as.character(n))
    sprintf("%s (%.1f%%)", format(n, big.mark = ","), 100 * n / denom)
  }
  
  # Data Preparation
  # df_all is the analytic cohort
  # Includes: All registered TB cases within the specified year range.
  # Note: 'NR' (Not Recorded) values for geographic location are KEPT here 
  # to serve as the correct denominator for national-level proportions.
  
  df_all <- reg_clean %>%
    mutate(
      reg_year = suppressWarnings(as.integer(as.character(reg_year)))
    ) %>%
    filter(!is.na(reg_year), reg_year >= year_min, reg_year <= year_max)
  
  # Identify count for footnote before filtering for the ST/OI cohort
  n_unclassified_geo <- nrow(df_all %>% filter(oi_st == "unclassified"))
  
  # df_stoi is the Geographic Comparison Cohort
  # Includes: Only cases with a confirmed location in South Tarawa (ST) or Outer Islands (OI).
  # Excludes: Cases marked 'NR' or 'Unknown' for location.
  # Usage: Used for the two columns comparing ST and OI
  
  df_stoi <- df_all %>%
    filter(!is.na(oi_st), oi_st %in% c("st", "oi"))
  
  # Denominators for each geographic column
  
  denom_st <- nrow(df_stoi %>% filter(oi_st == "st"))
  denom_oi <- nrow(df_stoi %>% filter(oi_st == "oi"))
  denom_all <- nrow(df_all)
  
  # Block Builder Engine
  
  # This internal helper takes one variable and creates the summary rows for the table.
  # It handles the "All" cohort and the "ST/OI" cohort separately to ensure
  # we are using the correct denominators for each.
  
  tab_one <- function(var, levels = NULL, block) {
    
    # NSE Setup: Convert the column name (e.g., sex_clean) into a symbol 
    # so we can use it inside tidyverse functions with '!!'
    
    var_sym <- rlang::ensym(var)
    
    # Nested Helper: Standardize cleaning logic across both cohorts 
    # This prevents "logic drift" between the National and Regional columns.
    
    process_vals <- function(dat, use_stoi_filter = FALSE) {
      d <- dat %>%
        transmute(
          value = as.character(!!var_sym),
          
          # For processing the regional st/oi cohorts
          group = if(use_stoi_filter) as.character(oi_st) else "all"
        ) %>%
        
        mutate(
          
          # Handling of NA, Unclassified, NR
          # Convert any "" to actual NAs
          value = case_when(
            is.na(value) | value == "" ~ "not recorded",  # Truly blank
            value == "unclassified" ~ "unclassified",     # Algorithmic failure
            TRUE ~ value                                  # Valid clinical data
          )
        )
      
      # Unknowns: applies toggle to filter out missing values if parameter is FALSE
      
      if (!include_unknown_rows) {
        d <- d %>% filter(!(value %in% c("not recorded", "unclassified")))
      }
      return(d)
    }
    
    # Execute the cleaning logic for both groups
    # 'vals_all' uses the full register (National)
    # 'vals_stoi' uses the filtered register (South Tarawa / Outer Islands)
    
    vals_all  <- process_vals(df_all)
    vals_stoi <- process_vals(df_stoi, use_stoi_filter = TRUE)
    
    # Aggregate counts
    n_all <- vals_all %>% 
      count(value, name = "All_n")
    n_stoi <- vals_stoi %>%
      count(group, value, name = "n") %>%
      pivot_wider(names_from = group, values_from = n, values_fill = 0) %>%
      # Logic change: pivot keys are now lowercase 'st' and 'oi'
      rename(ST_n = any_of("st"), OI_n = any_of("oi"))
    
    # Ensure columns exist even if one region has 0 cases for this entire block
    if (!"ST_n" %in% names(n_stoi)) n_stoi$ST_n <- 0L
    if (!"OI_n" %in% names(n_stoi)) n_stoi$OI_n <- 0L
    
    # Join OI/ST counts to counts for all 
    res <- n_all %>%
      left_join(n_stoi, by = "value") %>%
      mutate(across(ends_with("_n"), ~replace_na(., 0L))) %>%
      
      # Calculate P-values (ST vs OI only)
      mutate(
        p_raw = map2_dbl(ST_n, OI_n, ~get_st_oi_pvalue(.x, denom_st, .y, denom_oi)),
        p_fmt = if_else(p_raw < 0.001, "<0.001", sprintf("%.3f", p_raw)),
        sig   = map_chr(p_raw, format_p_stars)
      )
    
    # Handle Levels and Ordering
    all_found_values <- unique(res$value)
    standard_levels  <- setdiff(all_found_values, c("unclassified", "not recorded"))
    final_ordering   <- c(standard_levels, "unclassified", "not recorded")
    
    res <- res %>%
      mutate(value = factor(value, levels = final_ordering)) %>%
      arrange(value)
    
    # 6. Final Formatting
    res %>% 
      transmute(
        Attribute = block, 
        Group     = paste0("  ", label_ki_tb(as.character(value))),
        All       = format_cell(All_n, denom_all), 
        ST        = format_cell(ST_n, denom_st), 
        OI        = format_cell(OI_n, denom_oi),
        P         = replace_na(p_fmt, ""), 
        Sig       = sig
      )
  }
  
  # Table Construction 
  
  # All cases
  all_cases <- tibble(
    Attribute = "All cases", 
    Group = "Total registered cases",
    All = format(denom_all, big.mark = ","), 
    ST = format(denom_st, big.mark = ","), 
    OI = format(denom_oi, big.mark = ","),
    P = "", 
    Sig = ""
  )
  
  # Sex, age, registration category, PTB/EPTB
  sex_tab <- tab_one(sex_clean, block = "Sex")
  age_tab <- tab_one(age_group_new, block = "Age group")
  cat_tab <- tab_one(cat_clean, block = "Category")
  disease_tab <- tab_one(ptb_eptb, block = "Disease type")
  
  # Bacteriology/DRTB derived columns
  df_all <- df_all %>%
    mutate(
      
      bc_status = case_when(
        bc_cd_dx_comp == "bc" ~ "bc",
        bc_cd_dx_comp == "cd" ~ "cd",
        bc_cd_dx_comp == "unclassified" ~ "unclassified",
        TRUE ~ "not recorded"
      ),
      
      drtb_status = case_when(
        drtb_clean == "drtb" ~ "drtb",
        drtb_clean == "dstb" ~ "dstb",
        drtb_clean == "unclassified" ~ "unclassified",
        TRUE ~ "not recorded"
      )
    )
  
  # Bacteriology and DRTB - add '_status'  columns to df_stoi from df_all idempotently
  df_stoi <- df_stoi %>%
    select(-any_of(c("bc_status", "drtb_status"))) %>%
    left_join(select(df_all, tb_id, bc_status, drtb_status), by = "tb_id")
  
  # Bacteriology, DRTB
  bac_tab <- tab_one(bc_status, block = "Bacteriology")
  dr_tab  <- tab_one(drtb_status, block = "Drug Resistance")
  
  # Outcomes with specific logic for "Treatment successful"
  # We use the raw labels to filter, then let the block engine handle the rest
  outcome_tab_raw <- tab_one(outcome_clean, block = "Outcome") %>%
    mutate(Group = case_when(
      str_detect(Group, "Cured") ~ "    Cured",
      str_detect(Group, "Completed") ~ "    Completed",
      TRUE ~ Group
    ))
  
  # Create the "Treatment successful" summary row
  # This sums 'Cured' and 'Treatment completed' (Levels 1 and 2)
  # Note: Uses the raw counts from df_all and df_stoi to ensure correct p-value calculation
  success_stats <- df_all %>%
    mutate(is_success = outcome_clean %in% c("cured", "completed")) %>%
    group_by(group = "all") %>%
    summarise(n = sum(is_success, na.rm = TRUE), .groups = "drop") %>%
    bind_rows(
      df_stoi %>%
        mutate(is_success = outcome_clean %in% c("cured", "completed")) %>%
        group_by(group = oi_st) %>%
        summarise(n = sum(is_success, na.rm = TRUE), .groups = "drop")
    ) %>%
    pivot_wider(names_from = group, values_from = n) %>%
    rename(All_n = all, ST_n = st, OI_n = oi)
  
  success_row <- success_stats %>%
    mutate(
      p_raw = map2_dbl(ST_n, OI_n, ~get_st_oi_pvalue(.x, denom_st, .y, denom_oi)),
      p_fmt = if_else(p_raw < 0.001, "<0.001", sprintf("%.3f", p_raw)),
      sig_stars = map_chr(p_raw, format_p_stars)
    ) %>%
    transmute(
      Attribute = "Outcome", 
      Group = "  Treatment successful",
      All = format_cell(All_n, denom_all), 
      ST = format_cell(ST_n,  denom_st), 
      OI = format_cell(OI_n,  denom_oi),
      P = p_fmt, 
      Sig = sig_stars
    )
  
  # Combine success row with the rest of the outcome table
  # We adjust the individual rows to look like sub-categories
  outcome_tab <- bind_rows(success_row, outcome_tab_raw)
  
  # Final Assembly
  
  final_df <- bind_rows(
    all_cases, 
    sex_tab, 
    age_tab, 
    cat_tab, 
    disease_tab, 
    bac_tab, 
    dr_tab, 
    outcome_tab)
  
  ft_obj <- final_df %>%
    as_grouped_data(groups = "Attribute") %>%
    as_flextable(hide_grouplabel = TRUE) %>%
    set_header_labels(Group = "Attribute", All = "Total*", ST = "South Tarawa", OI = "Outer Islands", P = "p-value**", Sig = "") %>%
    add_footer_lines(paste0("* n unclassified geography = ", n_unclassified_geo)) %>%
    add_footer_lines("** p-value indicates difference in proportions between South Tarawa and Outer Islands (Chi-squared test). Significance: * p<0.05, ** p<0.01, *** p<0.001.") %>%
    style_ki_table(caption = paste("Demographic and clinical characteristics of TB cases,", year_min, "–", year_max)) %>% 
    align(j = 2:5, align = "center", part = "all") %>%
    width(j = 5, width = 0.7) %>%
    width(j = 6, width = 0.3) %>%
    bold(i = ~ !is.na(Attribute)) # Bold the block headers
  
  return(list(
    data = final_df, 
    ft = ft_obj, 
    metadata = list(
      years = c(year_min, year_max), 
      n_total = denom_all,
      n_st = denom_st,
      n_oi = denom_oi,
      n_unclassified_geo = n_unclassified_geo
    )
  ))
}




# CNR Calculation and Plotting -----------------------------------------

## Tables -------------

build_cnr_tables <- function(
    reg_clean, 
    pop_est_data, 
    geo_levels = c("national", "division", "oi_st", "island", "council", "village"), 
    exclude_geos = c("unclassified"),
    year_start = 1998,
    year_end = 2024
) {
  
  years_range <- year_start:year_end
  
  # Map pop-geo-names to reg-column-names
  reg_col_mapping <- c(
    "national"="kiribati", 
    "division"="division", 
    "oi_st"="oi_st", 
    "island"="island",  
    "council"="tarawa_council", 
    "village"="st_village"
  )
  
  process_tier <- function(tier_name) {
    
    # Get populations for specified geos
    pops <- pop_est_data %>% 
      filter(geo == tier_name, year %in% years_range, !(name %in% exclude_geos))
    
    reg_col <- reg_col_mapping[tier_name]
    cases_raw <- reg_clean %>% filter(reg_year %in% years_range, !(island %in% exclude_geos))
    
    if (tier_name == "national") {
      cases_raw <- cases_raw %>% mutate(target_name = "kiribati")
    } else {
      cases_raw <- cases_raw %>% mutate(target_name = str_to_lower(as.character(.data[[reg_col]])))
    }
    
    # Get cases for specified geos, then aggregate
    cases <- cases_raw %>%
      group_by(year = reg_year, name = target_name, sex = str_to_lower(sex_clean)) %>%
      summarise(cases = n(), .groups = "drop")
    
    cases_total <- cases %>% group_by(year, name) %>% summarise(cases = sum(cases), .groups = "drop") %>% mutate(sex = "total")
    
    # Join cases to pops in long format for specified geos
    full_join(pops, bind_rows(cases, cases_total), by = c("year", "name", "sex")) %>%
      mutate(cases = replace_na(cases, 0), cnr = (cases / population) * 100000) %>%
      filter(!is.na(population)) %>% arrange(name, year, sex)
  }
  
  map(set_names(geo_levels), process_tier)
}

## Plots -------------

build_cnr_plots <- function(
    cnr_list, 
    sex_mode = "total",     # total, split, all
    facet_by_geo = TRUE, 
    national_ref = NULL     # pass cnr_list$national here
) {
  
  make_plot <- function(df, tier_label) {
    if (is.null(df)) return(NULL)
    
    # 1. Prepare Main Plot Data
    plot_data <- df %>%
      filter(case_when(
        sex_mode == "total" ~ sex == "total",
        sex_mode == "split" ~ sex %in% c("male", "female"),
        sex_mode == "all"   ~ sex %in% c("male", "female", "total"),
        TRUE ~ TRUE
      )) %>%
      mutate(
        pretty_name = label_ki_tb(name), 
        pretty_sex  = label_ki_tb(sex)
      )
    
    p <- ggplot(plot_data, aes(x = year, y = cnr))
    
    # 2. Add National Reference (as a ghosted background layer)
    # This plots the national total behind the sub-regional data
    if (!is.null(national_ref)) {
      ref_line_data <- national_ref %>% filter(sex == "total")
      p <- p + geom_line(
        data = ref_line_data, 
        aes(x = year, y = cnr), 
        color = "grey80", linewidth = 0.8, linetype = "dashed", inherit.aes = FALSE
      )
    }
    
    # 3. Apply Aesthetics based on Sex Mode
    if (sex_mode == "total") {
      p <- p + 
        geom_line(aes(color = pretty_name), linewidth = 1) + 
        geom_point(aes(color = pretty_name))
      
    } else if (sex_mode %in% c("split", "all")) {
      # Use color for sex and potentially linetype/size to distinguish 'Total'
      p <- p + 
        geom_line(aes(color = pretty_sex, linetype = pretty_sex, linewidth = pretty_sex)) + 
        geom_point(aes(color = pretty_sex), size = 1.5) +
        scale_linewidth_manual(values = c("Male" = 0.8, "Female" = 0.8, "Total" = 1.2)) +
        scale_linetype_manual(values = c("Male" = "solid", "Female" = "solid", "Total" = "dotted")) +
        scale_color_manual(values = c(
          "Male" = "#2c7bb6",   # Blue
          "Female" = "#d7191c", # Red
          "Total" = "#404040"   # Dark Grey
        ))
    }
    
    # 4. Final Formatting
    p <- p + theme_ki_tb() + 
      labs(
        title = paste("TB CNR:", label_ki_tb(tier_label)), 
        subtitle = paste("Mode:", str_to_title(sex_mode), 
                         if(!is.null(national_ref)) "| Dashed grey: National Total" else ""), 
        y = "Rate per 100,000", x = "Year", 
        color = NULL, linetype = NULL, linewidth = NULL
      )
    
    if (facet_by_geo) {
      p <- p + facet_wrap(~pretty_name)
    }
    
    return(p)
  }
  
  map2(cnr_list, names(cnr_list), make_plot)
}







# Proportion Plots (PTB, BC, Childhood) --------------------------------

build_proportion_data <- function(reg_clean, type = "ptb") {
  years <- get_complete_register_years(reg_clean)
  
  df <- reg_clean %>% 
    filter(reg_year %in% years) %>%
    mutate(is_target = case_when(
      type == "ptb" ~ ptb_eptb == "ptb",
      type == "bc"  ~ bc_cd_dx_comp == "bc",
      type == "child" ~ age_group_10yr == "0-9"
    ))
  
  national <- df %>% group_by(year = reg_year) %>% summarise(n = sum(is_target, na.rm = TRUE), total = n(), prop = n/total) %>% mutate(geo = "national")
  regional <- df %>% filter(oi_st %in% c("st", "oi")) %>% group_by(year = reg_year, geo = oi_st) %>% summarise(n = sum(is_target, na.rm = TRUE), total = n(), prop = n/total)
  
  bind_rows(national, regional) %>% mutate(geo_label = label_ki_tb(geo))
}

plot_proportion_trends <- function(df, type = "ptb") {
  titles <- c("ptb"="Proportion of Pulmonary TB (PTB)", "bc"="Bacteriological Confirmation (BC) Rate", "child"="Proportion of Childhood TB (0-9 years)")
  
  ggplot(df, aes(x = year, y = prop, color = geo_label, group = geo_label)) +
    geom_line(linewidth = 1) + geom_point(size = 2) +
    theme_ki_tb() + scale_y_continuous(labels = percent_format(), limits = c(0, if(type == "child") NA else 1)) +
    scale_color_manual(values = c("Kiribati (National)"="black", "South Tarawa"="#E69F00", "Outer Islands"="#56B4E9")) +
    labs(title = titles[type], x = "Year", y = "Percent", color = NULL)
}

# Monthly Time Series --------------------------------------------------

build_monthly_trend_data <- function(reg_clean, start_date = "1998-01-01", end_date = "2024-12-01", geo_code = NULL) {
  start_d <- as.Date(start_date); end_d <- as.Date(end_date)
  
  df <- reg_clean %>%
    filter(!is.na(date_reg_clean)) %>%
    mutate(month_date = floor_date(date_reg_clean, "month"))
  
  if (!is.null(geo_code)) df <- df %>% filter(oi_st == geo_code)
  
  df %>%
    filter(month_date >= start_d, month_date <= end_d) %>%
    group_by(month_date) %>%
    summarise(n = n()) %>%
    complete(month_date = seq(start_d, end_d, by = "1 month"), fill = list(n = 0))
}

plot_monthly_trend <- function(monthly_data) {
  ggplot(monthly_data, aes(x = month_date, y = n)) +
    geom_line(linewidth = 0.5) + geom_smooth(method = "loess", span = 0.2, color = "red", se = FALSE, linetype = "dashed") +
    theme_ki_tb() + labs(title = "Monthly TB Case Notifications", y = "Count", x = "Month")
}


# Seasonal series from monthly --------------------------------------------------


plot_seasonal_subseries <- function(monthly_data) {
  
  sub_data <- monthly_data %>%
    mutate(
      month_label = lubridate::month(month_date, label = TRUE, abbr = TRUE),
      year = lubridate::year(month_date)
    )
  
  monthly_means <- sub_data %>%
    group_by(month_label) %>%
    summarise(mean_n = mean(n), .groups = "drop")
  
  ggplot(sub_data, aes(x = year, y = n)) +
    # Background line
    geom_line(color = "grey70", linewidth = 0.3) +
    # Add points for clarity in small facets
    geom_point(color = "grey50", size = 0.5) + 
    # Seasonal Mean
    geom_hline(data = monthly_means, aes(yintercept = mean_n), 
               color = "blue", linetype = "dashed", linewidth = 0.6) +
    facet_grid(. ~ month_label) +
    theme_ki_tb() +
    # Standardize Y axis to start at 0
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
    scale_x_continuous(breaks = c(2000, 2010, 2020), labels = c("'00", "'10", "'20")) +
    theme(
      panel.spacing = unit(0.2, "lines"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7), # Slightly smaller font
      strip.background = element_rect(fill = "grey95", color = "grey80"),
      panel.border = element_rect(color = "grey90", fill = NA) # Adds definition to facets
    ) +
    labs(
      title = "Seasonal Subseries: TB Notifications by Month",
      subtitle = paste0("Blue dashed line: monthly mean (", 
                        min(sub_data$year), "–", 
                        max(sub_data$year), ")"),
      x = "Year",
      y = "Monthly Notifications"
    )
}




# Population Diagnostic Plots -------------------------------------------

## Rederive Diagnostic Data -------------------------------------------

get_pop_diagnostics <- function(pop_master, mode = "shares", target_geo = "island") {
  
  if (mode == "shares") {
    # Calculate what % of the target geo each 'name' represents per year
    pop_master %>%
      filter(geo == target_geo, sex == "total") %>%
      group_by(year) %>%
      mutate(share_final = population / sum(population, na.rm = TRUE)) %>%
      ungroup()
    
  } else if (mode == "ratios") {
    # Calculate % Male per 'name' per year
    pop_master %>%
      filter(geo == target_geo) %>%
      pivot_wider(names_from = sex, values_from = population) %>%
      mutate(male_pct_final = male / total) %>%
      select(year, name, geo, male_pct_final)
  }
}

## Plot Geographic Shares (Island or Village) --------------------------
plot_pop_shares <- function(share_data, level = "island", exclude_name = NULL) {
  
  # Set aesthetics based on level
  config <- list(
    island = list(fill = "darkseagreen", color = "darkseagreen4", title = "Island Population Shares"),
    village = list(fill = "slategray3", color = "slategray4", title = "South Tarawa Village Shares")
  )[[level]]
  
  df <- share_data
  if (!is.null(exclude_name)) df <- df %>% filter(name != exclude_name)
  
  # Determine census anchor lines based on data availability
  census_years <- if(level == "island") c(1990, 2020) else c(2005, 2020)
  
  ggplot(df, aes(x = year, y = share_final)) +
    geom_area(fill = config$fill, alpha = 0.4) +
    geom_line(color = config$color, linewidth = 0.8) +
    geom_vline(xintercept = census_years, linetype = "dashed", color = "firebrick", alpha = 0.4) +
    facet_wrap(~label_ki_tb(name)) + 
    theme_ki_tb() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = config$title,
      subtitle = paste("Dashed lines indicate census anchor years | Level:", str_to_title(level)),
      x = "Year", y = "Share of Total (%)"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

## Plot Sex Ratio Trends -----------------------------------------------
plot_sex_ratio_diagnostics <- function(ratio_data, target_geo = "island") {
  
  # Set aesthetics
  config <- if(target_geo == "island") {
    list(fill = "darkseagreen", color = "darkseagreen4", vlines = c(1990, 2020))
  } else {
    list(fill = "slategray3", color = "slategray4", vlines = c(2005, 2020))
  }
  
  ratio_data %>%
    filter(geo == target_geo) %>%
    ggplot(aes(x = year, y = male_pct_final)) +
    geom_area(fill = config$fill, alpha = 0.4) +
    geom_line(color = config$color, linewidth = 0.8) +
    geom_hline(yintercept = 0.5, linetype = "dotted", color = "black", alpha = 0.6) +
    geom_vline(xintercept = config$vlines, linetype = "dashed", color = "firebrick", alpha = 0.5) +
    facet_wrap(~label_ki_tb(name)) + 
    theme_ki_tb() +
    coord_cartesian(ylim = c(0.4, 0.6)) + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = paste(str_to_title(target_geo), "Sex Ratios: Percentage Male"),
      subtitle = "Dotted line: 50% parity | Dashed lines: census coverage",
      x = "Year", y = "% Male"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}




