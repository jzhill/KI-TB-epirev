# KI-TB-epirev

**Retrospective review of TB epidemiology in Kiribati 1998–2025**

This repository contains the data science pipeline for the Kiribati National Tuberculosis Programme (NTP) retrospective register analysis. It is a core component of the **PEARL Project**, designed to transform legacy paper-to-Excel records into a high-quality, publishable epidemiological dataset.

## Quick Start

1.  **System Setup**: Install [R](https://cran.r-project.org/ "null") and [RStudio](https://posit.co/download/rstudio-desktop/ "null").

2.  **Clone Repository**: Fork and clone this repo using the [Happy Git with R](https://happygitwithr.com/fork-and-clone "null") method.

3.  **Environment**: Open the `.Rproj` file. RStudio should automatically prompt you to install `renv`. Run `renv::restore()` to install all specific package versions.

4.  **Data Acquisition**: Ensure the raw `.xlsx` TB register is placed in `data-raw/register/`.

5.  **Execution**: Run scripts sequentially: `R/01_inspect_load_data.R`, then `R/02_clean_data.R`.

## About this Analysis

This analysis was commissioned by the Kiribati NTP in collaboration with the PEARL project, with funding from the Australian Government.

### Purpose

-   **Strategic Planning**: To identify longitudinal trends (1998–2025) in Case Notification Rates (CNR) to inform the National Strategic Plan for TB.

-   **Service Improvement**: To pinpoint geographic "hotspots" and demographic gaps in TB detection and treatment completion.

-   **Scientific Contribution**: To generate high-quality figures and tables for peer-reviewed publications regarding TB transmission in high-burden Pacific settings.

### The PEARL Project Context

The PEARL project is an Australian aid initiative implemented by the University of Sydney in partnership with the Kiribati Ministry of Health and Medical Services (MHMS). While the flagship initiative involves mass screening in South Tarawa, this retrospective review provides the essential "baseline" required to measure the impact of these community-based interventions.

## Technical Approach

This project is built on modern "Tidy" data science principles. It moves away from "black-box" cleaning toward a transparent, reproducible pipeline.

### Core Stack

-   **Language**: [R Programming](https://www.r-project.org/ "null")

-   **Framework**: [Tidyverse](https://www.tidyverse.org/ "null") (for readable, piped data manipulation).

-   **Reproducibility**: [renv](https://rstudio.github.io/renv/ "null") for package management and [here](https://here.r-lib.org/ "null") for robust file paths.

### Learning Resources

For users familiar with Excel who wish to understand the R logic used here, we recommend:

-   [R for Data Science (2e)](https://r4ds.hadley.nz/ "null"): The definitive guide to the `tidyverse` approach used in this repo.

-   [The Epidemiologist R Handbook](https://epirhandbook.com/en/ "null"): A vital resource for standardizing TB data and cleaning health records.

## Data Pipeline

The pipeline is designed to be reproducible and auditable, converting the rich but messy Excel sheets to a meaningful, analysable data set.

Summary of pipeline:

1.  **Ingestion**: `01_inspect_load_data.R` extracts data from annual worksheets, removing visual headers, unwanted rows, or any data outside the valid data range.

2.  **Verification**: The script generates `header_verification.csv` and `sheet_technical_summary.csv`. **Users must inspect these** to ensure all columns were mapped correctly.

3.  **Cleaning & Coding**: `02_clean_data.R` standardizes dates, ages, and clinical classifications.

4.  **Error Logging**: Any data that cannot be automatically coded is sent to `data_error_log.csv`.

5.  **Refinement**: If "Unclassified" values appear, users update the \_lookup.csv files in `data-raw/` and re-run the pipeline.

6.  **Analysis**: Clean data is used for calculations, figures, tables and geographic mapping.

## Repository Structure

-   `~/data-raw`: Input files (Register, Population Census, Lookups). **Never edit files here directly except for lookup tables.**

-   `~/data-processed`: Created automatically. Contains versioned folders for each run of the register.

-   `~/R`: The engine room. Contains modular scripts and `functions_all.R`.

-   `~/figures` & `~/reports`: Destination for final epidemiological outputs.

-   `~/renv`: Folders for R package management (ensures the code works on your machine exactly as it did on ours).

## Raw project data required

### TB register file from NTP

This is a copy of the .xlsx TB register maintained by the Kiribati National TB Programme data officer. There is a valid data range in each worksheet of the register, starting from the first row where a patient is registered for the year. The script expects a register file with the following:

-   Worksheets for each year of register data, named in the format YYYY

-   Visual style header blocks at the top of each sheet

-   TB registration case data with one row per case (see specific expectations in 01\_ section below)

Owned by Kiribati MHMS and used with permission.

### TBreg_column_names.csv

This is a .csv file that translates the visual header blocks into clean analysable and meaningful column names for each year of the register. They register has changed over time with columns being renamed, added and removed, and this list acts as a catalogue to insert correct column names after removing the visual heading blocks from each sheet.

### geo_helper.xlsx

A multi-sheet lookup tool that resolves messy, free-text addresses into official census-coded islands and villages. It includes a `lookup_table` that translates common misspellings or local synonyms into standardized geographic identifiers.

### pop_island_census.csv

Contains historical population data from the 1947–2020 Kiribati Censuses. This provides the denominators required to calculate Case Notification Rates (CNR) per 100,000 population.

### \_lookup.csv files

These files (e.g., for treatment outcomes or disease sites) enable **explicit coding**. Instead of guessing what a shorthand note means, we explicitly map strings like "P" to "Pulmonary" and "EP" to "Extra-pulmonary" in a transparent CSV.

## User inputs - IMPORTANT TO UNDERSTAND THIS

### Verify column names and data ranges

This is very important.

1.  The script takes the data from each annual worksheet once it has determined the valid data range WITHOUT HEADERS.

2.  Then we apply headings to each column from the TBreg_column_names.csv file.

3.  If the columns in the raw data register have changed, then we need to figure out how and update the TBreg_column_names.csv file accordingly.

There are two verification files that are generated by the 01\_ script for us to check:

1.  `header_verification.csv`: this file outputs a long list of column headings proposed for each year, matched with any data from the header block in that column. We need to check: 1) column names seem to match what was in the header block and 2) it looks like the right number of columns present for each sheet
2.  `sheet_technical_summary.csv`: this file outputs a summary of the number of rows and columns that have been detected for the data range, and where the range starts and ends. This is what is used to import the TB case data.

### Inspect error log and lookup files

This allows us to make sure that the our data coding steps are not missing a whole lot of data. The error logger will pick up any rows where the raw column contained data and the coded column did not. If there is new data with uncoded data, then we can add coding references to the lookup file. For example, if we see a new spelling error for the island Nonouti as "Gonouti", then we need to add that to the lookup.

### Geography string-based search

In the example above, there are two methods for applying a new lookup code. For island spelling errors, we can use the "lookup strings" worksheet from the raw `geo_helper.xlsx` file. This sheet contains a list of search strings or synonyms for each island or village. Usage:

1.  Add new synonym underneath the island or village list in "lookup strings". (There is an `=LET()` function in "lookup table" worksheet that translates the synonyms into long format.)
2.  Save the file
3.  Run `04_geo_lookup_helper.R`
4.  Copy the `unique_geo_lookup_processed.csv` into data-raw
5.  Check that coding has been applied correctly, update the manual coding if needed
6.  Save the file as `unique_geo_lookup.csv` (replacing the old one)
7.  Run 02\_ to bring in the new coding.

### Good practice for lookup files

There is a loop in the lookup file logic to make sure that manual coding is perpetuated through the data processing

1.  Unique values are extracted from the raw columns and saved in data-processed
2.  User copies the unique file, appends `_lookup.csv` and saves in data-raw
3.  Updates with any new lookup values and saves
4.  Run the 02\_ script again, generate unique output csv, copy across again
5.  Etc

Whenever editing the lookup files, need to be aware of this loop!

## Processing Logic

### functions_all.R

A central library of custom R functions. By separating logic (how to parse a date) from the workflow (cleaning the data), we ensure the main scripts remain readable for health professionals while maintaining high technical rigor.

### 01: Inspect and Load

The register file is non-rectangular, spread across years, has merged rows and columns, and data entered outside the central data range. This script:

-   Identifies the "Valid Data Range" by looking for the first row with a Name and TB Number.

-   Verifies that the column count matches the metadata expected for that specific year.

-   **User Action**: Always review `header_verification.csv` after running to ensure no columns shifted in the raw Excel file.

#### Important note about register sheets structure for loading

The script looks for the first row in each sheet where

1.  Column C contains a valid integer (TB number)
2.  Column D contains a text string (name)
3.  Column E contains a text string with either "M", "F", "MALE", "FEMALE" (sex)

Anything above this first row is considered part of the header block.

### 02: Tidy and Clean

A mix of cleaning methods are used, for example including existing tools provided in R packages to interpret common data types (eg dates or age groups), inline coding where the column contains constrained/clean data (sex), direct lookup files where a variety of important data is present over the years (disease type) or a combination of string lookup and manual coding for complicated or subjective data (address, bacteriology).

-   **Demographics**: Standardizes age into WHO-recommended brackets.

-   **Clinical**: Uses regex logic to extract GeneXpert and Smear results from messy lab strings.

-   **Geographic Coding**: Uses `04_geo_lookup_helper.R` logic to infer the island and village. If a patient is listed in "Bairiki," the script automatically assigns them to "South Tarawa."

#### Extra note about geographic coding

-   This is one of the most labour intensive but productive parts of the data project

-   Location data has been recorded mostly in "address"

-   Addresses have been entered as a mix of descriptive and coded data into one field

-   No additional coding/columns for location at island or village level

-   Developed a process of search strings and manual coding to allocate each patient to an island, and a village if in south tarawa

### 04: Helper Scripts

-   **Population Estimates**: Extrapolates annual population figures between census years using linear growth assumptions, providing yearly denominators for trend analysis.

-   **Geography Inference**: Handles the complex task of matching text strings against the `geo_helper` reference.

## Analysis and Outputs

To maintain a "Clean Code" approach, we separate **Data Preparation** from **Visualisation**:

1.  **Preparation**: Functions filter and aggregate data (e.g., "Get CNR by Island and Year").

2.  **Output**: Dedicated functions take those tables and produce standardized, "publication-ready" ggplot2 figures or GT tables.
