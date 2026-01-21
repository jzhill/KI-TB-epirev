# AGENT HANDOVER: KI‑TB‑epirev Project

This document provides specific operational instructions for GenAI agents (LLMs) contributing to the **KI‑TB‑epirev** repository. It supplements the `README.md` by defining the "how-to" of the code logic, coding style, and data safety protocols.

## 1. Project Logic & Architecture

The pipeline is designed to handle **non-rectangular, visual Excel data** spanning 25+ years.

### Key Design Pillars:

-   **Metadata-Driven Ingestion**: We do not hard-code ranges. Script `01_` detects start/end rows based on "anchors" (TB No, Name, Sex). Column names are mapped via `data-raw/TBreg_column_names.csv`.

-   **Explicit Coding (Lookup Workflow)**: We avoid nested `case_when` or complex regex for mapping categories in the main cleaning script. Instead, we use a **Value → Lookup → Join** pattern.

-   **The "Reference Slug"**: All processed data is stored in `data-processed/<reference_slug>/`. This slug is derived from the source filename (e.g., `TB_Register_20240501`).

## 2. Technical Stack & Coding Standards

When proposing code, you **must** adhere to these preferences:

-   **Tidyverse First**: Use `dplyr`, `tidyr`, and `stringr`.

-   **Explicit Imports**: Use `import::from(package, function)` at the top of `R/functions_all.R`. Do not use `library()` for large packages inside function files.

-   **File Paths**: Always wrap paths in `here::here()`.

-   **RStudio Headings**: Follow the required format:

    -   `# Section Title -----------------` (Level 1)

    -   `## Subsection Title -------------` (Level 2)

    -   *Note: At least 4 dashes are required for RStudio outline recognition.*

-   **Object Storage**: Use `qs::qsave()` and `qs::qread()` for intermediate data objects to preserve data types and speed.

## 3. Data Governance (Critical)

-   **No PII**: Never ask for or output names, specific dates of birth, or exact home addresses.

-   **Debug via Inventory**: If cleaning fails, ask for a `count()` of unique values or the `data_error_log.csv` summary rather than raw rows.

-   **Zero-Identification Policy**: Any example data provided in chat must be synthetic or heavily anonymized.

## 4. Specific Workflows for Agents

### Adding a New Column/Variable

1.  **Identify**: Check if the raw column is already in the `TBreg_column_names.csv` mapping.

2.  **Process**: Add the cleaning logic to `R/02_clean_data.R`.

3.  **Audit**: Add the new variable to the `cleaning_map` in `02_` to ensure it appears in the `data_error_log.csv` if parsing fails.

### Handling Messy Strings & Geography

-   **Geography Inference**: Use `R/04_geo_lookup_helper.R`. It matches raw strings against `geo_helper.xlsx`.

-   **Synonym Updates**: If an island name is misspelled (e.g., "Gonouti" for "Nonouti"), instruct the user to:

    1.  Add the synonym to the "lookup strings" sheet in `data-raw/geo_helper.xlsx`.

    2.  Run `04_geo_lookup_helper.R`.

    3.  Copy `unique_geo_lookup_processed.csv` to `data-raw/unique_geo_lookup.csv`.

-   **Lab Results**: Use existing regex in `R/functions_all.R`. Check `neg_patterns` and `unc_patterns` before adding new logic.

### The Lookup Maintenance Loop

Agents must respect the iterative "loop" for all lookup-based coding:

1.  Run `02_clean_data.R` to generate unique value inventories in `data-processed/`.

2.  Copy unique files (e.g., `unique_geo.csv`) to `data-raw/`, append `_lookup.csv`, and add coding.

3.  Re-run `02_clean_data.R` and monitor `data_error_log.csv`. The log identifies rows where raw data exists but coded values are `NA`.

### Troubleshooting the Ingestion (`01_`)

If a sheet fails to load or columns are misaligned, use the verification files:

1.  **`header_verification.csv`**: Compare proposed names against the raw visual header block. If they don't match, suggest updates to `TBreg_column_names.csv`.

2.  **`sheet_technical_summary.csv`**: Check the detected start/end rows and column counts.

3.  **Anchor Check**: Ensure Col C (TB No), Col D (Name), and Col E (Sex) are the correct anchors for the problematic year.

### Troubleshooting the Coding or Errors

If there are unexpected results in analysis or problems running the script

1.  Prompt the user to check \_lookup.csv files, the error log output, and confirm any updates have been applied.
2.  Note the loop between lookup, audit of unique values and coding - once any lookup has been edited, then prompt the user to run 02\_ again and then replace the appropriate lookup.csv file with the new version from data-processed.

## 5. Collaboration Contract

-   **Minimalism**: Propose small, modular changes.

-   **Validation**: Every code change should be accompanied by a suggestion for a QA check (e.g., "After this mutate, run `count(new_var)` to check for NAs").

-   **Documentation**: Update this `agents.md` if you introduce a new architectural pattern (e.g., a new stage `03_`).

## 6. Current Roadmap for Agents

1.  Inspection and loading phase is now mature
2.  Cleaning and coding is done to a high level, there may be additional tasks occasionally
3.  Focus on analysis, reporting, outcomes, developing scripts and data transformations to support figures, plots, tables and further investigation.
4.  Make sure the project remains reproducible, transferable and easy to hand over and add collaborators.
