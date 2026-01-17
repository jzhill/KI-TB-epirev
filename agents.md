---
editor_options: 
  markdown: 
    wrap: 72
---

# KI TB register retrospective analysis — agent guide (agents.md)

This document is the working “operating manual” for any assistant (human
or LLM) contributing code, analysis, or documentation to the
**KI‑TB‑epirev** project.

**Project objective** - Produce high‑quality retrospective
epidemiological analyses from the Kiribati National Tuberculosis
Programme (NTP) register to support **strategic planning** and generate
**publishable outputs**. - The raw register is a longitudinal Excel
workbook with annual worksheets and a visually structured header block
(merged cells, gaps, non‑rectangular layout). The pipeline converts that
into a stable analytic dataset using robust range detection + metadata
column mapping + lookup‑based coding.

**Data governance** - Treat the raw register as **highly sensitive**. Do
**not** request, reproduce, or paste patient‑level content into chats,
issues, or documentation. - Prefer aggregations, tabulations, and QA
outputs. If you must debug parsing/cleaning, do it using **structure**,
**counts**, **unique value inventories**, and **non‑identifying
examples**.

------------------------------------------------------------------------

## 1) Repository map and key artefacts

**Top level** - `R/` : pipeline and analysis scripts (tidyverse‑first) -
`data-raw/` : raw inputs and reference tables (incl. the Excel
register) - `data-processed/` : outputs keyed to a specific register
file “reference slug” - `reports/` : Quarto or written outputs (if/when
added) - `figures/` : exported figures - `renv.lock` : package
reproducibility

**Raw data inputs (do not modify without versioning)** -
`data-raw/register/*.xlsx` : source register workbook (annual
worksheets) - `data-raw/TBreg_column_names.csv` : canonical column names
per year (schema contract) - `data-raw/geo_helper.xlsx` : census codes +
synonym lookup tables for geo inference -
`data-raw/unique_geo_lookup.csv` : lookup mapping raw strings →
island/village - `data-raw/unique_disease_sites_lookup.csv` : lookup
mapping raw strings → PTB/EPTB + site/type flags -
`data-raw/unique_registration_category_lookup.csv` : lookup mapping raw
strings → registration category - `data-raw/pop_island_census.csv` :
population denominators (outer island / ST etc.)

**Processed outputs (per register version)** -
`data-processed/current_register_reference.txt` : points to the current
reference folder - `data-processed/<ref>/register_combined_raw.qs` : raw
combined rectangle (as text) -
`data-processed/<ref>/register_combined_clean.qs` : cleaned + coded
analytic dataset - `data-processed/<ref>/sheet_technical_summary.csv` :
detected ranges per year -
`data-processed/<ref>/header_verification.csv` : raw header
concatenation vs metadata names -
`data-processed/<ref>/data_dictionary_raw_skim.csv` : skim summary of
raw - `data-processed/<ref>/data_error_log.csv` : “raw had value, clean
is NA” audit log - `data-processed/<ref>/unique_*.csv` : unique value
inventories to support iterative lookup coding

**Important conventions** - The processed folder name `<ref>` is derived
from the raw register filename (see `get_clean_ref()`); it is the unit
of reproducibility.

------------------------------------------------------------------------

## Project setup

### renv

this project uses the renv package to maintain a deployable and
reproducible environment. make sure that scripts use library() at the
top to define required libraries, and the user is responsible for
triggering renv::install(), renv::update() and renv::snapshot() as
necessary.

### import::from()

We use `import::from()` to import only the specific functions we need
(instead of attaching whole packages). This reduces masking/collisions
and makes dependencies explicit. These imports live at the top of
`functions_all.R`, so when it is sourced, the required function names
are available for the helper functions defined there.

### Functions, datasets and environments

Since we want a degree of portability and we want the data to be
available for quarto documents, i have tried to keep functions in
functions_all.R and use the qsave package to store objects nicely. this
means that other environments can source from functions_all.R and load
cleaned data. this is also the approach from one stage of cleaning to
the next, eg 01\_ to 02\_ to 03\_.

## 2) End‑to‑end pipeline (the contract)

### Stage A — Inspect and load (R/01_inspect_load_data.R)

Purpose: convert a “visual” Excel register sheet into a consistent
rectangular dataset.

Core logic: 1. Find latest register file in `data-raw/register/`. 2.
Identify annual sheets by cleaned sheet names matching `^\\d{4}$`. 3.
For each year sheet: - detect the start row using a TB number + name +
sex pattern - detect end row (first run of 3 consecutive blanks in TB
number column) - detect last header column by scanning header cells
above the start row - assign canonical column names from
`TBreg_column_names.csv` using “closest applicable year” logic - read
only the rectangle defined by start/end rows and metadata column length
4. Bind all years and validate: - no entirely empty columns - row counts
match intended totals across sheets - anchor fields (name, TB number)
present - TB number unique within year 5. Write: -
`sheet_technical_summary.csv`, `header_verification.csv` - raw combined
register (`.qs` + `.csv`) and sheet list (`register_raw_list.qs`)

**Do not change** the range detection without adding new tests and
checking: - row count parity, missing anchors, and duplicates.

### Stage B — Clean and code (R/02_clean_data.R)

Purpose: create an analytic dataset with stable IDs, typed variables,
and lookup‑coded geography/disease/category.

Core outputs: - `tb_id` (unique per row): `reg_year` + formatted TB
number (`tb_no_clean`) - `*_clean` columns: - `date_reg_clean`,
`date_start_clean` from `parse_mixed_date()` (Excel serials + text) -
`age_clean` from `clean_age_column()` - `sex_clean` from
`clean_sex_column()` - age groupings: - `age_group_10yr`,
`age_group_who`, `age_group_new` - coded geography from lookup: -
`island`, `ST_village`, `NT_village`, `division`, codes, and `ST_OI` -
factor level ordering derived from census codes - coded disease/site
flags from lookup: - `ptb_eptb` and `type_*`, `site_*` logical flags -
coded registration category: - `registration_category` with strict
conflict resolution (one match only) - QA: - exports `unique_geo.csv`,
`unique_disease_sites.csv`, `unique_registration_category.csv`
inventories - writes `data_error_log.csv` for systematic review

**Design principle:** Raw free‑text is not “fixed” by ad‑hoc string
rules in the main pipeline. Instead: 1) generate a unique‑value
inventory, 2) update a lookup file, 3) re‑run the pipeline.

### Stage C — Helper workflow for geo synonyms (R/04_geo_lookup_helper.R)

Purpose: accelerate geo lookup completion by auto‑suggesting
island/village from synonym tables.

-   Uses `geo_helper.xlsx`:
    -   `lookup_table` (search_term → official_name + category)
    -   `census_codes` (official names + codes)
-   Writes `unique_geo_lookup_processed.csv` into current processed
    folder for manual review and merge back.

------------------------------------------------------------------------

## 3) Coding style and collaboration rules

**R style** - Prefer tidyverse pipelines (`dplyr`, `tidyr`, `stringr`),
aligned with *R for Data Science* / *Epi R Handbook* idioms. - Write
small, composable functions in `R/functions_all.R` (or a new
`R/functions_*.R` if it grows). - Avoid hidden side effects. Any file
write should be explicit and land in `data-processed/<ref>/` or
`figures/`.

**File paths** - Always use `here::here()`. - Never hard‑code absolute
paths.

**Data types** - During import, keep columns as character to avoid bind
conflicts across years. - Perform typing and categorisation in the
cleaning stage.

**Factors** - For geography, factor level ordering should follow
official census codes. - For program categories (PTB/EPTB, registration
category), set explicit levels.

**Conflict handling** - Prefer deterministic rules: - column priority
lists (first match wins) - strict one‑category rule for registration
category (multiple matches → NA + audit)

**Performance** - Avoid rowwise operations in main pipelines unless
unavoidable. - Vectorize text detection when building inference flags
(as in geo helper).

**Rstudio headings** - note that headings in the style — Heading — are
not recognised by rstudio. instead, we use the style \# Code chunk
heading 1 ————— and \## Code chunk heading 2 ——————. There must be at
least 4 hyphens following the heading to be recognised by rstudio. Notes
or comments in code can be simply preface by #, without any hyphens.

------------------------------------------------------------------------

## 4) Lookup tables: how to extend safely

Lookup files are part of the “knowledge base” of the project. Extending
them is normal and expected.

**General pattern** 1. Run `02_clean_data.R`. 2. Open
`data-processed/<ref>/unique_*.csv` to see new unique values. 3. Update
the corresponding lookup in `data-raw/`: - `unique_geo_lookup.csv` -
`unique_disease_sites_lookup.csv` -
`unique_registration_category_lookup.csv` 4. Re‑run `02_clean_data.R`
and confirm: - `data_error_log.csv` decreases - category distributions
look sensible

**Rules** - Lookups match on **(original_column, clean_value)**. -
`clean_value` must be lowercased + trimmed. - Never delete prior
mappings without a reason; prefer edits that preserve auditability. -
Use `nr` consistently for “not recorded” rather than NA if the project
convention is `nr`.

------------------------------------------------------------------------

## 5) QA/validation expectations (minimum set)

Any substantive change should preserve or improve these checks:

**Import/structure (Stage A)** - No empty columns created by bind. - Sum
of per‑sheet intended rows == combined rows. - No missing anchor fields
(name + TB number) in retained rows. - TB number unique within year.

**Cleaning/coding (Stage B)** - `tb_id` is unique. - Date parsers
respect plausible year range and do not silently create out‑of‑range
dates. - Age parsing handles months/weeks conventions; unknown formats
are audited. - Registration category: - conflicts are left NA and
auditable. - Geography: - island not in census list should be audited. -
ST/NT villages only populated when island matches parent.

**Audit outputs** - `data_error_log.csv` is reviewed regularly; it is
the to‑do list for lookup completion.

------------------------------------------------------------------------

## 6) Common tasks and how an agent should proceed

### Task: add a new derived epidemiology variable

1.  Define the variable precisely (clinical/program definition and time
    reference).
2.  Identify raw columns involved.
3.  Implement as a clean `mutate()` step in `02_clean_data.R` (or a new
    `03_*` analysis script if purely analytic).
4.  Add a QA tabulation (counts by year; missingness).
5.  If it requires parsing messy strings, prefer the lookup workflow.

### Task: add a new year / changed register layout

1.  Update `TBreg_column_names.csv` for that year if column names
    changed.
2.  Run `01_` and inspect:
    -   `sheet_technical_summary.csv`
    -   `header_verification.csv`
3.  Only if range detection fails, adjust structural functions carefully
    and re‑validate row counts.

### Task: produce a figure/table for reporting

-   Implement in a `03_*` script with clear inputs (read
    `register_combined_clean.qs`).
-   Save outputs into `figures/` (and/or `data-processed/<ref>/` if
    intermediate).
-   Use clear filenames including the `<ref>` or a date tag if needed.

------------------------------------------------------------------------

## 7) Known pain points (design notes)

-   The register workbook is “visual” with merged headers and variable
    spacing; import relies on pattern detection rather than fixed
    ranges.
-   Many fields contain compound information; the safest strategy is
    **inventory → lookup → rerun**.
-   Geography requires careful handling (island/village constraints;
    consistent official naming; factor ordering by census codes).

------------------------------------------------------------------------

## 8) Suggested next extensions (roadmap)

These are safe, incremental improvements that preserve your existing
architecture:

1.  **Unit tests / lightweight assertions**
    -   Add `testthat` checks for key helper functions (TB number
        formatting, date parsing range, start/end row detection on
        synthetic tables).
2.  **Episode/person linkage strategy**
    -   Decide how to represent re‑registrations across years
        (episode‑based vs person‑based). If person‑based is needed,
        design a privacy‑preserving linkage approach (e.g., hashed
        identifiers if permissible).
3.  **Outcome and cascade variables**
    -   Define and compute treatment outcomes, delays (reg→start),
        bacteriology variables, and WHO indicator outputs.
4.  **Quarto reporting layer**
    -   Add `reports/` templates that read the processed `.qs` and
        produce standardized tables/figures.

------------------------------------------------------------------------

## 9) Minimal working example for any agent

**To reproduce the current dataset:** 1. Restore packages via renv. 2.
Run `R/01_inspect_load_data.R`. 3. Run `R/02_clean_data.R`. 4. Use
`data-processed/<ref>/register_combined_clean.qs` for analyses.

------------------------------------------------------------------------

## 10) If you are an LLM assistant: operating rules

-   Do not request patient‑level examples.
-   When proposing code changes:
    -   keep changes minimal and local,
    -   preserve existing checks,
    -   propose new QA summaries where relevant.
-   When parsing messy strings, prefer the established lookup workflow.
-   Use tidyverse idioms and explicit factor levels.
-   Treat `TBreg_column_names.csv` + lookup files as the project’s
    schema/ontology.
