# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Research pipeline analyzing how Fortune 500 companies integrate UN Sustainable Development Goals (SDGs) in their annual reports. The pipeline counts SDG keyword frequencies in corporate reports, then runs econometric regressions relating SDG mentions to firm financial performance (revenue, ROA, Tobin's Q).

## Environment Setup

Copy `.env.template` to `.env` and set:
```
DATA_FOLDER="PATH_TO_DATA/Fortune500_SDG_Analysis/data"
PROJECT_FOLDER="PATH_TO_GITHUB_REPO/repo/Fortune500_SDG_Analysis"
```

**Note:** `DATA_FOLDER` points to a Dropbox folder (large binary files, raw reports). `PROJECT_FOLDER` points to this git repo. These are separate locations.

Install R packages by sourcing `./code/requirements.R`. Python dependencies (for WRDS data fetching and FX rates) are managed via `uv` (see `pyproject.toml`); requires Python >=3.13.

## Running the Pipeline

```r
# Run full pipeline (sequential, ~16+ hours total)
source("./master_script.R")

# Run individual steps
source("./code/01_1_keyword.R")         # Process keyword dictionary (~1 sec)
source("./code/02_2_run_wordCount.R")   # Count keywords (~14 hrs, parallel)
source("./code/02_4_run_CIP_colocation_count.R")  # Colocation indices (~2.6 hrs)
source("./code/03_1_TF-IDF.R")          # TF-IDF plots
source("./code/04_1_Heatmap.R")         # Heatmaps by SDG/firm
source("./code/05_1_time_trend.R")      # Time trend analysis
source("./code/06_1_most_frequent_keyword.R")  # Top keywords
source("./code/07_4_merge_regression.R")       # Merge with financials
source("./code/07_5_regress_revenue_nkeyword.R")  # Main regressions
```

```bash
# Python scripts (require WRDS credentials)
python ./code/pull_firm_characteristics.py    # Fetch Compustat data
python ./code/07_3_fetch_exchange_rates.py    # Fetch FX rates
```

## Architecture

### Data Flow

```
Raw Annual Reports (TXT/HTM in Dropbox)
    → [02_2] Parallel keyword counting (by NAICS sector, uses all cores - 1)
    → Keyword counts RDS files (in Dropbox)
    → [07_1] Aggregate to firm-year-SDG level
    → [07_4] Merge with Compustat financials + convert currencies to USD
    → Final regression dataset (firm-year × SDG + financial controls)
    → [07_5] Regressions with fixest (robust SEs, FE)
    → LaTeX tables + figures in data/result/
```

### Key Design Decisions

**File naming convention for reports:** `gvkey_companyname/` or `_companyname/` (no gvkey). `parseFilenameFromPath()` in `utils/func.R` handles both cases. Matching between report folders and company reference uses a cascading strategy: gvkey match → name match → fuzzy match.

**Keyword patterns:** Raw keywords support `*` (wildcard), `AND` (both terms must appear in sentence), `;` (alternative forms). Step 01 compiles these into regex patterns stored as RDS.

**Parallel processing:** `02_2_run_wordCount.R` splits across 19 NAICS sectors and uses `doFuture`/`progressr` for parallelism. Progress is logged to `./logs/progress_NAICS*.log`.

**Currency handling:** Annual reports filed in non-USD currencies are converted using FX rates fetched in step 07_3. The `curcd` and `datadate` fields from Compustat identify which currency/year to use.

### Key Files

| File | Role |
|------|------|
| `code/config.R` | Loads `.env`, sets `DROPBOX_PATH` and `PROJECT_PATH` |
| `code/requirements.R` | Installs/loads all R packages |
| `utils/func.R` | Core helpers: `readReports()`, `getGvkeyMap()`, `parseFilenameFromPath()` |
| `utils/count_keyword_frequency_parallel.R` | Keyword counting using `stringi::stri_count` with regex |
| `utils/compute_colocation_complementary_index.R` | Computes CIP (Complementarity, Independence, Pressure) indices |
| `utils/weakWords.R` | Custom stopwords list used during tokenization |
| `data/raw_data/keyword/Keyword_all.csv` | Master SDG keyword dictionary |

### Output Locations

- Figures (PNG, 300 DPI): `data/result/figures/`
- Tables (LaTeX): `data/result/tables/`
- Intermediate RDS files: Dropbox `DATA_FOLDER` (not in git)

### Testing

`utils/func.R` contains `testParseFilenameFromPath()` for validating the folder naming convention parser. Run it directly in R to verify parsing logic after changes.

## Key R Packages

- `fixest` — regressions with fixed effects and robust SEs
- `stringi` — fast regex-based keyword counting (`stri_count`)
- `tidytext` / `stopwords` — tokenization
- `doFuture` / `progressr` — parallel processing with progress bars
- `dotenv` — loads `.env` into environment variables
- `kableExtra` — LaTeX table generation
- `viridis` / `ggrepel` — visualization helpers
