# Fortune 500 Analysis

# Project structure and crucial paths

The project folder should contain files such that the file structure satifies:

```shell
.
├── Fortune500_SDG_Analysis.Rproj
├── code
│   ├── 01_1_keyword.R
│   ├── 02_1_parallel_wordCount.R
│   ├── 02_1_parallel_wordCount_progressbar.R
│   ├── 02_2_run_wordCount.R
│   ├── 03_1_TF-IDF.R
│   ├── 04_1_Heatmap.R
│   ├── 05_1_time_trend.R
│   ├── 06_1_most_frequent_keyword.R
│   ├── 07_1_setup_keyword_count_data.R
│   ├── 07_2_fetch_curcd_datadate.R
│   ├── 07_3_fetch_exchange_rates.ipynb
│   ├── 07_4_regress_revenue_on_keyword_count.R
│   ├── config.R
│   ├── requirements.R
│   └── utils
│       ├── convert_pdf_to_txt.R
│       ├── func.R
│       ├── pull_historical_compustat_headers.R
│       └── weakWords.R
├── data
│   ├── Readme.md
│   ├── company_reference
│   │   ├── company_reference
│   │   │   └── gvkeys-rank_mapping.csv
│   │   ├── company_reference.xlsx
│   │   ├── gvkeys.txt
│   │   └── readme.md
│   ├── raw_data
│   │   ├── TM Final_FortuneG500 (2021)_v2.xlsx
│   │   ├── UoA-SDG-Keyword-List-Ver.-1.1.xlsx
│   │   ├── compustat
│   │   │   ├── company_value_global.csv
│   │   │   ├── company_value_global_mining.csv
│   │   │   ├── company_value_us.csv
│   │   │   └── readme.md
│   │   ├── keyword
│   │   │   ├── Keyword_SDSN.csv
│   │   │   ├── Keyword_all.csv
│   │   │   └── Readme.md
│   │   └── manual_edit_keywords.xlsx
│   └── result
│       ├── ...
├── master_script.R
└── readme.md
```

A `.env` file should be created following `.env.template`'s format.


# Replication steps

## `master_script.R`

In the root folder, there is a master script that shows each step to process data, create features, and perform analyses.

Raw data such as keyword sets, company mapping (using rank-year or gvkey-year) are managed by git; However, other binary files are stored in Dropbox and should not be managed by git to avoid making the repository unnecessarily big.


# TODOs

- [x] Add regression analyses to compare keyword counts for different SDG categories within the same industry.

