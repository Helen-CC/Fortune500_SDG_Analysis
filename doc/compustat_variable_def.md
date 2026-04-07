# Compustat Variable Definitions

Variables pulled by `code/pull_firm_characteristics.py` from WRDS Compustat.  
All monetary variables are in the **local reporting currency** (see `curcd`) and in **millions** unless noted.  
Coverage: fiscal years 2015–present.

---

## Compustat North America (`comp.funda`)

Output file: `{DATE}_comp_funda_us.csv`

| Variable | Full Name | Definition & Unit |
|----------|-----------|-------------------|
| `gvkey` | Global Company Key | Unique firm identifier in Compustat (zero-padded 6-digit string) |
| `datadate` | Data Date | Fiscal year-end date |
| `fyear` | Fiscal Year | Fiscal year (integer) |
| `conm` | Company Name | Standard company name as recorded in Compustat |
| `curcd` | Currency Code | ISO 4217 currency code of the reporting currency (e.g., USD, EUR) |
| `fic` | Country of Incorporation | ISO 3166-1 alpha-3 country code of the country where the firm is incorporated |
| `costat` | Company Status | Active (`A`) or Inactive (`I`) status indicator |
| `at` | Assets — Total | Book value of total assets; millions of local currency |
| `sale` | Sales/Turnover (Net) | Net sales or revenues after deducting returns, allowances, and discounts; millions of local currency |
| `revt` | Revenue — Total | Total revenues including non-operating revenues; millions of local currency |
| `emp` | Employees | Total number of employees; thousands |
| `ni` | Net Income | Net income (loss) after all charges including taxes and extraordinary items; millions of local currency |
| `ib` | Income Before Extraordinary Items | Pre-tax income minus income taxes, before extraordinary items and discontinued operations; millions of local currency |
| `oiadp` | Operating Income After Depreciation | Earnings before interest and taxes (EBIT) after deducting depreciation and amortization; millions of local currency |
| `ceq` | Common/Ordinary Equity — Total | Book value of common shareholders' equity (total equity minus preferred stock); millions of local currency |
| `pstk` | Preferred Stock — Total | Total carrying value of preferred/preference stock; millions of local currency |
| `prcc_f` | Price — Fiscal Year-End Close | Closing stock price at fiscal year-end; local currency per share |
| `csho` | Common Shares Outstanding | Number of common shares outstanding at fiscal year-end; millions of shares |
| `dltt` | Long-Term Debt — Total | Total long-term borrowings due after one year; millions of local currency |
| `dlc` | Debt in Current Liabilities | Short-term borrowings and current portion of long-term debt; millions of local currency |
| `naics` | NAICS Code | North American Industry Classification System code (header/primary code, from `naicsh`) |
| `sic` | SIC Code | Standard Industrial Classification code (header/primary code, from `sich`) |
| `roa` | Return on Assets | Derived: `ni / at` |
| `ros` | Return on Sales | Derived: `ni / revt` |
| `tobin_q` | Tobin's Q | Derived: `(prcc_f × csho + dltt + dlc) / at` |

---

## Compustat Global (`comp.g_funda`)

Output file: `{DATE}_comp_funda_global.csv`

The global table shares most variables with the North America table but has two differences in sourcing:

| Variable | Full Name | Definition & Unit |
|----------|-----------|-------------------|
| `gvkey` | Global Company Key | Unique firm identifier in Compustat (zero-padded 6-digit string) |
| `datadate` | Data Date | Fiscal year-end date |
| `fyear` | Fiscal Year | Fiscal year (integer) |
| `conm` | Company Name | Standard company name as recorded in Compustat |
| `curcd` | Currency Code | ISO 4217 currency code of the reporting currency |
| `fic` | Country of Incorporation | ISO 3166-1 alpha-3 country code of the country where the firm is incorporated |
| `costat` | Company Status | Active (`A`) or Inactive (`I`) status indicator |
| `at` | Assets — Total | Book value of total assets; millions of local currency |
| `sale` | Sales/Turnover (Net) | Net sales after deducting returns and allowances; millions of local currency |
| `revt` | Revenue — Total | Total revenues including non-operating revenues; millions of local currency |
| `emp` | Employees | Total number of employees; thousands |
| `ni` | Net Income — Consolidated | Sourced from `nicon` (Net Income (Loss) — Consolidated) in `comp.g_funda`; millions of local currency |
| `ib` | Income Before Extraordinary Items | Pre-tax income minus income taxes, before extraordinary items; millions of local currency |
| `oiadp` | Operating Income After Depreciation | EBIT after depreciation and amortization; millions of local currency |
| `ceq` | Common/Ordinary Equity — Total | Book value of common shareholders' equity; millions of local currency |
| `pstk` | Preferred Stock — Total | Total carrying value of preferred/preference stock; millions of local currency |
| `prcc_f` | Price — Fiscal Year-End Close | Not available in `comp.g_funda`; populated as `NULL` |
| `csho` | Common Shares Outstanding | Sourced from `cshoi` (Common Shares Outstanding — Issue) in `comp.g_funda`; millions of shares |
| `dltt` | Long-Term Debt — Total | Total long-term borrowings due after one year; millions of local currency |
| `dlc` | Debt in Current Liabilities | Short-term borrowings and current portion of long-term debt; millions of local currency |
| `naics` | NAICS Code | North American Industry Classification System code (from `naicsh`) |
| `sic` | SIC Code | Standard Industrial Classification code (from `sich`) |
| `roa` | Return on Assets | Derived: `ni / at` |
| `ros` | Return on Sales | Derived: `ni / revt` |
| `tobin_q` | Tobin's Q | Derived: `(prcc_f × csho + dltt + dlc) / at`; `NULL` for global firms since `prcc_f` is unavailable |
