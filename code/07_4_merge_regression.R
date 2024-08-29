#' @title Create sdg_data for regressions
# clean up environment
rm(list = ls()); gc()

library(ggplot2) # for plotting
library(fixest) # for regression models
library(dplyr) # for dataframe operations
library(tidyr) # for data cleaning
library(readr) # for RDS data loading
library(glue) # for string concatenation
library(dotenv) # for environmental variable loading
source("./code/config.R", encoding = '')

# import company reference (gvkey-year-rank mapping)
gvkey_rank_mapping <- read_csv("./data/company_reference/company_reference/gvkeys-rank_mapping.csv")
# pick up a set of keywords to use
# df_final_key <- read_rds(glue("{DROPBOX_PATH}/cleaned_data/df_final_key_all.rds"))
# obtain keyword counts from all SDG categories and from all industries
sdg_data <- read_rds(glue("{DROPBOX_PATH}/cleaned_data/df_regression_by_SDG_NAICS_firmyear.RDS"))

sdg_data <- sdg_data %>%
  left_join(gvkey_rank_mapping %>%
              select(-fyear) %>%
              rename(compname = Name), 
            by = c("rank" = "Rank"))

str(sdg_data)
sdg_data$gvkey %>% as.numeric() %>% summary()


# TODO: figure out whether multiple NAICS code is a problem
# duplications: a firm having more than one NAICS code
sdg_data %>%
  group_by(gvkey, year, sdg_number) %>%
  count() %>%
  drop_na() %>%
  arrange(desc(n)) %>%
  filter(n > 1) %>%
  pull(gvkey) %>%
  unique()

# manually clean up
sdg_data <- sdg_data %>%
  filter(!(gvkey == 101846 & naics != 51))

sdg_data %>%
  group_by(gvkey, year, sdg_number) %>%
  count() %>%
  drop_na() %>%
  arrange(desc(n)) %>%
  filter(n > 1)

#' @section import Compustat data and convert currencies
firm_characteristics_us <- read_csv(glue("{DROPBOX_PATH}/raw_data/compustat/company_value_us.csv"))
firm_characteristics_glob <- read_csv(glue("{DROPBOX_PATH}/raw_data/compustat/company_value_global.csv"))
df_fx <- read_csv(glue("{DROPBOX_PATH}/cleaned_data/exchange_rates_to_usd.csv"))


# convert currencies using FX data
firm_characteristics_glob %>% colnames()
firm_characteristics_glob <- firm_characteristics_glob %>%
  select(gvkey, fyear, conm, curcd, at, emp, revt, naics, sic, datadate) %>%
  left_join(df_fx, 
            join_by(closest(datadate <= datadate)))

firm_characteristics_glob_converted <- firm_characteristics_glob %>%
  mutate(across(c(at, revt), ~ case_when(
    curcd == "AUD" ~ . / AUD_USD,
    curcd == "BRL" ~ . / BRL_USD,
    curcd == "CHF" ~ . / CHF_USD,
    curcd == "CNY" ~ . / CNY_USD,
    curcd == "EUR" ~ . / EUR_USD,
    curcd == "GBP" ~ . / GBP_USD,
    curcd == "HKD" ~ . / HKD_USD,
    curcd == "INR" ~ . / INR_USD,
    curcd == "JPY" ~ . / JPY_USD,
    curcd == "KRW" ~ . / KRW_USD,
    curcd == "MXN" ~ . / MXN_USD,
    curcd == "MYR" ~ . / MYR_USD,
    curcd == "NOK" ~ . / NOK_USD,
    curcd == "RUB" ~ . / RUB_USD,
    curcd == "SAR" ~ . / SAR_USD,
    curcd == "SEK" ~ . / SEK_USD,
    curcd == "SGD" ~ . / SGD_USD,
    curcd == "THB" ~ . / THB_USD,
    curcd == "TRY" ~ . / TRY_USD,
    curcd == "TWD" ~ . / TWD_USD,
    curcd == "UGX" ~ . / UGX_USD,
    curcd == "USD" ~ . / 1,
    TRUE ~ NA_real_
  )))

firm_characteristics_glob_converted %>% colnames()
firm_characteristics_glob_converted <- firm_characteristics_glob_converted %>%
  select(gvkey, fyear, conm, at, revt, emp, naics, sic)


# Merge US, global firm characteristics with keyword counts
df_merged <- firm_characteristics_us %>% 
  select(gvkey, fyear, conm, at, revt, emp, naics, sic) %>%
  full_join(firm_characteristics_glob_converted) %>%
  distinct() %>%
  # label the dupes
  group_by(gvkey, fyear) %>%
  mutate(dup_id = row_number()) %>%
  ungroup() %>%
  # remove the rows with dup_id > 1
  filter(dup_id == 1)

df_merged %>%
  group_by(gvkey, fyear) %>%
  count() %>%
  drop_na() %>%
  arrange(desc(n)) %>%
  filter(n > 1)


df_merged <- df_merged %>%
  mutate(gvkey = as.numeric(gvkey)) %>%
  left_join(sdg_data %>%
              select(gvkey, rank, year, sdg, sdg_number, 
                     country = Country, n_keyword, naics_self = naics),
            join_by(gvkey == gvkey, closest(fyear <= year))) 

df_merged %>%
  group_by(gvkey, fyear, sdg) %>%
  count() %>%
  drop_na() %>%
  arrange(desc(n)) %>%
  filter(n > 1)


df_merged <- df_merged %>%
  # create sic2 industry classification
  mutate(sic2 = stringr::str_sub(sic, 1, 2)) %>%
  # drop uncessary variables
  # select(-fyear)
  # make sure fyear and year aligns
  filter(fyear == year)
  
#' save merged data
#' at `(firm, year, sdg, keyword_count)` level
write_rds(df_merged, glue("{DROPBOX_PATH}/cleaned_data/regression_data_merged.RDS"))

