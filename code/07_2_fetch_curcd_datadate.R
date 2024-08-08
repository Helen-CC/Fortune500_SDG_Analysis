# pull a table of currency and datadate table for later conversion 
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

firm_characteristics_glob <- read_csv(glue("{DROPBOX_PATH}/raw_data/compustat/company_value_global.csv"))

unique_curcd <- firm_characteristics_glob %>%
  select(curcd) %>% 
  drop_na() %>%
  distinct() %>%
  arrange(curcd)

write_csv(unique_curcd, glue("{DROPBOX_PATH}/raw_data/compustat/unique_curcd.csv"))

