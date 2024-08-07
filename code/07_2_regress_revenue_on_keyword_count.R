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

# pick up a set of keywords to use
df_final_key <- read_rds(glue("{DROPBOX_PATH}/cleaned_data/df_final_key_all.rds"))

#' @section Join two dataframes from script 1 and 2 to obtain keyword counts from all SDG categories and from all industries
# iterate through all NAICS codes
NAICS2_CODES <- c(11, 21, 22, 23, 31, 32, 33, 42, 44, 45, 48, 49, 51, 52, 53, 54, 60, 62, 72)
sdg_data <- read_rds(glue("{DROPBOX_PATH}/cleaned_data/df_regression_by_SDG_NAICS_firmyear.RDS"))

#' @section Regression

# create indicator variables
# TODO: add more indicators
data <- sdg_data %>% 
  mutate(isNAICS21 = ifelse(naics == 21, 1, 0),
         isNAICS31 = ifelse(naics == 31, 1, 0),
         isNAICS11 = ifelse(naics == 11, 1, 0)) 

# inspect column names
sdg_data %>% colnames()

# plot histograms to see the difference of distributions
sdg_data %>% 
  ggplot()+
  geom_histogram(aes(n_keyword))+
  facet_wrap(~naics, nrow = 2)

#' run regressions
#' Identification:
#'     the number of keywords mentioned in SDGXX by a company ~ industry indicators + error
# add fixed effect
reg1 <- feols(n_keyword ~ isNAICS21 | year, data = data)
# show the regression table
etable(reg1, coefstat = "tstat")

# if there is more than one industry indicators...
reg2 <- feols(n_keyword ~ isNAICS21 + isNAICS31 + isNAICS11 - 1, 
              data = sdg_data)
etable(reg2, coefstat = "tstat")
