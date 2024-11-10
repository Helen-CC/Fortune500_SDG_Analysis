#' The script shows the summary statistics of the data we use in regrssion analyses
#' Since we're using panel data, the level of each observation is firm-year level
#' 
rm(list = ls())
library(fixest)
library(readr)
library(stringr)
library(dplyr)
library(dtplyr)
library(tidyr)
library(kableExtra)
source("./code/config.R")

#' Load data
#' at `(firm, year, sdg, keyword_count)` level
df_merged <- read_rds(glue("{DROPBOX_PATH}/cleaned_data/regression_data_merged.RDS")) %>%
  mutate(naics2 = str_sub(naics, 0, 2)) 

# Only focusing on mining firms
gvkeys_mining <- read_csv(glue("{DROPBOX_PATH}/raw_data/compustat/company_value_global_mining.csv"))
gvkeys_mining <- gvkeys_mining %>% pull(gvkey) %>% unique()

gvkey_to_exclud <- c(272126, # Shaanxi
                     212777, # INDIAN OIL CORP LTD
                     208175  # OIL & NATURAL GAS CORP LTD
                     )

df_merged <- df_merged %>%
  mutate(is_mining = ifelse(gvkey %in% gvkeys_mining, 1, 0))

df_merged %>%
  group_by(gvkey, year) %>%
  count() %>%
  arrange(desc(n))

df_merged %>%
  group_by(gvkey, fyear, sdg) %>%
  count() %>%
  arrange(desc(n))

#' make summ table
# Add custom function to calculation residual standard deviations (after purging for bond and month variation)
sd_fe <- function(x){
  temp_lm <- felm(x ~ 0 | year + gvkey | 0 | 0, data = data)
  sd(temp_lm$residuals)
}

df_merged %>% colnames()
df_merged %>% View

# Outcome variables
other_outcomes <- c('at', 'revt', 'emp', 'is_mining',
                    'n_keyword'
                    )
other_outcomes_text <- c('asset', 'revenue', 'num employees', 'mining firm indicator',
                    'avg. num keywords in a SDG catgory'
                    )
# order <- order(other_outcomes_text)
# other_outcomes <- other_outcomes[order]
# other_outcomes_text <- other_outcomes_text[order]
 
# Combined
sel <- c(other_outcomes)
sel_text <- c(other_outcomes_text)

# Function for descriptives
multi.summary <- function(x) {
  c(mean = mean(x, na.rm = T), sd = sd(x, na.rm = T), p = quantile(x, probs = c(.25, .5, .75), na.rm = T), Obs. = sum(!is.na(x)))}

# Create and format table
table_descriptives <- sapply(df_merged %>%
                               select(sel), multi.summary)
table_descriptives <- t(table_descriptives)
table_descriptives <- data.frame(table_descriptives)

# Custom number format functions
# Trailing zeros
cust_format1 <- function(x, digits){formatC(x, format="f", big.mark=",", digits = 2)}
# No trailing zero
cust_format2 <- function(x, digits){formatC(x, format="f", big.mark=",", digits = 0)}
# Transform
table_descriptives <- table_descriptives %>%
  mutate_at(vars(-'Obs.'), cust_format1)
table_descriptives <- table_descriptives %>%
  mutate_at(vars('Obs.'), cust_format2)

# Name columns and rows
colnames(table_descriptives) <- c('Mean', 'StDev', 'p$^{25\\%}$','p$^{50\\%}$',
                                  'p$^{75\\%}$', 'Obs.')
rownames(table_descriptives) <- sel_text

# Output tables to LaTeX
kable(table_descriptives, booktabs = TRUE, row.names = T, align = "c", format = 'latex', linesep = "", digits = 2, escape = F) %>% 
  save_kable(file = './data/result/tables/summary_statistics_firm_year_sdg.tex')

# Preview
kbl(table_descriptives, booktabs = TRUE, row.names = T, align = "c", 
    linesep = "", digits = 2, escape = F) 

