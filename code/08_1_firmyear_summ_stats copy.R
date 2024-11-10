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



# create counts for each SDG & sum of all counts
# df_merged %>%
#   pivot_wider(names_from = sdg, values_from = n_keyword) %>%
#   View
default_value <- NA_real_
df_firmyear <- df_merged %>%
  mutate(sdg0_count = ifelse(sdg_number == 0, n_keyword, default_value),
         sdg1_count = ifelse(sdg_number == 1, n_keyword, default_value),
         sdg2_count = ifelse(sdg_number == 2, n_keyword, default_value),
         sdg3_count = ifelse(sdg_number == 3, n_keyword, default_value),
         sdg4_count = ifelse(sdg_number == 4, n_keyword, default_value),
         sdg5_count = ifelse(sdg_number == 5, n_keyword, default_value),
         sdg6_count = ifelse(sdg_number == 6, n_keyword, default_value),
         sdg7_count = ifelse(sdg_number == 7, n_keyword, default_value),
         sdg8_count = ifelse(sdg_number == 8, n_keyword, default_value),
         sdg9_count = ifelse(sdg_number == 9, n_keyword, default_value),
         sdg10_count = ifelse(sdg_number == 10, n_keyword, default_value),
         sdg11_count = ifelse(sdg_number == 11, n_keyword, default_value),
         sdg12_count = ifelse(sdg_number == 12, n_keyword, default_value),
         sdg13_count = ifelse(sdg_number == 13, n_keyword, default_value),
         sdg14_count = ifelse(sdg_number == 14, n_keyword, default_value),
         sdg15_count = ifelse(sdg_number == 15, n_keyword, default_value),
         sdg16_count = ifelse(sdg_number == 16, n_keyword, default_value),
         ) %>%
  group_by(gvkey, year) %>%
  fill(c(sdg0_count, sdg1_count, sdg2_count, sdg3_count, sdg4_count, sdg5_count,
         sdg6_count, sdg7_count, sdg8_count, sdg9_count, sdg10_count, sdg11_count,
         sdg12_count, sdg13_count, sdg14_count, sdg15_count, sdg16_count), .direction = "downup") %>%
  # drop_na() %>%
  ungroup() %>%
  select(-sdg, -sdg_number, -n_keyword) %>%
  distinct() 

df_firmyear <- df_firmyear %>%
  drop_na(c(rank, year)) %>%
  # fill NA as 0s in SDG counts
  mutate(sdg0_count = ifelse(is.na(sdg0_count), 0, sdg0_count),
         sdg1_count = ifelse(is.na(sdg1_count), 0, sdg1_count),
         sdg2_count = ifelse(is.na(sdg2_count), 0, sdg2_count),
         sdg3_count = ifelse(is.na(sdg3_count), 0, sdg3_count),
         sdg4_count = ifelse(is.na(sdg4_count), 0, sdg4_count),
         sdg5_count = ifelse(is.na(sdg5_count), 0, sdg5_count),
         sdg6_count = ifelse(is.na(sdg6_count), 0, sdg6_count),
         sdg7_count = ifelse(is.na(sdg7_count), 0, sdg7_count),
         sdg8_count = ifelse(is.na(sdg8_count), 0, sdg8_count),
         sdg9_count = ifelse(is.na(sdg9_count), 0, sdg9_count),
         sdg10_count = ifelse(is.na(sdg10_count), 0, sdg10_count),
         sdg11_count = ifelse(is.na(sdg11_count), 0, sdg11_count),
         sdg12_count = ifelse(is.na(sdg12_count), 0, sdg12_count),
         sdg13_count = ifelse(is.na(sdg13_count), 0, sdg13_count),
         sdg14_count = ifelse(is.na(sdg14_count), 0, sdg14_count),
         sdg15_count = ifelse(is.na(sdg15_count), 0, sdg15_count),
         sdg16_count = ifelse(is.na(sdg16_count), 0, sdg16_count),
         ) %>%
  mutate(sum_n_keyword = sdg0_count + sdg1_count  + sdg2_count + sdg3_count + sdg4_count + sdg5_count
          + sdg6_count + sdg7_count + sdg8_count + sdg9_count + sdg10_count + sdg11_count + sdg12_count
          + sdg13_count + sdg14_count + sdg15_count + sdg16_count)

df_firmyear %>%
  group_by(gvkey, year) %>%
  count() %>%
  arrange(desc(n))

df_firmyear %>%
  group_by(gvkey, fyear) %>%
  count() %>%
  arrange(desc(n))

#' make summ table



# Add custom function to calculation residual standard deviations (after purging for bond and month variation)
sd_fe <- function(x){
  temp_lm <- felm(x ~ 0 | year + gvkey | 0 | 0, data = data)
  sd(temp_lm$residuals)
}

df_firmyear%>%colnames()

# Outcome variables
other_outcomes <- c('at', 'revt', 'emp', 'is_mining',
                    'sdg0_count', 
                    'sdg1_count',
                    'sdg2_count',
                    'sdg3_count',
                    'sdg4_count',
                    'sdg5_count',
                    'sdg6_count',
                    'sdg7_count',
                    'sdg8_count',
                    'sdg9_count',
                    'sdg10_count',
                    'sdg11_count',
                    'sdg12_count',
                    'sdg13_count',
                    'sdg14_count',
                    'sdg15_count',
                    'sdg16_count',
                    'sum_n_keyword'
                    )
other_outcomes_text <- c('asset', 'revenue', 'num employees', 'mining firm indicator',
                    'num keywords in SDG 0', 
                    'num keywords in SDG 1', 
                    'num keywords in SDG 2', 
                    'num keywords in SDG 3', 
                    'num keywords in SDG 4', 
                    'num keywords in SDG 5', 
                    'num keywords in SDG 6', 
                    'num keywords in SDG 7', 
                    'num keywords in SDG 8', 
                    'num keywords in SDG 9', 
                    'num keywords in SDG 10', 
                    'num keywords in SDG 11', 
                    'num keywords in SDG 12', 
                    'num keywords in SDG 13', 
                    'num keywords in SDG 14', 
                    'num keywords in SDG 15', 
                    'num keywords in SDG 16', 
                    'num keywords'
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
table_descriptives <- sapply(df_firmyear %>%
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
  save_kable(file = './data/result/tables/summary_statistics.tex')

# Preview
kbl(table_descriptives, booktabs = TRUE, row.names = T, align = "c", 
    linesep = "", digits = 2, escape = F) 

