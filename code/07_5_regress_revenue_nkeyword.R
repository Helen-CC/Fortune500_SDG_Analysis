rm(list = ls())
library(fixest)
library(readr)
source("./code/config.R")
#' Load data
#' at `(firm, year, sdg, keyword_count)` level
df_merged <- read_rds(glue("{DROPBOX_PATH}/cleaned_data/regression_data_merged.RDS")) %>%
  mutate(naics2 = str_sub(naics, 0, 2)) 

# # black list
# df_merged <- df_merged %>%
#   filter(gvkey != "100424")

# # filter on NAICS code 2 == NAICS
# df_merged %>%
#   filter(naics2 == naics_self)
#  df_merged %>%
#   filter(naics2 != naics_self) %>%
#    View
  




# Only focusing on mining firms
gvkeys_mining <- read_csv(glue("{DROPBOX_PATH}/raw_data/compustat/company_value_global_mining.csv"))
gvkeys_mining <- gvkeys_mining %>% pull(gvkey) %>% unique()

df_merged <- df_merged %>%
  mutate(is_mining = ifelse(gvkey %in% gvkeys_mining, 1, 0))



# create counts for each SDG & sum of all counts
# df_merged %>%
#   pivot_wider(names_from = sdg, values_from = n_keyword) %>%
#   View
default_value <- 0
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
         ) 

df_firmyear %>%
  group_by(gvkey, year) %>%
  count() %>%
  arrange(desc(n))

df_firmyear %>%
  group_by(gvkey, fyear) %>%
  count() %>%
  arrange(desc(n))


# select only mining firms
df_merged_mining <- df_merged %>% filter(is_mining == 1)

#' @section inspecting column names
df_merged %>% colnames()

# plot histograms to see the difference of distributions
df_merged %>% 
  ggplot()+
  geom_histogram(aes(n_keyword))+
  facet_wrap(~sic2, nrow = 2)


#' @section Regression
#' @description
#' List of sets of regressions
#' fixed effects (FEs) are generally year & industry (SIC or NAICS) fixed effects

#' Regression A: revenue ~ n_keyword + SDG Dummies + year & industry FEs
#' Regression B: revenue ~ n_keyword + SDG FEs + year & industry FEs
#' Regression C: revenue ~ n_keyword + mining indicator + SDG Dummies + year & industry FEs 
#' Regression D: revenue ~ n_keyword + mining indicator + SDG FEs + year & industry FEs
#' Regression E: revenue ~ n_keyword, conditioning on mining firms solely, with SDG FEs + year & industry FEs

#' run regressions
#' Identification:
#'     the number of keywords mentioned in SDGXX by a company ~ industry indicators + error
# add fixed effect


reg.A_naics <- feols(revt ~ n_keyword + at + emp + i(sdg) - 1 | csw0(year, naics2), 
              vcov = "HC1",
              data = df_merged)
reg.A_sic2 <- feols(revt ~ n_keyword + at + emp + i(sdg) - 1 | csw0(year, sic2), 
              vcov = "HC1",
              data = df_merged)

reg.B_naics <- feols(revt ~ n_keyword + at + emp | csw0(sdg, year, naics2), 
               vcov = "HC1",
               data = df_merged)
reg.B_sic2 <- feols(revt ~ n_keyword + at + emp | csw0(sdg, year, sic2), 
              vcov = "HC1",
              data = df_merged)



etable(reg.A_naics, coefstat = "tstat")
etable(reg.A_sic2, coefstat = "tstat")
etable(reg.B_naics, coefstat = "tstat")
etable(reg.B_sic2, coefstat = "tstat")


# write LaTeX tables
save_reg_table <- function(reg_model, filename){
  etable(reg_model, 
         coefstat = "tstat", 
         file = glue("./data/result/tables/{filename}.tex"),
         replace = T)
}

save_reg_table(reg.A_naics, "tab_reg_revt_on_nkeywords_A_naics")
save_reg_table(reg.A_sic2, "tab_reg_revt_on_nkeywords_A_sic2")
save_reg_table(reg.B_naics, "tab_reg_revt_on_nkeywords_B_naics")
save_reg_table(reg.B_sic2, "tab_reg_revt_on_nkeywords_B_sic2")



reg.C_naics <- feols(revt ~ n_keyword + at + emp + is_mining + i(sdg) - 1| csw0(year, naics2), 
              vcov = "HC1",
              data = df_merged)
reg.C_sic2 <- feols(revt ~ n_keyword + at + emp + is_mining + i(sdg) - 1| csw0(year, sic2), 
              vcov = "HC1",
              data = df_merged)

reg.D_naics <- feols(revt ~ n_keyword + at + emp + is_mining | csw0(sdg, year, naics2), 
              vcov = "HC1",
              data = df_merged)
reg.D_sic2 <- feols(revt ~ n_keyword + at + emp + is_mining | csw0(sdg, year, sic2), 
              vcov = "HC1",
              data = df_merged)

reg.E_naics <- feols(revt ~ n_keyword + at + emp | csw0(sdg, year, naics2), 
              vcov = "HC1",
              data = df_merged_mining)
reg.E_sic2 <- feols(revt ~ n_keyword + at + emp | csw0(sdg, year, sic2), 
              vcov = "HC1",
              data = df_merged_mining)


etable(reg.C_naics, coefstat = "tstat")
etable(reg.C_sic2, coefstat = "tstat")
etable(reg.D_naics, coefstat = "tstat")
etable(reg.D_sic2, coefstat = "tstat")
etable(reg.E_naics, coefstat = "tstat")
etable(reg.E_sic2, coefstat = "tstat")

save_reg_table(reg.C_naics, "tab_reg_revt_on_nkeywords_mining_C_naics")
save_reg_table(reg.C_sic2, "tab_reg_revt_on_nkeywords_mining_C_sic2")
save_reg_table(reg.D_naics, "tab_reg_revt_on_nkeywords_mining_D_naics")
save_reg_table(reg.D_sic2, "tab_reg_revt_on_nkeywords_mining_D_sic2")
save_reg_table(reg.E_naics, "tab_reg_revt_on_nkeywords_mining_E_naics")
save_reg_table(reg.E_sic2, "tab_reg_revt_on_nkeywords_mining_E_sic2")

