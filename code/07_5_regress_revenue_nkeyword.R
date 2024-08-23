rm(list = ls())
library(fixest)
library(readr)
source("./code/config.R")
#' Load data
#' at `(firm, year, sdg, keyword_count)` level
df_merged <- read_rds(glue("{DROPBOX_PATH}/cleaned_data/regression_data_merged.RDS"))

# Only focusing on mining firms
gvkeys_mining <- read_csv(glue("{DROPBOX_PATH}/raw_data/compustat/company_value_global_mining.csv"))
gvkeys_mining <- gvkeys_mining %>% pull(gvkey) %>% unique()

df_merged <- df_merged %>%
  mutate(is_mining = ifelse(gvkey %in% gvkeys_mining, 1, 0))
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
#' fixed effects (FEs) are year & industry (SIC or NAICS) fixed effects

#' Regression A: revenue ~ n_keyword + SDG Dummies and fixed effects
#' Regression B: revenue ~ n_keyword, with fixed effects
#' Regression C: revenue ~ n_keyword + mining indicator + SDG Dummies, with fixed effects
#' Regression D: revenue ~ n_keyword + mining indicator, with fixed effects
#' Regression E: revenue ~ n_keyword, conditioning on mining firms solely, with fixed effects

#' run regressions
#' Identification:
#'     the number of keywords mentioned in SDGXX by a company ~ industry indicators + error
# add fixed effect


reg.A_naics <- feols(revt ~ n_keyword + at + emp + i(sdg) - 1 | csw0(year, naics), 
              vcov = "HC1",
              data = df_merged)
reg.A_sic2 <- feols(revt ~ n_keyword + at + emp + i(sdg) - 1 | csw0(year, sic2), 
              vcov = "HC1",
              data = df_merged)

reg.B_naics <- feols(revt ~ n_keyword + at + emp | csw0(sdg,year, naics), 
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




reg.C_naics <- feols(revt ~ n_keyword + at + emp + is_mining | csw0(sdg,year, naics), 
              vcov = "HC1",
              data = df_merged)
reg.C_sic2 <- feols(revt ~ n_keyword + at + emp + is_mining | csw0(sdg,year, sic2), 
              vcov = "HC1",
              data = df_merged)
reg.D_naics <- feols(revt ~ n_keyword + at + emp + is_mining + i(sdg) - 1| csw0(sdg,year, naics), 
              vcov = "HC1",
              data = df_merged)
reg.D_sic2 <- feols(revt ~ n_keyword + at + emp + is_mining + i(sdg) - 1| csw0(sdg,year, sic2), 
              vcov = "HC1",
              data = df_merged)

reg.E_naics <- feols(revt ~ n_keyword + at + emp | csw0(sdg,year, naics), 
              vcov = "HC1",
              data = df_merged_mining)
reg.E_sic2 <- feols(revt ~ n_keyword + at + emp | csw0(sdg,year, sic2), 
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

