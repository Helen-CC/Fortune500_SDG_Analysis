#' @title Create sdg_data for regressions
# clean up environment
rm(list = ls()); gc()

library(ggplot2) # for plotting
library(fixest) # for regression models
library(dplyr) # for dataframe operations
library(tidyr) # for data cleaning
library(stringr)
library(forcats)
library(readr) # for RDS data loading
library(glue) # for string concatenation
library(dotenv) # for environmental variable loading
source("./code/config.R", encoding = '')

# pick up a set of keywords to use
df_final_key <- read_rds(glue("{DROPBOX_PATH}/cleaned_data/df_final_key_all.rds"))

#' @section Join two dataframes from script 1 and 2 to obtain keyword counts from all SDG categories and from all industries
# iterate through all NAICS codes
NAICS2_CODES <- c(11, 21, 22, 23, 31, 32, 33, 42, 44, 45, 48, 49, 51, 52, 53, 54, 60, 62, 72)
sdg_data <- tibble()

for (NAICS2 in NAICS2_CODES) {
  # Load keyword counts (need to attach the corresponding SDG category label)
  df.wordCount <- read_rds(glue("{DROPBOX_PATH}/cleaned_data/df_colocation_CIP_wordcount_NAICS{NAICS2}.RDS"))
  # attach SDG label and clean up firm-year observations
  df.combine <- df.wordCount %>%
    left_join(df_final_key, by = c("keyword" = "word")) %>% 
    mutate(year = as.numeric(str_extract(name, "20\\d+")),
           name = str_replace(name, "_20\\d+", ""),
           rank = as.numeric(str_extract(name, "\\d+")),
           name = str_replace(name, "\\d+\\s", "")) %>%
    # meaningless to divide by 0, so we filter them out
    filter(n_keyword > 0)
  
  # compute the colocation index for each firm i, year t, SDG category k: this is always between 0 and 1
  # deoominator: the total number of keyword occurrences in firm i's financial report in year t for SDG category k
  # numerator: the total number of keyword occurrences in firm i's financial report in year t for SDG category k 
  #            that co-occur with "index-specific keywords" within a given length of window
  # divide the numerator by the denominator gives a colocation index in level i,t,k
  
  ## Find the denominator
  ## the group's primary key is firm's name and SDG category
  df.long <- df.combine %>% 
    group_by(rank, name, sdg, year) %>% 
    summarise(n_keyword = sum(n_keyword),
              n_complementary_keyword = sum(n_complementary),
              n_independent_keyword = sum(n_independent),
              n_pressure_keyword = sum(n_pressure),
              n_noCIP_keyword = sum(n_no_cip),
              rank = head(rank, 1)
    ) %>% 
    ungroup() %>%
    mutate(
      colocate_index_complementary = n_complementary_keyword / n_keyword,
      colocate_index_independent = n_independent_keyword / n_keyword,
      colocate_index_pressure = n_pressure_keyword / n_keyword,
      colocate_index_noCIP = n_noCIP_keyword / n_keyword
    )
  
  # tidy up the dataframe 
  df.tidy <- df.long %>%
    mutate(sdg_number = as.numeric(str_extract(sdg, "\\d+"))) %>%
    mutate(sdg = as_factor(sdg)) %>%
    mutate(sdg = fct_reorder(sdg, sdg_number)) %>% 
    filter(!is.na(sdg)) %>% 
    mutate(naics = NAICS2)
  
  sdg_data <- bind_rows(sdg_data, df.tidy)
}

# firm-year-SDG level colocation index data
View(sdg_data)
# save the sdg_data
sdg_data |> write_rds(glue("{DROPBOX_PATH}/cleaned_data/df_colocation_index_firm-year-SDG_level.RDS"))




