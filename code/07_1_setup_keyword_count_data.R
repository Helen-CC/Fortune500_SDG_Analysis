#' @title Create sdg_data for regressions
# clean up environment
rm(list = ls()); gc()

library(ggplot2) # for plotting
library(fixest)  # for regression models
library(dplyr)   # for dataframe operations
library(tidyr)   # for data cleaning
library(stringr)
library(forcats)
library(readr)   # for RDS data loading
library(glue)    # for string concatenation
library(dotenv)  # for environmental variable loading
source("./code/utils/func.R")
source("./code/config.R", encoding = '')

# pick up a set of keywords to use
df_final_key <- read_rds(glue("{DROPBOX_PATH}/cleaned_data/df_final_key_all.rds"))
df.Gvkey     <- getGvkeyMap(
  glue("{DROPBOX_PATH}/company_reference/company_reference_master.xlsx")
)

#' @section Join two dataframes from script 1 and 2 to obtain keyword counts
#' from all SDG categories and from all industries
# iterate through all NAICS codes
NAICS2_CODES <- c(11, 21, 22, 23, 31, 32, 33, 42, 44, 45, 48, 49, 51, 52,
                  53, 54, 60, 62, 72)
sdg_data <- tibble()

for (NAICS2 in NAICS2_CODES) {
  # Load keyword counts (need to attach the corresponding SDG category label)
  df.wordCount <- read_rds(
    glue("{DROPBOX_PATH}/cleaned_data/df_wordCount_NAICS{NAICS2}.RDS")
  )

  # attach SDG label, parse year/name, and attach gvkey
  df.combine <- df.wordCount %>%
    left_join(df_final_key, by = c("keyword" = "word")) %>%
    mutate(
      year = as.numeric(str_extract(name, "\\d{4}$")),
      name = str_remove(name, "_\\d{4}$")
    ) %>%
    left_join(df.Gvkey, by = "name")

  ## Find the numerator
  ## the group's primary key is firm's gvkey and SDG category
  df.long <- df.combine %>%
    filter(n_keyword > 0) %>%
    group_by(gvkey, name, sdg, year) %>%
    summarise(n_keyword = sum(n_keyword), .groups = "drop")

  # attach sdg ordering and naics label
  df.plot <- df.long %>%
    mutate(sdg_number = as.numeric(str_extract(sdg, "\\d+"))) %>%
    mutate(sdg = as_factor(sdg)) %>%
    mutate(sdg = fct_reorder(sdg, sdg_number)) %>%
    filter(!is.na(sdg)) %>%
    mutate(naics = NAICS2)

  sdg_data <- bind_rows(sdg_data, df.plot)
}

# save the sdg_data
sdg_data |>
  write_rds(glue(
    "{DROPBOX_PATH}/cleaned_data/df_regression_by_SDG_NAICS_firmyear.RDS"
  ))
