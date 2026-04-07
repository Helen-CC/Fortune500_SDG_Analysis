# Functions repeatedly used
library(dplyr)
library(stringr)
library(assertthat)
library(tidyverse)
library(glue)
source("./code/config.R", encoding = '')

getGvkeyMap <- function(
  EXCEL_PATH = glue("{DROPBOX_PATH}/company_reference/company_reference_master.xlsx")
  ){
  "
  Description:
    the function load the excel file that records the 500 companies and the SIC code, NAICS code
    return a dataframe that shows the mapping of company and code
  "
  ## Load company Rank and NAICS code mapping
  df.RankCode <- readxl::read_excel(EXCEL_PATH, 
                                    sheet = "master") %>% 
    select(gvkey = gvkey, # 當 column name 不以數字開頭、沒特殊符號、沒有空格時，可不加back tick
           name = Name, 
           sic = `SIC`, # 當 col name 不合以上條件就要用 `` back tick 包住，是 dplyr 套件的用法
           naics = `NAICS`) %>% 
    mutate(naics2 = floor(naics/100)) %>%  # floor 功能是取整數，小數點後直接去掉
    # drop if gvkey is missing
    # drop_na(gvkey) %>%
    # drop duplicates
    distinct(.keep_all = TRUE)
  
  return(df.RankCode)
}
getGvkeyMap() %>% filter(naics2 == 11)


parseFilenameFromPath <- function(txt_path) {
  path_prefix <- as.character(
    glue("{DROPBOX_PATH}/raw_data/Fortune_500_report/")
  )
  # Extract the top-level folder name (first path component after prefix).
  # Works for both gvkey-prefixed folders (e.g. 001690_Apple)
  # and no-gvkey folders (e.g. _CHS).
  filename <- txt_path %>%
    str_replace(fixed(path_prefix), "") %>%
    str_extract("^[^/]+")
  return(filename)
}


readReports <- function(NAICS2_CODE) {
  "
  Description:
    input: the first 2 digits of NAICS code
    output: a dataframe that selected the given codes and lists the corresponding companies and annual report contents

  Directory naming convention:
    - {6-digit-gvkey}_{company name}  e.g. 001690_Apple
    - _{company name}                 e.g. _CHS  (no gvkey available)

  Matching logic:
    1. If a folder has a 6-digit gvkey prefix, match to getGvkeyMap() via gvkey.
    2. If no gvkey (folder starts with _), match via the company name field.
  "

  path_prefix <- glue("{DROPBOX_PATH}/raw_data/Fortune_500_report/")
  txt_files <- fs::dir_ls(path_prefix, recurse = TRUE, regexp = "\\.txt|\\.htm")

  df.Gvkey <- getGvkeyMap()

  # Parse folder name: extract gvkey and company name from directory structure
  # Folder conventions:
  #   {6-digit-gvkey}_{name}  e.g. 001690_Apple
  #   _{name}                 e.g. _CHS  (no gvkey)
  df.txt <- tibble(path = as.character(txt_files)) %>%
    mutate(
      folder = .data$path %>%
        str_replace(fixed(path_prefix), "") %>%
        str_extract("^[^/]+"),
      # 6-digit gvkey prefix before underscore; NA if absent
      gvkey = str_extract(.data$folder, "^\\d{6}(?=_)"),
      # company name: strip leading gvkey_ or bare _
      name_in_path = str_replace(.data$folder, "^(\\d{6}_|_)", "")
    )

  # Determine naics2 per file:
  #   rows with gvkey  -> join on gvkey
  #   rows without     -> join on company name
  # Deduplicate before joining to avoid many-to-many relationships
  # (a company can appear with multiple NAICS codes in the master sheet)
  gvkey_naics <- df.Gvkey %>%
    filter(!is.na(.data$gvkey)) %>%
    select("gvkey", naics2_by_gvkey = "naics2") %>%
    distinct(.data$gvkey, .keep_all = TRUE)

  name_naics <- df.Gvkey %>%
    filter(is.na(.data$gvkey)) %>%
    select("name", naics2_by_name = "naics2") %>%
    distinct(.data$name, .keep_all = TRUE)

  df.txt <- df.txt %>%
    left_join(gvkey_naics, by = "gvkey") %>%
    left_join(name_naics, by = c("name_in_path" = "name")) %>%
    mutate(
      naics2 = if_else(
        !is.na(.data$gvkey),
        .data$naics2_by_gvkey,
        .data$naics2_by_name
      )
    )

  txt.toRead <- df.txt %>%
    filter(.data$naics2 == NAICS2_CODE) %>%
    pull(.data$path)

  ## Read files and combine
  df_doc <- tibble()
  for (txt in txt.toRead) {
    filename <- parseFilenameFromPath(txt)
    year     <- str_extract(txt, "20\\d{2}")

    df.tmp <- read_lines(txt) %>%
      as_tibble() %>%
      summarise(value = str_c(.data$value, collapse = "\\s")) %>%
      mutate(
        name  = filename,
        # 6-digit prefix if present; NA for no-gvkey companies
        gvkey = str_extract(.data$name, "^\\d{6}"),
        year  = as.numeric(year),
        name  = paste0(.data$name, "_", year)
      ) %>%
      # keep rows where gvkey is NA; only drop missing text or year
      filter(!is.na(.data$value), !is.na(.data$year)) %>%
      arrange(.data$gvkey, .data$year)
    df_doc <- df_doc %>% bind_rows(df.tmp)
  }

  return(df_doc)
}
# df_test <- readReports(NAICS2_CODE = 11)
readReports(NAICS2_CODE = 21)

testParseFilenameFromPath <- function() {
  # Folder naming convention: {6-digit-gvkey}_{name} or _{name}
  testStrings <- c(
    "001690_Apple/Apple AR txt/Apple 2021.txt",
    "019661_Toyota Motor/Toyota AR txt/Toyota 2021.txt",
    "100737_Volkswagen/Volkswagen 2021.txt",
    "_CHS/CHS AR txt/CHS 2021.txt",
    "_Huawei Investment & Holding/Huawei 2021.txt"
  )
  expectedStrings <- c(
    "001690_Apple",
    "019661_Toyota Motor",
    "100737_Volkswagen",
    "_CHS",
    "_Huawei Investment & Holding"
  )
  for (i in seq_along(testStrings)) {
    cat(">>> Test case ", i, "\n")
    cat(">>>  test string is: ", testStrings[i], "\n")
    cat(">>>  expected result: ", expectedStrings[i], "\n")
    cat(">>>  parsed result is: ", parseFilenameFromPath(testStrings[i]), "\n\n")
    assertthat::assert_that(
      are_equal(expectedStrings[i],
                parseFilenameFromPath(testStrings[i]))
    )
  }
  cat(">>> All tests pass\n")
}

testParseFilenameFromPath()


getComplementaryIndexKeywords <- function(EXCEL_PATH = "./data/raw_data/Complementarity_independence_pressure_keywords.xlsx"){
  "
  Description:
    the function load the excel file that the keyword list of complementary, independence, and pressure indices keywords 
    return a named list stores the three sets of keywords
  "
  ## Load company Rank and NAICS code mapping
  df.indices_keywords <- readxl::read_excel(EXCEL_PATH, sheet = "Sheet1") %>% 
    select(complementary = `Complementarity`,
           independent = `Independence`, 
           pressure = `Pressure`)
  n_rows = nrow(df.indices_keywords)
  df.indices_keywords <- df.indices_keywords %>% .[3:n_rows,]
  
  indicies_keyword_list <- list(
    complementary = df.indices_keywords %>% pull(complementary) %>% .[!is.na(.)],
    independent = df.indices_keywords %>% pull(independent) %>% .[!is.na(.)],
    pressure = df.indices_keywords %>% pull(pressure) %>% .[!is.na(.)]
  )
  
  return(indicies_keyword_list)
}

# list_tmp <- getComplementaryIndexKeywords("./data/raw_data/Complementarity_independence_pressure_keywords.xlsx")
# list_tmp[["complementary"]]
# list_tmp[["independent"]]
# list_tmp[["pressure"]]


