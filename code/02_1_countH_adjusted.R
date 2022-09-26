rm(list = ls())
library(tidyverse)
library(tidytext)
library(stringi)
library(fs)

# Load data
df_final_key <- read_rds("./data/cleaned_data/df_final_key.rds")
df_final_key_old <- read_rds("./data/rds/df_final_key.rds")

# a comparison: check correctness
setdiff(df_final_key$word, df_final_key_old$word) #A集合插集B 集合，新的比舊的
setdiff(df_final_key_old$word, df_final_key$word)

# IO of annual reports
txt_files <- fs::dir_ls("./data/raw_data/Fortune_500_report/", 
                        recurse = TRUE, regexp = "\\.txt") 
#recurse 意思 遞規 點進去資料夾的時候用，資料夾點到最裡面那層
df.txt <- bind_cols(
  path = txt_files
  ) %>% 
  mutate(rank = str_replace(path, "./data/raw_data/Fortune_500_report/", "")) %>% 
  mutate(rank = str_extract(rank, pattern = "\\d+"))

# Select txt files based on NAICS code
## Load company Rank and NAICS code mapping
df.RankCode <- readxl::read_excel("./data/raw_data/TM Final_FortuneG500 (2021)_v2.xlsx", 
                                  sheet = "Fortune Global 500 2021") %>% 
  select(rank = Rank, #不以數字開頭、沒特殊符號、沒有空格，可不加back tick
         name = Name, 
         sic = `SIC Code`, #有空格，特殊符號 要用 `` back tick dplyr 套件用法
         naics = `NAICS Code & Description (Eikon)`) %>% 
  mutate(naics2 = floor(naics/100))
# TODO: not sure the relation between the specific number and the field of industry
# SPECIFIC_ROWS <- c(16, 126, 211, 411, 395, 116, 191, 417, 405, 53, 324, 391, 408, 478, 37, 153, 302, 299, 360, 204, 234, 125, 248)
# SPECIFIC_ROWS <- c(126, 211)
SPECIFIC_ROWS <- c(79, 112, 131, 146, 202, 210, 211, 236, 270, 282, 289, 309, 321, 368, 370, 390, 422, 450, 454, 463, 466, 469, 474)
df.RankCode %>% 
  filter(rank %in% SPECIFIC_ROWS) %>% 
  View

df.txt %>% 
  filter(rank %in% SPECIFIC_ROWS) %>% 
  View


# A BETTER WAY TO SELECT NAICS STARTING WITH 31
TARGET_ROWS <- df.RankCode %>% 
  filter(naics2 == 31) %>%  #要換產業做這裡31改
  pull(rank) %>% 
  unique() %>% 
  sort()

SPECIFIC_ROWS == TARGET_ROWS
#SPECIFIC_ROWS 只是為了確認之前結果，選出來的公司對不對
# compute word count
## After selecting specific companies
txt.toRead <- df.txt %>% 
  filter(rank %in% SPECIFIC_ROWS) %>% 
  pull(path)

## new a df to store the path of the file to read and the contents
df_doc <- tibble()
for (txt in txt.toRead) {
  filename <- str_replace(txt, "./data/raw_data/Fortune_500_report/", "") %>% 
    str_extract("\\d+(\\s\\w+)+")
  year <- str_extract(txt, "20\\d{2}")
  df.tmp <- read_lines(txt) %>% 
    as_tibble() %>% 
    summarise(value = str_c(value, collapse = "\\s")) %>% 
    mutate(name = filename,
           rank = str_extract(name, "\\d+"),
           rank = as.numeric(rank),
           name = paste0(name, "_", year)) %>% 
    drop_na() %>% 
    arrange(rank)
  df_doc <- df_doc %>% bind_rows(df.tmp)
}

## post-processing the read files
df_sentence <- df_doc %>% 
  # convert character vector between encodings
  mutate(value = iconv(value, "", "UTF-8")) %>% 
  unnest_tokens(output = text, input = value, token = "sentences") %>% 
  drop_na() %>% 
  arrange(rank)

## New an empty tibble to store the final result
df_keyword_n <- tibble()
for (txtfile in unique(df_sentence$name)) {
  for (keyword in df_final_key$word) {
    message(keyword)
    n_keyword <- df_sentence %>% 
      filter(name == txtfile) %>% 
      pull(text) %>% 
      map_dbl(~stringi::stri_count(., regex = keyword)) %>% 
      sum(.)
    df_keyword_n_tmp <- tibble(name = txtfile,
                               n_keyword,
                               keyword)
    df_keyword_n <- df_keyword_n %>% bind_rows(df_keyword_n_tmp)
  }
}

## Save files
df_keyword_n %>% write_rds("./data/cleaned_data/df_wordCount_SPECIFIC.rds")

df_keyword_n %>% filter(n_keyword > 0)
