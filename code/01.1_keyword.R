rm(list = ls())
# Load necessary packages
library(tidyverse)
library(readxl)
library(clipr)

# Setup working directory
setwd("/Volumes/TOSHIBA3T/2022-09-07/Fortune500_SDG_Analysis")
getwd()

# Load keyword dictionary
df_keyword <- readxl::read_excel("./data/raw_data/UoA-SDG-Keyword-List-Ver.-1.1.xlsx", 
                                 sheet = "keyword") %>% 
  rename(word = `SDG Keywords`, word2 = `Alternatives`, sdg = `SDG`)

# Combine the 2 colums -> use keywords in both columns at once
df_keyword_unnest <- df_keyword %>% 
  select(sdg, word) %>% 
  bind_rows(df_keyword %>% 
              select(word2, sdg) %>% 
              drop_na() %>% 
              rename(word = word2)
            ) %>% 
  # sort the rows in SDG order
  mutate(SDG_order = str_extract(sdg, pattern = "\\d+"),
         SDG_order = as.numeric(SDG_order),
         ) %>% 
  arrange(SDG_order) %>% 
  select(-SDG_order) %>% 
  # trim white spaces at both sides
  mutate(word = str_trim(word, side = "both")) %>% 
  # add regular expressions
  mutate(word = str_replace_all(word, "\\*", ".?")) %>%
  mutate(word = str_replace_all(word, " AND ", ".*?")) %>% #舉例 Economic Resource AND Access 在一個句子裡面同時出現，不一定要前後
  mutate(word = str_split(word, "; ")) %>% #把excel 裡面同一格有 分號; 的分開到不同row 如row 101
  unnest(c(word))

# Create a dataframe of keywords without spaces -> nspace
df_keyword_nspace <- df_keyword_unnest %>% 
  filter(!str_detect(word, " ")) %>% # TODO: better change space into `\\s`
  ## TODO: need to know the variable meaning
  mutate(lll = str_length(word), lll_b = str_count(word, "[A-Z]")) %>%
  mutate(word = if_else(lll > lll_b, str_to_lower(word), word))
# Create a dataframe of keywords with spaces
df_keyword_space <- df_keyword_unnest %>% 
  filter(str_detect(word," ")) %>%
  mutate(word = str_to_lower(word))

# ## The following code demonstrate that why `\s` is better than a single white space
# df.test.s <- df_keyword_unnest %>% 
#   filter(!str_detect(word, "\\s")) %>% 
#   mutate(lll = str_length(word), lll_b = str_count(word, "[A-Z]")) %>%
#   mutate(word = if_else(lll > lll_b, str_to_lower(word), word))
# df.test.space <- df_keyword_unnest %>% 
#   filter(!str_detect(word, " ")) %>% 
#   mutate(lll = str_length(word), lll_b = str_count(word, "[A-Z]")) %>%
#   mutate(word = if_else(lll > lll_b, str_to_lower(word), word))
# x <- df.test.s$word
# y <- df.test.space$word
# setdiff(y, x)


# Create a dataframe to merge the dataframe with and without spaces
df_bind <- df_keyword_nspace %>% 
  bind_rows(df_keyword_space) %>%
  mutate(word = str_trim(word, "both")) %>%
  mutate(word = if_else(str_detect(word, "\\.\\*\\?"), 
                        word, str_c("\\b",word,"\\b"))) %>%
  # sort the rows in SDG order
  mutate(SDG_order = str_extract(sdg, pattern = "\\d+"),
         SDG_order = as.numeric(SDG_order),
         ) %>% 
  arrange(SDG_order) %>% 
  select(sdg, word) 

# clean up env
rm(list=setdiff(ls(), "df_bind"))

### 有些要手動改
#https://docs.google.com/spreadsheets/d/1fZdE9WcFYI_d_sD4BgBpI5D1QhOYlRngsuSEtB7w694/edit#gid=470532546
# change `or` into `|`
df_manual <- df_bind %>%
  filter(str_detect(word, "\\sor\\s")) %>%
  mutate(word = str_replace_all(word, "\\sor\\s", "|"))

df_manual %>% write_clip()
df_manual_new <- read_clip() %>% as_tibble()
df_manual_new_ok <- df_manual_new %>% separate(value, into = c("sdg","word","word_new"), sep = "\\t") %>% slice(-1) %>%
  select(-2) %>% rename(word = 2)

df_manual2 <- df_bind %>%
  filter(!str_detect(word, " or ")) %>%
  bind_rows(df_manual_new_ok) %>%
  arrange(sdg) %>%
  mutate(word = str_replace_all(word, "\\{", "(")) %>%
  mutate(word = str_replace_all(word, "\\}", ")")) %>%
  mutate(word = str_remove_all(word, "“|”")) %>%
  filter(str_detect(word, "not"))

df_manual2 %>% write_clip()
df_manual_new2 <- read_clip() %>% as_tibble()
df_manual_new2_ok <- df_manual_new2 %>% separate(value, into = c("sdg","word","word_new"), sep = "\\t") %>% slice(-1) %>%
  select(-2) %>% rename(word = 2)

df_final_key <- df_bind %>%
  filter(!str_detect(word, " or ")) %>%
  filter(!str_detect(word, " not")) %>%
  bind_rows(df_manual_new_ok) %>%
  bind_rows(df_manual_new2_ok) %>%
  arrange(sdg) %>%
  mutate(word = str_replace_all(word, "\\{", "(")) %>%
  mutate(word = str_replace_all(word, "\\}", ")")) %>%
  mutate(word = str_remove_all(word, "“|”")) %>%
  filter(!str_detect(word, "not")) %>%
  distinct() %>%
  filter(!str_detect(word,"goverance")) #看看要不要governance 這個字

df_final_key %>% write_rds("C:\\Users\\User\\Desktop\\Research Proposal\\Y3, Manuscript 1 TM\\Help_Ted Helen\\df_final_key.rds")

str_detect(c("i went there for immigration.", "i went there for immigration and disease."),
           '^(?=.*immigration)(?!.*disease)')

library(stringr)
var_list <- c("other", "dimension_1", "dimension_2", "metric_1", "metric_2", "metric_3_penetration")
str_detect(var_list, "^(?=.*metric)(?!.*penetration)")

# (?<!See )
df_final_key %>% write_rds("data/df_final_key.rds")
# str_count("Economic Resources are Access gg Economic Resources gg Access", "Economic Resources.*?Access")
# str_count("managerial positions is for women and managerial positions is for female ", 
#           "managerial positions.*?(women|female)")
# 
# str_extract_all(string = c("ababc","ac","cde"),pattern = "(ab)?c")
# str_extract_all(string = c("abc","ac","cde"),pattern = "ab?c")
# 
# 
# library(tidyverse)
# library(clipr)
# library(psychTools)
# 
# # https://docs.google.com/spreadsheets/d/1fZdE9WcFYI_d_sD4BgBpI5D1QhOYlRngsuSEtB7w694/edit#gid=0
# df_raw <- read_clip() %>% as_tibble() %>% rename(sdg = 1, query = 2)
# df_raw %>% separate(value, into = c("sdg","query"), sep = "\\t") %>% slice(-1) %>%
#   mutate(query = str_remove(query, "TITLE-ABS-KEY ")) %>%
#   mutate(query = str_replace(query, "\\) \\)"," \\)")) %>%
#   mutate(query = str_split(query, "\\} OR \\{")) %>%
#   unnest(c(query)) %>% 
#   filter(sdg == "SDG1") %>%
#   write_clip()
# 
# df_raw %>% separate(value, into = c("sdg","query"), sep = "\\t") %>% slice(-1) %>%
#   mutate(query = str_remove(query, "TITLE-ABS-KEY ")) %>%
#   mutate(query = str_replace(query, "\\) \\)"," \\)")) %>%
#   mutate(query = str_split(query, "\\} OR \\{")) %>%
#   unnest(c(query)) %>% 
#   filter(sdg == "SDG2") %>%
#   write_clip()
# 
# df_raw %>% separate(value, into = c("sdg","query"), sep = "\\t") %>% slice(-1) %>%
#   mutate(query = str_remove(query, "TITLE-ABS-KEY ")) %>%
#   mutate(query = str_replace(query, "\\) \\)"," \\)")) %>%
#   mutate(query = str_split(query, "\\} OR \\{|\\} OR \\(|\\) OR \\(")) %>%
#   unnest(c(query)) %>% 
#   mutate(query = str_trim(query, "both")) %>%
#   filter(sdg == "SDG1") %>% 
#   mutate(query = str_replace_all(query, " AND ", ".*?")) %>%
#   mutate(query = str_remove_all(query, "\\{|\\}|\\)|\\(")) %>%
#   mutate(query = str_trim(query, "both")) %>%
#   write_clip()
# 
# df_raw %>% separate(value, into = c("sdg","query"), sep = "\\t") %>% slice(-1) %>%
#   mutate(query = str_remove(query, "TITLE-ABS-KEY ")) %>%
#   mutate(query = str_replace(query, "\\) \\)"," \\)")) %>%
#   mutate(query = str_split(query, "\\} OR \\{|\\} OR \\(|\\) OR \\(")) %>%
#   unnest(c(query)) %>% 
#   mutate(query = str_trim(query, "both")) %>%
#   mutate(query = str_replace_all(query, " AND ", ".*?")) %>%
#   mutate(query = str_remove_all(query, "\\{|\\}|\\)|\\(")) %>%
#   mutate(query = str_trim(query, "both")) %>%
#   mutate(query = str_split(query, "OR")) %>%
#   unnest(c(query)) %>%
#   mutate(query = str_trim(query, "both")) %>%
#   write_clip()
# 
