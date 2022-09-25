rm(list = ls())
library(tidyverse)
library(tidytext)
library(ggrepel)
library(viridis)
library(ggplot2)
source("./code/weakWords.R")

df_final_key <- read_rds("./data/cleaned_data/df_final_key.rds")

# IO of annual reports
txt_files <- fs::dir_ls("./data/raw_data/Fortune_500_report/", 
                        recurse = TRUE, regexp = "\\.txt")
df.txt <- bind_cols(
  path = txt_files
  ) %>% 
  mutate(rank = str_replace(path, "./data/raw_data/Fortune_500_report/", "")) %>% 
  mutate(rank = str_extract(rank, pattern = "\\d+"))

# Select txt files based on NAICS code
## Load company Rank and NAICS code mapping
df.RankCode <- readxl::read_excel("./data/raw_data/TM Final_FortuneG500 (2021)_v2.xlsx", 
                                  sheet = "Fortune Global 500 2021") %>% 
  select(rank = Rank, 
         name = Name, 
         sic = `SIC Code`, 
         naics = `NAICS Code & Description (Eikon)`) %>% 
  mutate(naics2 = floor(naics/100))

# A BETTER WAY TO SELECT NAICS STARTING WITH 31
## TODO: input the NAICS2 you want
NAICS2 <- 21
TARGET_ROWS <- df.RankCode %>% 
  filter(naics2 == NAICS2) %>% 
  pull(rank) %>% 
  unique() %>% 
  sort()

# compute word count
## After selecting specific companies
txt.toRead <- df.txt %>% 
  filter(rank %in% TARGET_ROWS) %>% 
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
           year = as.numeric(year)) %>% 
    drop_na() %>% 
    arrange(rank, year)
  df_doc <- df_doc %>% bind_rows(df.tmp)
}


### TF-IDF for the selected firms 

# create tokens (words) from sentences
df.tokens <- df_doc %>% unnest_tokens(word, value) %>% 
  filter(!word %in% stopwords::stopwords()) %>% 
  filter(!word %in% base::letters) %>% 
  anti_join(stop_words) %>% 
  filter(!str_detect(word, "\\d+\\w")) %>% 
  filter(!str_detect(word, "\\d+")) %>% 
  filter(!word %in% weak_words)

# regardless of year
df.words <- df.tokens %>%
  count(name, word, sort = TRUE)

total.words <- df.words %>% group_by(name) %>% summarize(total = sum(n))
df.words <- left_join(df.words, total.words, by = "name")


# TF-IDF
df.words <- df.words %>%
  bind_tf_idf(word, name, n) %>% 
  arrange(desc(tf_idf))

df.words <- df.words %>% 
  mutate(name = factor(name, levels = unique(df.words$name)))

p1 <- df.words %>% 
  group_by(name) %>% 
  slice_max(tf_idf, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(tf_idf, word, fill = name)) +
  geom_col(show.legend = FALSE) +
  labs(x = "TF-IDF", y = NULL) +
  facet_wrap(~name, ncol = 2, scales = "free")

p1 %>% 
  ggsave(filename = "./data/result/fig_Tf-IDF_NAICS_21.png", 
         device = "png",
         dpi = 300, 
         units = "in",
         height = 12, width = 12)
