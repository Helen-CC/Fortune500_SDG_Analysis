rm(list = ls())
library(tidyverse)
library(tidytext)
library(ggrepel)
library(viridis)
library(ggplot2)
source("./code/utils/func.R")
source("./code/utils/weakWords.R")
source("./code/config.R", encoding = '')

# Load keywords
df_final_key <- read_rds(glue("{DROPBOX_PATH}/cleaned_data/df_final_key_all.RDS"))

# IO of annual reports
df.Gvkey <- getGvkeyMap(glue("{DROPBOX_PATH}/company_reference/company_reference_master.xlsx"))

## A BETTER WAY TO SELECT NAICS STARTING WITH XX
## TODO: Change the following code if needed
NAICS2_CODE <- 22
df.doc <- readReports(NAICS2_CODE)


# TF-IDF for the selected firms 
## create tokens (words) from sentences
df.tokens <- df.doc %>% unnest_tokens(word, value) %>% 
  filter(!word %in% stopwords::stopwords()) %>% 
  filter(!word %in% base::letters) %>% 
  anti_join(stop_words) %>% 
  filter(!str_detect(word, "\\d+\\w")) %>% 
  filter(!str_detect(word, "\\d+")) %>% 
  filter(!word %in% weak_words) %>% 
  mutate(name = str_remove(name, "_\\d{4}"))

## regardless of year
df.words <- df.tokens %>%
  count(name, word, sort = TRUE)

total.words <- df.words %>% group_by(name) %>% summarize(total = sum(n))
df.words <- left_join(df.words, total.words, by = "name")

## TF-IDF
df.words <- df.words %>%
  bind_tf_idf(word, name, n) %>% 
  arrange(desc(tf_idf))

df.words <- df.words %>% 
  mutate(name = factor(name, levels = unique(df.words$name)))

## Plot
df.plot <- df.words %>%
  select(name, word, tf_idf) %>%
  group_by(name) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  # reorder_within creates unique "word___name" levels
  # so each facet sorts independently
  mutate(word = tidytext::reorder_within(word, tf_idf, name))

p1 <- df.plot %>%
  ggplot(aes(tf_idf, word, fill = name)) +
  geom_col(show.legend = FALSE) +
  labs(x = "TF-IDF", y = NULL) +
  facet_wrap(~name, ncol = 2, scales = "free") +
  tidytext::scale_y_reordered()
p1

## Save plot
p1 %>% 
  ggsave(filename = "./data/result/fig_Tf-IDF_NAICS_21.png", 
         device = "png",
         dpi = 300, 
         units = "in",
         height = 12, width = 12)

