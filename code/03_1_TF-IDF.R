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
NAICS2_CODE <- 21
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

## Paginated save: split into pages when there are too many companies
companies          <- levels(droplevels(df.plot$name))
n_companies        <- length(companies)
companies_per_page <- 20   # 10 rows × 2 cols
height_per_row     <- 3    # inches per panel row

pages <- split(companies,
               ceiling(seq_along(companies) / companies_per_page))

for (page_i in seq_along(pages)) {
  page_companies <- pages[[page_i]]
  n_this_page    <- length(page_companies)
  plot_height    <- ceiling(n_this_page / 2) * height_per_row

  p <- df.plot %>%
    filter(name %in% page_companies) %>%
    droplevels() %>%
    ggplot(aes(tf_idf, word, fill = name)) +
    geom_col(show.legend = FALSE) +
    labs(x = "TF-IDF", y = NULL) +
    facet_wrap(~name, ncol = 2, scales = "free") +
    tidytext::scale_y_reordered()

  suffix <- if (length(pages) > 1) glue("_p{page_i}") else ""
  ggsave(
    plot     = p,
    filename = glue(
      "./data/result/fig_Tf-IDF_NAICS_{NAICS2_CODE}{suffix}.png"
    ),
    device = "png",
    dpi    = 300,
    units  = "in",
    width  = 12,
    height = plot_height
  )
}

