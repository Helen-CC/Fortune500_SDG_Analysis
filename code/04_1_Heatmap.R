rm(list = ls())
library(tidyverse)
library(tidytext)
library(ggplot2)
source("./code/utils/func.R")

df_final_key <- read_rds("./data/cleaned_data/df_final_key.rds")
df.RankCode <- getRankCodeMap("./data/raw_data/TM Final_FortuneG500 (2021)_v2.xlsx")
df.doc_31 <- readReports(NAICS2_CODE = "31")
df.doc_21 <- readReports(NAICS2_CODE = "21")



df.word_31 <- df.doc_31 %>% 
  unnest_tokens(output = word, input = value, token = "words")
df.word_21 <- df.doc_21 %>% 
  unnest_tokens(output = word, input = value, token = "words")

df.wordlen_31 <- df.word_31 %>% 
  group_by(year, rank) %>% 
  count(name) %>% 
  mutate(name = str_replace(name, "\\d+\\s", ""))
df.wordlen_21 <- df.word_21 %>% 
  group_by(year, rank) %>% 
  count(name) %>% 
  mutate(name = str_replace(name, "\\d+\\s", ""))

# Join two dataframes from script 1 and 2
df.word_21 %>% head()
df_final_key %>% head()
df.wordCount_31 <- read_rds("./data/cleaned_data/df_wordCount_naics_31.rds")

df.combine <- df.wordCount_31 %>%
  left_join(df_final_key, by = c("keyword" = "word")) %>% 
  mutate(year = as.numeric(str_extract(name, "20\\d+")),
         name = str_replace(name, "_20\\d+", ""),
         rank = as.numeric(str_extract(name, "\\d+")),
         name = str_replace(name, "\\d+\\s", ""))


df.long <- df.combine %>% 
  filter(n_keyword > 0) %>%
  group_by(name, sdg) %>% 
  summarise(n_keyword = sum(n_keyword),
            rank = head(rank, 1)
            ) %>% 
  ungroup()

df.long_join <- df.long %>% 
  left_join(df.wordlen_31 %>% 
              group_by(name) %>% 
              summarise(n = sum(n))
            ) %>%
  mutate(per_keyword = n_keyword/n)

# Plot
df.plot <- df.long_join %>%
  mutate(sdg_number = as.numeric(str_extract(sdg, "\\d+"))) %>%
  mutate(sdg = as_factor(sdg)) %>%
  mutate(sdg = fct_reorder(sdg, sdg_number))

df.plot %>%
  ggplot(aes(x = name, y = sdg, fill = per_keyword)) + 
  # grid plot
  geom_tile() +
  coord_flip() +
  theme_bw() +
  scale_linetype(guide = "none") +
  scale_fill_gradient(low = "snow", high = "navyblue", # change color
                      labels = scales::percent,
                      breaks=c(0, 0.0025, 0.005) # breaks indicate percentile
                      ) +
  labs(x = "Company", y = "SDG", 
       title = "Consumer Goods Manufacturing", 
       fill = "percentage") +
  # fill in colors in blank grids
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(legend.position="bottom") #+
  # set font
  # theme(plot.title = element_text(family = "Noto Sans CJK TC Medium", face = "plain", size = 18),
  #       legend.text = element_text(family = "Noto Sans CJK TC Medium", face = "plain"), 
  #       text = element_text(family = "Noto Sans CJK TC Medium"))


## Save plot
p1 %>% 
  ggsave(filename = paste0("./data/result/fig_heatmap_NAICS", NAICS2_CODE, ".png"), 
         device = "png",
         dpi = 300, 
         units = "in",
         height = 12, width = 12)
