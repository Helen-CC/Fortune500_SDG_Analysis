rm(list = ls())
library(tidyverse)
library(tidytext)
library(psych)
library(psychTools)
# library(clipr)
library(ggplot2)


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
NAICS2 <- 31
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




df_word <- df_doc %>% 
  unnest_tokens(output = word, input = value, token = "words")

df_word_length <-df_word %>% 
  group_by(year, rank) %>% 
  count(name) %>% 
  mutate(name = str_replace(name, "\\d+\\s", ""))



#df_final_key %>% filter(!row_number() %in% c(278)) # 去掉 SDG11 governance 這個字 #沒成功!!!
df_raw <- read_rds("./data/rds/df_keyword_21.rds")

df_join <- df_raw %>% 
  mutate(name = str_remove(name, ".*/")) %>% #整理縮減name column 檔名
  mutate(name = str_remove(name, " ([0-9]){4}.txt")) %>%
  mutate(name = str_remove(name, " ([0-9]){4} Annual Report.txt")) %>%
  mutate(name = str_remove(name, " 20-F")) %>%
  mutate(name = str_remove_all(name, " AR| Annual Report| Group| \\(Group\\)")) %>%
  left_join(df_final_key %>% rename(sdg_word = word)) %>% #欄位名稱改統一
  mutate(sdg = coalesce(sdg, "SDG0"))

df_long <- df_join %>% filter(n_keyword > 0) %>%
  group_by(name, sdg) %>% summarise(n_keyword = sum(n_keyword)) %>% ungroup()

df_long_join <- df_long %>% 
  left_join(df_word_length %>% group_by(name) %>% summarise(n = sum(n))) %>%
  mutate(per_keyword = n_keyword/n)


df_long_join %>%
  mutate(sdg_number = str_extract(sdg, "[0-9]{1,2}")) %>%
  mutate(sdg_number = as.integer(sdg_number)) %>%
  mutate(sdg = as_factor(sdg)) %>%
  mutate(sdg = fct_reorder(sdg, sdg_number)) %>%
  ggplot(aes(x = name, y = sdg, fill = per_keyword)) + geom_tile() +
  coord_flip() +
  theme_bw() +
  scale_linetype(guide = "none") +
  #這裡可以改顏色low = "某顏色", high = "某顏色"
  scale_fill_gradient(low = "snow", high = "navyblue", labels = scales::percent, breaks=c(0,0.0025,0.005))+ #breaks 這邊寫要的百分比的值
  #x =可以改圖上x軸名稱，y= 圖上y 軸名稱
  labs(x= "Company",y= "SDG", title = "Consumer Goods Manufacturing", fill = "percentage") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(family = "Noto Sans CJK TC Medium", face = "plain", size = 18),
        legend.text = element_text(family = "Noto Sans CJK TC Medium", face = "plain"), 
        text = element_text(family = "Noto Sans CJK TC Medium"))
