# install.packages("psych")
# install.packages("psychTools")
# install.packages("psych", repos="http://personality-project.org/r", type="source" )
# install.packages("ggplot2")
# install.packages("extrafontdb")
rm(list = ls())
library(tidyverse)
library(tidytext)
library(psych)
library(psychTools)
library(clipr)
library(ggplot2)

# IO of annual reports
foldername <- list.files(path = "./data/raw_data/Fortune_500_report/", pattern = '', full.names = T)
# make sure they are all folders(資料夾)
foldername <- foldername[!str_detect(foldername, "\\.xlsx$|\\.docx$|\\.R$|\\.ini$")]
foldername <- foldername[!str_detect(foldername, "udpipe")]
filename <- list()
i <- 1
for (i in 1:length(foldername)) {
  print(i)
  inner_foldername_tmp <- list.files(path = foldername[i], pattern = '', full.names = T)
  inner_foldername <- inner_foldername_tmp[!str_detect(inner_foldername_tmp, "pdf")]
  filename[[i]] <- list.files (path= inner_foldername, pattern = "", full.names = T)
}

index_now <- 1
k <- index_now
### 這邊在外面沒錯 要重跑就要歸零
df_doc <- tibble()
### 讀你要的 text
for (k in c(445, 46, 484, 18, 30, 107, 127, 151, 161, 164, 392, 268, 469, 136, 339, 425, 194, 217, 333, 343, 175, 307)) {
  for (fn in 1:length(filename[[k]])) {
    df_doc_tmp <- read_lines(filename[[k]][fn]) %>% as_tibble() %>% 
      summarise(value = str_c(value, collapse = " ")) %>%
      mutate(name = filename[[k]][fn])
    df_doc <- df_doc %>% bind_rows(df_doc_tmp)
  }
  message("finish", "")
  index_now <- index_now + 1
  Sys.sleep(0) #這裡0可以自己改
}

df_word <- df_doc %>% 
  unnest_tokens(output = word, input = value, token = "words")

df_word_length <- df_word %>% 
  count(name) %>%
  mutate(year = str_extract(name, "([0-9]){4}")) %>%
  mutate(name = str_remove(name, ".*/")) %>% #整理縮減name column 檔名
  mutate(name = str_remove(name, " ([0-9]){4}.txt")) %>%
  mutate(name = str_remove(name, " ([0-9]){4} AR.txt")) %>%
  mutate(name = str_remove(name, " 20-F")) %>%
  mutate(name = str_remove_all(name, " AR| Annual Report| Group| \\(Group\\)"))
# 存word_length
# df_word_length %>% write_rds ("word_length21.rds")
# df_word_length %>% write_csv("word_length21.csv")

df_final_key <- read_rds("./data/rds/df_final_key.rds")
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

# df_long_join %>% write_rds ("long_join31.rds")
# df_long_join %>% write_csv("long_join31.csv")

# df_wide <- df_join %>% filter(n_keyword > 0) %>%
#   group_by(name, sdg) %>% summarise(n_keyword = sum(n_keyword)) %>% ungroup() %>%
#   pivot_wider(names_from = sdg, values_from = n_keyword, values_fill = list(n_keyword = 0)) %>%
#   select(name, SDG0, SDG1, SDG2, SDG3, SDG4, SDG5, SDG6, SDG7, SDG8, SDG9, SDG10, SDG11, SDG12, SDG13, SDG14, SDG15, SDG16)

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
