install.packages("viridis")
library(tidyverse)
library(tidytext)
library(ggrepel)
library(viridis)

df_final_key <- read_rds("df_final_key.rds")

# IO of annual reports
foldername <- list.files(path = "F:\\Fortune  500 report", pattern = '', full.names = T)
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

### 以公司為單位的 tf idf
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
  #Sys.sleep(60)
}

# Resourcing  global growth Annual Report  2015
df_word <- df_doc %>% 
  unnest_ngrams(output = word, input = value, n = 2, n_min = 1, ngram_delim = "___", stopwords = stopwords::stopwords())

df_word_uni <- df_word %>% filter(!str_detect(word, "[0-9]")) %>% 
  filter(!str_detect(word, "___")) %>%
  filter(word!="aramco") %>% #可以放tfidf 已出現但不想要的字，不用正規表達，一次放一個字
  filter(word!="sinopec") %>%
  filter(word!="glencore") %>%
  filter(word!="gazprom") %>%
  filter(word!="sar") %>%
  filter(word!="h.e") %>%
  filter(word!="koz") %>%
  filter(word!="ffo") %>%
  filter(word!="indd") %>%
  filter(word!="chen") %>%
  filter(word!="ooo") %>%
  filter(word!="neft") %>% 
  filter(word!="bcm") %>%
  filter(word!="pao") %>%
  filter(word!="hsec") %>%
  filter(!(word %in% c("fy","ovl","x", "m", "anglo", "bhp", "bhp's", "equinor",
                       "", "rquinor\\'s", "indianoil", "nn", "shri", "ncnn", "nc",
                       "nnnn", "nnc", "ncn", "lukoil", "company\\'s", "mitsui", "mitsui\\'s",
                       "ihh", "div", "ind", "tinto", "riotinto.com", "rio", "tinto\\'s",
                       "rosneft", "rosneft's", "x", "rn", "company's", "fi", "saudi", "group's",
                       "leung", "vale", "ore", "refi", "dan", "crore", "cnn", "nnn", "nnno",
                       "cerrej", "nok", "hkfrs", "soes", "tahun", "pada", "kumba", "waio", "dlc",
                       "carlton", "cutifani", "ordos", "dpi", "asa", "ugsfs", "ngv", "yamal",
                       "oao", "cerrej", "ompany", "ndian", "contd", "qurna", "ipp",
                       "gestamp", "penske", "ongc", "hpcl", "goi", "sez", "tolgoi",
                       "oyu", "stip", "kennecott", "pjsc", "mmt", "krub", "refi", "riyals",
                       "rabigh", "zakat", "arlanxeo", "hkfrs", "henan", "hkfrss", "yuk",
                       "hainan", "dan", "pada", "untuk", "dari","quellaveco", "namdeb", "datun",
                       "ncs", "sdfi", "ugsf", "ao", "neftekhim", "kt", "cn", "nncn", "rore", "ritek",
                       "timan", "bussan", "ptl", "videsh", "ompl", "ccps", "upto", "jean", "csc",
                       "6o", "jsc", "yuganskneftegaz", "mmb", "shpp", "scfd", "pif", "deruyck", "epec",
                       "perseroan", "dengan", "dalam", "komisaris", "dewan", "atau", "keuangan",
                       "direksi", "〇", "бо", "х"))) %>% 
  filter(!str_detect(word, "mitsui.?s|group.?s|statoil.?"))#’‘
df_word_uni %>% count(name)

df_word_bi <- df_word %>% filter(!str_detect(word, "[0-9]")) %>%
  filter(str_detect(word, "___"))

df_word_uni_n <- df_word_uni %>% 
  #mutate(name = str_remove(name, " text file/.*|txt/.* |Group/.*")) %>%
  mutate(name = str_remove_all(name, "([0-9]){1,4}.txt|F:\\Fortune  500 report/"))%>%
  count(name, word)
df_word_uni_n %>% count(name)

df_uni_tfidf <- df_word_uni_n %>% 
  anti_join(stop_words) %>%
  bind_tf_idf(word, name, n)

df_word_uni_n %>% write_rds("df_word_uni_n_21.rds") #目前結果存出去
df_uni_tfidf %>% write_rds("df_uni_tfidf21.rds")
df_uni_tfidf %>% write_csv("df_uni_tfidf21.csv")

#tfidf 畫圖

df_uni_tfidf %>% count(name)

#df_uni_tfidf %>% mutate(name = str_remove_all(name, "F:\\Fortune  500 report/"))
df_uni_tfidf_cleanname <- df_uni_tfidf %>%
              mutate(name = str_remove_all(name, "F:\\\\Fortune  500 report/"))%>%
              mutate(name = str_remove_all(name, "/.*"))%>%
              mutate(name = str_remove_all(name, "[0-9]{1,4} "))
df_uni_tfidf_cleanname %>% count(name)
  
p_tw_black <-
df_uni_tfidf_cleanname %>% #filter(name %in% c("Anglo American",
                                              #"BHP Group",
                                              #"China National Coal Group",
                                              #"Equinor",
                                              #"Gazprom",
                                              #"Glencore",
                                              #"Indian Oil",
                                              #"Lukoil"))
  group_by(name) %>% #我在網路上查的
  top_n(10) %>% 
  ungroup() %>% view 
  #mutate(name = if_else(str_detect(name, "Natural Gas"), "Indian Oil & Natural Gas", name)) %>%
  mutate(name = if_else(str_detect(name, "Yanchang"), "Shaanxi Yanchang Petroleum", name))  %>%
  mutate(name = as.factor(name),
         word = reorder_within(word, tf_idf, name)) %>% #name 公司名，tf_idf 
  #mutate(word = reorder(word, tf_idf)) %>% 
  ggplot(aes(word, tf_idf, fill = name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~name, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "TF-IDF",
       x = NULL,
       title = "What were the most common baby names in each decade?",
       subtitle = "Via US Social Security Administration") 
  #scale_fill_manual(values=c("blue", "black", "gold", "red")) #自己選顏色
  #scale_fill_viridis(option="plasma", discrete = TRUE)
  
p_tw_black %>% 
  ggsave(filename = "tfidf21.png", dpi = 300, height = 12, width = 12)

#子軒原來寫的
group_by(name) %>% arrange(-tf_idf) %>%
  slice(1:15) %>% ungroup() %>%
  ggplot(aes(x = word, y = tf_idf)) + geom_col() +
  facet_wrap(name ~ ., scale = "free") +
  coord_flip()

#bi_n 算跟作圖
df_word_bi_n <- df_word_bi %>% 
  mutate(name = str_remove(name, " text file/.*")) %>%
  count(name, word)

df_bi_tfidf <- df_word_bi_n %>% 
  anti_join(stop_words) %>%
  bind_tf_idf(word, name, n)

#113-124 網上查錯Faceting variables must have at least one value
df_bi_tfidf %>% filter(name %in% c("F:\\Fortune  500 report/5 Sinopec Group/Sinopec Group AR txt/Sinopec Group AR ",
                                    "F:\\Fortune  500 report/14 Saudi Aramco/Saudi Aramco AR txt/Saudi Aramco AR ",
                                    "F:\\Fortune  500 report/34 Glencore/Glencore AR txt/Glencore AR ",
                                    "F:\\Fortune  500 report/84 Gazprom/Gazprom AR txt/Gazprom AR ")) %>%
  group_by(name) %>%
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, tf_idf)) %>% 
  ggplot(aes(word, tf_idf, fill = name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~name, scales = "free") +
  coord_flip()
#126-134 子軒寫錯Faceting variables must have at least one value
df_bi_tfidf %>% filter(name %in% c("F:\\Fortune  500 report/5 Sinopec Group/Sinopec Group AR txt/Sinopec Group AR ",
                                   "F:\\Fortune  500 report/14 Saudi Aramco/Saudi Aramco AR txt/Saudi Aramco AR ",
                                   "F:\\Fortune  500 report/34 Glencore/Glencore AR txt/Glencore AR ",
                                   "F:\\Fortune  500 report/84 Gazprom/Gazprom AR txt/Gazprom AR ")) %>%
  group_by(name) %>% arrange(-tf_idf) %>% 
  slice(1:10) %>% ungroup() %>%
  ggplot(aes(x = word, y = tf_idf)) + geom_col() +
  facet_wrap(name ~ ., scale = "free") +
  coord_flip()

df_word_bi_n %>% write_rds("data/df_word_bi_n.rds")
df_bi_tfidf %>% write_rds("data/df_bi_tfidf.rds")