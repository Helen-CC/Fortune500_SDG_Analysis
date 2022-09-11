#install.packages("clipr") #給write_clip function
library(tidyverse)
library(tidytext)
library(clipr)
library(stringi)

df_final_key <- read_rds("df_final_key.rds")
df_final_key %>% write_clip()

# IO of annual reports
# 這裡的path 要根據annual report 放的路徑改
# (原)foldername <- list.files(path = "annual", pattern = '', full.names = T)
foldername <- list.files(path = "F:\\Fortune  500 report", pattern = '', full.names = T)

# make sure they are all folders(資料夾)
#foldername <- foldername[!str_detect(foldername, "udpipe")]
#filename <- list()
#i <- 1

#13-16 新寫的，以下是之前的，但根據我現在電腦本機路徑改
foldername <- foldername[!str_detect(foldername, "\\.xlsx$|\\.docx$|\\.R$|\\.ini$")]
foldername <- foldername[!str_detect(foldername, "udpipe")]
filename <- list()
i <- 1

filename %>% as_tibble %>% write_clip()
filename %>% write_rds("C:\\Users\\User\\Desktop\\Research Proposal\\Y3, Manuscript 1 TM\\Help_Ted Helen\\filename.rds")

for (i in 1:length(foldername)) {
  print(i)
  inner_foldername_tmp <- list.files(path = foldername[i], pattern = '', full.names = T)
  inner_foldername <- inner_foldername_tmp[!str_detect(inner_foldername_tmp, "pdf")]
  filename[[i]] <- list.files(path = inner_foldername, pattern = '', full.names = T)
  filename[[i]] <-  filename[[i]][str_detect(filename[[i]], "txt")]
}

df_keyword_n <- tibble()

index_now <- 1
k <- index_now
fn=1
for (k in c(16, 126, 211, 411, 395, 116, 191, 417, 405, 53, 324, 391, 408, 478, 37, 153, 302, 299, 360, 204, 234, 125, 248)) {
#for (k in index_now:length(filename)) {
df_doc <- tibble()
  for (fn in 1:length(filename[[k]])) {
    df_doc_tmp <- read_lines(filename[[k]][fn]) %>% as_tibble() %>% 
      summarise(value = str_c(value, collapse = " ")) %>%
      mutate(name = filename[[k]][fn])
    df_doc <- df_doc %>% bind_rows(df_doc_tmp)
  }
  
  df_sentence <- df_doc %>% mutate(value = iconv(value, "", "UTF-8")) %>% 
    unnest_tokens(output = text, input = value, token = "sentences")
  # df_sentence %>% glimpse()
  #i=1 i 是一家公司裡面的第幾個檔
  #j=1 j 是第幾個關鍵字
  
  for (i in 1:length(unique(df_sentence$name))) {
    for(j in 1:nrow(df_final_key)){
    # for(j in 101:200){
      if(j %% 100 == 0){cat('.\n')}
      message(df_final_key$word[j])
      n_keyword <- df_sentence %>% filter(name == unique(df_sentence$name)[i]) %>%
        pull(text) %>% map_dbl(~stringi::stri_count(., regex = df_final_key$word[j])) %>%
        sum(.)
      sdg_word <- df_final_key$word[j]
      df_keyword_n_tmp <- tibble(name = df_doc[i, ]$name, n_keyword, sdg_word)
      df_keyword_n <- df_keyword_n %>% bind_rows(df_keyword_n_tmp)
    }
  }
  message("finish", "")
  index_now <- index_now + 1
  Sys.sleep(60)
  
}

df_keyword_n %>% filter(n_keyword > 0)
df_keyword_n %>% write_rds("df_keyword_31.rds")

Mining21 <- read_rds("df_keyword_21.rds")
Mining21 %>% write_csv ("Mining21.csv")
Manufacturing31 <-  read_rds("df_keyword_31.rds")
Manufacturing31 %>% write_csv ("Manufacturing31.csv")
Manufacturing33 <-  read_rds("df_keyword_33.rds")
Manufacturing33 %>% write_csv ("Manufacturing33.csv")

index_now %>% write_rds

# https://github.com/Aurora-Network-Global/sdg-queries
# https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Fcpb-ap-se2.wpmucdn.com%2Fblogs.auckland.ac.nz%2Fdist%2F8%2F761%2Ffiles%2F2020%2F10%2FUoA-SDG-Keyword-List-Ver.-1.1.xlsx&wdOrigin=BROWSELINK
