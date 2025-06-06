rm(list = ls())
# 05
# time series line plot showing the ratio of keyword presence over years and across SDG categories
# ratio = number of keywords shown / total number of words
# the denominator within a year are all the same.
# the x-axis is time (year)
# the y-axis is ratio
# SDG0-SDG16 -> 17 lines

library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(forcats)
source("./code/config.R", encoding = '')

# Load data
df <- read_rds(glue("{DROPBOX_PATH}/cleaned_data/df_wordCount_NAICS21.rds"))
df_final_key <- read_rds(glue("{DROPBOX_PATH}/cleaned_data/df_final_key_all.rds"))

str(df); class(df)
# Merge SDG categories
df <- df %>% 
  left_join(df_final_key, by = c('keyword' = 'word')) %>% 
  # add year and rank
  mutate(year = str_extract(name, "_\\d+"),
         year = str_remove(year, "_"),
         year = as.numeric(year),
         name = str_remove(name, "_\\d+")) %>% 
  mutate(rank = str_extract(name, "\\d+"),
         rank = as.numeric(rank),
         name = str_remove(name, "\\d+"),
         name = str_trim(name))

# Join Methods in dplyr (the package that deals with dataframes)
# 
# dplyr::inner_join()
# left_join
# right_join
# outer_join
# anti_join

# Prepare for plot
df.plot <- df %>% 
  # find the numerator
  group_by(name, rank, year, sdg) %>% 
  summarise(n_keyword = sum(n_keyword)) %>% 
  ungroup() %>% 
  # find the denominator
  group_by(name, rank, year) %>% 
  mutate(n_keyword_total = sum(n_keyword)) %>% 
  ungroup() %>% 
  # find the ratio
  mutate(ratio = n_keyword / n_keyword_total * 100) %>% 
  mutate(ratio = round(ratio, 2)) %>% 
  # find sdg number
  mutate(sdg_number = str_extract(sdg, "\\d+"),
         sdg_number = as.numeric(sdg_number)) %>% 
  # set sdg as factor data
  mutate(sdg = as_factor(sdg)) %>% 
  mutate(sdg = fct_reorder(sdg, sdg_number)) %>% 
  filter(!is.na(sdg))

## Pick a company
## TODO: input a company's rank
AVAILABLE_COMPANIES <- df %>% select(name, rank) %>% distinct()
company_rank <- 5
company_name <- AVAILABLE_COMPANIES %>% filter(rank == company_rank) %>% pull(name)

label_data <- df.plot |> 
  filter(rank == company_rank) |> 
  group_by(sdg) |> 
  filter(year == max(year)) |> 
  ungroup()

# # df.plot可以看每個sdg 次數及ratio
# p1 <- df.plot %>% 
#   filter(rank == company_rank) %>% 
#   ggplot(aes(x = year, y = ratio, color = sdg)) +
#   geom_line()+
#   ggtitle(company_name)+
#   labs(x = "year", y = "percentage")+
#   #這兩行調x y 軸字大小
#   theme(axis.text.y = element_text(size = 13),
#         axis.text.x = element_text(size = 13))
# 
# p1

# SET THE END YEAR FOR PLOT #我不用手動改最後一年這裡
MAX_YEAR <- df.plot %>% filter(rank == company_rank) %>% pull(year) %>% max()
# Find top 5 SDG categories for the last year (2020)
top_5_sdg <- df.plot %>%
  filter(rank == company_rank, year == MAX_YEAR) %>%
  # choose the top n SDG categories to show a text box
  top_n(3, ratio) %>%
  pull(sdg)

# scale the range of y-axis
y_max <- max(df.plot %>% filter(rank == company_rank) %>% pull(ratio))+10
# Plot the data
p1 <- df.plot %>% 
  filter(rank == company_rank) %>% 
  ggplot(aes(x = year, y = ratio, color = sdg)) +
  geom_line() +
  ggtitle(company_name) +
  labs(x = "Year", y = "Percentage") +
  theme(axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13)) +
  # add text box to the end of 2020 data that indicates the top n SDG category
  geom_text(data = df.plot %>% 
              filter(rank == company_rank, sdg %in% top_5_sdg, year == MAX_YEAR),
            aes(label = sdg, y = ratio + 0.5),
            size = 3, hjust = 0.5, vjust = 0, check_overlap = TRUE) +
  #by = 1, every years will show on png; by = 2, every 2 years
  scale_x_continuous(breaks = seq(min(df.plot$year), max(df.plot$year), by = 1))+
  ylim(0, y_max)

p1

# p2 <- df.plot |> 
#   # choose the company
#   filter(rank == company_rank) %>% 
#   ggplot(aes(x = year, y = ratio, color = sdg)) +
#   geom_line()+
#   geom_text(data = label_data, aes(label = sdg),
#             nudge_x = 0.5, check_overlap = TRUE, hjust = 0, vjust = 0)+
#   ggtitle(company_name)+
#   labs(x = "year", y = "percentage")+
#   #這兩行調x y 軸字大小
#   theme(axis.text.y = element_text(size = 13),
#         axis.text.x = element_text(size = 13))+
#   scale_x_continuous(breaks = seq(min(df.plot$year), max(df.plot$year), by = 1))
# p2

## Save plot
#p1 %>% 
  #ggsave(filename = paste0("./data/result/fig_timetrend_across_SDGcate_", company_name, ".png"), 
        # device = "png",
         #dpih = 300, 
        # units = "in",
        # height = 12, width = 12)

## Save plot that is not managed by git
p1 %>% 
  ggsave(filename = paste0("./data/result/TimeTrend/fig_timetrend_across_SDGcate_", company_name, ".png"), 
         device = "png",
         dpi = 300, 
         units = "in",
         height = 6, width = 8)

