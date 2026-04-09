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
source("./code/utils/func.R")
source("./code/config.R", encoding = '')

# Load data
## TODO: Change NAICS2_CODE if needed
NAICS2_CODE <- 21

df_final_key <- read_rds(glue("{DROPBOX_PATH}/cleaned_data/df_final_key_all.rds"))
df.Gvkey     <- getGvkeyMap(
  glue("{DROPBOX_PATH}/company_reference/company_reference_master.xlsx")
)

df <- read_rds(
  glue("{DROPBOX_PATH}/cleaned_data/df_wordCount_NAICS{NAICS2_CODE}.rds")
)

# Merge SDG categories and attach gvkey
df <- df %>%
  left_join(df_final_key, by = c("keyword" = "word")) %>%
  # parse year and strip from name
  # e.g. "5 Sinopec Group_2020" -> year=2020, name="5 Sinopec Group"
  mutate(
    year = as.numeric(str_extract(name, "\\d{4}$")),
    name = str_remove(name, "_\\d{4}$")
  ) %>%
  # attach gvkey via company name
  left_join(df.Gvkey, by = "name")

# Prepare for plot
df.plot <- df %>%
  # find the numerator
  group_by(gvkey, name, year, sdg) %>%
  summarise(n_keyword = sum(n_keyword), .groups = "drop") %>%
  # find the denominator
  group_by(gvkey, name, year) %>%
  mutate(n_keyword_total = sum(n_keyword)) %>%
  ungroup() %>%
  # find the ratio
  mutate(ratio = n_keyword / n_keyword_total * 100) %>%
  mutate(ratio = round(ratio, 2)) %>%
  # set sdg as ordered factor
  mutate(sdg_number = as.numeric(str_extract(sdg, "\\d+"))) %>%
  mutate(sdg = as_factor(sdg)) %>%
  mutate(sdg = fct_reorder(sdg, sdg_number)) %>%
  filter(!is.na(sdg))

## Pick a company
## TODO: set COMPANY_GVKEY to the gvkey of the company you want to plot
AVAILABLE_COMPANIES <- df %>% select(gvkey, name) %>% distinct()
COMPANY_GVKEY <- AVAILABLE_COMPANIES$gvkey[1]  # default: first company
company_name  <- AVAILABLE_COMPANIES %>%
  filter(gvkey == COMPANY_GVKEY) %>%
  pull(name) %>%
  head(1)

# SET THE END YEAR FOR PLOT
MAX_YEAR <- df.plot %>%
  filter(gvkey == COMPANY_GVKEY) %>%
  pull(year) %>%
  max()

# Find top 3 SDG categories for the last year
top_sdg <- df.plot %>%
  filter(gvkey == COMPANY_GVKEY, year == MAX_YEAR) %>%
  top_n(3, ratio) %>%
  pull(sdg)

# scale the range of y-axis
y_max <- df.plot %>%
  filter(gvkey == COMPANY_GVKEY) %>%
  pull(ratio) %>%
  max() + 10

# Plot the data
p1 <- df.plot %>%
  filter(gvkey == COMPANY_GVKEY) %>%
  ggplot(aes(x = year, y = ratio, color = sdg)) +
  geom_line() +
  ggtitle(company_name) +
  labs(x = "Year", y = "Percentage") +
  theme(axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13)) +
  geom_text(
    data = df.plot %>%
      filter(gvkey == COMPANY_GVKEY, sdg %in% top_sdg, year == MAX_YEAR),
    aes(label = sdg, y = ratio + 0.5),
    size = 3, hjust = 0.5, vjust = 0, check_overlap = TRUE
  ) +
  scale_x_continuous(
    breaks = seq(min(df.plot$year), max(df.plot$year), by = 1)
  ) +
  ylim(0, y_max)

p1

## Save plot
p1 %>%
  ggsave(
    filename = glue(
      "./data/result/TimeTrend/fig_timetrend_across_SDGcate_{company_name}.png"
    ),
    device = "png",
    dpi    = 300,
    units  = "in",
    height = 6, width = 8
  )
