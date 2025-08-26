#' Rerun selected word counts

rm(list = ls())
# NAICS2_CODES <- c(11, 21, 22, 23, 31, 32, 33, 42, 44, 45, 48, 49, 51, 52, 53, 54, 60, 62, 72)
NAICS2_CODES <- c(11, 21)
# NAICS2_CODES <- c(21) #如果只要（重）跑特定一個產業，即單獨放那個產業
# all industries in Excel: TM Final_FortuneG500 (2021)_v2
# NAICS2_CODES <- c(11, 21, 22, 23, 31, 32, 33, 42, 44, 45, 48, 49, 51, 52, 53, 54, 60, 62, 72)
# 19 industries in total
# 55 沒有年報 是空的

timeSpent <- c()
for (NAICS2_CODE in NAICS2_CODES) {
  # timing
  t0 <- Sys.time()
  source("./code/02_3_compute_colocation_complementary_index.R")
  t_end <- Sys.time()
  timeSpent <- c(timeSpent, t_end - t0)
  cat(">>> Time used: ", NAICS2_CODE, ": ",format(t_end - t0), "\n")
}

