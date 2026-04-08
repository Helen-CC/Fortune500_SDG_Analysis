#' Rerun selected word counts
rm(list = ls())

NAICS2_CODES <- c(11, 21, 22, 23, 31, 32, 33, 42, 44, 45, 48, 49, 51, 52, 53, 54, 60, 62, 72)
# NAICS2_CODES <- c(11, 72) #如果只要（重）跑特定一個產業，即單獨放那個產業
# 19 industries in total
# 55 沒有年報 是空的

timeSpent <- c()
for (NAICS2_CODE in NAICS2_CODES) {
  # timing
  t0 <- Sys.time()
  # source("./code/02_1_parallel_wordCount.R")
  source("./code/utils/count_keyword_frequency_parallel.R")
  t_end <- Sys.time()
  timeSpent <- c(timeSpent, t_end - t0)
  cat(">>> Time used: ", NAICS2_CODE, ": ",format(t_end - t0), "\n")
}

# =============================================================================
# Runtime log (11 cores, last run 2026-04-07)
# -----------------------------------------------------------------------------
# NAICS2  Description                          Time
# 11      Agriculture                          ~1.0 min
# 21      Mining, Quarrying, Oil & Gas         ~2.3 hrs
# 22      Utilities                            ~16.3 mins
# 23      Construction                         ~13.5 mins
# 31      Food, Textile Mfg                    ~2.3 hrs
# 32      Paper, Chemical, Plastics Mfg        ~1.2 hrs
# 33      Machinery, Electronics Mfg           ~1.7 hrs
# 42      Wholesale Trade                      ~21.5 mins
# 44      Retail Trade (stores)                ~19.7 mins
# 45      Retail Trade (non-store)             ~24.2 mins
# 48      Transportation                       ~11.1 mins
# 49      Warehousing & Couriers               ~4.4 mins
# 51      Information                          ~36.4 mins
# 52      Finance & Insurance                  ~3.0 hrs
# 53      Real Estate                          ~1.2 mins
# 54      Professional & Technical Services    ~6.4 mins
# 60      (Other)                              ~2.3 mins
# 62      Health Care & Social Assistance      ~6.9 mins
# 72      Accommodation & Food Services        ~1.4 mins
# -----------------------------------------------------------------------------
# Total                                        ~14.2 hrs
# =============================================================================
