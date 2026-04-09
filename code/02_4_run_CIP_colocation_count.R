#' Rerun selected word counts
rm(list = ls())

NAICS2_CODES <- c(11, 21, 22, 23, 31, 32, 33, 42, 44, 45, 48, 49, 51, 52, 53, 54, 60, 62, 72)
# NAICS2_CODES <- c(21) #如果只要（重）跑特定一個產業，即單獨放那個產業
# 19 industries in total
# 55 沒有年報 是空的

timeSpent <- c()
for (NAICS2_CODE in NAICS2_CODES) {
  # timing
  t0 <- Sys.time()
  source("./code/utils/compute_colocation_complementary_index.R")
  t_end <- Sys.time()
  timeSpent <- c(timeSpent, t_end - t0)
  cat(">>> Time used: ", NAICS2_CODE, ": ",format(t_end - t0), "\n")
}

# =============================================================================
# Runtime log (11 cores, last run 2026-04-07)
# -----------------------------------------------------------------------------
# NAICS2  Description                          Time
# 11      Agriculture                          ~16.8 secs
# 21      Mining, Quarrying, Oil & Gas         ~21.9 mins
# 22      Utilities                            ~4.3 mins
# 23      Construction                         ~3.3 mins
# 31      Food, Textile Mfg                    ~6.1 mins
# 32      Paper, Chemical, Plastics Mfg        ~18.4 mins
# 33      Machinery, Electronics Mfg           ~24.2 mins
# 42      Wholesale Trade                      ~5.3 mins
# 44      Retail Trade (stores)                ~5.2 mins
# 45      Retail Trade (non-store)             ~5.9 mins
# 48      Transportation                       ~2.7 mins
# 49      Warehousing & Couriers               ~1.2 mins
# 51      Information                          ~7.6 mins
# 52      Finance & Insurance                  ~39.7 mins
# 53      Real Estate                          ~17.9 secs
# 54      Professional & Technical Services    ~1.6 mins
# 60      (Other)                              ~32.4 secs
# 62      Health Care & Social Assistance      ~1.6 mins
# 72      Accommodation & Food Services        ~21.8 secs
# -----------------------------------------------------------------------------
# Total                                        ~2.6 hrs
# =============================================================================
