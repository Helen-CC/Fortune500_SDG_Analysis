# rm(list = ls())
rm(list = setdiff(ls(), c("NAICS2_CODE", "NAICS2_CODES", "timeSpent", "t0", "t1")))
library(tidyverse)
library(readr)
library(tidytext)
library(stringi)
library(fs)
library(foreach)
library(doParallel)
library(furrr)
library(doFuture)
library(progressr)
source("./code/utils/func.R", encoding = '')
source("./code/config.R", encoding = '')

# Set up parallel backend
# Registrate workers
n_cores <- parallel::detectCores() - 1  # Leave one core free for system processes
cl <- makeCluster(n_cores)
clusterEvalQ(cl, library(purrr))
clusterEvalQ(cl, library(dplyr))
registerDoParallel(cl)


# define function used in parallel computing
# index, total, and NAICS codes are passed only for logging
countColocationWords <- function(splited_data,
                                 sdg_keywords,
                                 comp_keywords,
                                 ind_keywords,
                                 press_keywords,
                                 window_size = 10,
                                 index = NULL,
                                 total = NULL,
                                 the_naics_code = NULL) {

  # loggin into file
  progress_message <- sprintf("Processing %d of %d\n", index, total)
  write_lines(progress_message, paste0("./logs/count_colocation_progress_NAICS", the_naics_code, "_", index, ".log"))
  t0_sub <- Sys.time()

  # Combine CIP keywords into single regex patterns for efficiency
  comp_regex <- paste(comp_keywords, collapse = "|")
  ind_regex <- paste(ind_keywords, collapse = "|")
  press_regex <- paste(press_keywords, collapse = "|")

  # Get the firm-year name
  name <- splited_data$name[1]

  # Tokenize sentences into words for window analysis
  df_words <- splited_data %>%
    mutate(sentence_id = row_number()) %>%
    unnest_tokens(output = word, input = text, token = "words", to_lower = TRUE) %>%
    group_by(sentence_id) %>%
    mutate(word_id = row_number()) %>%
    ungroup()

  # This will store the results for the current firm
  final_counts <- tibble()

  # Loop through each SDG keyword
  for (sdg_keyword in sdg_keywords) {
    # Find all sentences containing the SDG keyword
    sentences_with_keyword <- splited_data %>%
      mutate(sentence_id = row_number()) %>%
      filter(stringi::stri_detect(text, regex = sdg_keyword, case_insensitive = TRUE))

    n_keyword_total <- sum(stringi::stri_count(sentences_with_keyword$text, regex = sdg_keyword, case_insensitive = TRUE))
    
    if (n_keyword_total == 0) {
        # If the keyword doesn't appear, record 0 counts and move to the next
        keyword_counts_tmp <- tibble(
            name = name,
            keyword = sdg_keyword,
            n_keyword = 0,
            n_complementary = 0,
            n_independent = 0,
            n_pressure = 0,
            n_no_cip = 0
        )
        final_counts <- bind_rows(final_counts, keyword_counts_tmp)
        next
    }

    # To store classification for each occurrence
    classifications <- c()

    # Analyze each sentence where the SDG keyword appears
    for (id in sentences_with_keyword$sentence_id) {
      sentence_text <- tolower(sentences_with_keyword$text[sentences_with_keyword$sentence_id == id])
      sentence_words <- df_words %>% filter(sentence_id == id) %>% pull(word)
      
      # Find all start positions of the SDG keyword in the sentence
      keyword_occurrences <- stringi::stri_locate_all(sentence_text, regex = sdg_keyword, case_insensitive = TRUE)[[1]]

      for (i in 1:nrow(keyword_occurrences)) {
        # This logic is simplified; it finds words around the character location.
        # A more robust (but slower) method would map character positions to word indices.
        # This approximation is generally effective.
        pre_text <- tolower(stringi::stri_sub(sentence_text, 1, keyword_occurrences[i, "start"] - 1))
        post_text <- tolower(stringi::stri_sub(sentence_text, keyword_occurrences[i, "end"] + 1, -1))

        pre_window <- tail(stringi::stri_extract_all(pre_text, regex = "\\w+")[[1]], window_size)
        post_window <- head(stringi::stri_extract_all(post_text, regex = "\\w+")[[1]], window_size)
        
        window_text <- paste(c(pre_window, post_window), collapse = " ")

        # Check for CIP keyword presence in the window
        is_comp <- stringi::stri_detect(window_text, regex = comp_regex)
        is_ind <- stringi::stri_detect(window_text, regex = ind_regex)
        is_press <- stringi::stri_detect(window_text, regex = press_regex)
        
        classifications <- c(classifications, list(c(comp = is_comp, ind = is_ind, press = is_press)))
      }
    }
    
    # Aggregate counts for the current SDG keyword
    n_comp <- sum(sapply(classifications, `[`, "comp"))
    n_ind <- sum(sapply(classifications, `[`, "ind"))
    n_press <- sum(sapply(classifications, `[`, "press"))
    n_none <- sum(sapply(classifications, function(x) !any(x)))

    keyword_counts_tmp <- tibble(
      name = name,
      keyword = sdg_keyword,
      n_keyword = n_keyword_total,
      n_complementary = n_comp,
      n_independent = n_ind,
      n_pressure = n_press,
      n_no_cip = n_none
    )
    final_counts <- bind_rows(final_counts, keyword_counts_tmp)
  }
  return(final_counts)
}


# Load data
# df_final_key <- read_rds("./data/cleaned_data/df_final_key_all.rds")
df_final_key <- read_rds(glue("{DROPBOX_PATH}/cleaned_data/df_final_key_all.RDS"))

# IO of annual reports
df.RankCode <- getRankCodeMap("./data/raw_data/TM Final_FortuneG500 (2021)_v2.xlsx")

# Import keyword lists for Complementary/Indenpendent/Pressure Index construction

index_keyword_list <- getComplementaryIndexKeywords("./data/raw_data/Complementarity_independence_pressure_keywords.xlsx")
colocation_complementary_keywords <- index_keyword_list[["complementary"]]
colocation_independent_keywords <- index_keyword_list[["independent"]]
colocation_pressure_keywords <- index_keyword_list[["pressure"]]

## A BETTER WAY TO SELECT NAICS STARTING WITH XX
## Change the following code if needed
df.doc <- readReports(NAICS2_CODE)


# compute word count
## post-processing the read files
#' @note "\x0b" is a special non-UTF8 character that cannot be encoded, so we need to remove it
#' https://stackoverflow.com/questions/43533486/utf8-null-character-normalizing-whitespace-characters
df_sentence <- df.doc %>%
  # convert character vector between encodings
  mutate(value = stringr::str_replace(value, "\x0b", "")) %>%
  # mutate(value = iconv(value, "", "utf8", sub = "")) %>%
  unnest_tokens(output = text, input = value, token = "sentences") %>%
  drop_na() %>%
  arrange(rank)

# Parallel computing
## split dataframe by firm-year value
dfs_sentence <- split(df_sentence, df_sentence$name)
length(dfs_sentence)

# Create a progress bar
# total <- length(dfs_sentence)
# pb <- progress::progress_bar$new(format = "  [:bar] :percent :elapsed", total = total, clear = FALSE, width = 60)

# do parallel
t1 <- Sys.time()
cat(">>> Doing parallel computing with ", n_cores, " cores.\n")
# Modify the foreach loop to update the progress bar
res <- foreach(x = seq_along(dfs_sentence),
               .combine = rbind,
               .export = c("NAICS2_CODE", "df_final_key", "colocation_complementary_keywords", 
                            "colocation_independent_keywords", "colocation_pressure_keywords"),
               .packages = c("dplyr", "purrr", "stringi", "readr", "tidytext")) %dopar% {
                 result <- countColocationWords(
                    splited_data = dfs_sentence[[x]],
                    sdg_keywords = df_final_key$word,
                    comp_keywords = colocation_complementary_keywords,
                    ind_keywords = colocation_independent_keywords,
                    press_keywords = colocation_pressure_keywords,
                    window_size = 10,
                    index = x,
                    total = length(dfs_sentence),
                    the_naics_code = NAICS2_CODE
                 )
                 result
               }
t2 <- Sys.time()
cat(">>> Time used: ", format(t2 - t1), "\n")


## Save files
# path_name <- paste0("./data/cleaned_data/df_wordCount_NAICS", NAICS2_CODE, ".rds")
path_name <- glue("{DROPBOX_PATH}/cleaned_data/df_colocation_CIP_wordcount_NAICS{NAICS2_CODE}.RDS")
res %>% write_rds(path_name)

parallel::stopCluster(cl)

