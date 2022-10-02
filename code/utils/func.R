# IO of annual reports

getRankCodeMap <- function(EXCEL_PATH = "./data/raw_data/TM Final_FortuneG500 (2021)_v2.xlsx"){
  ## Load company Rank and NAICS code mapping
  df.RankCode <- readxl::read_excel(EXCEL_PATH, 
                                    sheet = "Fortune Global 500 2021") %>% 
    select(rank = Rank, 
           name = Name, 
           sic = `SIC Code`, 
           naics = `NAICS Code & Description (Eikon)`) %>% 
    mutate(naics2 = floor(naics/100))
  
  return(df.RankCode)
}

readReports <- function(NAICS2_CODE) {
  
  # read in files
  txt_files <- fs::dir_ls("./data/raw_data/Fortune_500_report/", 
                          # recurse means 遞歸, 會進入一層層子資料夾內取得所有檔案的路徑
                          recurse = TRUE, regexp = "\\.txt")
  # make the above vector of paths as a dataframe
  df.txt <- bind_cols(
    path = txt_files) %>% 
    mutate(rank = str_replace(path, "./data/raw_data/Fortune_500_report/", "")) %>% 
    mutate(rank = str_extract(rank, pattern = "\\d+"))
  
  # Select txt files based on NAICS code
  df.RankCode <- getRankCodeMap()
  
  # TO SELECT NAICS STARTING WITH THE CODE YOU INPUT
  TARGET_ROWS <- df.RankCode %>% 
    filter(naics2 == NAICS2_CODE) %>% 
    pull(rank) %>% 
    unique() %>% 
    sort()
  txt.toRead <- df.txt %>% 
    filter(rank %in% TARGET_ROWS) %>% 
    pull(path)
  
  ## Create a new df to store the path of the file to read and the contents
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
  
  return(df_doc)
}
