pkgLoad <- function( packages = "requirements" ) {
  
  if( length( packages ) == 1L && packages == "requirements" ) {
    # packages <- c( "data.table", "chron", "plyr", "dplyr", "shiny",
    #                "shinyjs", "parallel", "devtools", "doMC", "utils",
    #                "stats", "microbenchmark", "ggplot2", "readxl",
    #                "feather", "googlesheets", "readr", "DT", "knitr",
    #                "rmarkdown", "Rcpp")
    packages <- c( "tidyverse", "tidytext", "fs", "stringi", "readxl",
                   "hash", "viridis", "ggrepel")
  }
  
  packagecheck <- match( packages, utils::installed.packages()[,1] )
  
  packagestoinstall <- packages[ is.na( packagecheck ) ]
  
  if( length( packagestoinstall ) > 0L ) {
    utils::install.packages( packagestoinstall,
                             repos = "https://cloud.r-project.org"
    )
  } else {
    print( "All requested packages already installed" )
  }
  
  for( package in packages ) {
    suppressPackageStartupMessages(
      library( package, character.only = TRUE, quietly = TRUE )
    )
  }
  
}
pkgLoad()
