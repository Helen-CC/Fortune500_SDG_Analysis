library(dotenv)
library(glue)

DROPBOX_PATH <- Sys.getenv("DATA_FOLDER")
PROJECT_PATH <- Sys.getenv("PROJECT_FOLDER")

# Make sure the working directory is exactly the same as project path
if (PROJECT_PATH != getwd()) {
  setwd(PROJECT_PATH)
}
