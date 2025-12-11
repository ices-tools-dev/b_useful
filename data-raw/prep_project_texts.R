## code to prepare `landing_texts` dataset goes here. 
# These are the texts that the user sees on the homepage and in the 'About' section of the app.
library(readxl)
library(dplyr)
library(purrr)

path <- "data-raw/project_texts.xlsx"
sheets <- excel_sheets(path)
project_texts <- lapply(sheets, read_xlsx, path = path) 
names(project_texts) <- sheets
# texts <- map(texts, ~ mutate(., text = paste0("<p>", text, "</p>")))

usethis::use_data(project_texts, overwrite = TRUE)

