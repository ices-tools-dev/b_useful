## code to prepare `project_texts` dataset goes here. 

project_texts <- icesUtils::prepare_text_from_excel(path_to_file =  "data-raw/project_texts.xlsx")

usethis::use_data(project_texts, overwrite = TRUE)

