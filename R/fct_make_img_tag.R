#' make_biodiversity_img_tag 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
make_biodiversity_img_tag <- function(ecoregion, taxon, metric, result_type, year, ns) {
  
  # eco_acronym <- get_ecoregion_acronym(ecoregion)
  eco_acronym <- "NrS"
  metric_name <- str_replace_all(tolower(metric), " ", "_")
  file_name <- paste0(paste(eco_acronym, taxon, metric_name, result_type, year, sep = "_"), ".png")
  
  # Web path used by img tag
  webpath <- file.path("www/wp3", file_name)
  
  # Filesystem path used for existence check
  file_systempath <- app_sys("app/www/wp3", file_name)
  
  validate(
    need(file.exists(file_systempath),
         paste("No data available for",
               metric,
               "in the", ecoregion(), "ecoregion"))
  )
  
  tags$img(
    id = ns(paste0(paste(eco_acronym, taxon, metric_name, result_type, year, sep = "_"))),
    src = webpath,
    style = "width: 100%; cursor: pointer;",
    onclick = "toggleFullScreen(this)"
  )
}


make_img_tag <- function(filename, ns) {
  
  # Web path used by img tag
  webpath <- file.path("www/wp3", filename)
  
  # Filesystem path used for existence check
  file_systempath <- app_sys("app/www/wp3", filename)
  
  validate(
    need(file.exists(file_systempath),
         "File not available")
  )
  
  tags$img(
    id = ns(filename),
    src = webpath,
    style = "width: 100%; cursor: pointer;",
    onclick = "toggleFullScreen(this)"
  )
}