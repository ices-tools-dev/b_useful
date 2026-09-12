#' downloads UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom bslib navset_pill_list
mod_downloads_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(card_header("Download B-USEFUL Data", class = "bg-primary"),
    HTML(select_text(project_texts,
                     "downloads",
                     "introduction")),
    navset_pill_list(
    nav_panel(title = "Central and Eastern Mediterranean Model", 
              HTML(select_text(project_texts,
                              "downloads",
                              "ce_med")),
                "Download data bundle for Central and Eastern Mediterranean"),
              # tags$a(
              #   href ="",
              #   "Download data bundle for Central and Eastern Mediterranean")
              # ),
    nav_panel(title = "Iceland Model", 
              HTML(select_text(project_texts,
                               "downloads",
                               "iceland")),
              # tags$a(
              #   href ="",
              #   "Download data bundle for Iceland")
              # ),
                "Download data bundle for Iceland"),
    nav_panel(title = "North East Atlantic Model", 
              HTML(select_text(project_texts,
                               "downloads",
                               "ne_atlantic")),
              # tags$a(
              #   href ="",
              #   "Download data bundle for North East Atlantic")
              # ),
                "Download data bundle for North East Atlantic"),
    nav_panel(title = "Western Mediterranean Model", 
              HTML(select_text(project_texts,
                               "downloads",
                               "wmed")),
              tags$a(
                href = "https://github.com/ices-tools-dev/b_useful/releases/download/untagged-bfc4b9a8334b035d26ab/b_useful_data_bundle_wmed.zip",
                "Download data bundle for Western Mediterranean")
              )
    )
  )
  )
}
    
#' downloads Server Functions
#'
#' @noRd 
mod_downloads_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
  })
}
    
## To be copied in the UI
# mod_downloads_ui("downloads_1")
    
## To be copied in the server
# mod_downloads_server("downloads_1")
