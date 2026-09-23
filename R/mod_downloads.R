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
              tags$a(
                href ="https://github.com/ices-tools-dev/b_useful/releases/download/untagged-96b66ec53181c428bf04/b_useful_data_bundle_ce_med.zip",
                "Download data bundle for Central and Eastern Mediterranean")
              ),
    nav_panel(title = "Iceland Model", 
              HTML(select_text(project_texts,
                               "downloads",
                               "iceland")),
              tags$a(
                href ="https://github.com/ices-tools-dev/b_useful/releases/download/untagged-4f76433c762a365f53b3/b_useful_data_bundle_iceland.zip",
                "Download data bundle for Iceland")
              ),
                
    nav_panel(title = "North East Atlantic Model", 
              HTML(select_text(project_texts,
                               "downloads",
                               "ne_atlantic")),
              tags$a(
                href ="https://github.com/ices-tools-dev/b_useful/releases/download/untagged-9e0655bfc1963c9e209c/b_useful_data_bundle_nea.zip",
                "Download data bundle for North East Atlantic")
              ),
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
