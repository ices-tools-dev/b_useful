#' interactive_tool UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom bslib accordion accordion_panel
mod_interactive_tool_ui <- function(id) {
  ns <- NS(id)
  tagList(
    accordion(multiple = TRUE, open = TRUE,
      accordion_panel("Biodiversity filters",
                      mod_diversity_filters_ui(ns("diversity_filters_1"))),
      accordion_panel("Spatial filters",
                      mod_spatial_filters_ui(ns("spatial_filters_1"))),
      accordion_panel("Spatial Statistics",
                      coming_soon(card(card_body("This feature will provide an overview of biodiversity in the area resulting from user-defined filters")))),
      accordion_panel("Human Activities overlay",
                      coming_soon(card(card_body("This feature will allow the user to overlay selected human activities onto the user-defined area")))),
    )
  )
}
    
#' interactive_tool Server Functions
#'
#' @noRd 
mod_interactive_tool_server <- function(id, map_parameters, case_study, diversity_data, diversity_spatial, taxon){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    mod_diversity_filters_server("diversity_filters_1", map_parameters = map_parameters, case_study = case_study, diversity_data = diversity_data, taxon = taxon)
    mod_spatial_filters_server("spatial_filters_1", map_parameters = map_parameters, case_study = case_study, diversity_data = diversity_data, diversity_spatial = diversity_spatial, taxon = taxon)
  })
}
    
## To be copied in the UI
# mod_interactive_tool_ui("interactive_tool_1")
    
## To be copied in the server
# mod_interactive_tool_server("interactive_tool_1")
