#' model_info UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_model_info_ui <- function(id, model) {
  ns <- NS(id)
  tagList(
  card(  tags$h2("Authors"),
         HTML(select_text(project_texts,
                          model,
                          "authors")),
         br(),
         tags$h2("Study Area and Data Availability"),
         HTML(select_text(project_texts,
                          model,
                          "area_and_data")),
         br(),
         tags$h2("Trait Selection"),
         HTML(select_text(project_texts,
                          model,
                          "traits")),
         br(),
         tags$h2("Environmental Variables"),
         HTML(select_text(project_texts,
                          model,
                          "environment")),
         br(),
         tags$h2("Model Setup"),
         HTML(select_text(project_texts,
                          model,
                          "model")),
         br(),
         tags$h2("Further Reading"),
         tags$a(
           href = "https://b-useful.eu/b-useful/uploads/D3.1_B-USEFUL_report.pdf",
           target = "_blank",
           "Download the full deliverable report")
  )
  )
}
    
#' model_info Server Functions
#'
#' @noRd 
mod_model_info_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
  })
}
    
## To be copied in the UI
# mod_model_info_ui("model_info_1")
    
## To be copied in the server
# mod_model_info_server("model_info_1")
