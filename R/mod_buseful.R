#' buseful UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_buseful_ui <- function(id){
  ns <- NS(id)
  tagList(
   
    layout_column_wrap(heights_equal = "row", width = 1/2, fixed_width = FALSE, fillable = TRUE,
      card(
        card_header("The B-USEFUL Objective", class = "bg-primary"),
                  uiOutput(ns("objective"))),
      card(
        card_header("Research Themes", class = "bg-primary"),
        uiOutput(ns("themes")))
      ),
    layout_column_wrap(heights_equal = "row", width = 1/3, fixed_width = FALSE, fillable = TRUE,
      card(
        card_header("Case Studies", class = "bg-primary"),
        uiOutput(ns("case_study_regions"))),
      card(
        card_header("Partners", class = "bg-primary"),uiOutput(ns("who")),
                  card_image(file = NULL, src = "img/normal-reproduction-high-resolution.jpg", height = "50px", width = "75px", border_radius = "all")),
      card(
        card_header("The B-USEFUL Website", class = "bg-primary"),uiOutput(ns("website"))),
      )
  )
}
    
#' buseful Server Functions
#'
#' @noRd 
mod_buseful_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$objective <- renderUI({
      HTML(select_text(texts, "landing_page", "objective"))
    })
    
    output$case_study_regions <- renderUI({
      HTML(select_text(texts, "landing_page", "case_study_regions"))
    })
    
    output$themes <- renderUI({
      HTML(select_text(texts, "landing_page", "themes"))
    })
    
    output$who <- renderUI({
      HTML(select_text(texts, "landing_page", "who"))
    })
    
    output$website <- renderUI({
      HTML(select_text(texts, "landing_page", "website"))
    })
    
  })
}
    
## To be copied in the UI
# mod_buseful_ui("buseful_1")
    
## To be copied in the server
# mod_buseful_server("buseful_1")
