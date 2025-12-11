#' case_studies UI Function. This module provides text overview of the case studies for each case-study region, together with the projects map of the area.
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_case_studies_ui <- function(id){
  ns <- NS(id)
  tagList(
      card(
        card_header("Mediterreanean", class = "bg-warning"),
        card_body(
          layout_column_wrap(
            width = NULL, fill = FALSE,
            style = css(grid_template_columns = "4fr 1fr"),
            heights_equal = "row",
            uiOutput(ns("med_case")),
            card_image(
              file = NULL, src = "www/med.png",
                          height = "200px",
              border_radius = "none")))),
      
      card(
        card_header("Baltic", class = "bg-secondary"),
        card_body(
          layout_column_wrap(
            width = NULL, fill = FALSE,
            style = css(grid_template_columns = "1fr 4fr"),
            heights_equal = "row",
            card_image(
              file = NULL, src = "www/bs.png",
                          height = "200px",
              border_radius = "none"),
            uiOutput(ns("baltic_case"))))),
      card(
        card_header("Barents Sea", class = "bg-secondary"),
        card_body(
          layout_column_wrap(
            width = NULL, fill = FALSE,
            style = css(grid_template_columns = "1fr 4fr"),
            heights_equal = "row",
            card_image(
              file = NULL, src = "www/bs.png",
                          height = "200px",
              border_radius = "none"),
            uiOutput(ns("baltic_case"))))),
      
      card(
        card_header("Greater North Sea", class = "bg-success"),
        card_body(
          layout_column_wrap(
            width = NULL, fill = FALSE,
            style = css(grid_template_columns = "4fr 1fr"),
            heights_equal = "row",
            uiOutput(ns("gns_case")),
            card_image(
              file = NULL, src = "www/gns.png",
                          height = "200px",
              border_radius = "none")))),
      
      card(
        card_header("Bay of Biscay", class = "bg-info"),
        card_body(
          layout_column_wrap(
            width = NULL, fill = FALSE,
            style = css(grid_template_columns = "1fr 4fr"),
            heights_equal = "row",
            card_image(
              file = NULL, src = "www/ww.png",
                          height = "200px",
              border_radius = "none"),
            uiOutput(ns("ww_case"))))),
      card(
        card_header("Iberian Waters", class = "bg-info"),
        card_body(
          layout_column_wrap(
            width = NULL, fill = FALSE,
            style = css(grid_template_columns = "1fr 4fr"),
            heights_equal = "row",
            card_image(
              file = NULL, src = "www/ww.png",
                          height = "200px",
              border_radius = "none"),
            uiOutput(ns("ww_case")))))
  
)
}
    
#' case_studies Server Functions
#'
#' @noRd 
mod_case_studies_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    output$med_case <- renderUI({
      HTML(select_text(texts, "case_studies", "med_case"))
    })
    
    output$baltic_case <- renderUI({
      HTML(select_text(texts, "case_studies", "baltic_case"))
    })
    
    output$gns_case <- renderUI({
      HTML(select_text(texts, "case_studies", "gns_case"))
    })
    
    output$ww_case <- renderUI({
      HTML(select_text(texts, "case_studies", "ww_case"))
    })
    
  })
}
    
## To be copied in the UI
# mod_case_studies_ui("case_studies_1")
    
## To be copied in the server
# mod_case_studies_server("case_studies_1")
