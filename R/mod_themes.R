#' themes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_themes_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    card(
      card_header("Biodiversity", class = "bg-primary"),
      card_body(
        layout_column_wrap(
            width = NULL, fill = FALSE,
            style = css(grid_template_columns = "4fr 1fr"),
          heights_equal = "row",
          uiOutput(ns("biodiversity")),
          card_image(
            file = NULL, src = "www/socioeco.png",
            height = "200px",
            border_radius = "all",
            href = "https://ices-taf.shinyapps.io/seawise/")))),
    
    card(
      card_header("Risk", class = "bg-secondary"),
      card_body(
        layout_column_wrap(
          width = NULL, fill = FALSE,
          style = css(grid_template_columns = "1fr 4fr"),
          heights_equal = "row",
          card_image(
            file = NULL, src = "www/eco.png",
            height = "200px",
            border_radius = "all",
            href = "https://ices-taf.shinyapps.io/seawise/"),
          uiOutput(ns("risk"))))),
    
    card(
      card_header("Ecosystem Services", class = "bg-success"),
      card_body(
        layout_column_wrap(
          width = NULL, fill = FALSE,
          style = css(grid_template_columns = "4fr 1fr"),
          uiOutput(ns("ecosystem_services")),
          card_image(
            file = NULL, src = "www/fishery.png",
            height = "200px",
            border_radius = "all",
            href = "https://ices-taf.shinyapps.io/seawise/")))
      )
  )
}
    
#' themes Server Functions
#'
#' @noRd 
mod_themes_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    output$socioeconomic_effects <- renderUI({
      HTML(select_text(texts, "themes", "socioeconomic_effects"))
    })
    
    output$eco_effects <- renderUI({
      HTML(select_text(texts, "themes", "eco_effects"))
    })
    
    output$fishery_effects <- renderUI({
      HTML(select_text(texts, "themes", "fishery_effects"))
    })
    
    output$spatial_management <- renderUI({
      HTML(select_text(texts, "themes", "spatial_management"))
    })
    
    output$mse <- renderUI({
      HTML(select_text(texts, "themes", "mse"))
    })
    
  })
}
    
## To be copied in the UI
# mod_themes_ui("themes_1")
    
## To be copied in the server
# mod_themes_server("themes_1")
