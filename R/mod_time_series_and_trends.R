#' time_series_and_trends UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinyWidgets prettySwitch
mod_time_series_and_trends_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(layout_sidebar(sidebar = sidebar(
      radioButtons(ns("content_toggle"), 
                   label = "", 
                   choices = c("View Animation" = "animation", "Explore Time Series" = "time_series"),
                   selected = "animation"),
      prettySwitch(ns("show_trend"), "Show diversity trend"),
      radioButtons(ns("diversity_idx"), label = "Select diversity index", 
                                                       choices = c("Species Richness" = "Richness",
                                                                   "Evenness" = "evenness",
                                                                   "Shannon Index" = "shannon",
                                                                   "Functional Richness" = "fric",
                                                                   "Functional Evenness" = "feve",
                                                                   "Functional Dispersion" = "fdis",
                                                                   "Functional Diversity" = "fdiv")),
      uiOutput(ns("year_selector"))),
      uiOutput(ns("content"))
                      )
    )
  )
}
    
#' time_series_and_trends Server Functions
#'
#' @noRd 
mod_time_series_and_trends_server <- function(id, map_parameters, case_study, diversity_data, trends_data, taxon){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    output$year_selector <- renderUI({
      req(diversity_data())
      req(input$content_toggle)
      if(input$content_toggle == "time_series") {
        yr_summary <- summary(diversity_data()$Year)
        selectizeInput(inputId = ns("year_choices"), "Select Year(s)",
                       multiple = TRUE,
                       choices = yr_summary[1]:yr_summary[6],
                       selected = c(yr_summary[1], yr_summary[3], yr_summary[6]))
      }
    })
    
    output$content <- renderUI({
      req(input$content_toggle)
      req(input$diversity_idx)
      tagList(
        if(input$show_trend) {
          layout_column_wrap(width = "450px", height = 500,
                             if(input$content_toggle == "animation") {
                               card(
                                 mod_diversity_animation_ui(ns("diversity_animation_1"))
                               )} else if (input$content_toggle == "time_series") {
                                 card(
                                   mod_wp3_time_comparison_ui(ns("wp3_time_comparison_1"))
                                 )   
                               },
                             card(
                               mod_wp3_trends_ui(ns("trends_1"))
                             )
          )
        } else {
          if(input$content_toggle == "animation") {
            card(
              mod_diversity_animation_ui(ns("diversity_animation_1"))
            )} else if (input$content_toggle == "time_series") {
              card(
                mod_wp3_time_comparison_ui(ns("wp3_time_comparison_1"))
              )   
            }
        }
      )
    })
    
  mod_diversity_animation_server("diversity_animation_1", case_study = case_study, diversity_idx = reactive(input$diversity_idx), taxon = taxon)
  mod_wp3_time_comparison_server("wp3_time_comparison_1", map_parameters = map_parameters, case_study = case_study, diversity_data = diversity_data(), selected_years = reactive(input$year_choices), diversity_idx = reactive(input$diversity_idx))
  mod_wp3_trends_server("trends_1", map_parameters = map_parameters, case_study = case_study, trends_data = trends_data, taxon=taxon, diversity_idx = reactive(input$diversity_idx))
  
  })
}
    
## To be copied in the UI
# mod_time_series_and_trends_ui("time_series_and_trends_1")
    
## To be copied in the server
# mod_time_series_and_trends_server("time_series_and_trends_1")
