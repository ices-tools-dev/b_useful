#' wp3 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import ggplot2
#' @importFrom shiny NS tagList 
#' @importFrom dplyr filter mutate where across
#' @importFrom bslib card layout_sidebar sidebar
#' @importFrom RColorBrewer brewer.pal
#' @importFrom shinyWidgets sliderTextInput
mod_wp3_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Biodiversity",
               mod_wp3_time_comparison_ui(ns("wp3_time_comparison_1"))),
      tabPanel("Biodiversity trends",
               mod_wp3_trends_ui(ns("wp3_trends_1"))),
      tabPanel("Interactive Comparison",
               mod_wp3_interactive_comparison_ui(ns("wp3_interactive_comparison_1")))
      )
  )
}
    
#' wp3 Server Functions
#'
#' @noRd 
mod_wp3_server <- function(id, case_study){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    map_parameters <- reactive({
      req(fish_diversity)
      dat <- fish_diversity
      minlong <- min(dat$longitude) 
      maxlong <- max(dat$longitude)
      long_range <- maxlong-minlong
      minlong <- minlong -0.05*long_range
      maxlong <- maxlong +0.05*long_range
      
      minlat <- min(dat$latitude)
      maxlat <- max(dat$latitude)
      lat_range <- maxlat-minlat
      minlat <- minlat -0.05*lat_range
      maxlat <- maxlat +0.05*lat_range
      
      coordslim <- c(minlong,maxlong,minlat,maxlat)
      coordxmap <- round(seq(minlong,maxlong,length.out = 5))
      coordymap <- round(seq(minlat,maxlat,length.out = 5))
      map_parameters <- list(coordslim = coordslim,
                             coordxmap = coordxmap,
                             coordymap = coordymap)
    })
    
    
    mod_wp3_time_comparison_server("wp3_time_comparison_1", map_parameters = map_parameters)
    mod_wp3_trends_server("wp3_trends_1", map_parameters = map_parameters)
    mod_wp3_interactive_comparison_server("wp3_interactive_comparison_1", map_parameters = map_parameters)
     
  })
}