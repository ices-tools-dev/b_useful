#' results UI Function. This module provides the SEAwise results, loading and displaying the results of the work packages for the selected region.
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_results_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(column(width = 3,
                    titlePanel(title = textOutput(ns("region_title")))),
             column(width = 3)),
    tabsetPanel(
      tabPanel("Biodiversity development",
               mod_time_series_and_trends_ui(ns("time_series_and_trends_1"))),
      tabPanel("Interactive Comparison",
               mod_wp3_interactive_comparison_ui(ns("wp3_interactive_comparison_1"))),
      tabPanel("Prioritiser",
               mod_diversity_filters_ui(ns("diversity_filters_1"))),
      tabPanel("Interactive Tool",
               mod_interactive_tool_ui(ns("interactive_tool_1")))
    )
  )
}
    
#' results Server Functions
#'
#' @noRd 
mod_results_server <- function(id, case_study){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    display_region <- reactive ({
      switch(case_study(),
             "baltic_sea" = "Baltic Sea", 
             "barents_sea" = "Barents Sea", 
             "bay_of_biscay" = "Bay of Biscary", 
             "greater_north_sea" = "Greater North Sea", 
             "iceland" = "Iceland", 
             "ne_atlantic" = "North East Atlantic", 
             "western_mediterranean_sea" = "Western Mediterranean Sea",
             "c_mediterranean" = "Central Mediterranean")
    })
    output$region_title <- renderText(display_region())
    

    diversity_data <- reactive({
      req(!is.null(case_study))
      switch(case_study(),
             "greater_north_sea" = readRDS("data/gns_fish_diversity.rds"), 
             "eastern_mediterranean" = readRDS("data/.rds"),
             "western_mediterranean_sea" = readRDS("data/wmed_demersal_diversity.rds"))
    })
    
    diversity_spatial <- reactive({
      req(!is.null(case_study))
      switch(case_study(),
             "greater_north_sea" = readRDS("data/gns_fish_div_spatial.rds"), 
             "eastern_mediterranean" = readRDS("data/.rds"),
             "western_mediterranean_sea" = readRDS("data/wmed_demersal_div_spatial.rds"))
    })
    
    trends_data <- reactive({
      req(!is.null(case_study))
      switch(case_study(),
             "greater_north_sea" = readRDS("data/gns_fish_diversity_trends.rds"), 
             "west_med" = readRDS("data.rds"),
             "western_mediterranean_sea" = readRDS("data/wmed_demersal_diversity_trends.rds"))
    })
    
    map_parameters <- reactive({
      req(diversity_data())
      dat <- diversity_data()
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
    
    taxon <- reactive({
      req(case_study())
      if(case_study() == "greater_north_sea"){
        taxon <- "fish"
      } else if(case_study() == "western_mediterranean_sea"){
        taxon <- "demersal"  
      }
    })
    
    mod_time_series_and_trends_server("time_series_and_trends_1",
                                      map_parameters = map_parameters, 
                                      case_study = case_study, 
                                      diversity_data = diversity_data, 
                                      trends_data = trends_data, 
                                      taxon = taxon)
    mod_wp3_interactive_comparison_server("wp3_interactive_comparison_1", map_parameters = map_parameters, case_study = case_study, diversity_data = diversity_data(), diversity_spatial = diversity_spatial, taxon=taxon)
    mod_diversity_filters_server("diversity_filters_1", map_parameters = map_parameters, case_study = case_study, diversity_data = diversity_data(), taxon=taxon)
    mod_interactive_tool_server("interactive_tool_1", map_parameters = map_parameters, case_study = case_study, diversity_data = diversity_data(), diversity_spatial = diversity_spatial, taxon=taxon)
  })
}