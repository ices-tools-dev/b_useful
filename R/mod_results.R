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
     tabPanel("Interactive Tool",
               mod_interactive_tool_ui(ns("interactive_tool_1"))),
     tabPanel("Species Distributions",
               mod_species_distributions_ui(ns("species_distributions_1")))
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
             "north_east_atlantic" = "North East Atlantic", 
             "western_mediterranean_sea" = "Western Mediterranean Sea",
             "central-eastern_mediterranean_sea" = "Central-Eastern Mediterranean Sea")
    })
    output$region_title <- renderText(display_region())
    

    diversity_data <- reactive({
      req(!is.null(case_study))
      switch(case_study(),
             "greater_north_sea" = readRDS("data/gns_fish_diversity.rds"), 
             "central-eastern_mediterranean_sea" = readRDS("data/emed_demersal_diversity.rds"),
             "western_mediterranean_sea" = readRDS("data/wmed_demersal_diversity.rds"),
             "north_east_atlantic" = readRDS("data/nea_fish_diversity.rds"))
    })
    
    diversity_spatial <- reactive({
      req(!is.null(case_study))
      switch(case_study(),
             "greater_north_sea" = readRDS("data/gns_fish_div_spatial.rds"), 
             "central-eastern_mediterranean_sea" = readRDS("data/emed_demersal_div_spatial.rds"),
             "western_mediterranean_sea" = readRDS("data/wmed_demersal_div_spatial.rds"),
             "north_east_atlantic" = readRDS("data/nea_fish_div_spatial.rds"))
    })
    
    trends_data <- reactive({
      req(!is.null(case_study))
      switch(case_study(),
             "greater_north_sea" = readRDS("data/gns_fish_diversity_trends.rds"), 
             "central-eastern_mediterranean_sea" = readRDS("data/emed_demersal_diversity_trends.rds"),
             "western_mediterranean_sea" = readRDS("data/wmed_demersal_diversity_trends.rds"),
             "north_east_atlantic" = readRDS("data/nea_fish_diversity_trends.rds"))
    })
    
    pa_data <- reactive({
      req(!is.null(case_study))
      switch(case_study(),
             "greater_north_sea" = readRDS("data/gns_species_p_occurrence.rds"), 
             "central-eastern_mediterranean_sea" = readRDS("data/emed_species_p_occurrence.rds"),
             "western_mediterranean_sea" = readRDS("data/wmed_species_p_occurrence.rds"),
             "north_east_atlantic" = readRDS("data/nea_species_p_occurrence.rds"))
    })
    
    biomass_abundance_data <- reactive({
      req(!is.null(case_study))
      switch(case_study(),
             "greater_north_sea" = readRDS("data/gns_species_abundance.rds"), 
             "central-eastern_mediterranean_sea" = readRDS("data/emed_species_abundance.rds"),
             "western_mediterranean_sea" = readRDS("data/wmed_species_abundance.rds"),
             "north_east_atlantic" = readRDS("data/nea_species_abundance.rds"))
    })
    
    model_diagnostics <- reactive({
      req(!is.null(case_study))
      switch(case_study(),
             "greater_north_sea" = readRDS("data/_diagnostics.rds"), 
             "central-eastern_mediterranean_sea" = readRDS("data/emed_demersal_diagnostics.rds"),
             "western_mediterranean_sea" = readRDS("data/_diagnostics.rds"),
             "north_east_atlantic" = readRDS("data/_diagnostics.rds"))
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
      if(case_study() %in% c("greater_north_sea", "north_east_atlantic")){
        taxon <- "fish"
      } else if(case_study() %in% c("central-eastern_mediterranean_sea", "western_mediterranean_sea")){
        taxon <- "demersal"  
      }
    })
    
    mod_time_series_and_trends_server("time_series_and_trends_1",
                                      map_parameters = map_parameters, 
                                      case_study = case_study, 
                                      diversity_data = diversity_data, 
                                      trends_data = trends_data, 
                                      taxon = taxon)
    
    mod_diversity_filters_server("diversity_filters_1", map_parameters = map_parameters, case_study = case_study, diversity_data = diversity_data, taxon=taxon)
    mod_interactive_tool_server("interactive_tool_1", map_parameters = map_parameters, case_study = case_study, diversity_data = diversity_data, diversity_spatial = diversity_spatial, taxon=taxon)
    mod_species_distributions_server("species_distributions_1", map_parameters = map_parameters, case_study = case_study, pa_data = pa_data, biomass_abundance_data = biomass_abundance_data, model_diagnostics = model_diagnostics)
  })
}