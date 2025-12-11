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
    uiOutput(ns("dynamic_tabs"))
  )
}
    
#' results Server Functions
#'
#' @noRd 
mod_results_server <- function(id, case_study){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    output$dynamic_tabs <- renderUI({
      ns <- NS(id)  
      tabs <- list()
        tabs[[length(tabs) + 1]] <- tabPanel("biodiversity",
                                             #mod_wp3_ui(ns("wp3")),
                                             value = "wp3")
        tabs[[length(tabs) + 1]] <-  tabPanel("risk",
                                              #mod_wp4_ui(ns("wp4")),
                                              value = "wp4")
        tabs[[length(tabs) + 1]] <- tabPanel("ecosystem_services", 
                                             #mod_wp5_ui(ns("wp5")),
                                             value = "wp5")
      do.call(tabsetPanel, tabs)
    })
    
    
    display_region <- reactive ({
      switch(case_study(),
             "baltic_sea" = "Baltic Sea", 
             "barents_sea" = "Barents Sea", 
             "bay_of_biscay" = "Bay of Biscary", 
             "greater_north_sea" = "Greater North Sea", 
             "Iberian Peninsula" = "Iberian Peninsula", 
             "iceland" = "Iceland", 
             "ne_atlantic" = "North East Atlantic", 
             "w_mediterranean" = "Western Mediterranean",
             "c_mediterranean" = "Central Mediterranean")
    })
    output$region_title <- renderText(display_region())
    
    #mod_wp3_server("wp3", case_study)
    #mod_wp4_server("wp4", case_study)
    #mod_wp5_server("wp5", case_study)
  })
}
    
## To be copied in the UI
# mod_results_ui("results_baltic")
    
## To be copied in the server
# mod_results_server("results_baltic")
