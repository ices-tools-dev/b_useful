#' decision_support UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom dplyr filter
#' @importFrom DT renderDT
mod_decision_support_ui <- function(id){
  ns <- NS(id)
  tagList(
    card("Histograms", 
         radioButtons(ns("ecoregion_hist_log"), label = "Apply Log scale", choices = c("N", "Y"), inline = T),
         plotOutput(ns("ecoregion_hist")),
         plotOutput(ns("eez_hist"))
         )
  )
}
    
#' decision_support Server Functions
#'
#' @noRd 
mod_decision_support_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    output$data_table <- renderDT({})
    
    output$ecoregion_hist <- renderPlot({
      
      name <- "User defined polygon"
      fish_div_spatial$group <- "Ecoregion"
      
      subset_data <- fish_div_spatial %>% filter(Ecoregion.y == "Norwegian Sea")
      
      subset_data$group <- name
      plot_data <- bind_rows(fish_div_spatial, subset_data)
      plot <- ggplot(plot_data, aes(x= Richness, fill = group))+
        geom_histogram(position = "identity", alpha = 0.5, stat = "count", bins = 30) +
        labs(title ="",) 
      if(input$ecoregion_hist_log == "Y"){
        plot <- plot + scale_y_log10()
      } 
        plot
    })
    output$eez_hist <- renderPlot({
    })
  })
}
    
## To be copied in the UI
# mod_decision_support_ui("decision_support_1")
    
## To be copied in the server
# mod_decision_support_server("decision_support_1")
