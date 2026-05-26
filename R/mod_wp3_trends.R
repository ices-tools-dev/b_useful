#' wp3_trends UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom bslib layout_sidebar sidebar 
#' @importFrom dplyr pull
#' @importFrom shinycssloaders withSpinner
mod_wp3_trends_ui <- function(id) {
  ns <- NS(id)
  tagList(card_body(padding = 20, 
                    min_height = "50vh",
                    withSpinner(plotOutput(outputId = ns("biodiv_trends"))),
                    card("Figure Information", 
                         uiOutput(ns("fig_text_trend")),
                         height = "20vh"))
  )
}
    
#' wp3_trends Server Functions
#'
#' @noRd 
mod_wp3_trends_server <- function(id, map_parameters, case_study, trends_data, taxon, diversity_idx){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$biodiv_trends <- renderPlot({
      
      req(trends_data())
      req(map_parameters())
      req(diversity_idx())
      var <- paste0(diversity_idx(), "_trend")
  
      p <- ggplot() +
        geom_point(data = trends_data(), aes(x = longitude, y = latitude, color = .data[[var]]), size = 2) +
        scale_color_gradientn(colours = rev(brewer.pal(11, "RdYlBu")))+
        geom_sf(data = map_shape, fill = "grey")+
        scale_x_continuous(breaks= map_parameters()$coordxmap)+
        scale_y_continuous(breaks= map_parameters()$coordymap,expand=c(0,0))+
        coord_sf(xlim=c(map_parameters()$coordslim[1], map_parameters()$coordslim[2]), ylim=c(map_parameters()$coordslim[3],map_parameters()$coordslim[4]))+
        ylab("Latitude")+
        xlab("Longitude")
      p
    })
  })
}
