#' wp3_interactive_comparison UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList brushedPoints
#' @importFrom DT DTOutput renderDT datatable
#' @importFrom dplyr filter mutate where across
#' @importFrom bslib card layout_sidebar sidebar
mod_wp3_interactive_comparison_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(layout_sidebar(sidebar = sidebar(selectInput(ns("year_selector"), "Select Year", choices = 2010:2020, selected = 2020)),
                        plotOutput(outputId = ns("biodiv_compare"), 
                                                 brush = ns("plot_brush"),
                                                 dblclick = ns("plot_reset"), 
                                                 height = "75vh"))),
    card(DTOutput(outputId = ns("comparison_table")))
  )
}
    
#' wp3_interactive_comparison Server Functions
#'
#' @noRd 
mod_wp3_interactive_comparison_server <- function(id, map_parameters){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    selected_points <- reactiveVal(rep(FALSE, nrow(fish_diversity)))
    
    observeEvent(input$plot_brush, {
      brushed <- brushedPoints(fish_diversity, input$plot_brush, allRows = TRUE)$selected_
      selected_points(brushed | selected_points())
    })
    observeEvent(input$plot_reset, {
      selected_points(rep(FALSE, nrow(fish_diversity)))
    })
    
    output$biodiv_compare <- renderPlot({
      req(fish_diversity)
      req(map_parameters())
      
      dat <- fish_diversity 
      dat$Selected <- selected_points()
      dat <- dat %>%
        filter(Year == input$year_selector)
      
      p <- ggplot() +
        geom_point(data = dat, aes(x = longitude, y = latitude, col = Selected), size = 2) +
        geom_sf(data = map_shape, fill = "grey")+
        scale_x_continuous(breaks= map_parameters()$coordxmap)+
        scale_y_continuous(breaks= map_parameters()$coordymap,expand=c(0,0))+
        coord_sf(xlim=c(map_parameters()$coordslim[1], map_parameters()$coordslim[2]), ylim=c(map_parameters()$coordslim[3],map_parameters()$coordslim[4]))+
        ylab("Latitude")+
        xlab("Longitude")
      p
    })
    
    
    output$comparison_table <- renderDT({
      req(fish_diversity)
      dat <- fish_diversity %>%
        filter(Year == input$year_selector) %>% 
        mutate(across(where(is.double) & !c(longitude, latitude, Lon_c, Lat_c), ~round(.x, digits = 1))) %>% 
        mutate(across(c(longitude, latitude, Lon_c, Lat_c), ~round(.x, digits = 4)))
      res <- dat[selected_points(),]
      datatable(res)
    })
    
    
  })
}