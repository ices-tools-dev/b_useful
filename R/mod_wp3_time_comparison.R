#' wp3_time_comparison UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_wp3_time_comparison_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(height = "75vh",
         layout_sidebar(sidebar = 
                          sidebar(radioButtons(ns("diversity_idx"), label = "Select diversity index", choices = c("Species Richness" = "Richness", "Evenness" = "evenness", "Shannon Index" = "shannon", "Functional Richness" = "fric", "Functional Evenness" = "feve", "Functional Dispersion" = "fdis", "Functional Diversity" = "fdiv")),
                                  selectizeInput(inputId = ns("year_selector"), "Select Year(s)",
                                                 multiple = TRUE,
                                                 choices = 2010:2020,
                                                 selected = c("2010", "2015", "2020")),
                                  sliderInput(ns("point_size"), label = "Adjust point size in plot", min = 0.75, max = 2.5, value = 1.5, round = -1)),
                        card_body(padding = 0,
                                  verbatimTextOutput(ns("dimension_display")),
                                  plotOutput(ns("div_plot")))))
  )
}
    
#' wp3_time_comparison Server Functions
#'
#' @noRd 
mod_wp3_time_comparison_server <- function(id, map_parameters, case_study){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    reactive_data <- reactive({
      req(fish_diversity)
      req(input$year_selector)
      dat <- fish_diversity %>% 
        filter(Year %in% input$year_selector)
    })
    
    
    output$div_plot <- renderPlot({
      req(reactive_data())
      req(input$diversity_idx)
      req(input$point_size)
      req(map_parameters())
      
      p <- ggplot() +
        geom_point(data = reactive_data(), aes(x = longitude, y = latitude, color = !!rlang::sym(input$diversity_idx)), size = input$point_size) +
        scale_color_gradientn(colours = rev(brewer.pal(11, "RdYlBu")))+
        geom_sf(data = map_shape, fill = "grey")+
        scale_x_continuous(breaks= map_parameters()$coordxmap)+
        scale_y_continuous(breaks= map_parameters()$coordymap,expand=c(0,0))+
        coord_sf(xlim=c(map_parameters()$coordslim[1], map_parameters()$coordslim[2]), ylim=c(map_parameters()$coordslim[3],map_parameters()$coordslim[4]))+
        ylab("Latitude")+
        xlab("Longitude")
      
      if (length(input$year_selector>1)){
        p <- p + facet_wrap(~Year)
      } 
      p
    })
  })
}