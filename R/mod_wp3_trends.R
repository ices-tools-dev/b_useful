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
mod_wp3_trends_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(layout_sidebar(sidebar = sidebar(sliderTextInput(inputId = ns("year_slider"),label = "Year", choices = as.character(2010:2020), selected = "2010",
                                                  animate = animationOptions(interval = 3000, loop = TRUE)),
                                          radioButtons(ns("diversity_idx"), label = "Select diversity index", 
                                                       choices = c("Species Richness" = "Richness",
                                                                   "Evenness" = "evenness",
                                                                   "Shannon Index" = "shannon",
                                                                   "Functional Richness" = "fric",
                                                                   "Functional Evenness" = "feve",
                                                                   "Functional Dispersion" = "fdis",
                                                                   "Functional Diversity" = "fdiv"))),
         plotOutput(outputId = ns("biodiv_animation"), height = "75vh")))
 
  )
}
    
#' wp3_trends Server Functions
#'
#' @noRd 
mod_wp3_trends_server <- function(id, map_parameters){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    animation_data <- reactive({
      req(fish_diversity)
      req(input$year_slider)
      dat <- fish_diversity %>% 
        filter(Year %in% input$year_slider)
    })
    
    col_limits <- reactive({
      req(fish_diversity)
      req(input$diversity_idx)
      
      dat <- fish_diversity %>% 
        pull(!!rlang::sym(input$diversity_idx))
      range(dat)
      
    })
    
    output$biodiv_animation <- renderPlot({
      req(animation_data())
      req(map_parameters())
      
      p <- ggplot() +
        geom_point(data = animation_data(), aes(x = longitude, y = latitude, color = !!rlang::sym(input$diversity_idx)), size = 2) +
        scale_color_gradientn(colours = rev(brewer.pal(11, "RdYlBu")), 
                              limits = col_limits())+
        geom_sf(data = map_shape, fill = "grey")+
        scale_x_continuous(breaks= map_parameters()$coordxmap)+
        scale_y_continuous(breaks= map_parameters()$coordymap,expand=c(0,0))+
        coord_sf(xlim=c(map_parameters()$coordslim[1], map_parameters()$coordslim[2]), ylim=c(map_parameters()$coordslim[3],map_parameters()$coordslim[4]))+
        ylab("Latitude")+
        xlab("Longitude") +
        facet_wrap(~Year)
      p
    })
  })
}
