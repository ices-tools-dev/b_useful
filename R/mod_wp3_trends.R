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
  tagList(
    card(layout_sidebar(sidebar = sidebar(radioButtons(ns("diversity_idx"), label = "Select diversity index", 
                                                       choices = c("Species Richness" = "Richness",
                                                                   "Evenness" = "evenness",
                                                                   "Shannon Index" = "shannon",
                                                                   "Functional Richness" = "fric",
                                                                   "Functional Evenness" = "feve",
                                                                   "Functional Dispersion" = "fdis",
                                                                   "Functional Diversity" = "fdiv"))),
                        fluidRow(column(width = 6, card(height = "75vh",
                                                        card_header("Animation"), 
                                                        card_body(padding = 0,
                                                        withSpinner(uiOutput(outputId = ns("biodiv_animation"))))
                                                        )),
                                 column(width = 6, card(height = "75vh",
                                                        card_header("Trend 2010-2020"), 
                                                        card_body(padding = 20,
                                                        withSpinner(plotOutput(outputId = ns("biodiv_trends"), height = "70vh")))))
                                 )
                        )
    )
  )
}
    
#' wp3_trends Server Functions
#'
#' @noRd 
mod_wp3_trends_server <- function(id, map_parameters, case_study){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    col_limits <- reactive({
      req(fish_diversity)
      req(input$diversity_idx)
      
      dat <- fish_diversity %>% 
        pull(!!rlang::sym(input$diversity_idx))
      range(dat)
      
    })
   
    output$biodiv_animation <- renderUI({

      req(input$diversity_idx)
      
      eco_acronym <- "NrS"
      metric_name <- str_replace_all(tolower(input$diversity_idx), " ", "_")
      file_name <- paste0(paste(eco_acronym, "taxon", metric_name, "status", sep = "_"), ".gif")
      
      make_img_tag(filename = file_name,
                 ns = ns)
    }) %>% bindCache(input$diversity_idx)
    
    trend_data <- readRDS("data/fish_diversity_trends.rds")
    
    output$biodiv_trends <- renderPlot({
      
      req(trend_data)
      req(map_parameters())
      req(input$diversity_idx)
      var <- paste0(input$diversity_idx, "_trend")
  
      p <- ggplot() +
        geom_point(data = trend_data, aes(x = longitude, y = latitude, color = .data[[var]]), size = 2) +
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
