#' wp3_time_comparison UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom rlang sym
#' @importFrom shinycssloaders withSpinner
#' @importFrom stringr str_replace_all str_to_title
#' @importFrom bslib navset_card_tab nav_panel
mod_wp3_time_comparison_ui <- function(id) {
  ns <- NS(id)
  tagList(
      card_body(padding = 0,
                min_height = "50vh",
                withSpinner(plotOutput(ns("div_plot"), 
                           height = "70vh"))),
      card("Figure Information", uiOutput(ns("fig_text")), height = "20vh")
  )
}
    
#' wp3_time_comparison Server Functions
#'
#' @noRd 
mod_wp3_time_comparison_server <- function(id, map_parameters, case_study, diversity_data, diversity_idx, selected_years){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    reactive_data <- reactive({
      req(diversity_data)
      req(selected_years())
      dat <- diversity_data %>% 
        filter(Year %in% selected_years())
    })

    output$div_plot <- renderPlot({
      req(reactive_data())
      req(diversity_idx())
      req(map_parameters())
      
      p <- ggplot() +
        geom_point(data = reactive_data(), aes(x = longitude, y = latitude, color = !!sym(diversity_idx())), size = 2) +
        scale_color_gradientn(colours = rev(brewer.pal(11, "RdYlBu")))+
        geom_sf(data = map_shape, fill = "grey")+
        scale_x_continuous(breaks= map_parameters()$coordxmap)+
        scale_y_continuous(breaks= map_parameters()$coordymap,expand=c(0,0))+
        coord_sf(xlim=c(map_parameters()$coordslim[1], map_parameters()$coordslim[2]), ylim=c(map_parameters()$coordslim[3],map_parameters()$coordslim[4]))+
        ylab("Latitude")+
        xlab("Longitude")
      
      if (length(selected_years()>1)){
        p <- p + facet_wrap(~Year)
      } 
      p
    })
    
    output$fig_text <- renderText({
      diversity_indicator <- input$diversity_idx
      ecoregion <- str_to_title(str_replace_all(case_study(), pattern = "_", replacement = " "))
      if(case_study() == "greater_north_sea"){
        taxon <- "Fish"
      } else {
        taxon <- "Demersal"
      }
      fig_text <- select_text(project_texts, tab = "fig_text", section = "time_comparison")
      fig_text <- glue::glue(fig_text)
      HTML(fig_text)
    })
  })
}