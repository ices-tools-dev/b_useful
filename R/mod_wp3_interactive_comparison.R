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
#' @importFrom dplyr filter mutate where across select summarise
#' @importFrom tidyr pivot_longer
#' @importFrom bslib card layout_sidebar sidebar layout_column_wrap card_body
mod_wp3_interactive_comparison_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    card(layout_sidebar(sidebar = sidebar(selectInput(ns("year_selector"), "Select Year", choices = 2010:2020, selected = 2020),
                                          checkboxGroupInput(ns("diversity_idx"), label = "Select diversity index", 
                                                       choices = c("Species Richness" = "Richness",
                                                                   "Evenness" = "evenness",
                                                                   "Shannon Index" = "shannon",
                                                                   "Functional Richness" = "fric",
                                                                   "Functional Evenness" = "feve",
                                                                   "Functional Dispersion" = "fdis",
                                                                   "Functional Diversity" = "fdiv"),
                                                       selected = c("Richness", "evenness", "shannon"))),
                        uiOutput(ns("plot_panel"), fill = TRUE))),
    card(fluidRow(column(width = 4, 
                         card(card_header("Drag to select, double-click to deselect"),
                              card_body(padding = 0, plotOutput(outputId = ns("biodiv_compare"), 
                                         brush = ns("plot_brush"),
                                         dblclick = ns("plot_reset"), 
                                         height = "35vh")))),
                  column(width = 8, 
                         card(card_header("Summary Statistics"),
                         DTOutput(ns("summary_dt")))))),
    card(DTOutput(outputId = ns("detail_dt")))
  )
}
    
#' wp3_interactive_comparison Server Functions
#'
#' @noRd 
mod_wp3_interactive_comparison_server <- function(id, map_parameters, case_study){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$plot_panel <- renderUI({
      
      cards <- list()
      
      for(i in seq_along(input$diversity_idx)) {
        cards[[i]] <- card(min_height = "25vh",
                           card_header(toupper(input$diversity_idx[i])),
                           card_body(padding = 0,
                                     make_biodiversity_img_tag(ecoregion = case_study, 
                                                     taxon = "taxon", 
                                                     metric = input$diversity_idx[i],
                                                     result_type = "status", 
                                                     year = input$year_selector,
                                                     ns = ns)))
      }
    
      do.call(layout_column_wrap, c(cards, list(width = "400px")))
    })
    
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
    
    output$summary_dt <- renderDT({
      req(fish_diversity)
      req(input$year_selector)
      req(sum(selected_points())>0)
      
      dat <- fish_diversity[selected_points(),]
      
      dat %>%
        select(all_of(input$diversity_idx)) %>%
        summarise(across(
          everything(),
          list(
            Min = ~round(min(.x, na.rm = TRUE), digits = 2),
            "25th Percentile" = ~round(quantile(.x, digits = 2)[2]),
            Median = ~round(quantile(.x, digits = 2)[3]),
            "75th Percentile" = ~round(quantile(.x, digits = 2)[4]),
            Max = ~round(max(.x, na.rm = TRUE), digits = 2),
            Mean = ~round(mean(.x, na.rm = TRUE),  digits = 2),
            SD = ~round(sd(.x, na.rm = TRUE), digits = 2)
          )
        )) %>%
        pivot_longer(
          cols = everything(),
          names_to = c("Metric", ".value"),
          names_sep = "_"
        ) %>%
        datatable(options = list(dom =""),
                  rownames = FALSE)
          
    })
    
    output$detail_dt <- renderDT({
      req(fish_diversity)
      req(input$year_selector)
      req(selected_points)
      
      dat <- fish_diversity[selected_points(),]
      res <- fish_diversity %>%
        select(c(Year, Cell, longitude, latitude, Richness, shannon, evenness, fric, feve, fdis, fdiv)) %>% 
        filter(Year == input$year_selector) %>% 
        mutate(across(where(is.double) & !c(longitude, latitude), ~round(.x, digits = 1))) %>% 
        mutate(across(c(longitude, latitude), ~round(.x, digits = 4)))
      
      datatable(res)
    })
    
    
  })
}