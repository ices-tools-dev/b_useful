#' diversity_filters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom bslib tooltip
#' @importFrom bsicons bs_icon
mod_diversity_filters_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(layout_sidebar(sidebar = 
                          sidebar(uiOutput(ns("year_input")),
                                  radioButtons(
                                              ns("diversity_display"),
                                              label = "Select diversity index",
                                              choiceNames = list(
                                                tooltip(span("Species Richness", bs_icon("info-circle")), "Number of species present."),
                                                tooltip(span("Evenness", bs_icon("info-circle")), "How evenly individuals are distributed among species."),
                                                tooltip(span("Shannon Index", bs_icon("info-circle")), "Entropy-based diversity metric combining richness and evenness."),
                                                tooltip(span("Functional Richness", bs_icon("info-circle")), "Volume of occupied functional trait space."),
                                                tooltip(span("Functional Evenness", bs_icon("info-circle")), "Evenness of abundance distribution in trait space."),
                                                tooltip(span("Functional Dispersion", bs_icon("info-circle")), "Mean distance of species to trait-space centroid."),
                                                tooltip(span("Functional Divergence", bs_icon("info-circle")), "Degree to which abundance is distributed toward trait extremes.")
                                              ),
                                              choiceValues = c(
                                                "Richness", "evenness", "shannon",
                                                "fric", "feve", "fdis", "fdiv"
                                              )
                                            ),
                                  tags$hr(),
                                  sliderInput(ns("richness_percentile"),
                                              min = 0, max = 100, ticks = FALSE, value = c(0,100), round = TRUE,
                                              label = tooltip(
                                                trigger = list(
                                                  "Filter sites in Species Richness percentile",
                                                  bs_icon("info-circle")
                                                ),
                                                "Species Richness is the number of species that are known or predicted to be present in a given area.",
                                                tags$br(),
                                                "Setting the sliders to e.g. 70 and 100 would keep only the 30% of sites with the greatest number of species."
                                              )),
                                  sliderInput(ns("evenness_percentile"),
                                              min = 0, max = 100, ticks = FALSE, value = c(0,100), round = TRUE,
                                              label = tooltip(
                                                trigger = list(
                                                  "Filter sites in Evenness percentile",
                                                  bs_icon("info-circle")
                                                ),
                                                "Evenness is ",
                                                tags$br(),
                                                "Setting the sliders to e.g. 70 and 100 would keep only the 30% of sites where the abundance of species is most similar."
                                              )),
                                  sliderInput(ns("shannon_percentile"),
                                              min = 0, max = 100, ticks = FALSE, value = c(0,100), round = TRUE,
                                              label = tooltip(
                                                trigger = list(
                                                  "Filter sites in Shannon percentile",
                                                  bs_icon("info-circle")
                                                ),
                                                "Shannon is ",
                                                tags$br(),
                                                "Setting the sliders to e.g. 70 and 100 would keep only the 30% of sites where "
                                              )),
                                  sliderInput(ns("fric_percentile"),
                                              min = 0, max = 100, ticks = FALSE, value = c(0,100), round = TRUE,
                                              label = tooltip(
                                                trigger = list(
                                                  "Filter sites in Functional Richness percentile",
                                                  bs_icon("info-circle")
                                                ),
                                                "Functional Richness is ",
                                                tags$br(),
                                                "Setting the sliders to e.g. 70 and 100 would keep only the 30% of sites where "
                                              )),
                                  sliderInput(ns("feve_percentile"),
                                              min = 0, max = 100, ticks = FALSE, value = c(0,100), round = TRUE,
                                              label = tooltip(
                                                trigger = list(
                                                  "Filter sites in Functional Evenness percentile",
                                                  bs_icon("info-circle")
                                                ),
                                                "Functional Evenness ",
                                                tags$br(),
                                                "Setting the sliders to e.g. 70 and 100 would keep only the 30% of sites where "
                                              )),
                                  sliderInput(ns("fdis_percentile"),
                                              min = 0, max = 100, ticks = FALSE, value = c(0,100), round = TRUE,
                                              label = tooltip(
                                                trigger = list(
                                                  "Filter sites in Functional Dispersion percentile",
                                                  bs_icon("info-circle")
                                                ),
                                                "Functional Dispersion is ",
                                                tags$br(),
                                                "Setting the sliders to e.g. 70 and 100 would keep only the 30% of sites where "
                                              )),
                                  sliderInput(ns("fdiv_percentile"),
                                              min = 0, max = 100, ticks = FALSE, value = c(0,100), round = TRUE,
                                              label = tooltip(
                                                trigger = list(
                                                  "Filter sites in Functional Divergence percentile",
                                                  bs_icon("info-circle")
                                                ),
                                                " is ",
                                                tags$br(),
                                                "Setting the sliders to e.g. 70 and 100 would keep only the 30% of sites where "
                                              )),
                                  tags$hr(),
                                  coming_soon(actionButton(ns("make_polygon"), "Convert area to polygon")),
                                  coming_soon(actionButton(ns("download_polygon"), "Download user inputs and polygon"))
                                  ),
                        card(plotOutput(ns("plot_output")),
                             card("Figure Information", 
                                  uiOutput(ns("fig_text"))))
                        )
    )
  )
}
    
#' diversity_filters Server Functions
#'
#' @noRd 
mod_diversity_filters_server <- function(id, map_parameters, case_study, diversity_data, taxon){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    output$year_input <- renderUI({
      req(diversity_data())
      years <- unique(diversity_data()$Year)
      selectInput(ns("year_selector"), label = "Select year", choices = years, selected = max(years[years < 2025]))
    })
    
    selected_points <- reactiveVal(rep(FALSE, nrow(diversity_data())))
    
    filtered_data <- reactive({
      req(diversity_data())
      req(input$richness_percentile)
      
      test <- mutate(diversity_data(), 
                     richness_percentile = dplyr::percent_rank(Richness),
                     evenness_percentile = dplyr::percent_rank(evenness),
                     shannon_percentile = dplyr::percent_rank(shannon),
                     fric_percentile = dplyr::percent_rank(fric),
                     feve_percentile = dplyr::percent_rank(feve),
                     fdis_percentile = dplyr::percent_rank(fdis),
                     fdiv_percentile = dplyr::percent_rank(fdiv),
                     )
      
      selected_points(
        (test$richness_percentile > input$richness_percentile[1]/100) & 
          (test$richness_percentile < input$richness_percentile[2]/100) &
          (test$evenness_percentile > input$evenness_percentile[1]/100) & 
          (test$evenness_percentile < input$evenness_percentile[2]/100) &
          (test$shannon_percentile > input$shannon_percentile[1]/100) & 
          (test$shannon_percentile < input$shannon_percentile[2]/100) &
          (test$fric_percentile > input$fric_percentile[1]/100) & 
          (test$fric_percentile < input$fric_percentile[2]/100) &
          (test$feve_percentile > input$feve_percentile[1]/100) & 
          (test$feve_percentile < input$feve_percentile[2]/100) &
          (test$fdis_percentile > input$fdis_percentile[1]/100) & 
          (test$fdis_percentile < input$fdis_percentile[2]/100) &
          (test$fdiv_percentile > input$fdiv_percentile[1]/100) & 
          (test$fdiv_percentile < input$fdiv_percentile[2]/100)
      )
      # test %>% filter(
      #   (richness_percentile > input$richness_percentile[1]/100) & (richness_percentile < input$richness_percentile[2]/100),
      #   (evenness_percentile > input$evenness_percentile[1]/100) & (evenness_percentile < input$evenness_percentile[2]/100),
      #   (shannon_percentile > input$shannon_percentile[1]/100) & (shannon_percentile < input$shannon_percentile[2]/100),
      #   (fric_percentile > input$fric_percentile[1]/100) & (fric_percentile < input$fric_percentile[2]/100),
      #   (feve_percentile > input$feve_percentile[1]/100) & (feve_percentile < input$feve_percentile[2]/100),
      #   (fdis_percentile > input$fdis_percentile[1]/100) & (fdis_percentile < input$fdis_percentile[2]/100),
      #   (fdiv_percentile > input$fdiv_percentile[1]/100) & (fdiv_percentile < input$fdiv_percentile[2]/100),
      #   )
      test %>% filter(selected_points())
    })
    
    
    
    output$plot_output <- renderPlot({
      req(filtered_data())
      req(map_parameters())
      col_scale_limits <- range(diversity_data()[[input$diversity_display]])
      ggplot() +
        geom_point(data = filtered_data(), aes(x = longitude, y = latitude, col = !!sym(input$diversity_display)), size = 2) +
        scale_color_gradientn(colours = rev(brewer.pal(11, "RdYlBu")), limits = col_scale_limits)+
        geom_sf(data = map_shape, fill = "grey")+
        scale_x_continuous(breaks= map_parameters()$coordxmap)+
        scale_y_continuous(breaks= map_parameters()$coordymap,expand=c(0,0))+
        coord_sf(xlim=c(map_parameters()$coordslim[1], map_parameters()$coordslim[2]), ylim=c(map_parameters()$coordslim[3],map_parameters()$coordslim[4]))+
        ylab("Latitude")+
        xlab("Longitude")
    })
    
    return(list(selected_year = reactive(input$year_selector),
                selected_points = selected_points))
  })
}
    
## To be copied in the UI
# mod_diversity_filters_ui("diversity_filters_1")
    
## To be copied in the server
# mod_diversity_filters_server("diversity_filters_1")
