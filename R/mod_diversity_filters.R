#' diversity_filters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom bslib tooltip layout_columns
#' @importFrom bsicons bs_icon
#' @importFrom dplyr percent_rank
mod_diversity_filters_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(3, 6, 3),
      
      card(card_body(padding = 0,
           accordion(
                                      accordion_panel(
                                        "Biodiversity", icon = bsicons::bs_icon("sliders"),
                                      uiOutput(ns("year_input")),
                                      sliderInput(ns("richness_percentile"),
                                                  min = 0, max = 100, ticks = FALSE, value = c(0,100), round = TRUE,
                                                  label = tooltip(
                                                    trigger = list(
                                                      "Species Richness percentile",
                                                      bs_icon("info-circle")
                                                    ),
                                                    HTML(select_text(project_texts, tab = "tooltips", section = "richness")),
                                                    tags$br(),
                                                    "Setting the sliders to e.g. 70 and 100 would keep only the 30% of sites with the greadat number of species."
                                                  )),
                                      sliderInput(ns("evenness_percentile"),
                                                  min = 0, max = 100, ticks = FALSE, value = c(0,100), round = TRUE,
                                                  label = tooltip(
                                                    trigger = list(
                                                      "Evenness percentile",
                                                      bs_icon("info-circle")
                                                    ),
                                                    HTML(select_text(project_texts, tab = "tooltips", section = "evenness")),
                                                    tags$br(),
                                                    "Setting the sliders to e.g. 70 and 100 would keep only the 30% of sites where the abundance of species is most similar."
                                                  )),
                                      sliderInput(ns("shannon_percentile"),
                                                  min = 0, max = 100, ticks = FALSE, value = c(0,100), round = TRUE,
                                                  label = tooltip(
                                                    trigger = list(
                                                      "Shannon percentile",
                                                      bs_icon("info-circle")
                                                    ),
                                                    HTML(select_text(project_texts, tab = "tooltips", section = "shannon"))
                                                  )),
                                      sliderInput(ns("fric_percentile"),
                                                  min = 0, max = 100, ticks = FALSE, value = c(0,100), round = TRUE,
                                                  label = tooltip(
                                                    trigger = list(
                                                      "Functional Richness percentile",
                                                      bs_icon("info-circle")
                                                    ),
                                                    HTML(select_text(project_texts, tab = "tooltips", section = "fric"))
                                                  )),
                                      sliderInput(ns("feve_percentile"),
                                                  min = 0, max = 100, ticks = FALSE, value = c(0,100), round = TRUE,
                                                  label = tooltip(
                                                    trigger = list(
                                                      "Functional Evenness percentile",
                                                      bs_icon("info-circle")
                                                    ),
                                                    HTML(select_text(project_texts, tab = "tooltips", section = "feve"))
                                                  )),
                                      sliderInput(ns("fdis_percentile"),
                                                  min = 0, max = 100, ticks = FALSE, value = c(0,100), round = TRUE,
                                                  label = tooltip(
                                                    trigger = list(
                                                      "Functional Dispersion percentile",
                                                      bs_icon("info-circle")
                                                    ),
                                                    HTML(select_text(project_texts, tab = "tooltips", section = "fdis"))
                                                  )),
                                      sliderInput(ns("fdiv_percentile"),
                                                  min = 0, max = 100, ticks = FALSE, value = c(0,100), round = TRUE,
                                                  label = tooltip(
                                                    trigger = list(
                                                      "Functional Divergence percentile",
                                                      bs_icon("info-circle")
                                                    ),
                                                    HTML(select_text(project_texts, tab = "tooltips", section = "fdiv"))
                                                  )))
                                  )
                                )),
      
      card(withSpinner(plotOutput(ns("plot_output"), height = "90vh")),
           card(card_header("Figure Information"),
                uiOutput(ns("fig_text")), min_height = "15vh")
           ),
      
      card(card_body(padding = 0,
           accordion(
        accordion_panel(
                                          "Display", icon = bs_icon("display"),
                                        radioButtons(
                                                    ns("diversity_display"),
                                                    label = tooltip(span("Diversity index to display:", bs_icon("info-circle")), "Note this only impacts the display - biodiversity filters are always applied to their respective layers."),
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
                                                  )
                                        ),
        accordion_panel(
                                        "In Development", icon = bs_icon("wrench"),
                                      coming_soon(actionButton(ns("make_polygon"), "Convert area to polygon")),
                                      coming_soon(actionButton(ns("download_polygon"), "Download user inputs and polygon"))
                                      )))
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
      selectInput(ns("year_selector"), label = tooltip(span("Biodiversity year", bs_icon("info-circle")), "Filters will be applied to biodiversity values in the selected year."),
                  choices = years, selected = max(years[years < 2025]))
    })
    
    selected_points <- reactiveVal(rep(FALSE, nrow(diversity_data())))
    
    filtered_data <- reactive({
      req(diversity_data())
      req(input$richness_percentile)
      
      dat <- mutate(diversity_data(), 
                     richness_percentile = percent_rank(Richness),
                     evenness_percentile = percent_rank(evenness),
                     shannon_percentile = percent_rank(shannon),
                     fric_percentile = percent_rank(fric),
                     feve_percentile = percent_rank(feve),
                     fdis_percentile = percent_rank(fdis),
                     fdiv_percentile = percent_rank(fdiv),
                     )
      
      selected_points(
        (dat$richness_percentile > input$richness_percentile[1]/100) & 
          (dat$richness_percentile < input$richness_percentile[2]/100) &
          (dat$evenness_percentile > input$evenness_percentile[1]/100) & 
          (dat$evenness_percentile < input$evenness_percentile[2]/100) &
          (dat$shannon_percentile > input$shannon_percentile[1]/100) & 
          (dat$shannon_percentile < input$shannon_percentile[2]/100) &
          (dat$fric_percentile > input$fric_percentile[1]/100) & 
          (dat$fric_percentile < input$fric_percentile[2]/100) &
          (dat$feve_percentile > input$feve_percentile[1]/100) & 
          (dat$feve_percentile < input$feve_percentile[2]/100) &
          (dat$fdis_percentile > input$fdis_percentile[1]/100) & 
          (dat$fdis_percentile < input$fdis_percentile[2]/100) &
          (dat$fdiv_percentile > input$fdiv_percentile[1]/100) & 
          (dat$fdiv_percentile < input$fdiv_percentile[2]/100)
      )

      dat %>% filter(selected_points())
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
    output$fig_text <- renderText({
      req(input$year_selector)
      diversity_indicator <- input$diversity_display
      rich_pct <- paste(c(input$richness_percentile[1], input$richness_percentile[2]), collapse = " - ") 
      eve_pct <- paste(c(input$evenness_percentile[1], input$evenness_percentile[2]), collapse = " - ") 
      shannon_pct <- paste(c(input$shannon_percentile[1], input$shannon_percentile[2]), collapse = " - ") 
      fric_pct <- paste(c(input$fric_percentile[1], input$fric_percentile[2]), collapse = " - ") 
      feve_pct <- paste(c(input$feve_percentile[1], input$feve_percentile[2]), collapse = " - ") 
      fdis_pct <- paste(c(input$fdis_percentile[1], input$fdis_percentile[2]), collapse = " - ") 
      fdiv_pct <- paste(c(input$fdiv_percentile[1], input$fdiv_percentile[2]), collapse = " - ")
      
      yr <- input$year_selector
      ecoregion <- str_to_title(str_replace_all(case_study(), pattern = "_", replacement = " "))
      taxon <- taxon()
      
      fig_text <- select_text(project_texts, tab = "fig_text", section = "diversity_filters")
      fig_text <- glue(fig_text)
      HTML(fig_text)
    })
    
    return(list(selected_year = reactive(input$year_selector),
                selected_points = selected_points))
  })
}
    
## To be copied in the UI
# mod_diversity_filters_ui("diversity_filters_1")
    
## To be copied in the server
# mod_diversity_filters_server("diversity_filters_1")
