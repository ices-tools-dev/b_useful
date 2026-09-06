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
mod_diversity_filters_server <- function(
    id,
    map_parameters,
    case_study,
    diversity_data,
    taxon
) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    
    # ------------------------------------------------------------
    # Validated Arrow Dataset
    # ------------------------------------------------------------
    
    diversity_dataset <- reactive({
      req(diversity_data())
      diversity_data()
    })
    
    
    # ------------------------------------------------------------
    # Available years
    # ------------------------------------------------------------
    
    available_years <- reactive({
      
      req(diversity_data())
      
      diversity_dataset() |>
        dplyr::select(Year) |>
        dplyr::distinct() |>
        dplyr::arrange(Year) |>
        dplyr::collect() |>
        dplyr::pull(Year)
    })
    
    
    output$year_input <- renderUI({
      
      req(diversity_data())
      
      years <- available_years()
      
      req(length(years) > 0)
      
      previous_years <- years[years < 2025]
      
      default_year <- if (length(previous_years) > 0) {
        max(previous_years, na.rm = TRUE)
      } else {
        max(years, na.rm = TRUE)
      }
      
      
      selectInput(
        ns("year_selector"),
        label = tooltip(
          span(
            "Biodiversity year",
            bs_icon("info-circle")
          ),
          "Filters will be applied to biodiversity values in the selected year."
        ),
        choices = years,
        selected = default_year
      )
    })
    
    
    # ------------------------------------------------------------
    # Selected year
    # ------------------------------------------------------------
    
    selected_year <- reactive({
      
      req(input$year_selector)
      
      as.numeric(input$year_selector)
    })
    
    
    # ------------------------------------------------------------
    # Collect only selected year's required columns
    # ------------------------------------------------------------
    
    year_data <- reactive({
      
      req(diversity_data())
      req(selected_year())
      
      sel_year <- selected_year()
      
      diversity_dataset() |>
        dplyr::filter(
          Year == sel_year
        ) |>
        dplyr::select(
          row_id,
          Year,
          Cell,
          longitude,
          latitude,
          Richness,
          shannon,
          evenness,
          fric,
          feve,
          fdis,
          fdiv
        ) |>
        dplyr::collect()
    })
    
    
    # ------------------------------------------------------------
    # Calculate percentile values in R
    # ------------------------------------------------------------
    
    percentile_data <- reactive({
      
      req(diversity_data())
      
      dat <- year_data()
      
      req(nrow(dat) > 0)
      
      dat |>
        dplyr::mutate(
          richness_percentile = dplyr::percent_rank(Richness),
          evenness_percentile = dplyr::percent_rank(evenness),
          shannon_percentile = dplyr::percent_rank(shannon),
          fric_percentile = dplyr::percent_rank(fric),
          feve_percentile = dplyr::percent_rank(feve),
          fdis_percentile = dplyr::percent_rank(fdis),
          fdiv_percentile = dplyr::percent_rank(fdiv)
        )
    })
    
    
    # ------------------------------------------------------------
    # Apply percentile filters
    # ------------------------------------------------------------
    
    filtered_data <- reactive({
      
      req(diversity_data())
      
      req(input$richness_percentile)
      req(input$evenness_percentile)
      req(input$shannon_percentile)
      req(input$fric_percentile)
      req(input$feve_percentile)
      req(input$fdis_percentile)
      req(input$fdiv_percentile)
      
      dat <- percentile_data()
      
      dat |>
        dplyr::filter(
          richness_percentile > input$richness_percentile[1] / 100,
          richness_percentile < input$richness_percentile[2] / 100,
          
          evenness_percentile > input$evenness_percentile[1] / 100,
          evenness_percentile < input$evenness_percentile[2] / 100,
          
          shannon_percentile > input$shannon_percentile[1] / 100,
          shannon_percentile < input$shannon_percentile[2] / 100,
          
          fric_percentile > input$fric_percentile[1] / 100,
          fric_percentile < input$fric_percentile[2] / 100,
          
          feve_percentile > input$feve_percentile[1] / 100,
          feve_percentile < input$feve_percentile[2] / 100,
          
          fdis_percentile > input$fdis_percentile[1] / 100,
          fdis_percentile < input$fdis_percentile[2] / 100,
          
          fdiv_percentile > input$fdiv_percentile[1] / 100,
          fdiv_percentile < input$fdiv_percentile[2] / 100
        )
    })
    
    
    # ------------------------------------------------------------
    # Selected rows
    #
    # This replaces the old full-length logical vector.
    # ------------------------------------------------------------
    
    selected_points <- reactive({
      
      req(diversity_data())
      
      dat <- filtered_data()
      
      dat$row_id
    })
    
    
    # ------------------------------------------------------------
    # Colour scale
    #
    # Scale is based on all cells in the selected year, preserving
    # the intent of the old plot.
    # ------------------------------------------------------------
    
    colour_scale_limits <- reactive({
      
      req(diversity_data())
      req(input$diversity_display)
      
      dat <- year_data()
      
      indicator <- input$diversity_display
      
      req(indicator %in% names(dat))
      
      range(
        dat[[indicator]],
        na.rm = TRUE
      )
    })
    
    
    # ------------------------------------------------------------
    # Map
    # ------------------------------------------------------------
    
    output$plot_output <- renderPlot({
      
      req(diversity_data())
      req(map_parameters())
      req(input$diversity_display)
      
      dat <- filtered_data()
      
      req(nrow(dat) > 0)
      
      indicator <- input$diversity_display
      
      col_scale_limits <- colour_scale_limits()
      
      
      ggplot() +
        
        geom_sf(
          data = map_shape,
          fill = "grey"
        ) +
        
        geom_point(
          data = dat,
          aes(
            x = longitude,
            y = latitude,
            colour = .data[[indicator]]
          ),
          size = 2
        ) +
        
        scale_color_gradientn(
          colours = rev(
            brewer.pal(
              11,
              "RdYlBu"
            )
          ),
          limits = col_scale_limits
        ) +
        
        scale_x_continuous(
          breaks = map_parameters()$coordxmap
        ) +
        
        scale_y_continuous(
          breaks = map_parameters()$coordymap,
          expand = c(0, 0)
        ) +
        
        coord_sf(
          xlim = c(
            map_parameters()$coordslim[1],
            map_parameters()$coordslim[2]
          ),
          ylim = c(
            map_parameters()$coordslim[3],
            map_parameters()$coordslim[4]
          )
        ) +
        
        ylab("Latitude") +
        xlab("Longitude")
    })
    
    
    # ------------------------------------------------------------
    # Figure text
    # ------------------------------------------------------------
    
    output$fig_text <- renderText({
      
      req(diversity_data())
      req(input$year_selector)
      req(input$diversity_display)
      req(case_study())
      req(taxon())
      
      req(input$richness_percentile)
      req(input$evenness_percentile)
      req(input$shannon_percentile)
      req(input$fric_percentile)
      req(input$feve_percentile)
      req(input$fdis_percentile)
      req(input$fdiv_percentile)
      
      
      diversity_indicator <- input$diversity_display
      
      rich_pct <- paste(
        input$richness_percentile,
        collapse = " - "
      )
      
      eve_pct <- paste(
        input$evenness_percentile,
        collapse = " - "
      )
      
      shannon_pct <- paste(
        input$shannon_percentile,
        collapse = " - "
      )
      
      fric_pct <- paste(
        input$fric_percentile,
        collapse = " - "
      )
      
      feve_pct <- paste(
        input$feve_percentile,
        collapse = " - "
      )
      
      fdis_pct <- paste(
        input$fdis_percentile,
        collapse = " - "
      )
      
      fdiv_pct <- paste(
        input$fdiv_percentile,
        collapse = " - "
      )
      
      yr <- input$year_selector
      
      ecoregion <- str_to_title(
        str_replace_all(
          case_study(),
          pattern = "_",
          replacement = " "
        )
      )
      
      taxon <- taxon()
      
      
      fig_text <- select_text(
        project_texts,
        tab = "fig_text",
        section = "diversity_filters"
      )
      
      fig_text <- glue(fig_text)
      
      HTML(fig_text)
    })
    
    
    return(
      list(
        selected_year = selected_year,
        selected_points = selected_points
      )
    )
  })
}
    
## To be copied in the UI
# mod_diversity_filters_ui("diversity_filters_1")
    
## To be copied in the server
# mod_diversity_filters_server("diversity_filters_1")
