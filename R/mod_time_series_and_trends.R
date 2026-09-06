#' time_series_and_trends UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinyWidgets prettySwitch
#' @importFrom glue glue
mod_time_series_and_trends_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(min_height = "70vh", layout_sidebar(sidebar = sidebar(
      radioButtons(ns("content_toggle"), 
                   label = "", 
                   choiceNames = list(tooltip(span("Biodiversity Development", bs_icon("info-circle")), "Displays an animation of the annual development of the selected biodiversity index and the overall trend through the time series"),
                                      tooltip(span("Explore Time Series", bs_icon("info-circle")), "Enables selection of specific years from the time series to make side-by-side comparison of biodiversity indicators")),
                   choiceValues = c("animation", "time_series"),
                   selected = "animation"),
      radioButtons(
        ns("diversity_idx"),
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
      uiOutput(ns("year_selector"))),
      uiOutput(ns("content"))
                      )
    )
  )
}
    
#' time_series_and_trends Server Functions
#'
#' @noRd 
mod_time_series_and_trends_server <- function(
    id,
    map_parameters,
    case_study,
    diversity_data,
    trends_data,
    taxon
) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    
    # ============================================================
    # Validated data sources
    #
    # diversity_data():
    #   Arrow Dataset backed by Parquet
    #
    # trends_data():
    #   ordinary R object loaded from RDS
    # ============================================================
    
    diversity_dataset <- reactive({
      
      req(diversity_data())
      
      diversity_data()
    })
    
    
    trends_dataset <- reactive({
      
      req(trends_data())
      
      trends_data()
    })
    
    
    # ============================================================
    # Available years
    #
    # Do not use:
    #
    #   summary(diversity_data()$Year)
    #
    # with an Arrow Dataset.
    #
    # Instead, ask Arrow only for the distinct Year values and
    # collect that tiny result.
    # ============================================================
    
    available_years <- reactive({
      
      req(diversity_data())
      
      diversity_dataset() |>
        dplyr::select(Year) |>
        dplyr::distinct() |>
        dplyr::arrange() |>
        dplyr::collect() |>
        dplyr::pull(Year)
    })
    
    
    # ============================================================
    # Year selector
    # ============================================================
    
    output$year_selector <- renderUI({
      
      req(diversity_data())
      req(input$content_toggle)
      
      if (input$content_toggle == "time_series") {
        
        years <- available_years()
        
        req(length(years) > 0)
        
        # Preserve the spirit of the previous defaults:
        # first, third, and final available year.
        default_positions <- unique(
          pmin(
            c(1, 3, length(years)),
            length(years)
          )
        )
        
        default_years <- years[default_positions]
        
        
        selectizeInput(
          inputId = ns("year_choices"),
          
          label = tooltip(
            span(
              "Select Year(s)",
              bs_icon("info-circle")
            ),
            HTML(
              paste0(
                "Select years to compare from the dropdown. ",
                "Use <em>Backspace</em> or <em>Del</em> to deselect"
              )
            )
          ),
          
          multiple = TRUE,
          choices = years,
          selected = default_years
        )
      }
    })
    
    
    # ============================================================
    # Main content
    # ============================================================
    
    output$content <- renderUI({
      
      req(input$content_toggle)
      req(input$diversity_idx)
      
      if (input$content_toggle == "animation") {
        
        tagList(
          
          layout_column_wrap(
            width = "450px",
            
            card(
              withSpinner(
                mod_diversity_animation_ui(
                  ns("diversity_animation_1")
                )
              )
            ),
            
            card(
              mod_wp3_trends_ui(
                ns("trends_1")
              )
            )
          ),
          
          card(
            card_header(
              "Figure Information"
            ),
            uiOutput(
              ns("fig_text")
            ),
            min_height = "15vh"
          )
        )
        
      } else if (input$content_toggle == "time_series") {
        
        mod_wp3_time_comparison_ui(
          ns("wp3_time_comparison_1")
        )
      }
    })
    
    
    # ============================================================
    # Child modules
    # ============================================================
    
    mod_diversity_animation_server(
      "diversity_animation_1",
      case_study = case_study,
      diversity_idx = reactive({
        req(input$diversity_idx)
        input$diversity_idx
      }),
      taxon = taxon
    )
    
    
    # IMPORTANT:
    #
    # Pass the diversity Dataset REACTIVE downstream.
    #
    # Do not use:
    #
    #   diversity_data = diversity_data()
    #
    # because that evaluates the reactive here and prevents the
    # child module from controlling its own lazy Arrow query.
    
    mod_wp3_time_comparison_server(
      "wp3_time_comparison_1",
      
      map_parameters = map_parameters,
      case_study = case_study,
      
      diversity_data = diversity_dataset,
      
      selected_years = reactive({
        req(input$year_choices)
        
        # selectizeInput returns character values, so convert these
        # before using them in Arrow filters if Year is numeric.
        as.numeric(input$year_choices)
      }),
      
      diversity_idx = reactive({
        req(input$diversity_idx)
        input$diversity_idx
      }),
      
      taxon = taxon
    )
    
    
    # trends_data remains RDS-backed.
    #
    # trends_dataset() simply adds the explicit req(trends_data())
    # validation before handing the R object to the child module.
    
    mod_wp3_trends_server(
      "trends_1",
      
      map_parameters = map_parameters,
      case_study = case_study,
      
      trends_data = trends_dataset,
      
      taxon = taxon,
      
      diversity_idx = reactive({
        req(input$diversity_idx)
        input$diversity_idx
      })
    )
    
    
    # ============================================================
    # Figure text
    # ============================================================
    
    output$fig_text <- renderText({
      
      req(diversity_data())
      req(case_study())
      req(taxon())
      req(input$diversity_idx)
      
      diversity_indicator <- input$diversity_idx
      
      years <- available_years()
      
      req(length(years) > 0)
      
      # Keep this variable if your glue text expects yr_summary.
      # This recreates the useful summary without trying to call
      # summary() on the Arrow Dataset itself.
      yr_summary <- summary(years)
      
      
      ecoregion <- str_to_title(
        str_replace_all(
          case_study(),
          pattern = "_",
          replacement = " "
        )
      )
      
      
      # Preserve the object name expected by the glue template.
      taxon <- taxon()
      
      
      fig_text <- select_text(
        project_texts,
        tab = "fig_text",
        section = "animation_trend"
      )
      
      fig_text <- glue(fig_text)
      
      HTML(fig_text)
    })
  })
}

    
## To be copied in the UI
# mod_time_series_and_trends_ui("time_series_and_trends_1")
    
## To be copied in the server
# mod_time_series_and_trends_server("time_series_and_trends_1")
