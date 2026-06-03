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
mod_time_series_and_trends_server <- function(id, map_parameters, case_study, diversity_data, trends_data, taxon){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    output$year_selector <- renderUI({
      req(diversity_data())
      req(input$content_toggle)
      if(input$content_toggle == "time_series") {
        yr_summary <- summary(diversity_data()$Year)
        selectizeInput(inputId = ns("year_choices"), 
                       tooltip(span("Select Year(s)", bs_icon("info-circle")), HTML("Select years to compare from the dropdown. Use <em>Backspace</em> or <em>Del</em> to deselect")),
                                     multiple = TRUE,
                                     choices = yr_summary[1]:yr_summary[6],
                                     selected = c(yr_summary[1], yr_summary[3], yr_summary[6]))
      }
    })
    
    output$content <- renderUI({
      req(input$content_toggle)
      req(input$diversity_idx)
          if(input$content_toggle == "animation") {
            tagList(
              layout_column_wrap(width = "450px",
              card(withSpinner(mod_diversity_animation_ui(ns("diversity_animation_1")))),
              card(mod_wp3_trends_ui(ns("trends_1")))),
              card("Figure Information", 
                      uiOutput(ns("fig_text_trend")),
                      height = "20vh")
            )
              
            } else if (input$content_toggle == "time_series") {
              mod_wp3_time_comparison_ui(ns("wp3_time_comparison_1"))
            }
    })
    
  mod_diversity_animation_server("diversity_animation_1", case_study = case_study, diversity_idx = reactive(input$diversity_idx), taxon = taxon)
  mod_wp3_time_comparison_server("wp3_time_comparison_1", map_parameters = map_parameters, case_study = case_study, diversity_data = diversity_data(), selected_years = reactive(input$year_choices), diversity_idx = reactive(input$diversity_idx))
  mod_wp3_trends_server("trends_1", map_parameters = map_parameters, case_study = case_study, trends_data = trends_data, taxon=taxon, diversity_idx = reactive(input$diversity_idx))
  
  })
}
    
## To be copied in the UI
# mod_time_series_and_trends_ui("time_series_and_trends_1")
    
## To be copied in the server
# mod_time_series_and_trends_server("time_series_and_trends_1")
