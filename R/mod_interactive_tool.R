#' interactive_tool UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom bslib accordion accordion_panel
mod_interactive_tool_ui <- function(id) {
  ns <- NS(id)
  tagList(
    accordion(multiple = TRUE, open = TRUE,
      accordion_panel("Guidance",
                      card(HTML(select_text(project_texts, tab = "guidance", section = "interactive_tool"))
                      )
      ),
      accordion_panel("Biodiversity filters",
                      mod_diversity_filters_ui(ns("diversity_filters_1"))),
      accordion_panel("Spatial filters",
                      mod_spatial_filters_ui(ns("spatial_filters_1"))),
      accordion_panel("Biodiversity Statistics",
                      mod_diversity_statistics_ui(ns("diversity_statistics_1"))
                      ),
      accordion_panel("Human Activities overlay",
                      coming_soon(card(card_body("This feature will allow the user to overlay selected human activities onto the user-defined area")))),
    )
  )
}
    
#' interactive_tool Server Functions
#'
#' @noRd 
mod_interactive_tool_server <- function(
    id,
    map_parameters,
    case_study,
    diversity_data,
    diversity_spatial,
    taxon
) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    
    # ------------------------------------------------------------
    # Validated data sources
    # ------------------------------------------------------------
    
    diversity_dataset <- reactive({
      req(diversity_data())
      diversity_data()
    })
    
    
    diversity_spatial_data <- reactive({
      req(diversity_spatial())
      diversity_spatial()
    })
    
    
    # ------------------------------------------------------------
    # Diversity filtering
    # ------------------------------------------------------------
    
    user_diversity <- mod_diversity_filters_server(
      "diversity_filters_1",
      map_parameters = map_parameters,
      case_study = case_study,
      diversity_data = diversity_dataset,
      taxon = taxon
    )
    
    
    # ------------------------------------------------------------
    # Spatial filtering
    #
    # selected_year is passed in so that the spatial selection is
    # applied to the same year's rows as the diversity filters.
    # ------------------------------------------------------------
    
    user_spatial <- mod_spatial_filters_server(
      "spatial_filters_1",
      map_parameters = map_parameters,
      case_study = case_study,
      diversity_spatial = diversity_spatial_data,
      selected_year = user_diversity$selected_year,
      taxon = taxon
    )
    
    
    # ------------------------------------------------------------
    # Statistics
    #
    # selected_points now means selected row_id values rather than
    # positional logical vectors.
    # ------------------------------------------------------------
    
    mod_diversity_statistics_server(
      "diversity_statistics_1",
      selected_spatial = user_spatial$selected_points,
      selected_diversity = user_diversity$selected_points,
      diversity_data = diversity_dataset,
      selected_year = user_diversity$selected_year
    )
  })
}
    
## To be copied in the UI
# mod_interactive_tool_ui("interactive_tool_1")
    
## To be copied in the server
# mod_interactive_tool_server("interactive_tool_1")
