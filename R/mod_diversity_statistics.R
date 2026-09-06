#' diversity_statistics UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_diversity_statistics_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(card_header("Summary Statistics"),min_height = "120px",
         withSpinner(DTOutput(ns("summary_dt")))),
    card(card_header("Selection Detail"),min_height = "120px",
         withSpinner((DTOutput(outputId = ns("detail_dt")))))
    #coming_soon(card(card_body("This feature will provide an overview of biodiversity in the area resulting from user-defined filters")))
  )
}
    
#' diversity_statistics Server Functions
#'
#' @noRd 
mod_diversity_statistics_server <- function(
    id,
    diversity_data,
    selected_spatial,
    selected_diversity,
    selected_year
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
    # Combined row selection
    # ------------------------------------------------------------
    
    selected_rows <- reactive({
      
      req(diversity_data())
      req(selected_year())
      
      spatial_rows <- selected_spatial()
      diversity_rows <- selected_diversity()
      
      
      validate(
        need(
          length(spatial_rows) > 0,
          "Make a valid selection from the map to view summary statistics"
        )
      )
      
      
      rows <- intersect(
        diversity_rows,
        spatial_rows
      )
      
      
      validate(
        need(
          length(rows) > 0,
          "No data meets the selection criteria"
        )
      )
      
      
      rows
    })
    
    
    # ------------------------------------------------------------
    # Filtered statistics data
    #
    # Only matching row IDs and the selected year are read from
    # Parquet.
    # ------------------------------------------------------------
    
    filtered_data <- reactive({
      
      req(diversity_data())
      req(selected_year())
      
      rows <- selected_rows()
      sel_year <- selected_year()
      
      req(length(rows) > 0)
      
      
      diversity_dataset() |>
        
        dplyr::filter(
          Year == sel_year,
          row_id %in% rows
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
    # Summary table
    # ------------------------------------------------------------
    
    output$summary_dt <- renderDT({
      
      req(diversity_data())
      
      dat <- filtered_data()
      
      req(nrow(dat) > 0)
      
      
      dat |>
        
        dplyr::select(
          Richness,
          shannon,
          evenness,
          fric,
          feve,
          fdis,
          fdiv
        ) |>
        
        dplyr::summarise(
          dplyr::across(
            dplyr::everything(),
            list(
              Min = ~ round(
                min(.x, na.rm = TRUE),
                2
              ),
              
              `25th Percentile` = ~ round(
                quantile(
                  .x,
                  probs = 0.25,
                  na.rm = TRUE
                ),
                2
              ),
              
              Median = ~ round(
                median(
                  .x,
                  na.rm = TRUE
                ),
                2
              ),
              
              `75th Percentile` = ~ round(
                quantile(
                  .x,
                  probs = 0.75,
                  na.rm = TRUE
                ),
                2
              ),
              
              Max = ~ round(
                max(.x, na.rm = TRUE),
                2
              ),
              
              Mean = ~ round(
                mean(.x, na.rm = TRUE),
                2
              ),
              
              SD = ~ round(
                sd(.x, na.rm = TRUE),
                2
              ),
              
              CV = ~ round(
                sd(.x, na.rm = TRUE) /
                  mean(.x, na.rm = TRUE),
                2
              )
            )
          )
        ) |>
        
        tidyr::pivot_longer(
          cols = dplyr::everything(),
          names_to = c(
            "Metric",
            ".value"
          ),
          names_sep = "_"
        ) |>
        
        DT::datatable(
          options = list(
            dom = ""
          ),
          rownames = FALSE
        )
    })
    
    
    # ------------------------------------------------------------
    # Detail table
    # ------------------------------------------------------------
    
    output$detail_dt <- renderDT({
      
      req(diversity_data())
      
      res <- filtered_data()
      
      req(nrow(res) > 0)
      
      
      res <- res |>
        
        dplyr::select(
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
        
        dplyr::mutate(
          dplyr::across(
            where(is.double) &
              -c(longitude, latitude),
            ~ round(.x, digits = 1)
          )
        ) |>
        
        dplyr::mutate(
          dplyr::across(
            c(longitude, latitude),
            ~ round(.x, digits = 4)
          )
        )
      
      
      DT::datatable(
        res
      )
    })
  })
}
    
## To be copied in the UI
# mod_diversity_statistics_ui("diversity_statistics_1")
    
## To be copied in the server
# mod_diversity_statistics_server("diversity_statistics_1")
