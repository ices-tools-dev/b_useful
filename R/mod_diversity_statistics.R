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
mod_diversity_statistics_server <- function(id, diversity_data, selected_spatial, selected_diversity, selected_year){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
      
    filtered_data <- reactive({
      validate(
        need(sum(selected_diversity() * selected_spatial(), na.rm = TRUE) > 0, "No data meets the selection criteria"), 
        need(sum(selected_spatial()) > 0, "Make a valid selection from the map to view summary statistics"))
      req(diversity_data())
      req(selected_year())
      selected_points <- selected_spatial()*selected_diversity()
      dat <- mutate(diversity_data(), selected = selected_points)
      dat <- dat %>% filter(Year == selected_year(),
                            selected == TRUE)
    })
    
    output$summary_dt <- renderDT({
     req(filtered_data())
      
      dat <- filtered_data()
      dat %>%
        select(Richness, shannon, evenness, fric, feve, fdis, fdiv) %>%
        summarise(
          across(
            everything(),
            list(
              Min = ~ round(min(.x, na.rm = TRUE), 2),
              `25th Percentile` = ~ round(quantile(.x, probs = 0.25, na.rm = TRUE), 2),
              Median = ~ round(median(.x, na.rm = TRUE), 2),
              `75th Percentile` = ~ round(quantile(.x, probs = 0.75, na.rm = TRUE), 2),
              Max = ~ round(max(.x, na.rm = TRUE), 2),
              Mean = ~ round(mean(.x, na.rm = TRUE), 2),
              SD = ~ round(sd(.x, na.rm = TRUE), 2),
              CV = ~ round(sd(.x, na.rm = TRUE)/mean(.x, na.rm = TRUE),2)
            )
          )
        ) %>%
        pivot_longer(
          cols = everything(),
          names_to = c("Metric", ".value"),
          names_sep = "_"
        ) %>%
        datatable(
          options = list(dom = ""),
          rownames = FALSE
        )
    })
    
    output$detail_dt <- renderDT({
      req(filtered_data())
      
      res <-  req(filtered_data()) %>%
        select(c(Year, Cell, longitude, latitude, Richness, shannon, evenness, fric, feve, fdis, fdiv)) %>% 
        mutate(across(where(is.double) & -c(longitude, latitude), ~round(.x, digits = 1))) %>% 
        mutate(across(c(longitude, latitude), ~round(.x, digits = 4)))
      
      datatable(res)
    })
    
  })
}
    
## To be copied in the UI
# mod_diversity_statistics_ui("diversity_statistics_1")
    
## To be copied in the server
# mod_diversity_statistics_server("diversity_statistics_1")
