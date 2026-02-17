#' home UI Function. This module provides the home page of the application, including a welcome message, a map of the case study regions, and a list of the partners.
#' The map focuses on the case study regions, the drop-down box allows the user to select the region to view the results of.
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom bslib card card_header card_body card_image layout_column_wrap
#' @importFrom leaflet leafletOutput leafletProxy hideGroup showGroup 
#' @importFrom shinyWidgets virtualSelectInput updateVirtualSelect
#' @importFrom stringr str_replace_all 
#' @importFrom icesUtils select_text
#' @importFrom htmltools css
mod_home_ui <- function(id){
  ns <- NS(id)
  tagList(
    card(
      card_header("Welcome to the B-USEFUL Decision Support Tool", class = "bg-primary"),
      div(
        style = "column-count: 2; column-gap: 1.5rem; text-align: justify;",
          style = css(grid_template_columns = "4fr 1fr"),
          heights_equal = "row",
          uiOutput(ns("welcome"))
          )),
    card(height = "40vh", full_screen = FALSE,
      
      layout_column_wrap(width = 1/2,heights_equal = "all",
        card(min_height = "30vh",
                  tags$style(type = "text/css", "#map {margin-left: auto; margin-right: auto; margin-bottom: auto;}"),
                    leafletOutput(ns("map"), width = "100%")
             ),
        
        card(card_body(min_height = "30vh",
              selectInput(
                inputId = ns("selected_locations"),
                label = "",
                choices = c("Please select a case study region", eco_shape$Ecoregion),
                selected = NULL,selectize = TRUE,
                  
                multiple = FALSE,
                width = "100%")
              ))
        )
    ),card(
      card_header("Partners", class = "bg-primary"),uiOutput(ns("who"))
      
    ),
      card(
        card_header("Funded by the European Union", class = "bg-primary"),uiOutput(ns("funding")),
                  card_image(file = NULL, src = "img/normal-reproduction-high-resolution.jpg",
                             height = "50px", width = "75px", border_radius = "all", container = card_body)
    ),
  )
}
    
#' home Server Functions
#'
#' @noRd 
mod_home_server <- function(id, parent_session, selected_locations){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    output$map <- leaflet::renderLeaflet({
      print("Rendering map")
      print(paste("eco_shape dimensions:", nrow(eco_shape), "x", ncol(eco_shape)))
      print(paste("map_shape dimensions:", nrow(map_shape), "x", ncol(map_shape)))
      map_ecoregion(eco_shape, map_shape)
    })
    
    observeEvent(input$selected_locations, {

      temp_location <- input$selected_locations
      temp_location <- str_replace_all(temp_location, " ", "_")
      temp_location <- tolower(temp_location)
      selected_locations(temp_location)
      
      tab_to_show <- switch(
        selected_locations(),
        "baltic_sea" = "results_baltic",
        "barents_sea" = "results_barents",
        "bay_of_biscay" = "results_biscay",
        "greater_north_sea" = "results_gns",
        "iberia" = "results_iberia",
        "iceland" = "results_iceland",
        "western_mediterranean" = "results_w_med",
        "central-eastern_mediterranean" = "results_ce_med",
        "north_east_atlantic" = "results_nea"
      )
      
      updateNavbarPage(session = parent_session, inputId = "main-navbar", selected = tab_to_show)
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    
    output$welcome <- renderUI({
      text <- paste(select_text(project_texts, "landing_page", "welcome"))
      HTML(text)
    })
    
    output$who <- renderUI({
      HTML(select_text(project_texts, "landing_page", "who"))
    })
    output$funding <- renderUI({
      HTML(select_text(project_texts, "landing_page", "funding"))
    })
    output$ebfm_tool <- renderUI({
      HTML(select_text(project_texts, "landing_page", "lorem"))
    })

  })
}
    
## To be copied in the UI
# mod_home_ui("home_1")
    
## To be copied in the server
# mod_home_server("home_1")
