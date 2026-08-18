#' spatial_filters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_spatial_filters_ui <- function(id) {
  ns <- NS(id)
  tagList(
    navset_card_tab(
      nav_panel("Use the drawing tools to select map areas for more detail", 
                card_body(padding = 0,
         full_screen = T, 
         withSpinner(maplibreOutput(ns("spatial_filters_map")))
         )),
      nav_panel("Provide WKT Geometry",
                coming_soon(card(
                  textInput("wkt_string", "Accepted WKT Geometries are POLYGON, MULTIPOLYGON and GEOMETRY COLLECTION")
                )))
    )
  )
}
    
#' spatial_filters Server Functions
#'
#' @noRd 
mod_spatial_filters_server <- function(id, map_parameters, case_study, diversity_data, diversity_spatial, taxon){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    grid_low_res <- reactive({
      switch(case_study(),
             "greater_north_sea" = readRDS("data/gns_grid_low_res.rds"), 
             "western_mediterranean_sea" = readRDS("data/wmed_grid_low_res.rds"),
             "central-eastern_mediterranean_sea" = readRDS("data/emed_grid_low_res.rds"),
             "north_east_atlantic" = readRDS("data/nea_grid_low_res.rds"))
    })
    
    selected_points <- reactiveVal(rep(FALSE, nrow(diversity_data())))
    
    output$spatial_filters_map<- renderMaplibre({
      req(diversity_spatial())
      maplibre(bounds = diversity_spatial(), dragRotate=FALSE) %>% 
        add_circle_layer(id = "centroid",
                         source = grid_low_res(),
                         circle_radius = 3,
                         circle_stroke_color = "white",
                         circle_stroke_width = 2) %>%
        add_draw_control(position = "top-left",
                         freehand = TRUE, 
                         trash = TRUE,
                         rectangle = TRUE,
                         controls = list(
                           trash = TRUE,
                           point = FALSE,
                           line_string = FALSE,
                           combine_features = FALSE,
                           uncombine_features = FALSE)
        )
    })
    
    observeEvent(input$spatial_filters_map_drawn_features, {
      
      drawn <- input$spatial_filters_map_drawn_features
      
      if (!is.null(drawn)) {
        brushed_points <- rep(FALSE, times = nrow(diversity_spatial()))
        drawn_sf <- get_drawn_features(maplibre_proxy("spatial_filters_map"))
        
        brushed_points <- tryCatch({
          within <- st_within(diversity_spatial(), drawn_sf, sparse = FALSE)
          rowSums(within) >= 1
        },
        error = function(e) e)
        if (inherits(brushed_points, "error")) {
          showNotification("Invalid shape provided, please delete and try again", type = "error", duration = 10)
          validate(
            need(FALSE, paste("ERROR:", conditionMessage(brushed_points)))
          )
        }

        maplibre_proxy("spatial_filters_map") %>% 
          clear_layer("highlights") %>% 
          add_circle_layer(
            id = "highlights",
            source = diversity_spatial()[brushed_points,],
            circle_color = "red",
            circle_radius = 5,
            circle_stroke_color = "white",
            circle_stroke_width = 2
          )
        selected_points(brushed_points)
      }
    })
    
    return(list(
      selected_points = reactive(selected_points())
      # later maybe:
      # drawn_polygon = reactive(drawn_sf)
    ))
  })
}
    
## To be copied in the UI
# mod_spatial_filters_ui("spatial_filters_1")
    
## To be copied in the server
# mod_spatial_filters_server("spatial_filters_1")
