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
      # nav_panel("Provide WKT Geometry",
      #           coming_soon(card(
      #             textInput("wkt_string", "Accepted WKT Geometries are POLYGON, MULTIPOLYGON and GEOMETRY COLLECTION")
      #           )))
    )
  )
}
    
#' spatial_filters Server Functions
#'
#' @noRd 
mod_spatial_filters_server <- function(
    id,
    map_parameters,
    case_study,
    diversity_spatial,
    selected_year,
    taxon
) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    
    # ------------------------------------------------------------
    # Low-resolution display grid
    # ------------------------------------------------------------
    
    grid_low_res <- reactive({
      
      req(case_study())
      
      switch(
        case_study(),
        "greater_north_sea" =
          readRDS("data/gns_grid_low_res.rds"),
        
        "western_mediterranean_sea" =
          readRDS("data/wmed_grid_low_res.rds"),
        
        "central-eastern_mediterranean_sea" =
          readRDS("data/emed_grid_low_res.rds"),
        
        "north_east_atlantic" =
          readRDS("data/nea_grid_low_res.rds")
      )
    })
    
    
    # ------------------------------------------------------------
    # Validated sf data
    # ------------------------------------------------------------
    
    spatial_data <- reactive({
      
      req(diversity_spatial())
      
      dat <- diversity_spatial()
      
      req(inherits(dat, "sf"))
      req("row_id" %in% names(dat))
      req("Year" %in% names(dat))
      
      dat
    })
    
    
    # ------------------------------------------------------------
    # Spatial rows for selected year
    # ------------------------------------------------------------
    
    spatial_year_data <- reactive({
      
      req(diversity_spatial())
      req(selected_year())
      
      year <- selected_year()
      
      dat <- spatial_data()
      
      dat[
        dat$Year == year &
          !is.na(dat$Year),
      ]
    })
    
    
    # ------------------------------------------------------------
    # Selected row IDs
    # ------------------------------------------------------------
    
    selected_points <- reactiveVal(integer(0))
    
    
    # Reset spatial selection whenever the biodiversity year changes.
    observeEvent(
      selected_year(),
      {
        selected_points(integer(0))
      },
      ignoreInit = TRUE
    )
    
    
    # ------------------------------------------------------------
    # Map
    # ------------------------------------------------------------
    
    output$spatial_filters_map <- renderMaplibre({
      
      req(diversity_spatial())
      req(selected_year())
      
      dat <- spatial_year_data()
      
      req(nrow(dat) > 0)
      
      
      maplibre(
        bounds = dat,
        dragRotate = FALSE
      ) |>
        
        add_circle_layer(
          id = "centroid",
          source = grid_low_res(),
          circle_radius = 3,
          circle_stroke_color = "white",
          circle_stroke_width = 2
        ) |>
        
        add_draw_control(
          position = "top-left",
          freehand = TRUE,
          trash = TRUE,
          rectangle = TRUE,
          controls = list(
            trash = TRUE,
            point = FALSE,
            line_string = FALSE,
            combine_features = FALSE,
            uncombine_features = FALSE
          )
        )
    })
    
    
    # ------------------------------------------------------------
    # Spatial selection
    # ------------------------------------------------------------
    
    observeEvent(
      input$spatial_filters_map_drawn_features,
      {
        
        req(diversity_spatial())
        req(selected_year())
        
        drawn <- input$spatial_filters_map_drawn_features
        
        dat <- spatial_year_data()
        
        req(nrow(dat) > 0)
        
        
        # If all shapes have been removed, clear the selection.
        if (is.null(drawn)) {
          
          selected_points(integer(0))
          
          return()
        }
        
        
        drawn_sf <- get_drawn_features(
          maplibre_proxy("spatial_filters_map")
        )
        
        
        if (
          is.null(drawn_sf) ||
          nrow(drawn_sf) == 0
        ) {
          
          selected_points(integer(0))
          
          return()
        }
        
        
        brushed_points <- tryCatch(
          {
            
            within <- sf::st_within(
              dat,
              drawn_sf,
              sparse = FALSE
            )
            
            rowSums(within) >= 1
          },
          error = function(e) {
            e
          }
        )
        
        
        if (inherits(brushed_points, "error")) {
          
          showNotification(
            "Invalid shape provided, please delete and try again",
            type = "error",
            duration = 10
          )
          
          validate(
            need(
              FALSE,
              paste(
                "ERROR:",
                conditionMessage(brushed_points)
              )
            )
          )
          
          return()
        }
        
        
        # --------------------------------------------------------
        # Map highlights
        # --------------------------------------------------------
        
        maplibre_proxy(
          "spatial_filters_map"
        ) |>
          
          clear_layer(
            "highlights"
          ) |>
          
          add_circle_layer(
            id = "highlights",
            source = dat[brushed_points, ],
            circle_color = "red",
            circle_radius = 5,
            circle_stroke_color = "white",
            circle_stroke_width = 2
          )
        
        
        # --------------------------------------------------------
        # Persist selected row IDs
        # --------------------------------------------------------
        
        selected_points(
          dat$row_id[brushed_points]
        )
      }
    )
    
    
    return(
      list(
        selected_points = reactive({
          selected_points()
        })
      )
    )
  })
}
    
## To be copied in the UI
# mod_spatial_filters_ui("spatial_filters_1")
    
## To be copied in the server
# mod_spatial_filters_server("spatial_filters_1")
