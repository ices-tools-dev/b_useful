# -------------------------------------------------------------------------
# Helpers
# -------------------------------------------------------------------------

# Columns which describe the prediction grid rather than individual species
species_model_id_columns <- c(
  "Year",
  "Ecoregion",
  "ices_ecoregion",
  "Cell",
  "longitude",
  "latitude",
  "Lon_c",
  "Lat_c"
)


# Calculate plotting limits from a data frame containing longitude/latitude
make_map_parameters <- function(dat, padding = 0.05) {
  
  req(nrow(dat) > 0)
  
  minlong <- min(dat$longitude, na.rm = TRUE)
  maxlong <- max(dat$longitude, na.rm = TRUE)
  
  minlat <- min(dat$latitude, na.rm = TRUE)
  maxlat <- max(dat$latitude, na.rm = TRUE)
  
  long_range <- maxlong - minlong
  lat_range  <- maxlat - minlat
  
  # Avoid zero-width ranges
  if (long_range == 0) long_range <- 1
  if (lat_range == 0) lat_range <- 1
  
  minlong <- minlong - padding * long_range
  maxlong <- maxlong + padding * long_range
  
  minlat <- minlat - padding * lat_range
  maxlat <- maxlat + padding * lat_range
  
  list(
    coordslim = c(
      minlong,
      maxlong,
      minlat,
      maxlat
    ),
    coordxmap = round(
      seq(minlong, maxlong, length.out = 5)
    ),
    coordymap = round(
      seq(minlat, maxlat, length.out = 5)
    )
  )
}


# -------------------------------------------------------------------------
# Species distributions UI
# -------------------------------------------------------------------------

#' species_distributions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import ggplot2
#' @importFrom bslib layout_sidebar sidebar card card_header

mod_species_distributions_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    card(
      
      layout_columns(
        col_widths = c(3, 6, 3),
        
        # ---------------------------------------------------------------
        # Controls
        # ---------------------------------------------------------------
        
        card(
          uiOutput(ns("species_selector")),
          uiOutput(ns("model_type_selector")),
          uiOutput(ns("year_selector")),
          uiOutput(ns("focus_selector"))
        ),
        
        # ---------------------------------------------------------------
        # Main map
        # ---------------------------------------------------------------
        
        uiOutput(ns("main_panel")),
        
        # ---------------------------------------------------------------
        # Right panel
        # ---------------------------------------------------------------
        
        uiOutput(ns("right_panel"))
      ),
      
      card(
        "Figure Text"
      )
    )
  )
}


# -------------------------------------------------------------------------
# Species distributions server
# -------------------------------------------------------------------------

#' species_distributions Server Functions
#'
#' pa_data and biomass_abundance_data are reactive Arrow Dataset handles.
#' They should remain lazy until a species/year subset is requested.
#'
#' model_diagnostics may remain an RDS-backed reactive if it contains
#' non-tabular model objects.
#'
#' @noRd

mod_species_distributions_server <- function(
    id,
    map_parameters,
    case_study,
    pa_data,
    biomass_abundance_data,
    model_diagnostics
) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    
    # =====================================================================
    # Selected model type
    # =====================================================================
    
    selected_model_type <- reactive({
      
      req(input$model_input)
      
      input$model_input
    })
    
    
    # =====================================================================
    # Current Arrow Dataset
    #
    # IMPORTANT:
    # This returns an Arrow Dataset handle. It does NOT collect the
    # complete model dataset into R.
    # =====================================================================
    
    current_model_data <- reactive({
      
      req(case_study())
      req(selected_model_type())
      
      switch(
        
        selected_model_type(),
        
        occurrence = {
          req(pa_data())
          pa_data()
        },
        
        biomass = {
          req(biomass_abundance_data())
          biomass_abundance_data()
        },
        
        abundance = {
          req(biomass_abundance_data())
          biomass_abundance_data()
        },
        
        stop(
          paste(
            "Unknown model type:",
            selected_model_type()
          )
        )
      )
    })
    
    
    # =====================================================================
    # Species available in current model
    #
    # Dataset column names are metadata, so this does not require us to
    # read the underlying prediction values.
    # =====================================================================
    
    species <- reactive({
      
      dat <- current_model_data()
      
      cols <- names(dat)
      
      sort(
        cols[
          !cols %in% species_model_id_columns
        ]
      )
    })
    
    
    # =====================================================================
    # Years available in current model
    #
    # Only the Year column is scanned and only the distinct years are
    # collected into R.
    # =====================================================================
    
    years <- reactive({
      req(current_model_data())
      dat <- current_model_data()
      
      req("Year" %in% names(dat))
      
      dat |>
        dplyr::select(Year) |>
        dplyr::distinct() |>
        dplyr::arrange() |>
        dplyr::collect() |>
        dplyr::pull(Year)
    })
    
    
    # =====================================================================
    # Available ecoregions
    #
    # Only required for the NE Atlantic dual visualisation.
    # =====================================================================
    
    focus_regions <- reactive({
      
      req(
        case_study() == "north_east_atlantic"
      )
      req(
        current_model_data()
      )

      dat <- current_model_data()
      
      req("ices_ecoregion" %in% names(dat))
      
      dat |>
        dplyr::select(ices_ecoregion) |>
        dplyr::filter(!is.na(ices_ecoregion)) |>
        dplyr::distinct() |>
        dplyr::arrange(ices_ecoregion) |>
        dplyr::collect() |>
        dplyr::pull(ices_ecoregion)
    })
    
    
    # =====================================================================
    # Model selector
    # =====================================================================
    
    output$model_type_selector <- renderUI({
      
      req(case_study())
      
      choices <- switch(
        
        case_study(),
        
        western_mediterranean_sea =
          c(
            "Occurrence" = "occurrence",
            "Abundance" = "abundance"
          ),
        
        `central-eastern_mediterranean_sea` =
          c(
            "Occurrence" = "occurrence",
            "Abundance" = "abundance"
          ),
        
        north_east_atlantic =
          c(
            "Occurrence" = "occurrence"
          ),
        
        c(
          "Occurrence" = "occurrence",
          "Biomass" = "biomass"
        )
      )
      
      selectInput(
        ns("model_input"),
        label = "Select Model Type",
        choices = choices
      )
    })
    
    
    # =====================================================================
    # Species selector
    # =====================================================================
    
    output$species_selector <- renderUI({
      
      req(species())
      
      selectInput(
        ns("species_input"),
        label = "Select Species",
        choices = species()
      )
    })
    
    
    # =====================================================================
    # Year selector
    # =====================================================================
    
    output$year_selector <- renderUI({
      
      available_years <- years()
      
      req(length(available_years) > 0)
      
      default_year <- if (2020 %in% available_years) {
        2020
      } else {
        min(available_years)
      }
      
      selectizeInput(
        ns("year_input"),
        label = "Select Year",
        choices = available_years,
        selected = default_year
      )
    })
    
    
    # =====================================================================
    # NE Atlantic focus selector
    # =====================================================================
    
    output$focus_selector <- renderUI({
      
      if (case_study() != "north_east_atlantic") {
        return(NULL)
      }
      
      available_views <- focus_regions()
      
      req(length(available_views) > 0)
      
      selectizeInput(
        ns("focus_input"),
        label = "Select Ecoregion for focus view",
        choices = available_views,
        selected = available_views[[1]]
      )
    })
    
    
    # =====================================================================
    # Selected year
    #
    # selectizeInput normally returns character values. Convert back to
    # numeric so Arrow can compare it directly against a numeric Year
    # field.
    # =====================================================================
    
    selected_year <- reactive({
      
      req(input$year_input)
      
      input$year_input
    })
    
    
    # =====================================================================
    # Broad model data
    #
    # THIS IS THE MAIN PERFORMANCE CHANGE.
    #
    # Arrow does:
    #
    #   1. filter to the selected year
    #   2. select grid columns
    #   3. select ONE species column
    #   4. only then collect into R
    #
    # The selected species is renamed to "value" after collection so
    # downstream plotting code is independent of species name.
    # =====================================================================
    
    filtered_data <- reactive({
      
      req(input$species_input)
      req(selected_year())
      
      dat <- current_model_data()
      
      species_name <- input$species_input
      selected_year <- selected_year()
      dat |>
        dplyr::filter(
          Year == selected_year
        ) |>
        dplyr::select(
          dplyr::any_of(species_model_id_columns),
          dplyr::all_of(species_name)
        ) |>
        dplyr::collect() |>
        dplyr::rename(
          value = dplyr::all_of(species_name)
        )
    })
    
    
    # =====================================================================
    # NE Atlantic ecoregion data
    #
    # We already collected only one year + one species above, so filtering
    # the selected ecoregion in R is inexpensive.
    #
    # If the broad NEA subset later proves large enough to matter, this can
    # instead become a second Arrow query with:
    #
    #   filter(
    #     Year == selected_year(),
    #     Ecoregion == input$focus_input
    #   )
    #
    # =====================================================================
    
    focus_data <- reactive({
      
      req(
        case_study() == "north_east_atlantic"
      )
      
      req(input$focus_input)
      req(filtered_data())
      
      dat <- filtered_data()
      
      req("ices_ecoregion" %in% names(dat))
      selected_ecoregion <- input$focus_input
      dat |>
        dplyr::filter(
          ices_ecoregion == selected_ecoregion
        )
    })
    
    
    # =====================================================================
    # Focus-map parameters
    #
    # The ordinary/broad map continues to use map_parameters supplied by
    # the parent results module.
    #
    # The focus map derives its extent from the selected NEA ecoregion.
    # =====================================================================
    
    focus_map_parameters <- reactive({
      
      req(focus_data())
      
      make_map_parameters(
        focus_data(),
        padding = 0.05
      )
    })
    
    
    # =====================================================================
    # Diagnostics
    # =====================================================================
    
    diagnostics_data <- reactive({
      
      req(input$species_input)
      req(selected_model_type())
      req(model_diagnostics())
      
      dat <- model_diagnostics()
      
      req(dat)
      
      if ("model_component" %in% names(dat)) {
        
        dat |>
          dplyr::filter(
            species == input$species_input,
            model_component == selected_model_type()
          )
        
      } else {
        
        dat |>
          dplyr::filter(
            species == input$species_input
          )
      }
    })
    
    
    # =====================================================================
    # Diagnostics title
    # =====================================================================
    
    output$diagnostics_title <- renderUI({
      
      req(selected_model_type())
      
      title <- switch(
        
        selected_model_type(),
        
        occurrence =
          "Occurrence model diagnostics",
        
        biomass =
          "Biomass model diagnostics",
        
        abundance =
          "Abundance model diagnostics",
        
        "Model diagnostics"
      )
      
      tags$span(title)
    })
    
    
    # =====================================================================
    # Diagnostics panel
    # =====================================================================
    
    output$diagnostics_panel <- renderUI({
      
      req(diagnostics_data())
      
      dat <- diagnostics_data()
      
      if (nrow(dat) == 0) {
        
        return(
          tags$p(
            class = "text-muted",
            "No model diagnostics are available for this species."
          )
        )
      }
      
      
      # -------------------------------------------------------------------
      # Metric metadata
      # -------------------------------------------------------------------
      
      metric_info <- list(
        
        AUC = list(
          label = "AUC",
          direction = "higher",
          min = 0,
          max = 1,
          description =
            "Ability to discriminate observed presences from absences."
        ),
        
        TjurR2 = list(
          label = "Tjur R²",
          direction = "higher",
          min = 0,
          max = 1,
          description =
            paste(
              "Difference in mean predicted probability between",
              "observed presences and absences."
            )
        ),
        
        r2 = list(
          label = "R²",
          direction = "higher",
          min = 0,
          max = 1,
          description =
            "Agreement between observed and predicted values."
        ),
        
        RMSE = list(
          label = "RMSE",
          direction = "lower",
          min = NA_real_,
          max = NA_real_,
          description =
            paste(
              "Magnitude of prediction error.",
              "Lower values indicate better performance."
            )
        )
      )
      
      
      # -------------------------------------------------------------------
      # Determine available diagnostics
      # -------------------------------------------------------------------
      
      available_metrics <- names(metric_info)[
        
        vapply(
          
          names(metric_info),
          
          function(metric) {
            
            metric %in% dat$metric_name &&
              any(
                !is.na(
                  dat$value[
                    dat$metric_name == metric
                  ]
                )
              )
          },
          
          logical(1)
        )
      ]
      
      
      if (length(available_metrics) == 0) {
        
        return(
          tags$p(
            class = "text-muted",
            "No model diagnostics are available for this model."
          )
        )
      }
      
      
      # -------------------------------------------------------------------
      # Build diagnostic rows
      # -------------------------------------------------------------------
      
      metric_rows <- lapply(
        
        available_metrics,
        
        function(metric) {
          
          value <- dat$value[
            dat$metric_name == metric
          ][1]
          
          info <- metric_info[[metric]]
          
          has_fixed_scale <-
            !is.na(info$min) &&
            !is.na(info$max)
          
          
          if (has_fixed_scale) {
            
            position <- 100 *
              (value - info$min) /
              (info$max - info$min)
            
            position <- max(
              0,
              min(
                100,
                position
              )
            )
          }
          
          
          tags$div(
            
            class = "mb-4",
            
            # Metric name + value
            tags$div(
              
              class =
                "d-flex justify-content-between align-items-center mb-1",
              
              tags$strong(
                info$label
              ),
              
              tags$span(
                sprintf(
                  "%.2f",
                  value
                )
              )
            ),
            
            
            # Dot scale for bounded metrics
            if (has_fixed_scale) {
              
              tags$div(
                
                style = "
                  position: relative;
                  height: 18px;
                  margin-top: 4px;
                  margin-bottom: 2px;
                ",
                
                tags$div(
                  style = "
                    position: absolute;
                    left: 0;
                    right: 0;
                    top: 8px;
                    height: 2px;
                    background-color: #d9d9d9;
                  "
                ),
                
                tags$div(
                  style = paste0(
                    "
                    position: absolute;
                    top: 3px;
                    left: calc(",
                    position,
                    "% - 6px);
                    width: 12px;
                    height: 12px;
                    border-radius: 50%;
                    background-color: currentColor;
                    "
                  )
                )
              )
              
            } else {
              
              NULL
            },
            
            
            # Direction / interpretation
            tags$div(
              
              class =
                "d-flex justify-content-between",
              
              tags$small(
                class = "text-muted",
                if (info$direction == "higher") {
                  "Higher is better"
                } else {
                  "Lower is better"
                }
              ),
              
              if (has_fixed_scale) {
                
                tags$small(
                  class = "text-muted",
                  paste0(
                    info$min,
                    " – ",
                    info$max
                  )
                )
                
              } else {
                
                NULL
              }
            ),
            
            
            tags$small(
              class = "text-muted",
              info$description
            )
          )
        }
      )
      
      
      tagList(
        metric_rows
      )
    })
    
    
    # =====================================================================
    # Map metadata
    # =====================================================================
    
    map_legend_title <- reactive({
      
      switch(
        
        selected_model_type(),
        
        occurrence =
          "Probability of\nOccurrence",
        
        biomass =
          "Predicted\nBiomass",
        
        abundance =
          "Predicted\nAbundance",
        
        "Prediction"
      )
    })
    
    
    # =====================================================================
    # Common plotting function
    #
    # All model maps now use exactly the same plotting machinery.
    #
    # NEA simply calls it twice:
    #   - once with the broad data
    #   - once with the selected ecoregion
    # =====================================================================
    
    make_model_map <- function(dat, map_params) {
      
      req(nrow(dat) > 0)
      
      p <- ggplot() +
        
        geom_point(
          data = dat,
          aes(
            x = longitude,
            y = latitude,
            colour = value
          ),
          size = 2
        )
      
      
      # Occurrence is always on the probability scale
      if (selected_model_type() == "occurrence") {
        
        p <- p +
          
          scale_colour_gradientn(
            colours = rev(
              RColorBrewer::brewer.pal(
                11,
                "RdYlBu"
              )
            ),
            limits = c(0, 1),
            guide = guide_colourbar(
              title = map_legend_title()
            )
          )
        
      } else {
        
        # Biomass/abundance values are not constrained to 0-1
        p <- p +
          
          scale_colour_gradientn(
            colours = rev(
              RColorBrewer::brewer.pal(
                11,
                "RdYlBu"
              )
            ),
            guide = guide_colourbar(
              title = map_legend_title()
            )
          )
      }
      
      
      p +
        
        geom_sf(
          data = map_shape,
          fill = "grey"
        ) +
        
        scale_x_continuous(
          breaks = map_params$coordxmap
        ) +
        
        scale_y_continuous(
          breaks = map_params$coordymap,
          expand = c(0, 0)
        ) +
        
        coord_sf(
          xlim = c(
            map_params$coordslim[1],
            map_params$coordslim[2]
          ),
          ylim = c(
            map_params$coordslim[3],
            map_params$coordslim[4]
          )
        ) +
        
        labs(
          x = "Longitude",
          y = "Latitude",
          colour = map_legend_title()
        )
    }
    
    
    # =====================================================================
    # Broad model map
    #
    # Used:
    #   - as the main map for all non-NEA case studies
    #   - as the smaller contextual map for NEA
    # =====================================================================
    
    output$broad_map <- renderPlot({
      
      dat <- filtered_data()
      
      req(nrow(dat) > 0)
      
      make_model_map(
        dat = dat,
        map_params = map_parameters()
      )
    })
    
    
    # =====================================================================
    # NE Atlantic ecoregion-focused map
    # =====================================================================
    
    output$focus_map <- renderPlot({
      
      req(
        case_study() == "north_east_atlantic"
      )
      
      dat <- focus_data()
      
      req(nrow(dat) > 0)
      
      make_model_map(
        dat = dat,
        map_params = focus_map_parameters()
      )
    })
    
    
    # =====================================================================
    # Main panel
    #
    # NEA:
    #   large ecoregion-focused map
    #
    # Other case studies:
    #   large broad model map
    # =====================================================================
    
    output$main_panel <- renderUI({
      
      req(case_study())
      
      if (case_study() == "north_east_atlantic") {
        
        card(
          card_header(
            textOutput(
              ns("focus_map_title")
            )
          ),
          plotOutput(
            ns("focus_map"),
            height = "62vh"
          ),
          height = "70vh"
        )
        
      } else {
        
        card(
          plotOutput(
            ns("broad_map"),
            height = "65vh"
          ),
          height = "70vh"
        )
      }
    })
    
    
    output$focus_map_title <- renderText({
      
      req(input$focus_input)
      
      paste(
        input$focus_input,
        "focus"
      )
    })
    
    
    # =====================================================================
    # Right panel
    #
    # NEA keeps the dual visualisation:
    #
    #   broad model map
    #   +
    #   model diagnostics
    #
    # Other case studies only require diagnostics because their broad map
    # is already displayed in the main panel.
    # =====================================================================
    
    output$right_panel <- renderUI({
      
      req(case_study())
      
      if (case_study() == "north_east_atlantic") {
        
        tagList(
          
          card(
            card_header(
              "North East Atlantic"
            ),
            plotOutput(
              ns("broad_map"),
              height = "30vh"
            )
          ),
          
          card(
            card_header(
              uiOutput(
                ns("diagnostics_title")
              )
            ),
            uiOutput(
              ns("diagnostics_panel")
            )
          )
        )
        
      } else {
        
        card(
          card_header(
            uiOutput(
              ns("diagnostics_title")
            )
          ),
          uiOutput(
            ns("diagnostics_panel")
          )
        )
      }
    })
  })
}