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
    
    layout_columns(
      col_widths = c(3, 6, 3),
        
      card(
        radioButtons(
          ns("name_form"),
          label = "Select from species Latin binomial name or common name",
          choices = c(
            "Latin" = "latin",
            "Common name" = "common"
          )
        ),
        
        uiOutput(ns("species_selector")),
        
        uiOutput(ns("model_type_selector")),
        uiOutput(ns("year_selector")),
        uiOutput(ns("focus_selector"))
        
      )
      ,
      
      #---------------------------
      # Main map
      #---------------------------
      card(
        plotOutput(ns("map"))
      ),
      
      #---------------------------
      # Diagnostics
      #---------------------------
      card(
        card_header(
          uiOutput(ns("diagnostics_title"))
        ),
        
        uiOutput(ns("diagnostics_panel"))
      )
      
      #---------------------------
      # Figure text
      #---------------------------
    ),
      card(
        "Figure Text"
      )
  )
}


#' species_distributions Server Functions
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
    
    required_columns <- c(
      "Year",
      "Ecoregion",
      "Cell",
      "longitude",
      "latitude",
      "Lon_c",
      "Lat_c"
    )
    
    
    #============================================================
    # Selected model type
    #============================================================
    #
    # For now this is fixed to presence/absence.
    #
    # When the model-type selector is implemented, this reactive
    # can simply return input$model_type instead.
    #
    selected_model_type <- reactive({

      req(!is.null(input$model_input))
      input$model_input
    })
    
    
    #============================================================
    # Current model dataset
    #============================================================
    
    current_model_data <- reactive({
      
      req(case_study)
      req(selected_model_type())
      
      switch(
        
        selected_model_type(),
        
        "occurrence" = {
          req(pa_data())
          pa_data()
        },
        
        "biomass" = {
          req(biomass_abundance_data())
          biomass_abundance_data()
        },
        
        "abundance" = {
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
    
    
    #============================================================
    # Species available for the selected model
    #============================================================
    
    species <- reactive({
      
      dat <- current_model_data()
      
      cols <- colnames(dat)
      
      cols[!cols %in% required_columns]
    })
    
    
    #============================================================
    # Years available for the selected model
    #============================================================
    
    years <- reactive({
      
      dat <- current_model_data()
      
      sort(unique(dat$Year))
    })
    
    
    #============================================================
    # Species selector
    #============================================================
    
    output$species_selector <- renderUI({
      
      req(species())
      
      selectInput(
        ns("species_input"),
        label = "Select Species",
        choices = species()
      )
    })
    
    #============================================================
    # Model selector
    #============================================================
    
    output$model_type_selector <- renderUI({
      
      req(case_study)
      
      if (case_study() %in% c("western_mediterranean_sea", "central-eastern_mediterranean_sea")) {
        choices <- c("Occurrence" = "occurrence", "Abundance" = "abundance")
      } else if (case_study() == "north_east_atlantic") {
        choices <- c("Occurrence" = "occurrence")
      } else {
        choices <- c("Occurrence" = "occurrence", "Biomass" = "biomass")
      } 
      
      selectInput(
        ns("model_input"),
        label = "Select Model Type",
        choices = choices
      )
    })
    
    
    #============================================================
    # Year selector
    #============================================================
    
    output$year_selector <- renderUI({
      
      req(years())
      
      available_years <- years()
      
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
    
    #============================================================
    # Focus View selector
    #============================================================
    
    output$focus_selector <- renderUI({
      
      req(case_study() == "north_east_atlantic")
      
      available_views <- unique(current_model_data()$Ecoregion)
 
      selectizeInput(
        ns("focus_input"),
        label = "",
        choices = c("Select Ecoregion for focus view", available_views),
        selected = NULL
      )
    })
    
    
    #============================================================
    # Filter selected model data
    #============================================================
    
    filtered_data <- reactive({
      
      req(input$species_input)
      req(input$year_input)
      
      current_model_data() |>
        dplyr::select(
          dplyr::any_of(required_columns),
          dplyr::all_of(input$species_input)
        ) |>
        
        dplyr::filter(
          Year %in% input$year_input
        )
    })
    
    
    #============================================================
    # Diagnostics
    #============================================================
    
    diagnostics_data <- reactive({
      req(input$species_input)
      req(input$model_input)
      req(model_diagnostics())
      # req(selected_model_type())

      dat <- model_diagnostics()
      
      req(dat)
      
      # If model_diagnostics includes model_type,
      # filter by both species and model type.
      #
      # If model_type has not yet been added to the diagnostics
      # table, fall back to filtering by species only.
      
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
    
    
    #============================================================
    # Diagnostics title
    #============================================================
    
    output$diagnostics_title <- renderUI({
      req(selected_model_type())
      
      title <- switch(
        
        selected_model_type(),
        
        "presence_absence" =
          "Presence/absence model diagnostics",
        
        "occurrence" =
          "Occurrence model diagnostics",
        
        "biomass_abundance" =
          "Biomass/abundance model diagnostics",
        
        "Model diagnostics"
      )
      
      tags$span(title)
    })
    
    
    #============================================================
    # Diagnostics panel
    #============================================================
    
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
      
      
      #----------------------------------------------------------
      # Metric metadata
      #----------------------------------------------------------
      
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
            "Difference in mean predicted probability between observed presences and absences."
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
            "Magnitude of prediction error. Lower values indicate better performance."
        )
      )
      
      
      #----------------------------------------------------------
      # Determine which diagnostics are actually available
      #----------------------------------------------------------
      
      available_metrics <- names(metric_info)[
        
        vapply(
          
          names(metric_info),
          
          function(metric) {
            
            metric %in% dat$metric_name &&
              length(dat$metric_name) > 0 &&
              !is.na(dat$metric_name[1])
            
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
      
      
      #----------------------------------------------------------
      # Create each metric row
      #----------------------------------------------------------
      
      metric_rows <- lapply(
        
        available_metrics,
        
        function(metric) {
          
          value <- dat$value[dat$metric_name == metric]
          
          info <- metric_info[[metric]]
          
          
          # Normalised position for diagnostics with known 0-1 scales
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
              
              class = "d-flex justify-content-between align-items-center mb-1",
              
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
                
                # Horizontal line
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
                
                # Dot
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
              
              class = "d-flex justify-content-between",
              
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
            
            
            # Short description
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
    
    
    #============================================================
    # Map colour scale title
    #============================================================
    
    map_legend_title <- reactive({
      
      switch(
        
        selected_model_type(),
        
        "occurrence" = "Probability of\nOccurrence",
        
        "presence_absence" =
          "Probability of\nOccurrence",
        
        "biomass_abundance" =
          "Predicted\nBiomass / Abundance",
        
        "Prediction"
      )
    })
    
    
    #============================================================
    # Map
    #============================================================
    
    output$map <- renderPlot({
      
      dat <- filtered_data()
      
      req(dat)
      req(input$species_input)
      
      
      p <- ggplot() +
        
        geom_point(
          data = dat,
          aes(
            x = longitude,
            y = latitude,
            colour = .data[[input$species_input]]
          ),
          size = 2
        )
      
      
      #----------------------------------------------------------
      # Model-specific colour scales
      #----------------------------------------------------------
      
      if (selected_model_type() == "presence_absence") {
        
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
        
        # For abundance/biomass models we should not force
        # predictions onto a 0-1 scale.
        #
        # For now ggplot derives the limits from the displayed data.
        # This can later be replaced by a fixed global scale if desired.
        
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
    
  })
}