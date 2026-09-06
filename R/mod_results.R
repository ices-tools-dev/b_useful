# -------------------------------------------------------------------------
# Results data configuration
#
# Keep this OUTSIDE mod_results_server() so it is created once when the
# application starts, rather than once per Shiny session.
# -------------------------------------------------------------------------

results_data_config <- list(
  
  greater_north_sea = list(
    label = "Greater North Sea",
    taxon = "fish",
    diversity = "data/gns_fish_diversity.parquet",
    diversity_spatial = "data/gns_fish_div_spatial.parquet",
    trends = "data/gns_fish_diversity_trends.rds",
    occurrence = "data/gns_species_p_occurrence.parquet",
    biomass_abundance = "data/gns_species_abundance.parquet",
    diagnostics = "data/_diagnostics.rds"
  ),
  
  `central-eastern_mediterranean_sea` = list(
    label = "Central-Eastern Mediterranean Sea",
    taxon = "demersal",
    diversity = "data/emed_demersal_diversity.parquet",
    diversity_spatial = "data/emed_demersal_div_spatial.parquet",
    trends = "data/emed_demersal_diversity_trends.rds",
    occurrence = "data/emed_species_p_occurrence.parquet",
    biomass_abundance = "data/emed_species_abundance.parquet",
    diagnostics = "data/emed_demersal_diagnostics.rds"
  ),
  
  western_mediterranean_sea = list(
    label = "Western Mediterranean Sea",
    taxon = "demersal",
    diversity = "data/wmed_demersal_diversity.parquet",
    diversity_spatial = "data/wmed_demersal_div_spatial.parquet",
    trends = "data/wmed_demersal_diversity_trends.rds",
    occurrence = "data/wmed_species_p_occurrence.parquet",
    biomass_abundance = "data/wmed_species_biomass.parquet",
    diagnostics = "data/wmed_demersal_diagnostics.rds"
  ),
  
  north_east_atlantic = list(
    label = "North East Atlantic",
    taxon = "fish",
    diversity = "data/nea_fish_diversity.parquet",
    diversity_spatial = "data/nea_fish_div_spatial.parquet",
    trends = "data/nea_fish_diversity_trends.rds",
    occurrence = "data/nea_fish_p_occurrence.parquet",
    biomass_abundance = NULL,
    diagnostics = "data/nea_fish_diagnostics.rds"
  ),
  
  # These currently have no results-data paths, but keeping them here
  # means display names are still managed in one place.
  
  barents_sea = list(
    label = "Barents Sea",
    taxon = NULL
  ),
  
  iceland = list(
    label = "Iceland",
    taxon = NULL
  )
)


# -------------------------------------------------------------------------
# Results UI
# -------------------------------------------------------------------------

#' results UI Function
#'
#' This module provides the SEAwise results, loading and displaying
#' results for the selected region.
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_results_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      column(
        width = 3,
        titlePanel(
          title = textOutput(ns("region_title"))
        )
      ),
      column(width = 3)
    ),
    
    tabsetPanel(
      
      id = ns("results_tab"),
      
      tabPanel(
        "Species Distributions",
        value = "species_distributions",
        mod_species_distributions_ui(
          ns("species_distributions_1")
        )
      ),
      
      tabPanel(
        "Biodiversity development",
        value = "biodiversity",
        mod_time_series_and_trends_ui(
          ns("time_series_and_trends_1")
        )
      ),
      
      tabPanel(
        "Interactive Tool",
        value = "interactive_tool",
        mod_interactive_tool_ui(
          ns("interactive_tool_1")
        )
      )
    )
  )
}


# -------------------------------------------------------------------------
# Results server
# -------------------------------------------------------------------------

#' results Server Functions
#'
#' @noRd

mod_results_server <- function(id, case_study) {
  
  moduleServer(id, function(input, output, session) {
    
    # -------------------------------------------------------------------
    # Case-study configuration
    # -------------------------------------------------------------------
    
    case_files <- reactive({
      
      req(case_study())
      
      config <- results_data_config[[case_study()]]
      
      validate(
        need(
          !is.null(config),
          paste("No results configuration for", case_study())
        )
      )
      
      config
    })
    
    
    # -------------------------------------------------------------------
    # Region metadata
    # -------------------------------------------------------------------
    
    display_region <- reactive({
      case_files()$label
    })
    
    
    output$region_title <- renderText({
      display_region()
    })
    
    
    taxon <- reactive({
      req(case_files()$taxon)
      case_files()$taxon
    })
    
    
    # -------------------------------------------------------------------
    # Arrow Dataset handles
    #
    # open_dataset() does NOT load the full data into R.
    # These reactives return lazy Arrow Dataset objects.
    #
    # Filtering/selecting should occur inside the downstream modules,
    # followed by collect() only when the actual R data are needed.
    # -------------------------------------------------------------------
    
    diversity_data <- reactive({
      
      path <- case_files()$diversity
      
      req(path)
      
      arrow::open_dataset(path)
    })
    
    
    diversity_spatial <- reactive({
      
      path <- case_files()$diversity_spatial
      
      req(path)
      
      sfarrow::st_read_parquet(path)
    })
    
    
    trends_data <- reactive({
      
      path <- case_files()$trends
      
      req(path)
      
      readRDS(path)
    })
    
    
    pa_data <- reactive({
      
      path <- case_files()$occurrence
      
      req(path)
      
      arrow::open_dataset(path)
    })
    
    
    biomass_abundance_data <- reactive({
      
      path <- case_files()$biomass_abundance
      
      # For example, NE Atlantic currently has no equivalent dataset.
      req(path)
      
      arrow::open_dataset(path)
    })
    
    
    # -------------------------------------------------------------------
    # Diagnostics
    #
    # Leave these as RDS if they contain model/list objects rather than
    # straightforward rectangular data.
    # -------------------------------------------------------------------
    
    model_diagnostics <- reactive({
      
      path <- case_files()$diagnostics
      
      req(path)
      
      readRDS(path)
    })
    
    
    # -------------------------------------------------------------------
    # Map parameters
    #
    # Only longitude and latitude are brought into R.
    # The full diversity dataset remains lazy.
    # -------------------------------------------------------------------
    
    map_parameters <- reactive({
      
      dat <- diversity_data() |>
        dplyr::select(
          longitude,
          latitude
        ) |>
        dplyr::collect()
      
      
      minlong <- min(dat$longitude, na.rm = TRUE)
      maxlong <- max(dat$longitude, na.rm = TRUE)
      
      long_range <- maxlong - minlong
      
      minlong <- minlong - 0.05 * long_range
      maxlong <- maxlong + 0.05 * long_range
      
      
      minlat <- min(dat$latitude, na.rm = TRUE)
      maxlat <- max(dat$latitude, na.rm = TRUE)
      
      lat_range <- maxlat - minlat
      
      minlat <- minlat - 0.05 * lat_range
      maxlat <- maxlat + 0.05 * lat_range
      
      
      list(
        coordslim = c(
          minlong,
          maxlong,
          minlat,
          maxlat
        ),
        coordxmap = round(
          seq(
            minlong,
            maxlong,
            length.out = 5
          )
        ),
        coordymap = round(
          seq(
            minlat,
            maxlat,
            length.out = 5
          )
        )
      )
    })
    
    
    # -------------------------------------------------------------------
    # Downstream modules
    #
    # These receive reactive Arrow Dataset handles.
    # They should select/filter BEFORE collect().
    # -------------------------------------------------------------------
    
    mod_time_series_and_trends_server(
      "time_series_and_trends_1",
      map_parameters = map_parameters,
      case_study = case_study,
      diversity_data = diversity_data,
      trends_data = trends_data,
      taxon = taxon
    )
    
    
    mod_diversity_filters_server(
      "diversity_filters_1",
      map_parameters = map_parameters,
      case_study = case_study,
      diversity_data = diversity_data,
      taxon = taxon
    )
    
    
    mod_interactive_tool_server(
      "interactive_tool_1",
      map_parameters = map_parameters,
      case_study = case_study,
      diversity_data = diversity_data,
      diversity_spatial = diversity_spatial,
      taxon = taxon
    )
    
    
    mod_species_distributions_server(
      "species_distributions_1",
      map_parameters = map_parameters,
      case_study = case_study,
      pa_data = pa_data,
      biomass_abundance_data = biomass_abundance_data,
      model_diagnostics = model_diagnostics
    )
  })
}