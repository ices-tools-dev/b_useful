#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  selected_locations <- reactiveVal(NULL)
  
  mod_home_server("home_1", parent_session = session, selected_locations = selected_locations)
  #mod_biodiversity_server("biodiversity_1")
  #mod_decision_support_server("decision_support_1")
  
  
  
  
  
  # Monitor navbar selection to update selected_locations
  observeEvent(input$`main-navbar`, {
    # Check if the selected tab is one of the "Results" tabs
    if (input$`main-navbar` %in% c("results_baltic", "results_barents", "results_biscay",  "results_gns", "results_iberia", "results_iceland", "results_w_med", "results_ce_med", "results_nea")) {
      # Update selected_locations based on the results tab selected
      # Dynamically call the appropriate results module
      results_tab <- switch(input$`main-navbar`,
                            "results_baltic" = "baltic_sea",
                            "results_barents" = "barents_sea",
                            "results_biscay" = "bay_of_biscay",
                            "results_gns" = "greater_north_sea",
                            "results_iberia" = "iberia",
                            "results_iceland" = "iceland",
                            "results_w_med" = "western_mediterranean",
                            "results_ce_med" = "central-eastern_mediterranean",
                            "results_nea" = "north_east_atlantic",
                            "Home"
      )
      selected_locations(results_tab)
    }
  })
  # Initialize results modules based on selected_locations()
  observeEvent(selected_locations(), {
    req(selected_locations())
    
    # Dynamically call the appropriate results module
    switch(selected_locations(),
           "baltic_sea" = mod_results_server("results_baltic", case_study = selected_locations),
           "bay_of_biscay" = mod_results_server("results_bob", case_study = selected_locations),
           "barents_sea" = mod_results_server("results_barents", case_study = selected_locations),
           "greater_north_sea" = mod_results_server("results_gns", case_study = selected_locations),
           "iberia" = mod_results_server("results_iberia", case_study = selected_locations),
           "iceland" = mod_results_server("results_iceland", case_study = selected_locations),
           "iceland" = mod_results_server("results_iceland", case_study = selected_locations),
           "western_mediterranean" = mod_results_server("results_w_med", case_study = selected_locations),
           "central-eastern_mediterranean" = mod_results_server("results_ce_med", case_study = selected_locations),
           "north_east_atlantic" = mod_results_server("results_nea", case_study = selected_locations)
    )
  })
}
