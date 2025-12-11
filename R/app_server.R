#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_home_server("home_1")
  #mod_biodiversity_server("biodiversity_1")
  #mod_decision_support_server("decision_support_1")
}
