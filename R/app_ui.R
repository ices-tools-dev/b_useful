#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib bs_theme
#' @importFrom icesUtils navbar_dropdown_autoclose_js image_fullscreen_on_click_js
#' @noRd
app_ui <- function(request) {
  tagList(
    tags$head(
      tags$style(HTML("
    .leaflet-container {
      background: white !important;
    }
  "))
    ),
    tags$a(name="top"),
    golem_add_external_resources(),
    navbarPage(
      #  CSS to highlight active tab
                tags$style(HTML("
              /* Active tab */
              .nav-tabs > li.active > a,
              .nav-tabs > li.active > a:focus,
              .nav-tabs > li.active > a:hover {
                background-color: #007bff; /* Change to your preferred color */
                color: white !important;
              }
              
              /* Inactive tabs */
              .nav-tabs > li > a {
                background-color: #efeff0;
                color: #333;
              }
            .navbar, .bslib-page-navbar {
              position: relative;
              z-index: 1050;
            }
            ")), 
      windowTitle = "B-USEFUL Decision Support Tool",
      position = "static-top",
      id = "main-navbar",
      theme = bs_theme("lumen", version = 5),
      collapsible = TRUE,
      fluid = TRUE,
      title = span(tags$img(src ="www/buseful-logo-RGB.png",
                            style = "padding-right:2px;padding-bottom:10px; padding-top:2px;",
                            height = "50px"), 
                   "B-USEFUL Decision Support Tool"),
      tabPanel("Home", mod_home_ui("home_1")),
      tabPanel("Background", mod_story_map_ui("story_map_1")),
      navbarMenu("Results",
                tabPanel("North East Atlantic", value = "results_nea",
                         mod_results_ui("results_nea")),
                # tabPanel("Baltic Sea", value = "results_baltic",
                #          mod_results_ui("results_baltic")),
                # tabPanel("Barents Sea", value = "results_barents",
                #          mod_results_ui("results_barents")),
                tabPanel("Greater North Sea", value = "results_gns",
                         mod_results_ui("results_gns")),
                # tabPanel("Iceland", value = "results_iceland",
                #          mod_results_ui("results_iceland")),
                tabPanel("Western Mediterranean Sea", value = "results_w_med",
                         mod_results_ui("results_w_med")),
                tabPanel("Central-Eastern Mediterranean Sea", value = "results_ce_med",
                         mod_results_ui("results_ce_med"))
               ),
      tabPanel("Data Downloads", mod_downloads_ui("downloads_1")),
      tabPanel("Resources", mod_resources_ui("resources_1")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  add_resource_path(
    "img",
    app_sys("app/img")
  )

  tags$head(
    favicon(ext = "ico"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "buseful"
    ),
  image_fullscreen_on_click_js(),
  navbar_dropdown_autoclose_js()
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
