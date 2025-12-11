#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib bs_theme
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    navbarPage(
      # JavaScript to close dropdown with mouseclick
      tags$script(HTML("
 $(document).on('shiny:sessioninitialized', function() {
  $(document).on('click', function(event) {
    if (!$(event.target).closest('.navbar').length) {
      $('ul.navbar-nav .dropdown').removeClass('show').find('.dropdown-menu').removeClass('show');
    }
  });
});
  ")),
      theme = bs_theme("lumen", version = 5),
      # Custom CSS to highlight active tab
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
  ")),
      position = "static-top",
      collapsible = TRUE,
      windowTitle = "B-USEFUL Decision Support Tool",
      id = "main-navbar",
      fluid = TRUE,
      title = span(tags$img(src ="www/buseful-logo-RGB.png",
                            style = "padding-right:2px;padding-bottom:10px; padding-top:2px;",
                            height = "50px"), "B-USEFUL Decision Support"),
      tabPanel("Home", mod_home_ui("home_1")
               ),
      navbarMenu(title = "About",
                 tabPanel("B-USEFUL project",
                        mod_buseful_ui("buseful_1")),
                 tabPanel("Themes",
                          mod_themes_ui("themes_1")),
                 tabPanel("Case Studies",
                          mod_case_studies_ui("case_studies_1")),
               ),
      navbarMenu("Results",
                tabPanel("North East Atlantic", value = "results_nea",
                         mod_results_ui("results_nea")),
                tabPanel("Baltic Sea", value = "results_baltic",
                         mod_results_ui("results_baltic")),
                tabPanel("Barents Sea", value = "results_barents",
                         mod_results_ui("results_barents")),
                tabPanel("Bay of Biscay", value = "results_biscay",
                         mod_results_ui("results_biscay")),
                tabPanel("Greater North Sea", value = "results_gns",
                         mod_results_ui("results_ns")),
                tabPanel("Iberian Peninsula", value = "results_iberia",
                         mod_results_ui("results_iberia")),
                tabPanel("Iceland", value = "results_iceland",
                         mod_results_ui("results_iceland")),
                tabPanel("Western Mediterranean", value = "results_w_med",
                         mod_results_ui("results_w_med")),
                tabPanel("Eastern Mediterranean", value = "results_e_med",
                         mod_results_ui("results_e_med"))
               ),
      tabPanel("Resources", mod_resources_ui("resources_1")
      )
               # # Primary Tab: Biodiversity
               # mod_biodiversity_ui("biodiversity_1")
               # ,
               # # Primary Tab: Human Pressures
               # navbarMenu("Risk and Vulnerability",
               #            # Subtabs under Human Pressures
               #            tabPanel("Species vulnerability",
               #                     # UI elements
               #            ),
               #            tabPanel("Community vulnerability",
               #                     # UI elements
               #            ),
               # ),
               # navbarMenu("Ecosystem Function and Services"
               # ),
               # navbarMenu("Decision Support",
               #            tabPanel("User input",
               #                     mod_decision_support_ui("decision_support_1")),
               #            tabPanel("MPA match-mismatch"),
               # ),
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
      app_title = "b_useful"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
