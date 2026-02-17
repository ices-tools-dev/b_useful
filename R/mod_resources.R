#' resources UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom bslib card card_header
#' 
mod_resources_ui <- function(id){
  ns <- NS(id)
  tagList(
    card(
      card_header("Acknowledgements & Data Sources", class = "bg-primary"),
      tags$p(
        "This application has been developed by the International Council for the Exploration of the Sea (ICES) as part of the ",
        tags$a("B-USEFUL project", 
               href   = "https://b-useful.eu/", 
               target = "_blank", rel = "noopener noreferrer"),
        ", funded by the European Union’s Horizon 2020 research and innovation programme (Grant Agreement No. 101059823)."
      ),
      tags$p(
        "It integrates outputs from the B-USEFUL project. For full details on the research, see the ",
        tags$a(" deliverable reports", 
               href   = "https://b-useful.eu/library/deliverables/", 
               target = "_blank", rel = "noopener noreferrer"),
        "and",
        tags$a(" published articles", 
               href   = "https://b-useful.eu/library/publications/", 
               target = "_blank", rel = "noopener noreferrer"),
        
      )),
    card(card_header("Licensing and usage", class = "bg-primary"),
         tags$p(
           "The code and data in the B-USEFUL Decision Support Tool are available under the ",
           tags$a(
             "CC BY 4.0 license",
             href   = "https://creativecommons.org/licenses/by/4.0/",
             target = "_blank", 
             rel    = "noopener noreferrer"
           ),
         ),
         tags$p(tags$b("Recommended citation:"), "B-USEFUL Decision Support Tool, [date accessed]. ICES, Copenhagen, Denmark.",
            tags$a("__placeholder__"
              ,href =  "https://www.ices.dk/data/assessment-tools/Pages/B-USEFUL-DST.aspx"
            )),
         tags$p(
           "The application's source code is available on ",
           tags$a(
             "GitHub",
             href   = 'https://github.com/ices-tools-dev/buseful',
             target = "_blank", 
             rel    = "noopener noreferrer"
           )
         ),
        tags$p(
          "View the metadata record for the B-USEFUL Decision Support Tool ",
          tags$a(
            "__placeholder__",
            href   = "https://gis.ices.dk/geonetwork/srv/eng/catalog.search#/home",
            target = "_blank", 
            rel    = "noopener noreferrer"
          )
        )
    ),
    card(card_header("Contact", class = "bg-primary"),
         HTML('<p>If you experience problems with the B-USEFUL Decision Support Tool please <a href="accessions@ices.dk">let us know</a>.</p>')),
  )
}
    
#' resources Server Functions
#'
#' @noRd 
# mod_resources_server <- function(id){
#   moduleServer( id, function(input, output, session){
#     ns <- session$ns
#  
#   })
# }
    
## To be copied in the UI
# mod_resources_ui("resources_1")
    
## To be copied in the server
# mod_resources_server("resources_1")
