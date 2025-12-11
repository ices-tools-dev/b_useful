#' biodiversity UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom bslib card card_header
mod_biodiversity_ui <- function(id){
  ns <- NS(id)
    navbarMenu("Biodiversity",
    # Subtabs under Biodiversity
    tabPanel("Species Distribution",
             # UI elements
             card(card_header("Taxonomic Richness"),
                  plotOutput(ns("richness_plot"))),
             card(card_header("Richness Trends"),
                  plotOutput(ns("richness_trends"))),
    ),
    tabPanel("Species Traits",
             card(card_header("Functional Richness"),
                  plotOutput(ns("functional_richness_plot"))),
    ),
    tabPanel("Biodiversity Hotspots",
             # UI elements
    )
  )
}
    
#' biodiversity Server Functions
#'
#' @noRd 
mod_biodiversity_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    output$richness_plot <- renderPlot({
      
      plotFun(biodiversity$richness, variable = "Richness", years = c(2010, 2015, 2020))
    })
    
    output$richness_trends <- renderPlot({
      #This plot is wrong
      plotFun(biodiversity$richness_trends$richness_slopes, variable = "estimate", size = 1) +
        scale_color_gradientn(colours = rev(brewer.pal(11, "RdYlBu")),
                              limits = biodiversity$richness_trends$slopeLims)
      })
    
    
    output$functional_richness_plot <- renderPlot({
      
      plotFun(fish.diversity, variable = "fric", years = c(2010, 2015, 2020))
    })
  })
}
    
## To be copied in the UI
# mod_biodiversity_ui("biodiversity_1")
    
## To be copied in the server
# mod_biodiversity_server("biodiversity_1")
