#' diversity_animation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_diversity_animation_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    card_body(
      padding = 0,
      height = "70vh",
      style = "
        overflow: hidden;
      ",
      div(
        style = "
          width: 100%;
          height: 100%;
          display: flex;
          align-items: center;
          justify-content: center;
        ",
        uiOutput(
          outputId = ns("biodiv_animation"),
          style = "
            width: 100%;
            height: 100%;
          "
        )
      )
    )
  )
}
    
#' diversity_animation Server Functions
#'
#' @noRd 
mod_diversity_animation_server <- function(id, case_study, taxon, diversity_idx){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    output$biodiv_animation <- renderUI({
      req(diversity_idx())
      req(taxon())
      eco_acronym <- region_codes[names(region_codes) == case_study()]
      metric_name <- str_replace_all(tolower(diversity_idx()), " ", "_")
      file_name <- paste0(paste(eco_acronym, taxon(), metric_name, "status", sep = "_"), ".gif")
      
      make_img_tag(filename = file_name,
                   ns = ns)
    }) %>% bindCache(diversity_idx(), case_study(), taxon())
    
  })
}
    
## To be copied in the UI
# mod_diversity_animation_ui("diversity_animation_1")
    
## To be copied in the server
# mod_diversity_animation_server("diversity_animation_1")
