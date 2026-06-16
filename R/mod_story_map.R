#' story_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import mapgl 
mod_story_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      story_maplibre(threshold = 0.5,
        map_id = ns("map"),
        sections = list(
          "intro" = story_section(
            "30 by 30 Target",
            content = list(HTML("Across the globe, on land and in water, biodiversity is in sharp decline.<br><br>"),
            img(src = "www/extinctions_ipbes.png", width = "300px"),
            tags$br(),
            "To halt this decline, countries have adoped the Kunming-Montreal Global Biodiversity Framework, including the target:",
            tags$br(),
            tags$p(style = "padding: 1em;",
            '"by 2030 at least 30 per cent of terrestrial, inland water, and of coastal and marine areas, especially areas of particular importance for biodiversity and ecosystem functions and services, are effectively conserved and managed"')
          )
          ),
          "partner_countries" = story_section(
            "B-USEFUL",
            content = list(
                           "B-USEFUL can support the '30 by 30 target' by providing evidence-based guidance for ecosystem-based management and marine spatial planning to identify areas of high conservation status.",
                           tags$br(),
                           tags$br(),
                           "13 organisations from 11 countries contribute to B-USEFUL.",
                           tags$br(),
                           tags$br(),
                           tags$a(href = "https://b-useful.eu/", "Visit the project website"),
                           "for more information about the project and consortium."
          )
          ),
          "evidence" = story_section(
            "Comprehensive evidence, extensive coverage",
            content = list("Central to B-USEFUL is the application of a consistent methodology to model and predict biodiversity across a large area, covering regions with very different species composition, environmental and exploitation history - being able to model, compare, and predict biodiversity patterns despite such differnces is a key contribution of the B-USEFUL project.",
                           tags$br(),
                           tags$br(),
                           "The project utilises multiple data sources to generate insights, including: Species abundances,  traits, phylogeny (Species relatedness), and environmental conditions. An important source for this are Scientific Bottom Trawl Surveys, which are an ongoing, collaborative, and systematic sampling of the seafloor."

          )
          ),
         "indicators" = story_section(
            title = "Biodiversity Indicators", 
            content = list(
            "Biodiversity is complex and multifaceted.",
            tags$br(),
            tags$br(),
            "B-USEFUL provides a range of biodiversity indicator outputs to capture some of this complexity and to help address the question of what areas are important for biodiversity",
            # "The Essential Biodiversity Variable framework (Pereira et al. 2013) organises this complexity into 6 classes",
            tags$br(),
            tags$br(),
            "To learn more about some of the challenges to measuring biodiversity, see the video below:",
            
            HTML('<iframe width="360" height="203" src="https://www.youtube.com/embed/OzkcaLsTRic?si=fPJQ9wvZs4rTveex" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>'),
            textOutput(ns("diversity_explanation"))
            )
          ),
          "explore" = story_section(
            title = "Explore", 
            content = list(
              "Select a study region to explore the project results!",
              uiOutput("county_text"),
              tags$a(href="#top", 'Back to top')
            )
            
          )
        )
      )
    )
  )
}

    
#' story_map Server Functions
#'
#' @noRd 
mod_story_map_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$map <- renderMaplibre({
      maplibre(carto_style("voyager"),
               scrollZoom = FALSE)
      
  })
    
    on_section("map", "intro", {
      maplibre_proxy("map") %>% 
        clear_layer("partner_layer") %>% 
        fly_to(center = c(0, 0),
               zoom = 0,
               pitch = 0,
               bearing = 0)
    })  
    
    # on_section("map", "eu", {
    #   maplibre_proxy("map") %>% 
    #     clear_layer("partner_layer") %>% 
    #     add_fill_layer(
    #       id = "eu_layer",
    #       source = country_borders[country_borders$EU_STAT == "T",],
    #       fill_color = "blue",
    #       fill_opacity = 0.5
    #     ) |>
    #     fly_to(center = c(5,40),
    #            zoom = 2.5,
    #            pitch = 15,
    #            bearing = -30)
    #     # fit_bounds(
    #     #   isochrone,
    #     #   animate = TRUE,
    #     #   duration = 3000
    #     #   # pitch = 75
    #     # )
    # })
    
    on_section("map", "partner_countries", {
  
      maplibre_proxy("map") %>% 
        clear_layer("eu_layer") %>% 
        clear_layer("survey_layer") %>% 
        add_fill_layer(
          id = "partner_layer",
          source = country_borders[country_borders$NAME_ENGL %in% c("Norway", "United Kingdom", "Greenland", "Denmark", "France", "Italy", "Spain", "Iceland", "Greece", "Portugal", "Netherlands", "Germany"),],
          fill_color = "gold",
          fill_opacity = 0.5
        ) |>
        ease_to(center = c(-5,60),
               zoom = 3,
               pitch = 45,
               bearing = -10)
    })
    
    on_section("map", "evidence", {
  
      maplibre_proxy("map") %>% 
        clear_layer("partner_layer") %>%
        add_circle_layer(
          id = "survey_layer",
          source = trawls,
          circle_radius = 2
        )
    })
 
})
}
    
## To be copied in the UI
# mod_story_map_ui("story_map_1")
    
## To be copied in the server
# mod_story_map_server("story_map_1")
