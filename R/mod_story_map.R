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
            "Biodiversity under threat",
            content = list(HTML("Across the globe, on land and in water, biodiversity is in sharp decline.<br><br>"),
            # uiOutput("county_text"),
            img(src = "www/extinctions_ipbes.png", width = "300px")
          )
          ),
          "eu" = story_section(
            "EU Biodiversity Strategy for 2030",
            content = list(HTML("The EU has agreed a target to protect 30 % of land and sea by 2030.<br><br> B-USEFUL will provide evidence-based guidance for ecosystem-based management (EBM) and marine spatial planning (MSP) to identify areas of high conservation status.")
                           
            # uiOutput("county_text"),
            # plotOutput("county_plot")
          )
          ),
          "partner_countries" = story_section(
            "International Consortium",
            content = list("13 organisations from 11 countries contribute to the B-USEFUL project.",
                           tags$a(href = "https://b-useful.eu/", "Visit the project website"),
                           "for more information about the project and project consortium.",
            uiOutput("county_text")
          )
          ),
          "surveys" = story_section(
            "Bottom Trawl Surveys",
            content = list("B-USEFUL utilises multiple data sources to generate insights, with Scientific Bottom Trawl Surveys being particularly important.",
            uiOutput("county_text")
          )
          ),
          "results" = story_section(
            title = "Results", 
            content = list(
            "Check out these amazing results.",
            uiOutput("county_text")
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
        clear_layer("eu_layer") %>% 
        fly_to(center = c(0, 0),
               zoom = 0,
               pitch = 0,
               bearing = 0)
    })  
    
    on_section("map", "eu", {
      maplibre_proxy("map") %>% 
        clear_layer("partner_layer") %>% 
        add_fill_layer(
          id = "eu_layer",
          source = country_borders[country_borders$EU_STAT == "T",],
          fill_color = "blue",
          fill_opacity = 0.5
        ) |>
        fly_to(center = c(5,40),
               zoom = 2.5,
               pitch = 15,
               bearing = -30)
        # fit_bounds(
        #   isochrone,
        #   animate = TRUE,
        #   duration = 3000
        #   # pitch = 75
        # )
    })
    
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
    
    on_section("map", "surveys", {
  
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
