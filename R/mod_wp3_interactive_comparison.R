#' wp3_interactive_comparison UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList brushedPoints
#' @importFrom DT DTOutput renderDT datatable
#' @importFrom dplyr filter mutate where across select summarise
#' @importFrom tidyr pivot_longer
#' @importFrom bslib card layout_sidebar sidebar layout_column_wrap card_body
#' @importFrom stringr str_to_title str_replace_all
#' @importFrom sf st_within
#' @importFrom mapgl get_drawn_features renderMaplibre maplibreOutput maplibre add_circle_layer add_draw_control get_drawn_features maplibre_proxy clear_layer
#' @importFrom shinycssloaders withSpinner
mod_wp3_interactive_comparison_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    card(layout_sidebar(sidebar = sidebar(selectInput(ns("year_selector"), "Select Year", choices = 2010:2020, selected = 2020),
                                          checkboxGroupInput(ns("diversity_idx"), label = "Select diversity index", 
                                                       choices = c("Species Richness" = "Richness",
                                                                   "Evenness" = "evenness",
                                                                   "Shannon Index" = "shannon",
                                                                   "Functional Richness" = "fric",
                                                                   "Functional Evenness" = "feve",
                                                                   "Functional Dispersion" = "fdis",
                                                                   "Functional Diversity" = "fdiv"),
                                                       selected = c("Richness", "evenness", "shannon")),
                                          radioButtons(ns("content_type"), label = "Select Display", 
                                                       choices = c("Maps", "Histograms"),
                                                       selected = "Maps"
                                                      )),
                        uiOutput(ns("plot_panel"), fill = TRUE))),
    card(fluidRow(column(width = 5, 
                         card(card_header("Drag to select, double-click to deselect"),
                              full_screen = T, 
                              card_body(padding = 0, 
                                        height = "35vh",
                                        withSpinner(maplibreOutput(ns("interactive_map")))
                                        ) 
                              )
                         ),
                  column(width = 7, 
                         card(card_header("Summary Statistics"),
                         DTOutput(ns("summary_dt")))))),
    card(DTOutput(outputId = ns("detail_dt")))
  )
}
    
#' wp3_interactive_comparison Server Functions
#'
#' @noRd 
mod_wp3_interactive_comparison_server <- function(id, map_parameters, case_study){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$plot_panel <- renderUI({
      req(input$content_type, input$diversity_idx, input$year_selector)
      
      cards <- vector("list", length(input$diversity_idx))
      
      for (i in seq_along(input$diversity_idx)) {
        idx <- input$diversity_idx[i]
        plot_id <- paste0("hist_", i)
        
        if (input$content_type == "Histograms") {
          local({
            my_idx <- idx
            my_plot_id <- plot_id
            
            output[[my_plot_id]] <- renderPlot({
              ggplot(hist_data(), aes(x = .data[[my_idx]], after_stat(density), fill = Selected)) +
              geom_histogram(bins = 20)
           
            })
          })
        }
        
        eco_acronym <- "NrS"
        metric_name <- str_replace_all(tolower(idx), " ", "_")
        file_name <- paste0(paste(eco_acronym, "taxon", metric_name, "status", input$year_selector, sep = "_"),".png")
        
        cards[[i]] <- card(
          min_height = "25vh",
          card_header(str_to_title(idx)),
          card_body(
            padding = 0,
            if (input$content_type == "Maps") {
              make_img_tag(
                filename = file_name,
                ns = ns
              )
            } else if (input$content_type == "Histograms") {
              plotOutput(ns(plot_id), height = "100%")
            }
          )
        )
      }
      
      do.call(layout_column_wrap, c(cards, list(width = "400px")))
    })
    
    grid_low_res <- readRDS("data/grid_lov_res.rds")
    
    output$interactive_map <- renderMaplibre({
      maplibre(bounds = fish_div_spatial, dragRotate=FALSE) %>% 
        add_circle_layer(id = "centroid",
                         source = grid_low_res,
                         circle_radius = 3,
                         circle_stroke_color = "white",
                         circle_stroke_width = 2) %>%
        add_draw_control(position = "top-left",freehand = TRUE, rectangle = TRUE, draw_line_string = FALSE)
    })
    
    observeEvent(input$interactive_map_drawn_features, {
    
      drawn <- input$interactive_map_drawn_features
      
      if (!is.null(drawn)) {
        
        brushed_points <- rep(FALSE, times = nrow(fish_div_spatial))
        drawn_sf <- get_drawn_features(maplibre_proxy("interactive_map"))

        brushed_points <- tryCatch({
          within <- st_within(fish_div_spatial, drawn_sf, sparse = FALSE)
          rowSums(within) >= 1
        },
          error = function(e) e)
        if (inherits(brushed_points, "error")) {
          showNotification("Invalid shape provided, please delete and try again", type = "error", duration = 10)
          validate(
            need(FALSE, paste("ERROR:", conditionMessage(brushed_points)))
          )
        }
  
        maplibre_proxy("interactive_map") %>% 
          clear_layer("highlights") %>% 
          add_circle_layer(
            id = "highlights",
            source = fish_div_spatial[brushed_points,],
            circle_color = "red",
            circle_radius = 5,
            circle_stroke_color = "white",
            circle_stroke_width = 2
          )
        selected_points(brushed_points)
      }
    })
    
    selected_points <- reactiveVal(rep(FALSE, nrow(fish_diversity)))
    
    output$biodiv_compare <- renderPlot({
      req(fish_diversity)
      req(map_parameters())
      
      dat <- fish_diversity 
      dat$Selected <- selected_points()
      dat <- dat %>%
        filter(Year == input$year_selector)
      
      p <- ggplot() +
        geom_point(data = dat, aes(x = longitude, y = latitude, col = Selected), size = 2) +
        geom_sf(data = map_shape, fill = "grey")+
        scale_x_continuous(breaks= map_parameters()$coordxmap)+
        scale_y_continuous(breaks= map_parameters()$coordymap,expand=c(0,0))+
        coord_sf(xlim=c(map_parameters()$coordslim[1], map_parameters()$coordslim[2]), ylim=c(map_parameters()$coordslim[3],map_parameters()$coordslim[4]))+
        ylab("Latitude")+
        xlab("Longitude")
      p
    })
    
    hist_data <- reactive({
      dat <- fish_diversity 
      dat$Selected <- selected_points()
      dat <- dat[dat$Year == input$year_selector,]
    })
    
    output$summary_dt <- renderDT({
      req(fish_diversity)
      req(input$year_selector)
      req(sum(selected_points())>0)
      
      dat <- fish_diversity[selected_points(),]
      dat <- dat[dat$Year == input$year_selector,]
      
      dat %>%
        select(all_of(input$diversity_idx)) %>%
        summarise(
          across(
            everything(),
            list(
              Min = ~ round(min(.x, na.rm = TRUE), 2),
              `25th Percentile` = ~ round(quantile(.x, probs = 0.25, na.rm = TRUE), 2),
              Median = ~ round(median(.x, na.rm = TRUE), 2),
              `75th Percentile` = ~ round(quantile(.x, probs = 0.75, na.rm = TRUE), 2),
              Max = ~ round(max(.x, na.rm = TRUE), 2),
              Mean = ~ round(mean(.x, na.rm = TRUE), 2),
              SD = ~ round(sd(.x, na.rm = TRUE), 2),
              CV = ~ round(sd(.x, na.rm = TRUE)/mean(.x, na.rm = TRUE),2)
            )
          )
        ) %>%
        pivot_longer(
          cols = everything(),
          names_to = c("Metric", ".value"),
          names_sep = "_"
        ) %>%
        datatable(
          options = list(dom = ""),
          rownames = FALSE
        )
    })
    
    output$detail_dt <- renderDT({
      req(fish_diversity)
      req(input$year_selector)
      req(selected_points)
      
      res <- fish_diversity[selected_points(),] %>%
        select(c(Year, Cell, longitude, latitude, Richness, shannon, evenness, fric, feve, fdis, fdiv)) %>% 
        filter(Year == input$year_selector) %>% 
        mutate(across(where(is.double) & -c(longitude, latitude), ~round(.x, digits = 1))) %>% 
        mutate(across(c(longitude, latitude), ~round(.x, digits = 4)))
      
      datatable(res)
    })
  })
}