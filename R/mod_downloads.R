#' downloads UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_downloads_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("model_selection"), label = "Select from available Models",
                choices = c("North East Atlantic" = "nea", "Western Mediterranean" = "w_med", "Central-Eastern Mediterranean" = "ce_med")),
    uiOutput(ns("download_panel")),
    textOutput(ns("download_selection")),
    downloadButton(ns("download_data"), label = "Download files")
  )
}
    
#' downloads Server Functions
#'
#' @noRd 
mod_downloads_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    output$download_panel <- renderUI({
      download_options <- c("biodiversity", "p_occurrence", "biomass", "abundance", "diagnostics")
      if(input$model_selection == "nea") {
        checkboxGroupInput(ns("download_selector"), "Select outputs for download", choices = download_options[c(1,2,5)], inline = F)
      } else if(input$model_selection == "w_med"){
        checkboxGroupInput(ns("download_selector"), "Select outputs for download", choices = download_options[c(1,2,3,5)], inline = F)
      } else if(input$model_selection == "ce_med"){
        checkboxGroupInput(ns("download_selector"),  "Select outputs for download", choices = download_options[c(1,2,4,5)], inline = F)
      }
    })
    
    output$download_selection <- renderUI({
      req(input$model_selection, input$download_selector)
      browser()
      paste(input$model_selection, input$download_selector, sep = "/")
      
    })
    
    output$download_data <- downloadHandler(
      filename = function() {
        date_tag <- format(Sys.Date(), "%d-%b-%y")
        paste0("b-useful_data_bundle_", input$model_selection, "_", date_tag, ".zip")
      },
      
      content = function(file, selected_model) {
        # --- 1) zipped shapefiles (with acronym + date)
        data_zip_path <- system.file(
          "extdata",
          paste0(selected_model, ".zip"),
          package = "b_useful"
        )
        
        if (shp_zip_path == "") {
          stop("Could not find shapefile zip for ", selected_model)
        }
        
        # --- 2) Disclaimer.txt (fixed name; no acronym/date)
        # --- Temp workspace
        td <- tempfile("data_bundle_")
        dir.create(td, showWarnings = FALSE)
        on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)
        
        
        disc_path_buseful <- file.path(td, "Disclaimer_b_useful.txt")
        disc_url_buseful <- "https://raw.githubusercontent.com/ices-tools-prod/disclaimers/master/Disclaimer_b_useful.txt"
        if (!safe_download(disc_url_buseful, disc_path_buseful)) {
          writeLines(c(
            "Disclaimer for b_useful.",
            "The official disclaimer could not be fetched automatically.",
            paste("Please see:", disc_url_buseful)
          ), con = disc_path_buseful)
        }

        # --- Zip everything
        files_to_zip <- c(data_zip_path, disc_path_buseful)
        if (requireNamespace("zip", quietly = TRUE) && "zipr" %in% getNamespaceExports("zip")) {
          zipr(zipfile = file, files = files_to_zip)
        } else {
          owd <- setwd(td)
          on.exit(setwd(owd), add = TRUE)
          zip(zipfile = file, files = basename(files_to_zip))
        }
      },
      contentType = "application/zip"
    )
    
  })
}
    
## To be copied in the UI
# mod_downloads_ui("downloads_1")
    
## To be copied in the server
# mod_downloads_server("downloads_1")
