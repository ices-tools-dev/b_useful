#' #' @description Function to plot the interactive map of regions.
#' #'
#' #'
#' #' @noRd
#' #'
#' #' @param eco_shape (ecoregions' shapefile)
#' #' @param map_shape (europe's shapefile)
#' #'
#' #' @return leaflet object
#' #'
#' #' @examples
#' #' \dontrun{
#' #' map_ecoregion(eco_shape, map_shape)
#' #' }
#' #'
#' #'
#' #' @import leaflet
#' #'
#' map_ecoregion <- function(eco_shape, map_shape) {
#' 
#'   minZoom <- 0.5
#'   maxZoom <- 14
#'   resolutions <- 1.8 * (2^(maxZoom:minZoom))
#'   crs_laea <- leaflet::leafletCRS(
#'     crsClass = "L.Proj.CRS", code = "EPSG:3035",
#'     proj4def = "+proj=laea +x_0=0 +y_0=0 +lon_0= -1.235660 +lat_0=60.346958",
#'     resolutions = resolutions
#'   )
#'   
#'   #case_study_pal <- colorFactor(buseful_colours[c(1:6, 1:6, 1, 7)], eco_shape$Ecoregion)
#'   leaflet::leaflet(options = leaflet::leafletOptions(crs = crs_laea, minZoom = 0.75, maxZoom = 1.75, dragging = FALSE, scrollWheelZoom = FALSE)) %>%
#'     leaflet::addPolygons(
#'       data = map_shape,
#'       color = "black",
#'       weight = 1,
#'       fillOpacity = 0.4,
#'       fillColor = "#DFF0FA", 
#'       group = "Europe"
#'     ) %>%
#'     leaflet::addPolygons(
#'       data = eco_shape,
#'       fillColor = buseful_colours[c(1:6, 2:7, 1,2)],
#'       fillOpacity = 0.55,
#'       color = "black",
#'       stroke = TRUE,
#'       weight = 1,
#'       layerId = ~Ecoregion,
#'       group = "Eco_regions",
#'       label = ~Ecoregion
#'     ) %>%
#'     leaflet::setView(lng = 12.5, lat = 60, zoom = 0.75) 
#' }
map_ecoregion <- function(eco_shape, map_shape, x_padding = 0.1, y_padding = 0.1) {
  
  eco_shape <- sf::st_transform(eco_shape, 3035)
  map_shape <- sf::st_transform(map_shape, 3035)
  
  bbox <- sf::st_bbox(eco_shape)
  
  x_pad <- (bbox["xmax"] - bbox["xmin"]) * x_padding
  y_pad <- (bbox["ymax"] - bbox["ymin"]) * y_padding
  
  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = map_shape,
      fill = "#DFF0FA",
      colour = "black",
      linewidth = 0.3
    ) +
    ggplot2::geom_sf(
      data = eco_shape,
      ggplot2::aes(fill = Ecoregion),
      colour = "black",
      linewidth = 0.3,
      alpha = 0.55
    ) +
    ggplot2::scale_fill_manual(
      values = buseful_colours[2],
      guide = "none"
    ) +
    ggplot2::coord_sf(
      crs = sf::st_crs(3035),
      xlim = c(
        bbox["xmin"] - x_pad,
        bbox["xmax"] + x_pad
      ),
      ylim = c(
        bbox["ymin"] - y_pad,
        bbox["ymax"] + y_pad
      ),
      datum = NA,
      expand = FALSE
    ) +
    ggplot2::theme_void()
}