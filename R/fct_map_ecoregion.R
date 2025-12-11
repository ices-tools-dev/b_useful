#' @description Function to plot the interactive map of regions.
#'
#'
#' @noRd
#'
#' @param eco_shape (ecoregions' shapefile)
#' @param map_shape (europe's shapefile)
#'
#' @return leaflet object
#'
#' @examples
#' \dontrun{
#' map_ecoregion(eco_shape, map_shape)
#' }
#'
#'
#' @import leaflet
#'
map_ecoregion <- function(eco_shape, map_shape) {

  minZoom <- 0.5
  maxZoom <- 14
  resolutions <- 1.8 * (2^(maxZoom:minZoom))
  crs_laea <- leaflet::leafletCRS(
    crsClass = "L.Proj.CRS", code = "EPSG:3035",
    proj4def = "+proj=laea +x_0=0 +y_0=0 +lon_0= -1.235660 +lat_0=60.346958",
    resolutions = resolutions
  )
  
  #case_study_pal <- colorFactor(buseful_colours[c(1:6, 1:6, 1, 7)], eco_shape$Ecoregion)
  leaflet::leaflet(options = leaflet::leafletOptions(crs = crs_laea, minZoom = 0.75, maxZoom = 1.75, dragging = FALSE, scrollWheelZoom = FALSE)) %>%
    leaflet::addPolygons(
      data = map_shape,
      color = "black",
      weight = 1,
      fillOpacity = 0.4,
      fillColor = "#DFF0FA", 
      group = "Europe"
    ) %>%
    leaflet::addPolygons(
      data = eco_shape,
      fillColor = buseful_colours[c(1:6, 2:7, 1,2)],
      fillOpacity = 0.55,
      color = "black",
      stroke = TRUE,
      weight = 1,
      layerId = ~Ecoregion,
      group = "Eco_regions",
      label = ~Ecoregion
    ) %>%
    leaflet::setView(lng = 12.5, lat = 60, zoom = 0.75) 
}