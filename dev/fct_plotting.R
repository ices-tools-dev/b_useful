#' plotting 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' 
#' @import ggplot2
#' @importFrom RColorBrewer brewer.pal
plotFun <- function(data, variable, years = NULL, size = 0.5){
  if(!is.null(years)){
    data <- data %>%
      filter(Year %in% years)
  }
  
  p <- ggplot(data,
              aes(x = longitude, y = latitude, color = .data[[variable]])) +
    borders(fill = "grey", colour = "grey") + 
    coord_quickmap(xlim = range(data$longitude),
                   ylim = range(data$latitude)) +
    geom_point(size = size) +
    scale_color_gradientn(colours = rev(brewer.pal(11, "RdYlBu")))
  
  if(!is.null(years)){
    p <- p + facet_wrap(~ Year)
  }
  return(p)
}