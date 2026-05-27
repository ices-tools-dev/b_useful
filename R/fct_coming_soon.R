#' coming_soon 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
coming_soon <- function(title, description = NULL) {
  div(
    class = "coming-soon",
    tooltip(
    span("🔒 ", title),
    if (!is.null(description)) {description} else {
      "This feature is planned for a future release"
    }
  ))
}
