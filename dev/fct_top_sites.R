#' top_sites 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

pct_of_top_sites <- function(df1, df2, variable, threshold) {
  browser()
  #column_name <- glue::glue("Top site for ", enquo(variable), " in area")
  
  top_sites <- slice_max(df1, order_by = {{variable}}, prop = threshold/100)
  
  in_top_sites <- rownames(df2) %in% rownames(top_sites)
  
  (sum(in_top_sites)/nrow(top_sites))*100
  
}
