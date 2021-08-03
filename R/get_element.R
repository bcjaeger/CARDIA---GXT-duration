##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param .x
##' @param element_name
get_element <- function(.x, element_name){
  
  if(element_name %in% names(.x)) return(.x[[element_name]])
  
  if(is_tibble(.x)) return(NULL)
  
  map(.x, get_element, element_name = element_name)
  
}
