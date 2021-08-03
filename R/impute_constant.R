#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

impute_constant <- function(data){
  
  for(v in names(data)){
    
    unique_values <- unique(na.omit(data[[v]]))
    
    if(length(unique_values) == 1)
      data[[v]] <- unique_values
    
  }
  
  data
  
}
