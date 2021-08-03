##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param ... arguments passed to bootBCa

boot_bca <- function(...){
  
  result <- bootBCa(...)
  tibble(lwr = result[1],
         upr = result[2])
  
  
}
