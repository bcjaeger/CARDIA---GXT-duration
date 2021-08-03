##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param x
compute_pchange <- function(x) {

  c(0, diff(x) / x[-length(x)]) * 100

}
