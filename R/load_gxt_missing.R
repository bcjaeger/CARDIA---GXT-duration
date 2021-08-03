##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param file
load_gxt_missing <- function(file = "data/missexer.sas7bdat") {

  read_sas(file) %>% 
    mutate(
      across(starts_with('gxtcat'),
             recode,
             '0' = 'GXT completed',
             '1' = 'GXT terminated; medical reasons',
             '2' = 'GXT missing; other reasons'),
      across(starts_with('gxtcat'),
             fct_explicit_na, 
             na_level = 'GXT missing; other reasons')
    )

}
