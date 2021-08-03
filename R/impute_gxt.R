##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data_gxt_included
impute_gxt <- function(data_gxt_included) {

  data_to_impute <- data_gxt_included %>% 
    mutate(
      across(where(is.character), as.factor),
      ID = as.integer(ID)
    )
  
  # specify predictor matrix
  predmat <- make.predictorMatrix(data_to_impute)
  predmat[, 'ID'] <- -2 # clustering variable labeled as -2
  predmat[, c('exam', 'exam_year','exam_age')] <- 0 # same as age_centered

  # imputation in mice
  mice_impute <- suppressWarnings(
    mice(
      data_to_impute,
      maxit = 25,
      m = 5,
      method = '2l.pmm',
      predictorMatrix = predmat
    )
  )
  
  mice_impute %>% 
    complete(action = 'all') %>% 
    map(as_tibble)


}
