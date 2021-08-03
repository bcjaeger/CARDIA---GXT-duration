##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param seed
##' @param data
##' @param id
boot_longitudinal <- function(data, id = 'ID'){
  
  ids <- unique(data[[id]])
  N <- length(ids)
  
  output <- vector(mode = 'list', length = N)
  
  sampled_ids <- c()
  data_split <- split(data, f = data[[id]])
  
  for(i in seq(N)){
    
    new_id <- .new_id <- sample(ids, size = 1)
    
    n_previous_samples <- sum(new_id == sampled_ids)
    
    if(n_previous_samples > 0){
      .new_id <- paste(new_id, letters[n_previous_samples], sep = '_')
    }
    
    sampled_ids <- c(sampled_ids, new_id)
    
    new_data <- try(data_split[[new_id]])
    
    #if(inherits(new_data, 'try-error')) browser()
    
    new_data[[id]] <- .new_id
    
    output[[i]] <- new_data
    
  }
  
  bind_rows(output)
  
}
