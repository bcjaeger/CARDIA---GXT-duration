##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data
##' @param stat_names
##' @param estimate_index
boot_bca_df <- function(data, stat_names, estimate_index) {
  
  bind_to <- data %>% 
    select(-boot, -starts_with('age')) %>% 
    slice(1)
  
  stat_names %>% 
    set_names(str_remove(., '^age_')) %>% 
    map_dfr(
      .f = ~{
        
        est <- data[data$boot == estimate_index, .x, drop = TRUE]
        
        bounds <-  
          quantile(data[data$boot != estimate_index, .x, drop = TRUE], 
                   probs = c(0.025, 0.975))
        
        bounds_center <- 
          median(data[data$boot != estimate_index, .x, drop = TRUE])

        bounds_adjusted <- bounds - bounds_center
        
        tibble(est = est,
               lwr = est + bounds_adjusted['2.5%'],
               upr = est + bounds_adjusted['97.5%'])
        
      },
      .id = 'age'
    ) %>% 
    bind_cols(bind_to, .)

}
