#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_gxt_included
#' @param boot_iter
make_data_gxt_boots <- function(data_gxt_included, 
                                boot_iter,
                                analysis,
                                n_impute) {
  
  conflicted::conflict_prefer("filter", "dplyr")
  conflicted::conflict_prefer("lmer", "lmerTest")
  conflicted::conflict_prefer("summarize", "dplyr")
  conflicted::conflict_prefer("as_flextable", "flextable")
  conflicted::conflict_prefer("lag", "dplyr")

  ..data <- data_gxt_included[[analysis]]
  
  if(boot_iter == 0) 
    boot_data <- ..data 
  else{
    boot_data <- boot_longitudinal(data = ..data)
  }
  
  boot_data %>% 
    nest(data_unimputed = everything()) %>% 
    mutate(boot = boot_iter, .before = 1) %>% 
    # apply MI separately to each bootstrapped dataset. 
    mutate(
      imputes = map(data_unimputed,
                    .f = make_gxt_imputes, 
                    n_impute = n_impute)
    ) %>% 
    # format: one row per bootstrap, one column per impute
    unnest_wider(col = imputes) %>% 
    select(-data_unimputed) %>% 
    pivot_longer(cols = -boot, 
                 values_to = 'data',
                 names_to = 'impute',
                 names_prefix = 'Dataset_',
                 names_transform = list(impute = as.numeric)) %>% 
    mutate(analysis = analysis, .after = boot) 
   
}

# make_gxt_boots <- function(gxt_data, n_boot){
#   
#   orig <- tibble(boot = 0, data_unimputed = list(gxt_data))
#   
#   boots <- map(
#     .x = seq(n_boot),
#     .f = ~ boot_longitudinal(seed = .x, data = gxt_data)
#   ) %>% 
#     enframe(name = 'boot', value = 'data_unimputed')
#   
#   bind_rows(orig, boots)
#   
# }
