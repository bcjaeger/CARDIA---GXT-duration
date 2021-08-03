##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data_gxt_models
make_gxt_inference <- function(data_gxt_models) {

  data_gxt_models %>% 
    select(-effect_estimates) %>% 
    unnest(effect_pvalues) %>% 
    group_by(outcome, variable, subset_sex, term, model_type, analysis) %>% 
    summarize(p.value = median(p.value)) %>% 
    filter(str_detect(term, pattern = variable)) %>% 
    mutate(term = case_when(
      str_detect(term, '\\:') ~ "trend",
      str_detect(term, '^duration') ~ "duration",
      TRUE ~ 'base'
    )) %>% 
    # filter(!str_detect(term, '^duration')) %>% 
    # mutate(term = if_else(str_detect(term, '\\:'), 'trend', 'base')) %>% 
    pivot_wider(values_from = p.value, 
                names_from = c(term),
                names_prefix = 'pval_')

}
