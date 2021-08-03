##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

tabulate_gxt_missing <- function(data_gxt_missing) {

  data_table <- data_gxt_missing %>% 
    pivot_longer(cols = -ID, names_to = 'year') %>% 
    mutate(year = str_replace(year, 'gxtcat', 'Year ')) %>% 
    group_by(year) %>% 
    count(value) %>% 
    mutate(p = 100 * n / sum(n),
           tbl_value = table_glue("{n} ({p}%)"))
  
  inline <- data_table %>% 
    ungroup() %>% 
    mutate(
      across(.cols = c(year, value),
             .fns = str_replace_all,
             pattern = " |; ", 
             replacement = "_")
    ) %>% 
    as_inline(tbl_variables = c('year', 'value'),
              tbl_values = 'tbl_value')
  
  ft <- data_table %>% 
    select(-n, -p) %>% 
    pivot_wider(names_from = year, values_from = tbl_value) %>% 
    mutate(value = capitalize(str_remove(value, '^GXT '))) %>% 
    flextable() %>% 
    width(width = 1.2) %>% 
    width(j = 1, width = 2.5) %>% 
    align(align = 'center', j = 2:4, part = 'all') %>% 
    set_header_labels(value = 'Graded Exercise Test')
  
  list(
    inline = inline,
    table = ft
  )
  
  
}
