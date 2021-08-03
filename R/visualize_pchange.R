##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data_pchange
visualize_pchange <- function(data_pchange) {

  gg_data <- data_pchange %>% 
    mutate(age = as.numeric(age) + 20) %>% 
    # all % change are 0 at age 20
    filter(age > 20, age <= 40) %>% 
    mutate(age = fct_inorder(as.character(age)),
           subset_sex = recode(subset_sex, 'None' = 'Overall'))
  
  gg_data %>% 
    mutate(tbl_value = table_value(est)) %>% 
    split(f = list(.$outcome,
                   .$variable,
                   .$model_type)) %>%
    discard(.p = ~nrow(.x) == 0) %>% 
    map(~ ggplot(.x) + 
          # aes(x=age, y=est, fill=group) + 
          # geom_bar(position = position_dodge(),
          #          stat = 'identity') + 
          aes(x=age, y=est, ymin=lwr, ymax=upr, color=group) + 
          geom_pointrange(position = position_dodge(width = 1/3)) +
          facet_grid(analysis~subset_sex) +
          scale_fill_manual(values = c("purple", "orange")))
  
}
