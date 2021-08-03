##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data_model_age_female
##' @param data_model_age_male
visualize_race_sex <- function(data_gxt_tables,
                               data_gxt_estimates,
                               outcome_labels) {

  outcome_labels_new <- outcome_labels %>% 
    str_extract(pattern = '\\(.+\\)') %>% 
    str_remove_all('\\(|\\)') %>% 
    set_names(names(outcome_labels))
  
  gg_data_init <- data_gxt_tables %>% 
    mutate(age = as.numeric(age)) %>% 
    filter(age %in% seq(0, 30, by = 5)) %>% 
    filter(subset_sex %in% c("Female", "Male")) %>% 
    ungroup() %>% 
    filter(variable == 'race', model_type == 'baseline') %>% 
    select(-variable) %>% 
    filter(!str_detect(group, '_minus_'))
  
  gg_data <- gg_data_init %>% 
    mutate(outcome = recode(outcome, !!!outcome_labels_new),
           age = as.numeric(age) + 20,
           group = fct_relevel(group, 'White')) %>% 
    filter(age <= 55)

  pm <- function(x){
    
    x_sign <- ifelse(sign(x) > 0, '+', '-')
    
    x_sign[x == 0] <- ""
    
    table_glue("{x_sign}{abs(x)}")
    
  } 
  
  data_text <- data_gxt_estimates$perc_change %>%
    mutate(age = as.numeric(age)) %>% 
    filter(age %in% seq(0, 30, by = 5)) %>% 
    filter(subset_sex %in% c("Female", "Male")) %>% 
    ungroup() %>% 
    filter(variable == 'race', model_type == 'baseline') %>% 
    select(-variable) %>% 
    transmute(
      outcome = recode(outcome, !!!outcome_labels_new),
      subset_sex,
      analysis,
      group = fct_relevel(group, 'White'),
      age = as.numeric(age) + 20,
      text = table_glue("{pm(est)}%")
    ) %>% 
    # filter(age <= 50) %>% 
    left_join(select(gg_data, outcome:upr)) %>% 
    group_by(outcome) %>% 
    mutate(bump = diff(range(est))/20) %>% 
    group_by(outcome, subset_sex, age, analysis) %>% 
    mutate(
      text_max_y = suppressWarnings(if_else(
        outcome == 'GXT Duration, seconds',
        max(upr[outcome == 'GXT Duration, seconds']),
        max(upr[outcome == 'Heart rate, beats per minute'])
      )) + bump,
      text_min_y = suppressWarnings(if_else(
        outcome == 'GXT Duration, seconds',
        min(lwr[outcome == 'GXT Duration, seconds']),
        min(lwr[outcome == 'Heart rate, beats per minute'])
      )) - bump
    ) %>% 
    mutate(
      grp_high = if_else(
        mean(est[group=='Black'] > est[group == 'White']) > 1/2,
        'Black',
        'White'
      ),
      est = case_when(
        group == 'Black' & grp_high == 'Black' ~ text_max_y,
        group == 'Black' & grp_high == 'White' ~ text_min_y,
        group == 'White' & grp_high == 'Black' ~ text_min_y,
        group == 'White' & grp_high == 'White' ~ text_max_y
      )
    ) %>% 
    split(.$analysis)
  
  gg_data <- split(gg_data, gg_data$analysis) 
  
  map2(
    gg_data, 
    data_text,
    .f = ~ 
      ggplot(.x) +
        aes(
          x = age,
          y = est,
          ymin = lwr,
          ymax = upr,
          col = group,
          group = group
        ) +
        geom_line(position = position_dodge(width = 1 / 2)) +
        geom_pointrange(position = position_dodge(width = 1 / 2)) +
        geom_label(data = filter(.y, age != 20),
                   aes(
                     x = age,
                     y = est,
                     fill = group,
                     label = text
                   ),
                   size = 2.4,
                   show.legend = FALSE,
                   alpha = 0.25,
                   label.padding = unit(0.15, 'lines'),
                   inherit.aes = FALSE) +
        facet_grid(outcome ~ subset_sex, scales = 'free', switch = 'y') +
        scale_color_manual(values = c("purple", "orange")) +
        scale_fill_manual(values = c("purple", "orange")) +
        labs(x = 'Age, years', y = '', color = 'Race') +
        theme(
          strip.background = element_rect(colour = "black", fill = "grey90"),
          legend.justification = 'top'
        ) + 
        scale_x_continuous(limits = c(18, 52),
                           breaks = seq(from = 20, to = 50, by = 5))
  )
  
      

}
