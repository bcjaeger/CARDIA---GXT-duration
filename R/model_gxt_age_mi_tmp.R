##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data_gxt_mi
model_gxt_age <- function(data_gxt_mi,
                          n_boot = 10,
                          .outcomes,
                          .variables,
                          .control,
                          .intercept_only = NULL,
                          .subset_sex = NULL,
                          .subset_race = NULL) {
  
  model_output <- map(
    .x = set_names(.outcomes, .outcomes),
    .f = ~.model_gxt_age(
      data_gxt_mi = data_gxt_mi,
      n_boot = n_boot,
      .outcome = .x,
      .variables = set_names(.variables, .variables),
      .control = .control,
      .intercept_only = .intercept_only,
      .subset_sex = .subset_sex,
      .subset_race = .subset_race
    )
  )
  
  model_effects <- get_element(model_output, 'effects') %>% 
    map(bind_rows) %>% 
    bind_rows(.id = 'outcome') %>% 
    transmute(
      outcome, variable, group, x,
      est = predicted, lwr = conf.low, upr = conf.high,
      table_value = table_glue("{predicted}\n({conf.low}, {conf.high})")
    )
  
  model_pval <- get_element(model_output, 'pvalues') %>% 
    map(bind_rows) %>% 
    bind_rows(.id = 'outcome') %>% 
    select(outcome, variable, p_value, p_type) %>% 
    drop_na() %>% 
    pivot_wider(names_from = p_type, values_from = p_value)
  
  boot_diffs <- get_element(model_output, 'diffs') %>% 
    map(bind_rows, .id = 'variable') %>% 
    bind_rows(.id = 'outcome') %>% 
    transmute(
      outcome, variable, group, x,
      est, lwr, upr,
      table_value = table_glue("{est}\n({lwr}, {upr})")
    )
  
  boot_pchanges <- get_element(model_output, 'pchanges') %>% 
    map(bind_rows, .id = 'variable') %>% 
    bind_rows(.id = 'outcome') %>% 
    transmute(
      outcome, variable, group, x,
      est, lwr, upr,
      table_value = table_glue("{est}\n({lwr}, {upr})")
    )
  
  output_long_estimates <- bind_rows(model_effects, boot_diffs) %>% 
    arrange(outcome, variable, group)
  
  output_wide_estimates <- output_long_estimates %>% 
    select(-est, -lwr, -upr) %>% 
    pivot_wider(names_from = x, 
                names_prefix = 'age_',
                values_from = table_value) %>% 
    left_join(model_pval) %>% 
    mutate(diff_baseline = table_pvalue(diff_baseline),
           diff_trend = table_pvalue(diff_trend))
  
  output_wide_pchanges <- boot_pchanges %>% 
    select(-est, -lwr, -upr) %>% 
    pivot_wider(names_from = x, 
                names_prefix = 'age_',
                values_from = table_value)
  
  list(long_estimates = output_long_estimates,
       long_pchanges = boot_pchanges,
       wide_estimates = output_wide_estimates,
       wide_pchanges = output_wide_pchanges)

}

get_element <- function(.x, element_name){
  
  if(element_name %in% names(.x)) return(.x[[element_name]])
  
  if(is_tibble(.x)) return(NULL)
  
  map(.x, get_element, element_name = element_name)
  
}

.model_gxt_age <- function(data_gxt_mi, 
                           n_boot,
                           .outcome, 
                           .variables,
                           .control,
                           .intercept_only,
                           .subset_sex = NULL,
                           .subset_race = NULL){
  
  model_data <- data_gxt_mi %>% 
    bind_rows(.id = 'impute')
  
  if(!is.null(.subset_sex)){
    model_data <- filter(model_data, sex == .subset_sex)
    .variables <- setdiff(.variables, 'sex')
    .variables <- set_names(.variables, .variables)
    .control <- setdiff(.control, 'sex')
  }
  
  if(!is.null(.subset_race)){
    model_data <- filter(model_data, race == .subset_race)
    .variables <- setdiff(.variables, 'race')
    .variables <- set_names(.variables, .variables)
    .control <- setdiff(.control, 'race')
  }
  
  map(
    .x = .variables,
    .f = function(.variable){
      
      message(.variable)
      #if(.variable == 'educ') browser()
      
      ..control <- .control %>% 
        setdiff(.variable) %>% 
        paste(collapse = ' + ')
      
      ..intercept_only <- .intercept_only %>% 
        setdiff(.variable) %>% 
        paste(collapse = ' + ')
      
      if(.variable == 'overall'){
        
        model_formula <- as.formula(
          glue(
            "{.outcome} ~ rcs(age_centered)*{..control} + {..intercept_only} + (1|ID)"
          )
        )
        
      } else {
        
        model_formula <- as.formula(
          glue(
            "{.outcome} ~ rcs(age_centered)*({.variable}+{..control}) + {..intercept_only} + (1|ID)"
          )
        )
        
      }
      
      models <- model_data %>% 
        group_by(impute) %>% 
        group_map(
          ~ do.call(
            what = lmer,
            args = list(data = .x,
                        formula = model_formula)
          )
        )
      
      browser()
      
      terms <- c("age_centered [0, 5, 10, 15, 20, 25, 30]")
      
      if(.variable != 'overall'){
        terms <- c(terms, .variable)
      } 
      
      effect_data <- models %>% 
        map_dfr(
          .id = 'impute',
          .f = ~ ggeffect(.x, terms = terms) %>% 
            as_tibble() %>%
            mutate(variable = .variable, .before = 1,
                   group = as.character(group),
                   group = recode(group, '1' = 'overall')) %>% 
            relocate(variable, group, .before = 1)
        ) %>% 
        group_by(variable, group, x) %>% 
        summarize(
          pooled = list(
            pool.scalar(
              Q = predicted, 
              U = std.error^2,
              n = nrow(model_data) / nobs(models[[1]]),
              k = 1
            )
          )
        ) %>% 
        mutate(
          predicted = map_dbl(pooled, 'qbar'),
          std.error = sqrt(map_dbl(pooled, 't')), # total variance
          t_df = map_dbl(pooled, 'df'),
          conf.low = predicted + qt(p = 0.025, df = t_df) * std.error,
          conf.high = predicted + qt(p = 0.975, df = t_df) * std.error
        ) %>% 
        select(-pooled, -t_df)
      
      boot_pchanges <- try(map2_dfr(
        .x = models, 
        .y = split(model_data, f = model_data$impute),
        .f = boot_pchange,
        .id = 'impute',
        terms = terms, 
        n_boot = n_boot
      ) %>% 
        group_by(x, group) %>% 
        summarize(
          across(
            .cols = c(est, lwr, upr),
            .fns = mean
          )
        ), silent = TRUE)
      
      if(inherits(boot_pchanges, 'try-error')) browser()
      
      boot_diffs <- NULL
      
      if(.variable != 'overall'){
        
        boot_diffs <- try(map2_dfr(
          .x = models,
          .y = split(model_data, f = model_data$impute),
          .id = 'impute',
          .f = boot_diff,
          terms = terms, 
          n_boot = n_boot
        ) %>% 
          group_by(x, group) %>% 
          summarize(
            across(
              .cols = c(est, lwr, upr),
              .fns = mean
            )
          ), silent = TRUE)
        
        if(inherits(boot_diffs, 'try-error')) browser()
        
      }
      
      # ggplot(effect_data) +
      #   aes(x = x, y = predicted, col = group, group = group) +
      #   geom_point() +
      #   geom_line()
      
      ptrn_trend <- as.character(glue("rcs(age_centered):{.variable}"))
      
      pval_data <- map_dfr(
        .x = models, 
        .f = ~suppressWarnings(tidy(anova(.x)))
      ) %>% 
        group_by(term) %>% 
        summarize(p.value = median(p.value)) %>% 
        mutate(
          variable = .variable,
          p_type = case_when(
            term == variable ~ "diff_baseline",
            str_detect(term, pattern = fixed(ptrn_trend)) ~ "diff_trend",
            TRUE ~ NA_character_
          )
        ) %>% 
        rename(p_value = p.value) %>% 
        relocate(variable, .before = 1)
      
      list(
        effects = effect_data,
        pvalues = pval_data,
        diffs = boot_diffs,
        pchanges = boot_pchanges
      )
      
    }
  )
  
  
  
}


boot_bca <- function(...){
  
  result <- bootBCa(...)
  tibble(lwr = result[1],
         upr = result[2])
  
  
}

# boot_longitudinal <- function(seed, data, id = 'ID'){
#   
#   set.seed(seed)
#   
#   ids <- unique(data[[id]])
#   N <- length(ids)
#   
#   output <- vector(mode = 'list', length = N)
#   
#   sampled_ids <- c()
#   data_split <- split(data, f = data[[id]])
#   
#   for(i in seq(N)){
#     
#     new_id <- .new_id <- sample(ids, size = 1)
#     
#     n_previous_samples <- sum(new_id == sampled_ids)
#     
#     if(n_previous_samples > 0){
#       .new_id <- paste(new_id, letters[n_previous_samples], sep = '_')
#     }
#     
#     sampled_ids <- c(sampled_ids, new_id)
#     
#     new_data <- try(data_split[[new_id]])
#     
#     #if(inherits(new_data, 'try-error')) browser()
#     
#     new_data[[id]] <- .new_id
#     
#     output[[i]] <- new_data
#     
#   }
#   
#   bind_rows(output)
#   
# }

compute_pchange <- function(x) c(0, diff(x) / x[-length(x)]) * 100  


boot_analysis <- function(model, 
                          .model_data,
                          terms, 
                          n_boot){
  
  if(length(terms) > 1){
    boot_analysis_subgroups(model = model, 
                            .model_data = .model_data,
                            terms = terms, 
                            n_boot = n_boot)
  } else {
    boot_analysis_overall(model = model, 
                          .model_data = .model_data,
                          terms = terms, 
                          n_boot = n_boot)
  }
  
}

boot_analysis_subgroups <- function(model, 
                                    .model_data,
                                    terms, 
                                    n_boot){
  
  estimates <- as_tibble(ggeffect(model, terms = terms))  %>% 
    select(x, predicted, group) %>% 
    pivot_wider(names_from = group, values_from = predicted) %>% 
    mutate(across(-x, compute_pchange)) %>% 
    pivot_wider(names_from = x, values_from = -x)
  
  label_grp <- rep(unique(model_data[[terms[2]]]), 
                   each = ncol(estimates)/2)
  
}

boot_analysis_overall <- function(model, 
                                  .model_data,
                                  terms, 
                                  n_boot){
  
  estimates_pchange_long <- model %>% 
    ggeffect(terms = terms) %>% 
    as_tibble()  %>% 
    select(x, predicted) %>% 
    mutate(across(-x, compute_pchange),
           x = paste0("Overall_", x)) %>% 
    rename(est = predicted)
  
  estimates_pchange_wide <- estimates_pchange_long %>% 
    pivot_wider(names_from = x, 
                values_from = -x)
  
  boots <- tibble(
    id = seq(n_boot),
    data = map(
      .x = id,
      .f = boot_longitudinal, 
      id = 'ID',
      data = mutate(.model_data, ID = as.character(ID))
    )
  )
  
  boots_pchange_long <- boots %>% 
    mutate(
      estimates = map(
        .x = data,
        .f = ~ {
          
          boot_model <- do.call(
            what = lmer,
            args = list(data = .x,
                        formula = formula(model))
          )
          
          boot_estimates <- ggeffect(boot_model, terms = terms) %>% 
            as_tibble() %>% 
            select(x, predicted)
          
          mutate(boot_estimates, across(-x, compute_pchange))
          
        }
      )
    ) %>% 
    select(-data) %>% 
    unnest(estimates) %>% 
    group_by(x)
  
  boots_pchange_wide <- boots_pchange_long %>% 
    pivot_wider(names_from = x, values_from = -c(x,id),
                names_prefix = 'Overall_')
  
  map_dfr(set_names(names(estimates_pchange_wide),
                    names(estimates_pchange_wide)),
          ~boot_bca(estimate = estimates_pchange_wide[[.x]],
                    estimates = boots_pchange_wide[[.x]]),
          .id = 'x') %>% 
    left_join(estimates_pchange_long) %>% 
    relocate(est, .before = lwr) %>% 
    mutate(x = as.numeric(str_remove(x, '(.+)_'))) %>% 
    mutate(group = label_grp, .before = est)
  
  
}

boot_pchange <- function(model, 
                         model_data,
                         terms, 
                         n_boot){
  
  if(length(terms) > 1){
    
    estimates <- as_tibble(ggeffect(model, terms = terms))  %>% 
      select(x, predicted, group) %>% 
      pivot_wider(names_from = group, values_from = predicted) %>% 
      mutate(across(-x, compute_pchange)) %>% 
      pivot_wider(names_from = x, values_from = -x)
    
    label_grp <- rep(unique(model_data[[terms[2]]]), 
                     each = ncol(estimates)/2)
    
  } else {
    
    estimates <- as_tibble(ggeffect(model, terms = terms))  %>% 
      select(x, predicted) %>% 
      mutate(across(-x, compute_pchange)) %>% 
      pivot_wider(names_from = x, 
                  values_from = -x, 
                  names_prefix = 'Overall_')
    
    label_grp <- 'Overall'
    
  }
  
  boots <- tibble(
    id = seq(n_boot),
    data = map(
      .x = id,
      .f = boot_longitudinal, 
      id = 'ID',
      data = mutate(model_data, ID = as.character(ID))
    )
  ) %>% 
    mutate(
      estimates = map(
        .x = data,
        .f = ~ {
          
          boot_model <- do.call(
            what = lmer,
            args = list(data = .x,
                        formula = formula(model))
          )
          
          
          if(length(terms) > 1){
            
            boot_estimates <- ggeffect(boot_model, terms = terms) %>% 
              as_tibble() %>% 
              select(x, predicted, group) %>% 
              pivot_wider(values_from = predicted,
                          names_from = group)
            
          } else {
            
            boot_estimates <- ggeffect(boot_model, terms = terms) %>% 
              as_tibble() %>% 
              select(x, predicted) 
            
          }
          
          mutate(boot_estimates, across(-x, compute_pchange))
          
        }
      )
    ) %>% 
    select(-data) %>% 
    unnest(estimates) %>% 
    group_by(x)
  
  estimates_long <- estimates %>% 
    pivot_longer(everything(), names_to = 'x', values_to = 'est')
  
  boots_wide <- boots %>% 
    pivot_wider(names_from = x, values_from = -c(x,id),
                names_prefix = if_else(length(terms) > 1,
                                       true = '',
                                       false = 'Overall_'))
  
  map_dfr(set_names(names(estimates),
                    names(estimates)),
          ~boot_bca(estimate = estimates[[.x]],
                    estimates = boots_wide[[.x]]),
          .id = 'x') %>% 
    left_join(estimates_long) %>% 
    relocate(est, .before = lwr) %>% 
    mutate(x = as.numeric(str_remove(x, '(.+)_'))) %>% 
    mutate(group = label_grp, .before = est)
  
  
}

boot_diff <- function(model, 
                      model_data, 
                      terms, 
                      n_boot = 100){
  
  
  estimate_by_grp <- as_tibble(ggeffect(model, terms = terms)) %>% 
    select(x, predicted, group) %>% 
    pivot_wider(names_from = group, values_from = predicted)
  
  estimate_by_grp <- switch(
    terms[2],
    'sex' = mutate(estimate_by_grp, statistic = Male - Female),
    'race' = mutate(estimate_by_grp, statistic = White - Black),
    'meds_bp' = mutate(estimate_by_grp, statistic = yes - no),
    'paying_for_basics' = mutate(estimate_by_grp,
                                 statistic = somewhat_hard - not_very_hard),
    'educ' = mutate(estimate_by_grp, 
                    statistic = hs_ged_or_less - assoc_or_more)
  ) %>% 
    select(x, statistic) 
  
  label_grp <- switch(
    terms[2],
    'sex' = "Male - Female",
    'race' = "White - Black",
    'meds_bp' = "Yes - No",
    'paying_for_basics' = "Somewhat - not very hard",
    'educ' = 'HS/GED or less - Associates or more'
  )
  
  estimate_wide <- estimate_by_grp %>% 
    pivot_wider(names_from = x, 
                values_from = statistic,
                names_prefix = 'age_')
  

  boots <- try(
    tibble(
      id = seq(n_boot),
      data = map(
        .x = id,
        .f = boot_longitudinal, 
        id = 'ID',
        data = mutate(model_data, ID = as.character(ID))
      )
    ) %>% 
      mutate(
        estimates = map(
          .x = data,
          .f = ~ {
            
            boot_model <- do.call(
              what = lmer,
              args = list(data = .x,
                          formula = formula(model))
            )
            
            ggeffect(boot_model, terms = terms) %>% 
              as_tibble() %>% 
              select(x, predicted, group) %>% 
              pivot_wider(values_from = predicted,
                          names_from = group)
          }
        )
      ) %>% 
      select(-data) %>% 
      unnest(estimates) %>% 
      group_by(x), 
    silent = TRUE)
  
  if(inherits(boots, 'try-error')) browser()
  
  boots_wide <- try(
    switch(
      terms[2],
      'sex' = mutate(boots, statistic = Male - Female),
      'race' = mutate(boots, statistic = White - Black),
      'meds_bp' = mutate(boots, statistic = yes - no),
      'paying_for_basics' = mutate(boots, 
                                   statistic = somewhat_hard - not_very_hard),
      'educ' = mutate(boots, statistic = hs_ged_or_less - assoc_or_more)
    ) %>% 
      select(id, x, statistic) %>% 
      pivot_wider(names_from = x, 
                  values_from = statistic,
                  names_prefix = 'age_'),
    silent = TRUE
  )
  
  if(inherits(boots_wide, 'try-error')) browser()
  
  map_dfr(set_names(names(estimate_wide),
                    names(estimate_wide)),
          ~boot_bca(estimate = estimate_wide[[.x]],
                    estimates = boots_wide[[.x]]),
          .id = 'x') %>% 
    mutate(est = estimate_by_grp$statistic, .before = lwr) %>% 
    mutate(x = as.numeric(str_remove(x, 'age_')),
           group = label_grp,
           .before = est)
  
}