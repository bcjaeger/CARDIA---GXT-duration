##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data_gxt_included

##  COMMENTED OUT as this function is deprecated.
##  the MI version of this function is now preferred
# 
# model_gxt_age <- function(data_gxt_included,
#                           n_boot = 10,
#                           .outcomes,
#                           .variables,
#                           .control,
#                           .intercept_only = NULL,
#                           .subset_sex = NULL,
#                           .subset_race = NULL) {
#   
#   model_output <- map(
#     .x = set_names(.outcomes, .outcomes),
#     .f = ~.model_gxt_age(
#       data_gxt_included = data_gxt_included,
#       n_boot = n_boot,
#       .outcome = .x,
#       .variables = set_names(.variables, .variables),
#       .control = .control,
#       .intercept_only = .intercept_only,
#       .subset_sex = .subset_sex,
#       .subset_race = .subset_race
#     )
# 
#   )
#   
#   model_effects <- get_element(model_output, 'effects') %>% 
#     map(bind_rows) %>% 
#     bind_rows(.id = 'outcome') %>% 
#     transmute(
#       outcome, variable, group, x,
#       table_value = table_glue("{predicted}\n({conf.low}, {conf.high})")
#     )
#   
#   model_pval <- get_element(model_output, 'pvalues') %>% 
#     map(bind_rows) %>% 
#     bind_rows(.id = 'outcome') %>% 
#     select(outcome, variable, p_value, p_type) %>% 
#     drop_na() %>% 
#     pivot_wider(names_from = p_type, values_from = p_value)
#   
#   boot_diffs <- get_element(model_output, 'diffs') %>% 
#     map(bind_rows, .id = 'variable') %>% 
#     bind_rows(.id = 'outcome') %>% 
#     transmute(
#       outcome, variable, group, x,
#       table_value = table_glue("{est}\n({lwr}, {upr})")
#     )
#   
#   bind_rows(model_effects, boot_diffs) %>% 
#     arrange(outcome, variable, group) %>% 
#     pivot_wider(names_from = x, 
#                 names_prefix = 'age_',
#                 values_from = table_value) %>% 
#     left_join(model_pval) %>% 
#     mutate(diff_baseline = table_pvalue(diff_baseline),
#            diff_trend = table_pvalue(diff_trend))
# 
# }
# 
# get_element <- function(.x, element_name){
#   
#   if(element_name %in% names(.x)) return(.x[[element_name]])
#   
#   if(is_tibble(.x)) return(NULL)
#   
#   map(.x, get_element, element_name = element_name)
#   
# }
# 
# .model_gxt_age <- function(data_gxt_included, 
#                            n_boot,
#                            .outcome, 
#                            .variables,
#                            .control,
#                            .intercept_only,
#                            .subset_sex = NULL,
#                            .subset_race = NULL){
#   
#   model_data <- data_gxt_included %>% 
#     bind_rows(.id = 'impute')
#   
#   if(!is.null(.subset_sex)){
#     model_data <- filter(model_data, sex == .subset_sex)
#     .variables <- setdiff(.variables, 'sex')
#     .variables <- set_names(.variables, .variables)
#     .control <- setdiff(.control, 'sex')
#   }
#   
#   if(!is.null(.subset_race)){
#     model_data <- filter(model_data, race == .subset_race)
#     .variables <- setdiff(.variables, 'race')
#     .variables <- set_names(.variables, .variables)
#     .control <- setdiff(.control, 'race')
#   }
#   
#   map(
#     .x = .variables,
#     .f = function(.variable){
#       
#       ..control <- .control %>% 
#         setdiff(.variable) %>% 
#         paste(collapse = ' + ')
#       
#       ..intercept_only <- .intercept_only %>% 
#         setdiff(.variable) %>% 
#         paste(collapse = ' + ')
#       
#       if(.variable == 'overall'){
#         
#         model_formula <- as.formula(
#           glue(
#             "{.outcome} ~ rcs(age_centered)*{..control} + {..intercept_only} + (1|ID)"
#           )
#         )
#         
#       } else {
#         
#         model_formula <- as.formula(
#           glue(
#             "{.outcome} ~ rcs(age_centered)*({.variable}+{..control}) + {..intercept_only} + (1|ID)"
#           )
#         )
#         
#       }
#       
#       browser()
#       
#       
#       
#       model <- do.call(
#         what = lmer,
#         args = list(data = model_data,
#                     formula = model_formula)
#       )
#       
#       terms <- c("age_centered [0, 5, 10, 15, 20, 25, 30]")
#       
#       if(.variable != 'overall') terms <- c(terms, .variable)
#       
#       effect_data <- ggeffect(model, terms = terms) %>% 
#         as_tibble() %>%
#         mutate(variable = .variable, .before = 1,
#                group = as.character(group),
#                group = recode(group, '1' = 'overall')) %>% 
#         relocate(variable, group, .before = 1)
#       
#       boot_diffs <- NULL
#       
#       if(.variable != 'overall'){
# 
#         boot_diffs <- boot_diff(model = model,
#                                 model_data = model_data,
#                                 terms = terms,
#                                 n_boot = n_boot)
# 
#       }
#       
#       # ggplot(effect_data) +
#       #   aes(x = x, y = predicted, col = group, group = group) +
#       #   geom_point() +
#       #   geom_line()
#       
#       ptrn_trend <- as.character(glue("rcs(age_centered):{.variable}"))
#       
#       pval_data <- suppressWarnings(tidy(anova(model))) %>% 
#         mutate(
#           variable = .variable,
#           p_type = case_when(
#             term == variable ~ "diff_baseline",
#             str_detect(term, pattern = fixed(ptrn_trend)) ~ "diff_trend",
#             TRUE ~ NA_character_
#           )
#         ) %>% 
#         rename(p_value = p.value) %>% 
#         relocate(variable, .before = 1)
#       
#       list(
#         effects = effect_data,
#         pvalues = pval_data,
#         diffs = boot_diffs
#       )
#       
#     }
#   )
#   
#   
#   
# }
# 
# 
# boot_bca <- function(...){
#   
#   result <- bootBCa(...)
#   tibble(lwr = result[1],
#          upr = result[2])
#   
#   
# }
# 
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
#     if(inherits(new_data, 'try-error')) browser()
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
# 
# boot_diff <- function(model, 
#                       model_data, 
#                       terms, 
#                       n_boot = 100){
#   
#   estimate_by_grp <- as_tibble(ggeffect(model, terms = terms)) %>% 
#     select(x, predicted, group) %>% 
#     pivot_wider(names_from = group, values_from = predicted)
#   
#   estimate_by_grp <- switch(
#     terms[2],
#     'sex' = mutate(estimate_by_grp, statistic = Male - Female),
#     'race' = mutate(estimate_by_grp, statistic = White - Black)                  
#   ) %>% 
#     select(x, statistic) 
#   
#   label_grp <- switch(
#     terms[2],
#     'sex' = "Male - Female",
#     'race' = "White - Black"                  
#   )
#   
#   estimate_wide <- estimate_by_grp %>% 
#     pivot_wider(names_from = x, 
#                 values_from = statistic,
#                 names_prefix = 'age_')
#   
# 
#   boots <- tibble(id = seq(n_boot),
#                   data = map(.x = id,
#                              .f = boot_longitudinal, 
#                              id = 'ID',
#                              data = model_data)) %>% 
#     mutate(
#       estimates = map(
#         .x = data,
#         .f = ~ {
#           
#           boot_model <- do.call(
#             what = lmer,
#             args = list(data = .x,
#                         formula = formula(model))
#           )
#           
#           ggeffect(boot_model, terms = terms) %>% 
#             as_tibble() %>% 
#             select(x, predicted, group) %>% 
#             pivot_wider(values_from = predicted,
#                         names_from = group)
#         }
#       )
#     ) %>% 
#     select(-data) %>% 
#     unnest(estimates) %>% 
#     group_by(x)
#   
#   boots_wide <- switch(
#     terms[2],
#     'sex' = mutate(boots, statistic = Male - Female),
#     'race' = mutate(boots, statistic = White - Black)                  
#   ) %>% 
#     select(id, x, statistic) %>% 
#     pivot_wider(names_from = x, 
#                 values_from = statistic,
#                 names_prefix = 'age_')
#   
#   map_dfr(set_names(names(estimate_wide),
#                     names(estimate_wide)),
#           ~boot_bca(estimate = estimate_wide[[.x]],
#                     estimates = boots_wide[[.x]]),
#           .id = 'x') %>% 
#     mutate(est = estimate_by_grp$statistic, .before = lwr) %>% 
#     mutate(x = as.numeric(str_remove(x, 'age_')),
#            group = label_grp,
#            .before = est)
#   
# }