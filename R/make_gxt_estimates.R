##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data_gxt_models
make_gxt_estimates <- function(data_gxt_models) {

  data_bca <- data_gxt_models %>%
    select(-effect_pvalues) %>%
    unnest(effect_estimates) %>%
    group_by(boot,
             outcome,
             variable,
             subset_sex,
             age_centered,
             group,
             model_type,
             analysis) %>%
    # The MI point estimate from each bootstrap is used for BCA.
    summarize(predicted = median(predicted), .groups = 'drop')
  
  data_bca_grp_diff <- data_bca %>% 
    split(f = list(data_bca$outcome,
                   data_bca$variable)) %>% 
    map(droplevels) %>% 
    map(pivot_wider, names_from = group, values_from = predicted) %>% 
    map(
      ~ switch(
          .x$variable[1],
          'sex' = mutate(.x, stat = Male - Female) %>% 
            select(-Male, -Female),
          'race' = mutate(.x, stat = White - Black) %>% 
            select(-Black, -White),              
          'paying_for_basics' = .x %>% 
            mutate(stat = somewhat_hard - not_very_hard) %>% 
            select(-somewhat_hard, -not_very_hard),
          'educ' = mutate(.x, stat = hs_ged_or_less - assoc_or_more) %>% 
            select(-hs_ged_or_less, -assoc_or_more),
          'bmi_cat' = .x %>% 
            mutate(stat = under_or_normal - over_or_obese) %>% 
            select(-under_or_normal, -over_or_obese),
          'pa_self_cat' = .x %>% 
            mutate(stat = meeting_guidelines - not_meeting_guidelines) %>% 
            select(-meeting_guidelines, -not_meeting_guidelines),
          'alcohol' = .x %>% 
            mutate(stat = alc_yes - alc_no) %>% 
            select(-alc_no, -alc_yes),
          'smoke' = .x %>% 
            mutate(stat = never - former_current) %>% 
            select(-never, -former_current),
          'health_self' = .x %>% 
            mutate(stat = excellent_good - fair_poor) %>% 
            select(-excellent_good, -fair_poor),
          'overall' = NULL
        )
    ) %>% 
    discard(is.null) %>% 
    bind_rows() %>% 
    #filter(age_centered %in% seq(0, 30, by = 5)) %>% 
    pivot_wider(values_from = stat, 
                names_from = age_centered,
                names_prefix = 'age_') %>% 
    split(f = list(.$outcome, 
                   .$variable, 
                   .$subset_sex,
                   .$model_type,
                   .$analysis)) %>% 
    discard(.p = ~ nrow(.x) == 0) %>% 
    map_dfr(boot_bca_df,
            stat_names = paste0('age_', seq(0, 30, by = 1)),
            estimate_index = 0) %>% 
    mutate(
      group = case_when(
        variable == 'sex' ~ "Male_minus_Female" ,
        variable == 'race' ~ "White_minus_Black",
        variable == 'paying_for_basics' ~ "Somewhat_minus_notvery" ,
        variable == 'educ' ~ "hsged_minus_assoc",
        variable == 'bmi_cat' ~ 'under_minus_over',
        variable == 'pa_self_cat' ~ 'meeting_minus_not', 
        variable == 'alcohol' ~ 'alcyes_minus_alcno',
        variable == 'smoke' ~ 'never_minus_formercurrent',
        variable == 'health_self' ~ 'excellentgood_minus_fairpoor',
        variable == 'overall' ~ NA_character_
      ),
      .after = variable
    )
  
  # group = case_when(
  #   variable == 'sex' ~ "Male - Female" ,
  #   variable == 'race' ~ "White - Black",
  #   variable == 'meds_bp' ~ "Yes - No" ,
  #   variable == 'paying_for_basics' ~ "Somewhat - not very hard" ,
  #   variable == 'educ' ~ "HS/GED or less - Associates or more" ,
  #   variable == 'overall' ~ NA_character_
  # )
  
  data_bca_grp_vals <- data_bca %>% 
    # filter(age_centered %in% seq(0, 30, by = 5)) %>% 
    split(f = list(.$outcome, 
                   .$variable, 
                   .$subset_sex, 
                   .$group,
                   .$model_type,
                   .$analysis)) %>% 
    discard(~nrow(.x) == 0) %>% 
    map(pivot_wider, 
        names_from = age_centered, 
        names_prefix = 'age_',
        values_from = predicted) %>% 
    map_dfr(boot_bca_df,
            stat_names = paste0('age_', seq(0, 30, by = 1)),
            estimate_index = 0)
  
  data_bca_grp_pchange <- data_bca %>% 
    filter(age_centered %in% seq(0, 30, by = 5)) %>% 
    split(f = list(.$outcome, 
                   .$variable, 
                   .$subset_sex, 
                   .$group,
                   .$model_type,
                   .$analysis)) %>% 
    discard(~nrow(.x) == 0) %>% 
    map(group_by, boot) %>% 
    map(mutate, across(predicted, compute_pchange)) %>% 
    map(ungroup) %>% 
    map(pivot_wider, 
        names_from = age_centered, 
        names_prefix = 'age_',
        values_from = predicted) %>% 
    map_dfr(boot_bca_df,
            stat_names = paste0('age_', seq(0, 30, by = 5)),
            estimate_index = 0)
  
  list(
    values = bind_rows(data_bca_grp_diff, data_bca_grp_vals),
    perc_change = data_bca_grp_pchange
  )
  
  
  

}
