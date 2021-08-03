##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

make_gxt_models <- function(.boot, .impute, .analysis, data){
  
  conflicted::conflict_prefer("filter", "dplyr")
  conflicted::conflict_prefer("lmer", "lmerTest")
  conflicted::conflict_prefer("summarize", "dplyr")
  conflicted::conflict_prefer("as_flextable", "flextable")
  conflicted::conflict_prefer("lag", "dplyr")
  
  
  ..data <- filter(data, 
                   boot == .boot, 
                   impute == .impute,
                   analysis == .analysis) %>% 
    pull(data) %>% 
    .[[1]] %>% 
    mutate(
      bmi_cat = factor(
        bmi_cat,
        levels = c('under_or_normal',
                   'over_or_obese')
      ),
      pa_self_cat = factor(
        pa_self_cat,
        levels = c('meeting_guidelines',
                   'not_meeting_guidelines')
      ),
      alcohol = factor(
        alcohol,
        levels = c("alc_no", "alc_yes")
      ),
      paying_for_basics = factor(
        paying_for_basics,
        levels = c("not_very_hard",
                   "somewhat_hard")
      ),
      educ = factor(
        educ,
        levels = c('assoc_or_more',
                   'hs_ged_or_less')
      ),
      health_self = factor(
        health_self,
        levels = c("excellent_good",
                   "fair_poor")
      ),
      meds_bp = factor(
        meds_bp,
        levels = c("no", "yes")
      )
    )
  
  model_inputs <- 
    expand.grid(outcome = c("gxt_duration", "gxt_hr_s2"), 
                variable = c("overall", 
                             "sex", 
                             "race", 
                             "paying_for_basics", 
                             "educ",
                             "bmi_cat", 
                             "pa_self_cat",
                             "alcohol",
                             "smoke",
                             "health_self"),
                subset_sex = c('None', 'Male', 'Female'),
                stringsAsFactors = FALSE) %>% 
    as_tibble() %>% 
    mutate(
      variable_duration = case_when(
        variable == "bmi_cat" ~ "duration_bmi_cat_over_or_obese",
        variable == "pa_self_cat" ~ "duration_pa_self_cat_not_meeting_guidelines",
        variable == "paying_for_basics" ~ "duration_paying_for_basics_somewhat_hard",
        variable == "alcohol" ~ "duration_alcohol_alc_yes",
        variable == "smoke" ~ "duration_smoke_current" ,
        variable == "health_self" ~ "duration_health_self_fair_poor",
        variable == "educ" ~ "duration_educ_hs_ged_or_less",
        TRUE ~ NA_character_
      ),
      .after = variable
    ) %>% 
    arrange(outcome) %>% 
    filter(!(variable == 'sex' & subset_sex != 'None')) %>% 
    mutate(
      control = list(c('CENTER', 'sex', 'race', 'educ')), 
      control = map2(control, variable, setdiff),
      control = if_else(
        subset_sex == 'None',
        true = control,
        false = map(control, setdiff, "sex")
      ),
      control = map_chr(control, paste, collapse = " + "),
      terms = list(c("age_centered [0:30]")),
      terms = if_else(
        condition = variable == 'overall',
        true = terms, 
        false = map2(terms, variable, c)
      ),
      formula_model_1 = pmap_chr(
        .l = list(outcome, variable, control),
        .f = function(.y, .x, .z){
          if(.x == 'overall') out <- glue(
            "{.y} ~ ns(age_centered, df=3) * ({.z}) + (1 | ID)"
          )
          if(.x != 'overall') out <- glue(
            "{.y} ~ ns(age_centered, df=3) * ({.x} + {.z}) + (1 | ID)"
          )
          as.character(out)
        }
      ),
      formula_model_2 = pmap_chr(
        .l = list(outcome, variable, variable_duration, control),
        .f = function(.y, .x, ..x, .z){
          if(is.na(..x) | .x == 'overall') return(NA_character_)
          as.character(
            glue(
              "{.y} ~ ns(age_centered, df=3) * ({.x} + {.z}) + {..x} + (1 | ID)"
            )
          )
        }
      )
    )
  
  model_inputs <- model_inputs %>% 
    pivot_longer(cols = starts_with('formula'),
                 names_to = 'model_type',
                 values_to = 'model_formula',
                 names_prefix = 'formula_model_') %>% 
    mutate(
      terms = pmap(
        .l = list(model_type, terms, variable_duration),
        .f = function(.x, .y, .z)
          if_else( 
            .x == 2, 
            true = list(c(.y, paste0(.z, ' [0:30]'))), 
            false = list(.y)
          )
      )
    ) %>% 
    select(-variable_duration) %>%
    drop_na() 
  
  model_fits <- model_inputs %>% 
    mutate(
      model = map2(
        .x = model_formula,
        .y = subset_sex,
        .f = ~{
          
          mdl <- do.call(
            what = lmer,
            args = list(
              data = switch(
                .y, 
                'None' = ..data,
                'Female' = filter(..data, sex == 'Female'),
                'Male' = filter(..data, sex == 'Male')
              ), 
              formula = as.formula(.x))
          )
          
          mdl
          
        } 
      )
    )
  
  model_output <- model_fits %>% 
    mutate(
      duration_catg = recode(
        variable, 
        "bmi_cat" = "over_or_obese",
        "pa_self_cat" = "not_meeting_guidelines",
        "paying_for_basics" = "somewhat_hard",
        "alcohol" = "alc_yes",
        "smoke" = "former_current",
        "health_self" = "fair_poor",
        "educ" = "hs_ged_or_less"
      )
    ) %>% 
    mutate(
      effect_estimates = pmap(
        .l = list(model, terms, duration_catg),
        .f = function(.x, .y, .z) {
          
          uses_duration <- any(str_detect(.y[[1]], pattern = '^duration'))
          
          if(uses_duration){
            
            #if(.y[[1]][2] == 'smoke') browser()
            
            out <- 
              ggeffect(.x, terms = .y[[1]]) %>% 
              as_tibble() %>% 
              transmute(age_centered = x,
                        predicted, 
                        group,
                        duration = facet) %>% 
              filter(group != .z & duration == '0' |
                       group == .z & duration == age_centered)
            
          }
          
          
          if(!uses_duration)
            out <- ggeffect(.x, terms = .y[[1]]) %>% 
              as_tibble() %>% 
              transmute(age_centered = x,
                        predicted, 
                        group = recode(group, '1' = 'overall'))
          
          
          # plt <- ggplot(out) +
          #   aes(x = age_centered, y = predicted, col = group) +
          #   geom_point()
          # 
          # print(plt)
          
          out
          
        } 
      ),
      effect_pvalues = map(
        .x = model, 
        .f = ~suppressWarnings(tidy(anova(.x))) %>% 
          select(term, p.value)
      )
    ) %>% 
    select(-c(control, terms, model_formula, model)) %>% 
    mutate(model_type = recode(model_type,
                               '1' = 'baseline',
                               '2' = 'timevary')) %>% 
    mutate(boot = .boot,
           analysis = .analysis,
           impute = .impute, 
           .before = 1)
  
  model_output
  
}




