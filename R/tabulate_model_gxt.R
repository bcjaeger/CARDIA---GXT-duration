##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data_table
tabulate_model_gxt <- function(data_gxt_tables,
                               outcome_labels) {

  data_gxt_table_values <- data_gxt_tables %>% 
    mutate(age = as.numeric(age)) %>% 
    filter(age %in% seq(0, 30, by = 5)) %>% 
    mutate(tbl_value = table_glue("{est}\n({lwr}, {upr})")) %>% 
    select(-c(est, lwr, upr)) %>% 
    pivot_wider(names_from = age,
                values_from = tbl_value,
                names_prefix = 'age_') %>% 
    mutate(across(starts_with('pval'), table_pvalue))
  
  inline_estimates <- data_gxt_table_values %>%
    mutate(
      across(
        .cols = starts_with('age_'), 
        .fns = str_replace, 
        pattern = '\\n', 
        replacement = ' '
      )
    ) %>% 
    as_inline(
      tbl_variables = c('outcome', 
                        'variable',
                        'group',
                        'subset_sex',
                        'model_type',
                        'analysis'),
      tbl_values = c(
        "age_0",
        "age_5",
        "age_10",
        "age_15",
        "age_20",
        "age_25",
        "pval_base",
        "pval_trend"
      )
    )

  put_minus_last <- function(data){

    x <- data$group
    x_unique <- unique(as.character(x))
    x_minus_value <- x_unique[str_detect(x_unique, fixed(' - '))]
    x_values_no_minus <- setdiff(x_unique, x_minus_value)
    x_values_ordered <- c(x_values_no_minus, x_minus_value)
    data$group <- factor(data$group, levels = x_values_ordered)
    output <- data[order(data$group), ]
    output$group <- as.character(output$group)
    output

  }
  
  tbl_estimates <- data_gxt_table_values %>%
    mutate(
      variable = recode(
        variable,
        race = "Race",
        sex = "Sex",
        bmi_cat = 'BMI classification at baseline',
        pa_self_cat = "Self-reported physical activity at baseline",
        educ = "Education at baseline",
        paying_for_basics = 'Difficulty paying for basics',
        alcohol = "Alcohol use",
        smoke = "Smoking status at baseline",
        health_self = "Self reported health",
        overall = "Overall"
      ),
      group = recode(
        group,
        overall = "Overall",
        not_very_hard = 'Not very hard',
        somewhat_hard = 'Somewhat hard',
        under_or_normal = 'Underweight or normal',
        over_or_obese = 'Overweight or obese',
        over_over = "Overweight or obese throughout",
        over_under = "Overweight/obese then underweight/normal",
        under_over = "Underweight/normal then overweight/obese",
        under_under = "Underweight or normal throughout",
        meeting_guidelines = 'Meeting PA guidelines',
        not_meeting_guidelines = 'Not meeting PA guidelines',
        alc_yes = 'Drinks alcohol',
        alc_no = 'Does not drink alcohol',
        former_current = 'Former/current smoker',
        never = 'Never smoked',
        excellent_good = 'Excellent/good',
        fair_poor = 'Fair/poor',
        assoc_or_more = 'Associate\'s or more',
        hs_ged_or_less = 'Highschool/GED or less',
        White_minus_Black = 'White - Black',
        Male_minus_Female = 'Male - Female',
        under_minus_over = 'Underweight/normal - overweight/obese',
        meeting_minus_not = 'Meeting - not meeting PA guidelines',
        hsged_minus_assoc = 'Highschool/GED - Associate\'s',
        Somewhat_minus_notvery = 'Somewhat - not very hard',
        alcyes_minus_alcno = 'Drinks - does not drink alcohol',
        never_minus_formercurrent = 'Never - former/current',
        excellentgood_minus_fairpoor = 'Excellent/good - fair/poor'
      )
    ) %>%
    #select(-age_30, -age_25) %>%
    relocate(pval_base, pval_trend,pval_duration, .after = age_30) %>% 
    split(f = list(.$variable, .$outcome)) %>%
    map(put_minus_last) %>%
    bind_rows() %>% 
    mutate(variable = if_else(model_type == 'timevary',
                              paste(variable, '(duration adjusted)'), 
                              variable)) %>% 
    split(f = list(.$outcome, .$subset_sex, .$analysis)) %>% 
    map(select, 
        -outcome, 
        -subset_sex, 
        -model_type, 
        -analysis) %>% 
    map(
      mutate, 
      variable = factor(
        variable, 
        levels = c(
          "Overall",
          "Race",
          "Sex",
          "Alcohol use",
          "Alcohol use (duration adjusted)",
          "Education at baseline",
          "Education at baseline (duration adjusted)",
          "BMI classification at baseline",
          "BMI classification at baseline (duration adjusted)",
          "Difficulty paying for basics",
          "Difficulty paying for basics (duration adjusted)",
          "Self-reported physical activity at baseline",
          "Self-reported physical activity at baseline (duration adjusted)",
          "Self reported health",
          "Self reported health (duration adjusted)",
          "Smoking status at baseline",
          "Smoking status at baseline (duration adjusted)"
        )
      )
    ) %>% 
    map(arrange, variable) %>% 
    map(
      ~ .x %>% 
        as_grouped_data(groups = 'variable') %>%
        slice(-1) %>%
        as_flextable(hide_grouplabel = TRUE) %>%
        add_header_row(values = c("Group",
                                  "Age, years",
                                  "P-value\nfor difference"),
                       colwidths = c(1, 7, 3)) %>%
        theme_box() %>%
        set_header_labels(
          group = 'Group',
          age_0 = '20',
          age_5 = '25',
          age_10 = '30',
          age_15 = '35',
          age_20 = '40',
          age_25 = '45',
          age_30 = '50',
          pval_base = 'At age\n20 years',
          pval_trend = 'Over follow-up due to baseline status',
          pval_duration = 'Over follow-up due to duration'
        ) %>%
        align(align = 'center', part = 'all') %>%
        align(align = 'left', j = 1, part = 'all') %>%
        merge_v(target = "pval_base") %>%
        merge_v(target = "pval_trend") %>%
        merge_v(target = "pval_duration") %>% 
        merge_at(j = 1, part = 'header') %>%
        width(width = 0.8, j = 9:11) %>% 
        width(width = 1, j = 2:8) %>%
        width(width = 1.3, j = 1) %>%
        bg(i = ~ !is.na(variable), bg = 'grey80') %>%
        italic(i = ~ !is.na(variable), italic = TRUE)
    )
  

  list(inline = inline_estimates,
       table = tbl_estimates)

}
