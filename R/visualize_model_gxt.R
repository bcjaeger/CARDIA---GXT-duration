##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
visualize_model_gxt <- function(data_gxt_tables,
                                outcome_labels,
                                max_age = 50) {
  
  data_prepped_for_plots <- data_gxt_tables %>% 
    ungroup() %>% 
    filter(!str_detect(group, '_minus_')) %>% 
    mutate(
      age = as.numeric(age) + 20,
      across(starts_with('pval'), table_pvalue)
    ) %>% 
    group_by(outcome, variable, subset_sex, model_type) %>% 
    nest()
  
  variable_labels <- c(
    bmi_cat = 'Body mass index',
    educ = 'Education at\nYear 20 exam',
    overall = 'Overall',
    pa_self_cat = 'Meeting physical\nactivity guidelines',
    paying_for_basics = 'Difficulty paying\nfor basics',
    race = 'Race', 
    sex = 'Sex',
    alcohol = 'Alcohol use',
    smoke = 'Smoking status',
    health_self = 'Self-reported health'
  )
  
  category_labels <- c(
    alc_no = 'No',
    alc_yes = 'Yes',
    assoc_or_more = "Associate's or higher",
    Black = "Black",
    excellent_good = 'Excellent or good',
    fair_poor = 'Fair or poor',
    Female = "Female",
    former_current = 'Current or former',
    hs_ged_or_less = "Highschool/GED or less",
    Male = "Male",
    meeting_guidelines = "Yes",
    never = 'Never',
    not_meeting_guidelines = "No",
    not_very_hard = "Not very hard",
    over_or_obese = "Overweight/obese",
    overall = "Overall",
    somewhat_hard = "Somewhat hard",
    under_or_normal = "Underweight/normal",
    White = "White"
  )
  
  duration_labels <- c(
    alc_yes = 'drinking alcohol',
    fair_poor = 'with fair or poor self-rated health',
    former_current = 'smoking',
    not_meeting_guidelines = 'not meeting PA guidelines',
    somewhat_hard = 'where paying for basics was somewhat hard',
    over_or_obese = 'overweight or obese',
    hs_ged_or_less = 'with Highschool education or GED'
  )
  
  subset_labels <- c(
    None = "Data presented are from the overall CARDIA cohort",
    Male = "Data presented are from male participants in the CARDIA cohort",
    Female = "Data presented are from female participants in the CARDIA cohort"
  )
  
  data_plots <- data_prepped_for_plots %>%
    mutate(
      fname = paste(outcome, 
                    variable, 
                    subset_sex, 
                    model_type, 
                    sep = '_'),
      fname = paste0('img/', fname, '.png'),
      fig = pmap(
        .l = list(
          outcome, 
          variable,  
          subset_sex, 
          model_type, 
          data
        ),
        .f = function(..outcome, 
                      ..variable, 
                      ..subset_sex, 
                      ..model_type, 
                      ..data){
          # if(..variable == 'bmi_cat' & ..model_type == 'timevary') browser()
          
          ..data <- ..data %>% 
            filter(age <= max_age)
          
          if(..model_type == 'baseline'){
            ..model_type_label <- 
              paste("Estimates are adjusted for age, center,",
                    "education at year 20, race, and sex")
          }
          
          if(..model_type == 'timevary'){
            
            tmp <- ..data %>% 
              slice(n()) %>% 
              pull(group)
            
            ..model_type_label <- 
              paste("Estimates are adjusted for age, center,",
                    "education at year 20, race, sex,\nand",
                    "number of years", duration_labels[tmp])
            
          }
          
          label_data <- ..data %>% 
            mutate(
              label = glue(
                "P-value at baseline: {pval_base}\\
              \nP-value over follow-up: {pval_trend}"
              )
            ) %>% 
            group_by(analysis) %>% 
            # use 11 to get age = 30
            slice(11) %>% 
            mutate(est = if_else(..outcome == 'gxt_duration',
                                 true = 150, 
                                 false = 110)) %>% 
            separate(col = 'analysis', 
                     into = c('impute', 'mn'), 
                     sep = '\\.\\.') %>% 
            mutate(
              impute = recode(
                impute, 
                gxt_complete = 'Exclude missing GXT duration',
                gxt_impute = 'Impute missing GXT duration'
              ),
              mn = recode(
                mn, 
                'mn_exclude' = 'Remove Y7 Minnesota data',
                'mn_include' = 'Include Y7 Minnesota data'
              )
            )
          
          fig_data <- ..data %>% 
            mutate(group = recode(group, !!!category_labels)) %>% 
            separate(col = 'analysis', 
                     into = c('impute', 'mn'), 
                     sep = '\\.\\.') %>% 
            mutate(
              impute = recode(
                impute, 
                gxt_complete = 'Exclude missing GXT duration',
                gxt_impute = 'Impute missing GXT duration'
              ),
              mn = recode(
                mn, 
                'mn_exclude' = 'Remove Y7 Minnesota data',
                'mn_include' = 'Include Y7 Minnesota data'
              )
            )
          
          colors <- c("darkorange","purple","cyan4","cornflowerblue")
          
          if(..variable == 'overall'){
            fig_base <- ggplot(fig_data) + 
              aes(x = age, y = est, ymin = lwr, ymax = upr)
          } else {
            n_groups <- length(unique(na.omit(fig_data$group)))
            fig_base <- ggplot(fig_data) + 
              aes(x = age, y = est, ymin = lwr, ymax = upr, col = group) + 
              scale_color_manual(values = colors[seq(n_groups)])
          }
          
          # make all the figures go from 0 to 900 (duration) 
          # and 100 to 160 (heart rate)
          
          if(..outcome == 'gxt_duration'){
            ylim_vals <- c(0, 900)
          } else if (..outcome == 'gxt_hr_s2'){
            ylim_vals <- c(100, 160)
          }
          
          
          fig <- fig_base + 
            geom_pointrange(position = position_dodge(width = 1/3),
                            size = 1/4) + 
            labs(y = outcome_labels[..outcome],
                 x = 'Age, years',
                 col = variable_labels[..variable],
                 title = subset_labels[..subset_sex],
                 subtitle = ..model_type_label) +
            coord_cartesian(ylim = ylim_vals) +
            facet_grid(impute~mn) + 
            theme(text = element_text(size = 15),
                  #panel.border = element_blank(),
                  panel.grid.major.y = element_line(color = 'grey90'))
          
          if(fig_data$pval_base[1] != '--'){
            fig <- fig + 
              geom_text(data = label_data,
                        aes(x = age, y = est, label = label),
                        inherit.aes = FALSE,
                        size = 5) 
          }
          
          fig
          
        }
      )
    )
  
  for(i in seq(nrow(data_plots))){
    
    ggsave(filename = data_plots$fname[i], 
           plot = data_plots$fig[[i]],
           device = png(),
           width = 12, 
           height = 8)
    
    dev.off()
    
  }
  
  data_plots
  
}

# gg_data <- bind_rows(
#   Males = data_model_age_male$long,
#   Females = data_model_age_female$long,
#   .id = 'sex'
# ) %>% 
#   filter(group %in% c("Black", "White")) %>% 
#   unite(col = 'variable', group, sex, sep = ' ')
# 
# ggplot(gg_data) + 
#   aes(x = x, y = est, col = variable) +
#   facet_wrap(~outcome) +
#   geom_line()
