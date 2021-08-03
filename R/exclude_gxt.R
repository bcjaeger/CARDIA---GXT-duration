##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data_gxt_all
exclude_gxt <- function(data_gxt_all) {

  # data before any exclusions
  e0 <- data_gxt_all %>% 
    mutate(across(everything(), as.character)) %>% 
    pivot_longer(cols = -c(ID, CENTER, race, sex)) %>% 
    separate(name, into = c('variable', 'exam'), sep = '\\.\\.') %>% 
    pivot_wider(names_from = variable, values_from = value) %>% 
    mutate(
      across(
        .cols = c(exam_age,
                  gxt_duration,
                  gxt_hr_s2,
                  gxt_pe_max,
                  gxt_hr_max,
                  hr_max_predicted,
                  bmi,
                  pa_self),
        .fns = as.numeric
      ),
      gxt_valid = as.logical(gxt_valid),
      gxt_valid = replace(gxt_valid, is.na(gxt_duration), FALSE)
    )
  
  # exclusion 1: participants who completed gxt duration in >= 1 visit
  e1 <- e0 %>% 
    group_by(ID) %>% 
    mutate(n_complete_gxt = sum(!is.na(gxt_duration)),
           .after = ID) %>% 
    ungroup() %>% 
    filter(n_complete_gxt >= 1) %>% 
    select(-n_complete_gxt)
  
  # drop missings to get the actual count of GXT tests
  e1 %>% 
    drop_na(gxt_duration) %>% 
    count(exam)
  
  e1 %>% 
    drop_na(gxt_duration) %>% 
    pull(ID) %>% 
    unique() %>% 
    length()
  
  # exclusion 2: no beta blockers
  e2 <- e1 %>% 
    mutate(
      blocker_beta = replace(blocker_beta, is.na(blocker_beta), 'no'),
      blocker_beta_during_gxt = if_else(
        condition = exam %in% c("A","D","G") & blocker_beta == 'yes',
        true = 'yes',
        false = 'no'
      )
    ) %>% 
    filter(blocker_beta_during_gxt == 'no') %>% 
    select(-starts_with('blocker_'))
  
  # drop missings to get the actual count of GXT tests
  e2 %>% 
    drop_na(gxt_duration) %>% 
    count(exam)
  
  e2 %>% 
    drop_na(gxt_duration) %>% 
    pull(ID) %>% 
    unique() %>% 
    length()
  
  # message("Total N at Y0 completing >=1 GXT: ", sum(e1$exam == 'A'))
  
  data_branches <- list(
    # first approach: remove invalid GXT
    gxt_complete..mn_include = e2 %>% 
      filter(gxt_valid | exam %in% c("B","C","E","F")),
    
    # remove invalid GXT and drop minnesota Y7
    gxt_complete..mn_exclude = e2 %>% 
      filter(gxt_valid | exam %in% c("B","C","E","F")) %>%
      filter(!(CENTER == 'Minnesota' & exam == 'D')),
    
    # alternate approach: make GXT NA when GXT is not valid, don't exclude
    gxt_impute..mn_include = e2 %>% 
      mutate(gxt_duration = if_else(gxt_valid, 
                                    true = gxt_duration,
                                    false = NA_real_,
                                    missing = NA_real_)),
    
    # make GXT NA and drop minnesota Y7
    gxt_impute..mn_exclude = e2 %>% 
      mutate(gxt_duration = if_else(gxt_valid, 
                                    true = gxt_duration,
                                    false = NA_real_,
                                    missing = NA_real_)) %>% 
      filter(!(CENTER == 'Minnesota' & exam == 'D'))
  ) %>% 
    map(select, -gxt_valid)
  
  
  data_branches$gxt_complete..mn_include %>% 
    drop_na(gxt_duration) %>% 
    count(exam)
  
  data_branches$gxt_complete..mn_include %>% 
    drop_na(gxt_duration) %>% 
    pull(ID) %>% 
    unique() %>% 
    length()
  
  data_branches$gxt_complete..mn_exclude %>% 
    drop_na(gxt_duration) %>% 
    count(exam)
  
  data_branches$gxt_complete..mn_exclude %>% 
    drop_na(gxt_duration) %>% 
    pull(ID) %>% 
    unique() %>% 
    length()
  
  data_branches

}

  # %>% 
  #   mutate(
  #     exclusion = recode(
  #       exclusion,
  #       '1' = 'CARDIA Participants',
  #       '2' = 'Completed at least 1 graded exercise test',
  #       '3' = 'Valid maximal heart rate or exertion during test',
  #       '4' = 'Not using beta blocker during GXT exam'
  #       )
  #   )

# %>% 
#   mutate(
#     exam_year = make_numeric_exam(exam),
#     .after = exam
#   ) %>% 
#   group_by(ID) %>% 
#   mutate(exam_age = if_else(is.na(exam_age),
#                             exam_age[1] + exam_year,
#                             exam_age),
#          time_diff = lead(exam_year) - exam_year,
#          time_diff = replace(time_diff, exam_year == 20, 0),
#          .after = exam_year) %>%
#   mutate(CENTER = zoo::na.locf(CENTER)) %>% 
#   ungroup() %>% 
#   mutate(
#     age_centered = exam_age - 20,
#   ) 