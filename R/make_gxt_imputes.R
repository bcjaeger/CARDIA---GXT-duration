##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data_gxt
make_gxt_imputes <- function(data_gxt, 
                             n_impute = 3,
                             n_iter = 5) {

  not_all_na <- function(x) !all(is.na(x))
  not_all_same <- function(x) length(unique(na.omit(x))) > 1
  
  data_to_impute <- data_gxt %>% 
    pivot_wider(names_from = exam, 
                names_sep = '..',
                values_from = -c(ID, CENTER, race, sex, exam)) %>% 
    select(ID, CENTER, race, sex, 
           ends_with("A"),
           ends_with("B"),
           ends_with("C"),
           ends_with("D"),
           ends_with("E"),
           ends_with("F"),
           ends_with("G")) %>% 
    select(where(not_all_na)) %>% 
    select(where(not_all_same))
  
  vars <- vector(mode = 'list', length = ncol(data_to_impute))
  names(vars) <- names(data_to_impute)
  
  vars$ID <- NULL
  vars$exam_age..B <- NULL
  vars$exam_age..C <- NULL
  vars$exam_age..D <- NULL
  vars$exam_age..E <- NULL
  vars$exam_age..F <- NULL
  vars$exam_age..G <- NULL
  
  preds <- setdiff(
    x = names(data_to_impute),
    y = c("ID", 
          "exam_age..B", 
          "exam_age..C", 
          "exam_age..D",
          "exam_age..E", 
          "exam_age..F",
          "exam_age..G")
  )

  for(v in seq_along(vars)) vars[[v]] <- setdiff(preds, names(vars)[v])

  data_imputed <- data_to_impute %>% 
    miceRanger(m = n_impute, 
               maxiter = n_iter, 
               meanMatchCandidates = 10,
               vars = vars, 
               num.trees = 50)
  
  # plotVarConvergence(data_imputed, vars = 'gxt_duration..G')
  # plotModelError(data_imputed, vars = 'gxt_duration..G')
  # plotVarConvergence(data_imputed, vars = 'health_self..G')
  # plotDistributions(data_imputed, vars = 'health_self..A')
  
  completeData(data_imputed) %>% 
    map(
      ~as_tibble(.x) %>% 
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
          )
        ) %>% 
        group_by(ID) %>% 
        mutate(
          across(everything(), zoo::na.locf),
          bmi_cat = if_else(
            bmi < 25, 
            "under_or_normal", 
            "over_or_obese"
          ),
          # physical activity
          pa_self_cat = if_else(
            pa_self < 300, 
            'not_meeting_guidelines',
            'meeting_guidelines'
          ),
          smoke = as.character(smoke),
          smoke = correct_smoke(x=smoke)
        ) %>% 
        mutate(
          exam_year = make_numeric_exam(exam),
          exam_period = recode(
            exam,
            "A" = "A", 
            "B" = "A", 
            "C" = "A", 
            "D" = "D", 
            "E" = "D", 
            "F" = "D", 
            "G" = "G"
          ),
          age_centered = exam_age - 20,
          .after = exam
        ) %>% 
        group_by(ID) %>%
        mutate(
          bmi_cat = if_else(
            bmi < 25, 
            "under_or_normal", 
            "over_or_obese"
          ),
          # physical activity
          pa_self_cat = if_else(
            pa_self < 300, 
            'not_meeting_guidelines',
            'meeting_guidelines'
          ),
          smoke = as.character(smoke),
          smoke = correct_smoke(x=smoke),
          exam_age = if_else(is.na(exam_age),
                             exam_age[1] + exam_year,
                             exam_age),
          time_diff = lead(exam_year) - exam_year,
          time_diff = replace(time_diff, exam_year == 20, 0),
        ) %>%
        initialize_durations('paying_for_basics') %>% 
        initialize_durations('bmi_cat') %>% 
        initialize_durations('pa_self_cat') %>% 
        initialize_durations('alcohol') %>% 
        initialize_durations('smoke') %>% 
        initialize_durations('health_self') %>% 
        initialize_durations('educ') %>% 
        # convert these variables to their first observed
        mutate(
          across(
            .cols = c(paying_for_basics,
                      marital,
                      bmi_cat,
                      pa_self_cat,
                      alcohol,
                      smoke,
                      health_self,
                      educ),
            .fns = ~ .x[1]
          )
        ) %>% 
        ungroup() %>% 
        mutate(
          smoke = factor(
            smoke, 
            levels = c('never', 'former','current'),
            labels = c('never', 'former_current', 'former_current')
          )
        ) %>% 
        group_by(ID, exam_period) %>% 
        mutate(
          across(
            .cols = starts_with("duration"),
            .fns = sum
          )
        ) %>% 
        filter(exam %in% c("A","D","G")) %>% 
        group_by(ID) %>% 
        mutate(
          across(
            matches("^duration_"), 
            ~ c(0, cumsum(.x[-length(.x)])
            )
          )
        )
    )

}


# deprecated time-varying derivation functions
# derive_tv_bmi <- function(x){
#   
#   if(length(x) == 1) return(NA_character_)
#   
#   x_cut <- if_else(x < 25, 'under', 'over')
#   
#   paste(x_cut[1], x_cut[length(x)], sep = '_')
#   
# }
# 
# derive_tv_smoke <- function(x) {
#   
#   if(length(x) == 1) return(NA_character_)
#   
#   x_cut <- recode(x, 'former_current' = 'smoke..yes', 'never' = 'smoke..no')
#   
#   paste(x_cut[1], x_cut[length(x)], sep = '_')
#   
# }
# 
# derive_tv_pa <- function(x) {
#   
#   if(length(x) == 1) return(NA_character_)
#   
#   x_cut <- if_else(x < 300, 'pa..no', 'pa..yes')
#   
#   paste(x_cut[1], x_cut[length(x)], sep = '_')
#   
# }
# 
# derive_tv_alcohol <- function(x) {
#   
#   if(length(x) == 1) return(NA_character_)
#   
#   x_cut <- recode(x, 'alc_no' = 'alc..no', 'alc_yes' = 'alc..yes')
#   
#   paste(x_cut[1], x_cut[length(x)], sep = '_')
#   
# }
# 
# derive_tv_basics <- function(x) {
#   
#   if(length(x) == 1) return(NA_character_)
#   
#   x_cut <- recode(x, 
#                   'not_very_hard' = 'payforbasics..nvh', 
#                   'somewhat_hard' = 'payforbasics..swh')
#   
#   paste(x_cut[1], x_cut[length(x)], sep = '_')
#   
# }
# 
# derive_tv_health <- function(x) {
#   
#   if(length(x) == 1) return(NA_character_)
#   
#   x_cut <- recode(x, 
#                   'excellent_good' = 'healthsr..eg', 
#                   'fair_poor' = 'healthsr..fp')
#   
#   paste(x_cut[1], x_cut[length(x)], sep = '_')
#   
# }

# deprecated mice code
# # prepares the data for multiple imputation
# data_to_impute <- mutate(
#   data_gxt,
#   across(where(is.character), as.factor),
#   ID = as.integer(ID)
#   # self-reported health at baseline is difficult to
#   # impute b/c it does not vary over time. Also, it has
#   # very few missing values. Thus, we are assuming the 
#   # missing values are excellent/good self reported health,
#   # since this is the mode.
#   # health_self = replace(health_self,
#   #                       is.na(health_self),
#   #                       'excellent_good')
# )
# 
# # specify predictor matrix
# predmat <- make.predictorMatrix(data_to_impute)
# predmat[, 'ID'] <- -2 # clustering variable labeled as -2
# predmat[, c('exam', 'exam_year','exam_age')] <- 0 # same as age_centered
# predmat[, c('time_diff')] <- 0 # useless predictor
# 
# #predmat[, 'bmi'] <- 0
# 
# constant_vars <- data_to_impute %>% 
#   map_lgl(~length(unique(na.omit(.x))) == 1) %>% 
#   which()
# 
# # cant use constant variables as predictors
# if(length(constant_vars) > 0) predmat[, constant_vars] <- 0
# # imputation in mice
# # some models will not converge - this is okay
# # suppress warnings from models not converging
# mice_impute <- mice(
#     data_to_impute,
#     maxit = n_iter,
#     m = n_impute,
#     print = T,
#     # defaultMethod = rep('pmm', 4),
#     defaultMethod = c('2l.pan', 'rf', 'pmm', 'pmm'),
#     predictorMatrix = predmat
# )
# 
# # for imputation, we used time-varying data.
# # for modeling, we use baseline values or explicit time-varying factors.
# # the code below replaces time-varying values with baseline values for 
# # relevant variables and then creates 
# 
# out <- mice_impute %>% 
#   complete(action = 'all') %>% 
#   map(
#     ~ as_tibble(.x) %>% 
#       mutate(
#         exam_year = make_numeric_exam(exam),
#         .after = exam
#       ) %>% 
#       group_by(ID) %>%
#       mutate(
#         bmi_cat = if_else(
#           bmi < 25, 
#           "under_or_normal", 
#           "over_or_obese"
#         ),
#         # physical activity
#         pa_self_cat = if_else(
#           pa_self < 300, 
#           'not_meeting_guidelines',
#           'meeting_guidelines'
#         ),
#         smoke = as.character(smoke),
#         smoke = correct_smoke(x=smoke),
#         exam_age = if_else(is.na(exam_age),
#                            exam_age[1] + exam_year,
#                            exam_age),
#         time_diff = lead(exam_year) - exam_year,
#         time_diff = replace(time_diff, exam_year == 20, 0),
#         .after = exam_year)
#       ) %>%
#       initialize_durations('paying_for_basics') %>% 
#       initialize_durations('bmi_cat') %>% 
#       initialize_durations('pa_self_cat') %>% 
#       initialize_durations('alcohol') %>% 
#       initialize_durations('smoke') %>% 
#       initialize_durations('health_self') %>% 
#       ungroup() %>% 
#       mutate(
#         smoke = factor(
#           smoke, 
#           levels = c('never', 'former','current'),
#           labels = c('never', 'former_current', 'former_current')
#         ),
#         exam_period = recode(
#           exam,
#           "A" = "A", 
#           "B" = "A", 
#           "C" = "A", 
#           "D" = "D", 
#           "E" = "D", 
#           "F" = "D", 
#           "G" = "G"
#         ),
#         .after = exam
#       ) %>% 
#       group_by(ID, exam_period) %>% 
#       mutate(
#         across(
#           .cols = starts_with("duration"),
#           .fns = sum
#         )
#       ) %>% 
#       filter(exam %in% c("A","D","G")) %>% 
#       group_by(ID) #%>% 
#       # mutate(
#       #   across(
#       #     .cols = c(alcohol, health_self, paying_for_basics, smoke),
#       #     .fns = as.character
#       #   ),
#       #   # alcohol
#       #   alcohol = alcohol[1],
#       #   # self-reported health
#       #   # there are no instances where TV health_self is needed.
#       #   # health_self_tv = derive_tv_health(x = health_self),
#       #   health_self = health_self[1],
#       #   # paying for basics
#       #   paying_for_basics = paying_for_basics[1],
#       #   # smoking
#       #   smoke = smoke[1]
#       # )
#   ) %>% 
#   set_names(
#     paste(
#       "impute", 
#       seq(n_impute),
#       sep = '_'
#     )
#   )

# ggplot(bind_rows(out, .id = 'impute')) + 
#   aes(x = exam_age, y = gxt_duration, col = impute) + 
#   geom_smooth(se = FALSE)
# 
# ggplot(data_to_impute) + 
#   aes(x = exam_age, y = gxt_duration) + 
#   geom_smooth()