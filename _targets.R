
# TODO:
# - inclusion/exclusion figure (today)
# - change BMI, paying for basics, alcohol, education,
#          PA, smoking, and health_self to baseline status

## Load your packages, e.g. library(targets).
source("./packages.R")
library(future)
library(future.callr)
plan(callr)

theme_set(
  new = theme_bw() + 
    theme(panel.grid = element_blank(),
          text = element_text(size = 18))
)

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(

  outcome_labels = c(
    gxt_duration = "Estimated maximal fitness (GXT Duration, seconds)",
    gxt_hr_s2 = "Estimated submaximal fitness (Heart rate, beats per minute)"
  ),
  
  data_gxt_missing = load_gxt_missing(file = 'data/missexer.sas7bdat'),
  
  table_gxt_missing = tabulate_gxt_missing(data_gxt_missing),
  
  data_gxt_all = load_gxt_all(),
  
  data_gxt_included = exclude_gxt(data_gxt_all),
  
  tbl_characteristics = tabulate_characteristics(
    data_gxt_included = data_gxt_included
  ),
  
  analyses = names(data_gxt_included),
  boots = seq(0, 25),
  imputes = seq(5),
  
  # note: different bootstrapped datasets have different N
  tar_target(
    name = data_gxt_boots,
    command = make_data_gxt_boots(
      data_gxt_included = data_gxt_included, 
      boot_iter = boots,
      analysis = analyses,
      n_impute = max(imputes)
    ),
    pattern = cross(boots, analyses)
  ),
  
  tar_target(
    name = data_gxt_models,
    command = make_gxt_models(
      .boot = boots,
      .impute = imputes,
      .analysis = analyses,
      data = data_gxt_boots
    ),
    pattern = cross(boots, imputes, analyses)
  ),
  
  data_gxt_estimates = make_gxt_estimates(data_gxt_models),
  
  data_gxt_inference = make_gxt_inference(data_gxt_models),
  
  data_gxt_tables = left_join(data_gxt_estimates$values,
                              data_gxt_inference),
  
  fig_gxt_estimates = visualize_model_gxt(data_gxt_tables,
                                          outcome_labels),
  
  tbl_gxt_estimates = tabulate_model_gxt(data_gxt_tables,
                                         outcome_labels),
  
  fig_gxt_pchange = visualize_pchange(data_gxt_estimates$perc_change),
  
  # fig_gxt_descriptive = visualize_gxt_descriptive(data_gxt_included),
  
  fig_race_sex = visualize_race_sex(data_gxt_tables,
                                    data_gxt_estimates,
                                    outcome_labels)

)
