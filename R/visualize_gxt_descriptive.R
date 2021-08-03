##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data_gxt_all
visualize_gxt_descriptive <- function(data_gxt_included) {

  ggdata <- data_gxt_included %>% 
    mutate(
      race = fct_reorder2(race, .x = exam_age, .y = gxt_duration),
      exam = recode(exam, 
                    'A' = 'Year 0',
                    'D' = 'Year 7', 
                    'G' = 'Year 20'),
      exam = factor(exam, levels = c("Year 0", "Year 7", "Year 20"))
    ) 
  
  age_points_race_sex <- ggplot(ggdata) +
    aes(x = exam_age, 
        y = gxt_duration, 
        color = race) +
    geom_point(alpha = 0.25, position = position_jitter()) +
    facet_wrap(~sex) + 
    scale_color_manual(values = c("purple", "orange")) + 
    labs(color = 'Race', 
         x = 'Age, years',
         y = 'Graded exercise test duration, seconds')
  
  age_points_center <- ggplot(ggdata) +
    aes(x = exam_age, 
        y = gxt_duration) +
    geom_point(color = 'grey', position = position_jitter()) +
    facet_wrap(~CENTER) + 
    scale_color_manual(values = c("purple", "orange")) + 
    labs(color = 'Race', 
         x = 'Age, years',
         y = 'Graded exercise test duration, seconds')
  
  exam_points_race_sex <- ggplot(ggdata) +
    aes(x = exam, 
        y = gxt_duration, 
        color = race) +
    geom_point(alpha = 0.25,
               position = position_jitterdodge(dodge.width = 1/3)) +
    facet_wrap(~sex) + 
    scale_color_manual(values = c("purple", "orange")) + 
    labs(color = 'Race', 
         x = 'Exam',
         y = 'Graded exercise test duration, seconds')
  
  exam_points_center <- ggplot(ggdata) +
    aes(x = exam, 
        y = gxt_duration) +
    geom_point(alpha = 0.25, position = position_jitter()) +
    facet_wrap(~CENTER) + 
    scale_color_manual(values = c("purple", "orange")) + 
    labs(color = 'Race', 
         x = 'Exam',
         y = 'Graded exercise test duration, seconds')
  
  age_smoothed_race_sex <- ggplot(ggdata) +
    aes(x = exam_age, 
        y = gxt_duration, 
        color = race) +
    geom_smooth(method = 'loess') +
    facet_wrap(~sex) + 
    scale_color_manual(values = c("purple", "orange")) + 
    labs(color = 'Race', 
         x = 'Age, years',
         y = 'Graded exercise test duration, seconds')
  
  age_smoothed_center <- ggplot(ggdata) +
    aes(x = exam_age, 
        y = gxt_duration) +
    geom_smooth(method = 'loess') +
    facet_wrap(~CENTER) + 
    scale_color_manual(values = c("purple", "orange")) + 
    labs(color = 'Race', 
         x = 'Age, years',
         y = 'Graded exercise test duration, seconds')
  
  exam_smoothed_race_sex <- ggplot(drop_na(ggdata, gxt_duration)) +
    aes(x = gxt_duration, 
        y = exam, 
        color = race,
        fill = race) +
    stat_halfeye(alpha = 0.35) +
    # geom_point(alpha = 0.25,
    #            position = position_jitterdodge(dodge.width = 1/3)) +
    facet_wrap(~sex) +
    scale_color_manual(values = c("purple", "orange")) + 
    scale_fill_manual(values = c("purple", "orange")) +
    labs(color = 'Race', 
         fill = 'Race',
         x = 'Graded exercise test duration, seconds',
         y = 'Exam') + 
    coord_flip()
  
  exam_smoothed_center <- ggplot(drop_na(ggdata, gxt_duration)) +
    aes(x = gxt_duration, 
        y = exam) +
    stat_halfeye(alpha = 0.35) +
    # geom_point(alpha = 0.25,
    #            position = position_jitterdodge(dodge.width = 1/3)) +
    facet_wrap(~CENTER) +
    labs(color = 'Race', 
         fill = 'Race',
         x = 'Graded exercise test duration, seconds',
         y = 'Exam') + 
    coord_flip()
  
  gxt_duration <- list(
    age = list(points_race_sex = age_points_race_sex,
               points_center = age_points_center,
               smoothed_race_sex = age_smoothed_race_sex,
               smoothed_center = age_smoothed_center),
    exam = list(points_race_sex = exam_points_race_sex,
                points_center = exam_points_center,
                smoothed_race_sex = exam_smoothed_race_sex,
                smoothed_center = exam_smoothed_center)
  )
  
  age_points_race_sex <- ggplot(ggdata) +
    aes(x = exam_age, 
        y = gxt_hr_s2, 
        color = race) +
    geom_point(alpha = 0.25, position = position_jitter()) +
    facet_wrap(~sex) + 
    scale_color_manual(values = c("purple", "orange")) + 
    labs(color = 'Race', 
         x = 'Age, years',
         y = 'Heart rate, beats per minute')
  
  age_points_center <- ggplot(ggdata) +
    aes(x = exam_age, 
        y = gxt_hr_s2) +
    geom_point(color = 'grey', position = position_jitter()) +
    facet_wrap(~CENTER) + 
    scale_color_manual(values = c("purple", "orange")) + 
    labs(color = 'Race', 
         x = 'Age, years',
         y = 'Heart rate, beats per minute')
  
  exam_points_race_sex <- ggplot(ggdata) +
    aes(x = exam, 
        y = gxt_hr_s2, 
        color = race) +
    geom_point(alpha = 0.25,
               position = position_jitterdodge(dodge.width = 1/3)) +
    facet_wrap(~sex) + 
    scale_color_manual(values = c("purple", "orange")) + 
    labs(color = 'Race', 
         x = 'Exam',
         y = 'Heart rate, beats per minute')
  
  exam_points_center <- ggplot(ggdata) +
    aes(x = exam, 
        y = gxt_hr_s2) +
    geom_point(alpha = 0.25, position = position_jitter()) +
    facet_wrap(~CENTER) + 
    scale_color_manual(values = c("purple", "orange")) + 
    labs(color = 'Race', 
         x = 'Exam',
         y = 'Heart rate, beats per minute')
  
  age_smoothed_race_sex <- ggplot(ggdata) +
    aes(x = exam_age, 
        y = gxt_hr_s2, 
        color = race) +
    geom_smooth(method = 'loess') +
    facet_wrap(~sex) + 
    scale_color_manual(values = c("purple", "orange")) + 
    labs(color = 'Race', 
         x = 'Age, years',
         y = 'Heart rate, beats per minute')
  
  age_smoothed_center <- ggplot(ggdata) +
    aes(x = exam_age, 
        y = gxt_hr_s2) +
    geom_smooth(method = 'loess') +
    facet_wrap(~CENTER) + 
    scale_color_manual(values = c("purple", "orange")) + 
    labs(color = 'Race', 
         x = 'Age, years',
         y = 'Heart rate, beats per minute')
  
  exam_smoothed_race_sex <- ggplot(drop_na(ggdata, gxt_hr_s2)) +
    aes(x = gxt_hr_s2, 
        y = exam, 
        color = race,
        fill = race) +
    stat_halfeye(alpha = 0.35) +
    # geom_point(alpha = 0.25,
    #            position = position_jitterdodge(dodge.width = 1/3)) +
    facet_wrap(~sex) +
    scale_color_manual(values = c("purple", "orange")) + 
    scale_fill_manual(values = c("purple", "orange")) +
    labs(color = 'Race', 
         fill = 'Race',
         x = 'Heart rate, beats per minute',
         y = 'Exam') + 
    coord_flip()
  
  exam_smoothed_center <- ggplot(drop_na(ggdata, gxt_hr_s2)) +
    aes(x = gxt_hr_s2, 
        y = exam) +
    stat_halfeye(alpha = 0.35) +
    # geom_point(alpha = 0.25,
    #            position = position_jitterdodge(dodge.width = 1/3)) +
    facet_wrap(~CENTER) +
    labs(color = 'Race', 
         fill = 'Race',
         x = 'Heart rate, beats per minute',
         y = 'Exam') + 
    coord_flip()
  
  gxt_hr_s2 <- list(
    age = list(points_race_sex = age_points_race_sex,
               points_center = age_points_center,
               smoothed_race_sex = age_smoothed_race_sex,
               smoothed_center = age_smoothed_center),
    exam = list(points_race_sex = exam_points_race_sex,
                points_center = exam_points_center,
                smoothed_race_sex = exam_smoothed_race_sex,
                smoothed_center = exam_smoothed_center)
  )
  
  list(
    duration = gxt_duration,
    hr_s2 = gxt_hr_s2
  )

}
