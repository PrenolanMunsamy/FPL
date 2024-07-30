library(tidymodels)

linear_regression_recipe <- function(df){
  
  model_vars <-
    df %>% 
    dplyr::select(was_home,
                  GoalWeek,
                  contains("lag_1"),
                  contains("lag_2"),
                  contains("lag_3"),
                  contains("sum_last_3"),
                  contains("avg_last_3"),
                  contains("to_avg"),
                  player_appearance_order
    ) %>% 
    select(-contains("scored_2_or_more_points")) %>%
    select(-contains("total_points_grouped")) %>%
    select(-contains("threat")) %>% 
    select(-contains("influence")) %>% 
    select(-contains("creativity")) %>% 
    # select(-starts_with("opp_team_")) %>%
    # select(-starts_with("team_")) %>%
    colnames()
  
  model_formula <- 
    as.formula(paste0("total_points","~",
                      paste(model_vars,collapse = " + ")))
  
  mod_recipe <- 
    recipe(model_formula,
           data = df) %>%
    step_dummy(all_nominal_predictors(),one_hot = FALSE) %>% 
    step_nzv(all_numeric_predictors()) %>% 
    step_zv(all_numeric_predictors()) %>%  
    step_corr(all_numeric_predictors(),threshold = 0.4) %>% 
    step_normalize(all_numeric_predictors())
  
  return(mod_recipe=mod_recipe)
  
}
