library(tidymodels)

xgb_classif_recipe <- function(df){
  
  # model_vars <-
  #   setdiff(df %>% colnames(),vars_to_excl)
  
  model_vars <-
    df %>% 
    dplyr::select(#position,
      was_home,
      # played_60_or_more_mins,
      contains("increase"),
      # GoalWeek,
      contains("lag_1"),
      # contains("lag_2"),
      # contains("lag_3"),
      # contains("sum_last_3"),
      contains("avg_last_3"),
      #contains("sum_last_5"),
      #contains("avg_last_5"),
      contains("ratio")
      # contains("consistency_seg")
      # contains("cume"),
      # contains("difficulty")
      # player_appearance_order
    ) %>% 
    select(-contains("minutes")) %>% 
    select(-contains("value_lag")) %>% 
    select(-contains("threat")) %>% 
    select(-contains("influence")) %>% 
    select(-contains("creativity")) %>%
    select(-contains("selected")) %>% 
    colnames()
  
  print(model_vars)
  
  # df <-
  #   df %>% 
  #   mutate(scored_more_than_8_points = as.factor(ifelse(total_points>=8,1,0)))
  
  model_formula <-
    as.formula(paste0("scored_more_than_2_points","~",
                      paste(model_vars,collapse = " + ")))
  
  mod_recipe <- 
    recipe(model_formula,
           data = df) %>%
    step_dummy(all_nominal_predictors(),one_hot = FALSE) %>% 
    step_nzv(all_numeric_predictors()) %>% 
    step_normalize(all_numeric_predictors()) 
  # %>% 
  #   themis::step_downsample(scored_more_than_6_points)

  return(mod_recipe=mod_recipe)
  
}
