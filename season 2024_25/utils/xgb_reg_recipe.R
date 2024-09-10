library(tidymodels)

xgb_reg_recipe <- function(df){
  
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
                  # contains("avg_last_3"),
                  contains("sum_last_5"),
                  contains("avg_last_5"),
                  contains("ratio")
                  # contains("consistency_seg")
                  # contains("cume"),
                  # contains("difficulty")
                  # player_appearance_order
                  ) %>% 
    select(-contains("threat")) %>% 
    select(-contains("influence")) %>% 
    select(-contains("creativity")) %>%
    select(-contains("selected")) %>% 
    colnames()
  
  print(model_vars)
  
  model_formula <- 
    as.formula(paste0("total_points","~",
                      paste(model_vars,collapse = " + ")))
  
  mod_recipe <- 
    recipe(model_formula,
           data = df) %>%
    step_dummy(all_nominal_predictors(),one_hot = FALSE) %>% 
    step_nzv(all_numeric_predictors()) %>% 
    step_normalize(all_numeric_predictors()) 
  
  return(mod_recipe=mod_recipe)
  
}
