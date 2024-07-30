library(tidymodels)

xgb_classif_recipe <- function(df){
  
  # model_vars <-
  #   setdiff(df %>% colnames(),vars_to_excl)
  
  model_vars <-
    df %>% 
    dplyr::select(was_home,
                  contains("increase"),
                  # GoalWeek,
                  contains("lag_1"),
                  # contains("lag_2"),
                  # contains("lag_3"),
                  contains("sum_last_5"),
                  contains("avg_last_5"),
                  contains("ratio")
                  # contains("cume"),
                  # contains("difficulty")
                  # player_appearance_order
    ) %>% 
    select(-contains("threat")) %>% 
    select(-contains("influence")) %>% 
    select(-contains("creativity")) %>% 
    colnames()
  
  print(model_vars)
  
  df <-
    df %>% 
    mutate(scored_more_than_2_points = as.factor(scored_more_than_2_points))
  
  model_formula <-
    as.formula(paste0("scored_more_than_2_points","~",
                      paste(model_vars,collapse = " + ")))
  
  # model_formula <- 
  #   as.formula(paste0("total_points","~",
  #                     paste(model_vars,collapse = " + ")))
  
  mod_recipe <- 
    recipe(model_formula,
           data = df) %>%
    step_dummy(all_nominal_predictors(),one_hot = FALSE) %>% 
    step_nzv(all_numeric_predictors()) %>% 
    # step_corr(all_numeric_predictors(),threshold = 0.8) %>% 
    step_normalize(all_numeric_predictors()) 
  #step_impute_median(all_numeric_predictors()) %>% 
  #step_pca(all_numeric_predictors(),num_comp = 10)
  
  return(mod_recipe=mod_recipe)
  
}
