fit_cumulative_folds <- function(folds, 
                                 recipe,
                                 model_spec,
                                 param_grid,
                                 target
) {
  
  results <- list()
  
  # Iterate over each parameter combination
  param_combinations <- expand.grid(param_grid)
  print(param_combinations)
  for (i in seq_len(nrow(param_combinations))) {
    
    params <- param_combinations[i, ]
    # print(params)
    
    model_spec_tuned <- 
      model_spec %>%
      finalize_model(parameters = params)
    # print(model_spec_tuned)
    
    rmse_vals <- numeric(length(folds)-1)
    
    for (j in (seq(length(folds)-1))) {
      # Cumulative training data
      train_data <- do.call(rbind.data.frame,folds[1:j])
      
      # print(train_data %>% head())
      
      # Remaining data
      test_data <- do.call(rbind.data.frame,folds[-(1:j)])
      # print(test_data %>% head())
      
      # Prepare the recipe with training data
      prep_recipe <- prep(recipe, training = train_data)#, retain = TRUE
      train_data_prepped <- bake(prep_recipe, new_data = train_data)
      test_data_prepped <- bake(prep_recipe, new_data = test_data)
      
      print(test_data_prepped)
      
      model_formula <- as.formula(paste0(target,"~."))
      
      # Fit the model
      model_fit <- 
        model_spec_tuned %>%
        fit(model_formula, data = train_data_prepped)
      
      # Make predictions
      preds <- predict(model_fit, test_data_prepped) %>%
        bind_cols(test_data_prepped)
      
      # Calculate RMSE
      rmse_vals[j] <- rmse(preds, truth = target, estimate = .pred)$.estimate
    }
    
    avg_rmse <- mean(rmse_vals)
    
    results[[i]] <- list(params = params, avg_rmse = avg_rmse)
  }
  
  # Find the best parameters
  best_result <- results %>% bind_rows() %>% arrange(avg_rmse) %>% slice(1)
  
  return(best_result)
}

