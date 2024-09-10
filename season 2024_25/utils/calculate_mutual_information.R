## Mutual Information
library(infotheo)

# Define a function to calculate mutual information
calculate_mutual_information <- function(data, target_col) {
  predictors <- setdiff(names(data), target_col)
  
  # Discretize the data
  discretized_data <- data %>%
    mutate(across(everything(), as.factor))
  
  # Calculate mutual information
  mi_results <- sapply(predictors, function(predictor) {
    mutinformation(discretized_data[[target_col]], discretized_data[[predictor]])
  })
  
  mi_df <- tibble(
    Variable = predictors,
    MutualInformation = mi_results
  )
  
  mi_df %>%
    arrange(desc(MutualInformation)) %>% 
    mutate(MutualInformation = round(MutualInformation,2))
}

