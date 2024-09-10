library(dplyr)
library(purrr)

fpl_sum_n_previous <- function(data, vars, window) {
  # Check if window is numeric and greater than 0
  if (!is.numeric(window) || window <= 0) {
    stop("Window should be a positive numeric value")
  }
  
  # Create a function to calculate the cumulative sum of previous values
  calc_cumsum <- function(x, window) {
    n <- length(x)
    result <- numeric(n)
    for (i in seq_along(x)) {
      start_index <- max(1, i - window + 1)
      result[i] <- sum(x[start_index:i])
    }
    result <- lag(result)
    result[1] <- 0
    result
  }
  
  # Apply the calc_cumsum function to each specified variable
  data <- 
    data %>%
    # arrange(team, name, match_number) %>%
    # group_by(team, name) %>%
    arrange(name, YEAR,MONTH,DAY) %>%
    group_by(name) %>%
    mutate(across(all_of(vars), 
                  ~ calc_cumsum(.x, window),
                  .names = "{.col}_sum_last_{window}")) %>%
    ungroup() %>%
    mutate(across(contains("_sum_last_"), ~ ifelse(is.na(.x), 0, .x)))
  
  return(data)
}