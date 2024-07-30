library(dplyr)
library(purrr)

fpl_avg_fdr_n_next <- function(data, vars, window) {
  # Check if window is numeric and greater than 0
  if (!is.numeric(window) || window <= 0) {
    stop("Window should be a positive numeric value")
  }
  
  # Create a function to calculate the cumulative average of next values
  calc_cumavg <- function(x, window) {
    n <- length(x)
    result <- numeric(n)
    for (i in seq_along(x)) {
      end_index <- min(n, i + window)
      result[i] <- mean(x[(i + 1):end_index], na.rm = TRUE)
    }
    result
  }
  
  # print(result)
  
  # Apply the calc_cumavg function to each specified variable
  data <- 
    data %>%
    arrange(team_a_name, YEAR, MONTH, DAY) %>%
    group_by(team_a_name) %>%
    mutate(across(.cols = all_of(vars), 
                  .fns = ~ calc_cumavg(.x, window),
                  .names = "{.col}_avg_next_{window}")) %>%
    ungroup() %>%
    mutate(across(contains("_avg_next_"), ~ ifelse(is.na(.x), 0, .x)))
  
  return(data)
}