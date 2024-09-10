library(dplyr)
library(purrr)

fpl_avg_all_previous <- function(data, vars) {
  # Check if window is numeric and greater than 0
  # if (!is.numeric(window) || window <= 0) {
  #   stop("Window should be a positive numeric value")
  # }
  
  # Create a function to calculate the cumulative sum of previous values
  calc_cumavg <- function(x) {
    n <- length(x)
    result <- numeric(n)
    for (i in seq_along(x)) {
      # start_index <- max(1, i - window + 1)
      start_index <- 1
      result[i] <- mean((x[start_index:i]),na.rm = TRUE)
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
                  ~ calc_cumavg(.x),
                  .names = "{.col}_avg_last_all")) %>%
    ungroup() %>%
    mutate(across(contains("_avg_last_"), ~ ifelse(is.na(.x), 0, .x)))
  
  return(data)
}