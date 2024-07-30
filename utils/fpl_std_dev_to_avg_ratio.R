fpl_std_dev_to_avg_ratio <- function(df,window=NULL){
  # Create new columns with the ratio of std to avg
  # Assume `df` is your dataframe with the variables
  # Generate a list of variable names for standard deviation and average
  if (is.null(window)){
    std_vars <- grep("_std_last_all", names(df), value = TRUE)
    avg_vars <- grep("_avg_last_all", names(df), value = TRUE)  
  } else {
    std_vars <- grep(paste0("_std_last_",window), names(df), value = TRUE)
    avg_vars <- grep(paste0("_avg_last_",window), names(df), value = TRUE)
  }
  
  
  # Ensure that std_vars and avg_vars have corresponding pairs
  if(length(std_vars) != length(avg_vars)) {
    stop("The number of standard deviation variables does not match the number of average variables.")
  }
  
  # Create new variables for each pair
  for (i in seq_along(std_vars)) {
    new_var_name <- paste0(std_vars[i], "_to_avg_ratio")
    df[[new_var_name]] <- df[[std_vars[i]]] / df[[avg_vars[i]]]
  }  
  
  df %>% 
    mutate(across(.cols = contains("_ratio"),
                  .fns = ~ ifelse(is.na(.x)|is.nan(.x),0,.x)))
}

