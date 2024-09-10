library(tidyverse)
library(here)

import_all_gw_teams <- function(path){
  
  # List all files in the directory
  all_files <- 
    list.files(here(path),
               pattern = ".csv$", 
               full.names = TRUE)
  
  # Function to read and convert all columns to character
  read_csv_as_character <- function(file) {
    df <- suppressMessages(read_csv(file, col_types = cols(.default = "c")))
    df <-
      df %>%
      mutate(GoalWeek = file) %>%
      mutate(GoalWeek = str_replace(GoalWeek,".*gw","")) %>%
      mutate(GoalWeek = as.numeric(str_replace(GoalWeek,coll("_team.csv"),"")))

    df
  }
  
  # Read all CSV files that start with "gw" into a list of data frames
  data_list <- lapply(all_files, read_csv_as_character)
  
  # Optionally, you can combine all data frames into one
  combined_data <- do.call(bind_rows, data_list)
  combined_data %>% 
    arrange(GoalWeek)
}

gw_teams <- import_all_gw_teams("data/starting team/season 2022_23")

filter_df_by_variable <- function(df, variable) {
  # Check if the variable is a column in the dataframe
  if (!(variable %in% names(df))) {
    stop(paste("Variable", variable, "is not a column in the dataframe"))
  }
  
  # Get the unique values of the variable
  unique_values <- unique(df[[variable]])
  
  # Create an empty list to store the filtered dataframes
  filtered_list <- list()
  
  # Loop through each unique value and filter the dataframe
  for (value in unique_values) {
    filtered_list[[as.character(value)]] <- df[df[[variable]] == value, ]
  }
  
  return(filtered_list)
}

filtered_dataframes <- filter_df_by_variable(gw_teams, 'GoalWeek')

# Assuming your list of dataframes is called `df_list`
df_list <- filtered_dataframes

# Initialize lists to store results
unique_to_df1_list <- list()
unique_to_df2_list <- list()

# Loop through each sequential pair of dataframes
for (i in 1:(length(df_list) - 1)) {
  unique_to_df1_list[[i]] <- setdiff(df_list[[i]]$name, df_list[[i + 1]]$name)
  unique_to_df2_list[[i]] <- setdiff(df_list[[i + 1]]$name, df_list[[i]]$name)
}

# Print results for each pair
for (i in 1:(length(df_list) - 1)) {
  cat("week ", i, " to week ",i+1,unique_to_df1_list[[i]]," substituted for:")
  cat(unique_to_df2_list[[i]], "\n\n")
}
