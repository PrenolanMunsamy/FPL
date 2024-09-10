library(tidyverse)
library(here)

import_all_gws <- function(path,season){
  
  # List all files in the directory
  all_files <- 
    list.files(here(path),
               pattern = "^gw.*\\.csv$", 
               full.names = TRUE)
  
  # Function to read and convert all columns to character
  read_csv_as_character <- function(file) {
    df <- suppressMessages(read_csv(file, col_types = cols(.default = "c")))
    df <- 
      df %>% 
      mutate(GoalWeek = file) %>% 
      mutate(GoalWeek = str_replace(GoalWeek,".*gw","")) %>% 
      mutate(GoalWeek = as.numeric(str_replace(GoalWeek,coll(".csv"),"")))
    
    df
  }
  
  # Read all CSV files that start with "gw" into a list of data frames
  data_list <- lapply(all_files, read_csv_as_character)
  
  # Optionally, you can combine all data frames into one
  combined_data <- do.call(bind_rows, data_list)
  
  # Set Correct Data Types
  combined_data <- cast_correct_dtypes(combined_data)
  
  # Add Season
  combined_data <- add_season(combined_data,season)
  
}