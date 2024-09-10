import_historical_data <- function(path){
  
  # List all files in the directory
  all_files <- 
    list.files(here(path),
               pattern = ".csv$", 
               full.names = TRUE)
  print(all_files)
  
  # Function to read and convert all columns to character
  read_csv_as_character <- function(file) {
    df <- suppressMessages(read_csv(file, col_types = cols(.default = "c")))
    df
  }
  
  # Read all CSV files into a list of data frames
  data_list <- lapply(all_files, read_csv_as_character)
  
  # Optionally, you can combine all data frames into one
  combined_data <- do.call(bind_rows, data_list)
  
}