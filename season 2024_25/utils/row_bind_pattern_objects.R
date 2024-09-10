# Function to row bind all objects with a specific pattern
row_bind_pattern_objects <- function(pattern) {
  
  # Get the list of all object names in the global environment
  all_objects <- ls(envir = .GlobalEnv)
  
  # Filter the objects that match the pattern
  pattern_objects <- all_objects[grepl(pattern, all_objects)]
  
  # Initialize an empty list to store the data frames
  data_list <- list()
  
  # Loop through the matching objects and add them to the list
  for (obj_name in pattern_objects) {
    # Get the object
    obj <- get(obj_name, envir = .GlobalEnv)
    
    # Check if the object is a data frame
    if (is.data.frame(obj)) {
      data_list[[obj_name]] <- obj
    }
  }
  
  # Row bind all data frames in the list
  combined_data <- do.call(bind_rows, data_list)
  
  return(combined_data)
}