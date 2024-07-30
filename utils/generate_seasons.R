# Function to generate all seasons between two given seasons
generate_seasons <- function(first_season, curr_season) {
  # Extract the starting and ending years
  start_year <- as.numeric(substr(first_season, 1, 4))
  end_year <- as.numeric(substr(curr_season, 1, 4))
  
  # Generate the sequence of seasons
  seasons <- sapply(start_year:end_year, function(year) {
    next_year <- substr(as.character(year + 1), 3, 4)
    paste(year, next_year, sep = "_")
  })
  
  # Ensure the last season does not exceed curr_season
  seasons <- seasons[seasons <= curr_season]
  
  return(seasons)
}

