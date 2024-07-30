library(httr)
library(jsonlite)

# URL for the FPL API
url <- "https://fantasy.premierleague.com/api/bootstrap-static/"

# Fetch the data
response <- GET(url)
content <- content(response, as = "text")
data <- fromJSON(content)

# Extract relevant information ----
# **** Player Info ----
players <- data$elements

# Select the desired columns
players <- 
  players %>% 
  select(element_type, now_cost, id, first_name, second_name, team)

# Team Info ----
teams <- data$teams
