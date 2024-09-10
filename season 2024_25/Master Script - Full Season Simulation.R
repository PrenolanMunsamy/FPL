library(here)

# Path to the scripts
script1 <- here("scripts/season 2023_24","6. Prep Data For Prediction.R")
script2 <- here("scripts/season 2023_24","7. Predict Upcoming Week - XGB Reg.R")
script3 <- here("scripts/season 2023_24","8. Weekly Transfer.R")
# gw_script <- here("scripts/season 2022_23","99. Constants.R")

gw <- 1
first_season <- "2021_22"
prev_season <- "2022_23"
curr_season <- "2023_24"

# Loop through gw values from 2 to 38
for (gw in 2:38) {
  
  # Assign the gw value in the environment
  assign("gw", gw, envir = .GlobalEnv)

  # Source the gw script to set the gw value
  # source(gw_script)

  # Execute the three scripts in sequence
  # source(script1)
  # source(script2)
  source(script3)

  # Optionally, print the gw value to track progress
  print(paste("Completed execution for gw =", gw))
}


