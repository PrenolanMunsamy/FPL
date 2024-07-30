library(here)
# Path to the scripts
script1 <- here("scripts","5. Prep Data For Prediction.R")
# script2 <- here("scripts","6. Predict Upcoming Week - XGB Classif.R")
script2 <- here("scripts","6. Predict Upcoming Week - XGB Reg.R")
# script2 <- here("scripts","6. Predict Upcoming Week - XGB Reg Combined.R")
script3 <- here("scripts","7. Weekly Transfer.R")
gw_script <- here("scripts","99. Constants.R")

# Loop through gw values from 2 to 38
for (gw in 2:38) {
  # Assign the gw value in the environment
  assign("gw", gw, envir = .GlobalEnv)

  # Source the gw script to set the gw value
  source(gw_script)

  # Execute the three scripts in sequence
  # source(script1)
  # source(script2)
  source(script3)

  # Optionally, print the gw value to track progress
  print(paste("Completed execution for gw =", gw))
}


