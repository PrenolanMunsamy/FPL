library(tidyverse)
library(here)

# Loop to create folders gw1 to gw38
for (i in 2:38) {
  # Create folder name
  folder_name <- paste0("gw", i)
  
  # Full path to the new folder
  folder_path <- file.path(here("data/predictions/season 2024_25"), folder_name)
  
  # Create the folder
  dir.create(folder_path, showWarnings = FALSE)
}

# Print message upon completion
cat("Folders gw2 to gw38 created successfully.")
