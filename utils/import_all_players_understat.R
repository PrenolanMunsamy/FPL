library(tidyverse)
library(here)

import_all_players_understat <- function(path){
  
  # List all files in the directory
  all_files <- 
    list.files(here(path),
               pattern = ".csv$", 
               full.names = TRUE)
  
  player_level_files <- all_files[!grepl("^understat", basename(all_files))]
  
  team_level_files <- all_files[grepl("^understat", basename(all_files))]
  
  
  # Function to read and convert all columns to character
  read_csv_as_character <- function(file) {
    df <- suppressMessages(read_csv(file, col_types = cols(.default = "c")))
    
    df <- 
      df %>% 
      mutate(Player_Name_ID = str_replace(file,".*understat/","")) %>%
      mutate(Player_Name_ID = str_replace(Player_Name_ID,coll(".csv"),"")) %>% 
      mutate(Player_Name = gsub("_", " ", Player_Name_ID)) %>%
      mutate(Player_Name = gsub("\\d", "", Player_Name)) %>%
      mutate(Player_Name = stringi::stri_trans_general(Player_Name,"Latin-ASCII")) %>% 
      mutate(Player_Name = str_replace_all(Player_Name,coll("-")," ")) %>%
      mutate(Player_Name = tolower(Player_Name)) %>% 
      mutate(YEAR = year(date),
             MONTH = month(date),
             DAY = day(date)) %>%
      mutate(starter_vs_sub = ifelse(position=="Sub", 0, 1)) %>% 
      select(YEAR,MONTH,DAY,Player_Name_ID,Player_Name,
             shots,starter_vs_sub,xA,key_passes,npg,npxG,xGChain,xGBuildup)
    
    df
  }
  
  # Read all CSV files that start with "gw" into a list of data frames
  data_list <- lapply(player_level_files, read_csv_as_character)
  
  # Optionally, you can combine all data frames into one
  combined_data <- do.call(bind_rows, data_list)
  
}
