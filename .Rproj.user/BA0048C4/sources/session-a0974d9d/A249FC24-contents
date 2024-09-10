library(tidyverse)
library(here)

# Import helper functions ----
source(here("season 2024_25","99. Constants.R"))
source(here("season 2024_25/utils/cast_correct_dtypes.R"))
source(here("season 2024_25/utils/add_season.R"))
source(here("season 2024_25/utils/import_all_gws.R"))

# Import Data ----
# **** Previous Season Corrected Player Names ----
player_name_fix <- 
  readxl::read_xlsx(here(paste0("season ",prev_season,"/data/player names"),
                         "player name alignment.xlsx"),
                    sheet = 1,
                    col_types = c("text","text","text"))

player_name_fix <-
  player_name_fix %>% 
  mutate(final_name = ifelse(is.na(final_name),name_copy,final_name)) %>% 
  select(-c(name_copy))

sum(is.na(player_name_fix$final_name))

# **** Historical GoalWeek Data ----
season_combined_gws <- 
  read_csv(here(paste0("season ",curr_season,"/data/gws_previous_seasons"),
                "season_combined_gws.csv"))

# **** Understat Data ----
season_combined_understat <- 
  read_csv(here(paste0("season ",curr_season,"/data/player_understat_previous_seasons"),
                "season_combined_players_understat.csv"))

# Get Unique Names ----
combined_understat_names <-
  season_combined_understat %>% 
  select(Player_Name_ID,Player_Name) %>% 
  rename(name = Player_Name) %>% 
  # select(name) %>% 
  distinct()

season_combined_gws_names <-
  season_combined_gws %>% 
  filter(minutes>0) %>% 
  select(name) %>%
  mutate(name = stringi::stri_trans_general(name,"Latin-ASCII"),
         name = str_replace_all(name,coll("-")," "),
         name = tolower(name)) %>% 
  distinct()

# Check for missing names ----
season_combined_gws_names %>% 
  distinct() %>% 
  full_join(combined_understat_names %>% 
              mutate(name_copy = name)) %>% 
  select(-(Player_Name_ID)) %>%
  distinct() %>% 
  arrange(name) %>%
  write_csv(here(paste0("season ",curr_season,"/data/player names"),
                 "player name alignment.csv"))