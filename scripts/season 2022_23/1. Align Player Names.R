library(tidyverse)
library(here)

# Import Data ----
# **** Historicql GoalWeek Data ----
season_combined_gws <- 
  read_csv(here(paste0("data/processed/historical/season ",curr_season),
                "season_combined_gws.csv"))

# **** Current GoalWeek Data ----
curr_season_gws <-
  import_all_gws(paste0("data/raw/",
                        str_replace(curr_season,coll("_"),"-"),"/gws"),
                 season) %>% 
  mutate(season_x = curr_season)

# **** Understat Data ----
season_combined_understat <- 
  read_csv(here(paste0("data/processed/player_understat/season ",curr_season),
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

curr_season_gws_names <-
  curr_season_gws %>% 
  filter(minutes>0) %>% 
  select(name) %>%
  mutate(name = stringi::stri_trans_general(name,"Latin-ASCII"),
         name = str_replace_all(name,coll("-")," "),
         name = tolower(name)) %>% 
  distinct()

# Check for missing names ----
bind_rows(season_combined_gws_names,
          curr_season_gws_names) %>% 
  distinct() %>% 
  full_join(combined_understat_names %>% 
              mutate(name_copy = name)) %>% 
  select(-(Player_Name_ID)) %>% 
  arrange(name) %>% 
  write_csv(here("data","player name alignment.csv"))