library(tidyverse)
library(here)

# Import Data ----
# **** GoalWeek Data ----
season_2020_21_gws <- read_csv(here("data/processed/historical","season_2020_21_gws.csv"))
season_2021_22_gws <- read_csv(here("data/processed/historical","season_2021_22_gws.csv"))
season_2022_23_gws <- read_csv(here("data/processed/historical","season_2022_23_gws.csv"))

# **** Understat Data ----
season_2021_22_players_understat <- read_csv(here("data/processed/player_understat",
                                                  "season_2021_22_players_understat.csv"))

season_2022_23_players_understat <- read_csv(here("data/processed/player_understat",
                                                  "season_2022_23_players_understat.csv"))

season_2023_24_players_understat <- read_csv(here("data/processed/player_understat",
                                                  "season_2023_24_players_understat.csv"))

combined_understat <- bind_rows(season_2021_22_players_understat,
                                season_2022_23_players_understat,
                                season_2023_24_players_understat)

# Get Unique Names ----
combined_understat_names <-
  combined_understat %>% 
  select(Player_Name_ID,Player_Name) %>% 
  rename(name = Player_Name) %>% 
  # select(name) %>% 
  distinct()

season_2020_21_names <-
  season_2020_21_gws %>% 
  filter(minutes>0) %>% 
  select(name) %>%
  mutate(name = stringi::stri_trans_general(name,"Latin-ASCII"),
         name = str_replace_all(name,coll("-")," "),
         name = tolower(name)) %>% 
  distinct()

season_2021_22_names <-
  season_2021_22_gws %>% 
  filter(minutes>0) %>% 
  select(name) %>%
  mutate(name = stringi::stri_trans_general(name,"Latin-ASCII"),
         name = str_replace_all(name,coll("-")," "),
         name = tolower(name)) %>% 
  distinct()

season_2022_23_names <-
  season_2022_23_gws %>% 
  filter(minutes>0) %>% 
  select(name) %>%
  mutate(name = stringi::stri_trans_general(name,"Latin-ASCII"),
         name = str_replace_all(name,coll("-")," "),
         name = tolower(name)) %>% 
  distinct()

# Check for missing names ----
bind_rows(season_2020_21_names,
          season_2021_22_names,
          season_2022_23_names) %>% 
  distinct() %>% 
  full_join(combined_understat_names %>% 
              mutate(name_copy = name)) %>% 
  select(-(Player_Name_ID)) %>% 
  arrange(name) %>% 
  write_csv(here("data","player name alignment.csv"))


season_2020_21_names %>% 
  filter(name=="Thiago Thiago") %>% 
  View()