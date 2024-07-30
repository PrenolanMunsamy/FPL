# Import Libraries ----
library(tidyverse)
library(here)

# Import Helper Functions ----
source(here("utils/import_all_gws.R"))
source(here("utils/cast_correct_dtypes.R"))
source(here("utils/add_opponent_team_name.R"))
source(here("utils/add_season.R"))
source(here("utils/preprocessing.R"))
source(here("utils/fpl_lag_data.R"))
source(here("utils/fpl_sum_all_previous.R"))
source(here("utils/fpl_sum_n_previous.R"))
source(here("utils/fpl_avg_n_previous.R"))
source(here("utils/import_all_players_understat.R"))

# Import Data ----
# **** Historical GoalWeeks ----
season_2020_21_gws <- import_all_gws("data/raw/2020-21/gws","2020_21")
season_2021_22_gws <- import_all_gws("data/raw/2021-22/gws","2021_22")
season_2022_23_gws <- import_all_gws("data/raw/2022-23/gws","2022_23")

# # **** Set Correct Data Types
# season_2020_21_gws <- cast_correct_dtypes(season_2020_21_gws)
# season_2021_22_gws <- cast_correct_dtypes(season_2021_22_gws)
# season_2022_23_gws <- cast_correct_dtypes(season_2022_23_gws)
# 
# # Add Season Variable
# season_2020_21_gws <- add_season(season_2020_21_gws,"2020_21")
# season_2021_22_gws <- add_season(season_2021_22_gws,"2021_22")
# season_2022_23_gws <- add_season(season_2022_23_gws,"2022_23")

# **** Team ID and Name data ----
# Include Historical and Current Season
season_2020_21_teams <- read_csv(here("data/raw/2020-21","teams.csv"))
season_2021_22_teams <- read_csv(here("data/raw/2021-22","teams.csv"))
season_2022_23_teams <- read_csv(here("data/raw/2022-23","teams.csv"))
season_2023_24_teams <- read_csv(here("data/raw/2023-24","teams.csv"))

season_2020_21_teams <-
  season_2020_21_teams %>% 
  mutate(season_x = "2020_21")

season_2021_22_teams <-
  season_2021_22_teams %>% 
  mutate(season_x = "2021_22")

season_2022_23_teams <-
  season_2022_23_teams %>% 
  mutate(season_x = "2022_23")

season_2023_24_teams <-
  season_2023_24_teams %>% 
  mutate(season_x = "2023_24")

season_combined_teams <-
  bind_rows(season_2020_21_teams,
            season_2021_22_teams,
            season_2022_23_teams,
            season_2023_24_teams)

# **** Team Strength Data ----
season_2020_21_fdr <-read_csv(here("data/raw/2020-21","fixtures.csv"))
season_2021_22_fdr <-read_csv(here("data/raw/2021-22","fixtures.csv"))
season_2022_23_fdr <- read_csv(here("data/raw/2022-23","fixtures.csv"))

season_2020_21_fdr <-
  season_2020_21_fdr %>% 
  mutate(season_x = "2020_21")

season_2021_22_fdr <-
  season_2021_22_fdr %>% 
  mutate(season_x = "2021_22")

season_2022_23_fdr <-
  season_2022_23_fdr %>% 
  mutate(season_x = "2022_23")

season_combined_fdr <-
  bind_rows(season_2020_21_fdr,
            season_2021_22_fdr,
            season_2022_23_fdr)

# **** Player Understat Data ----
# Historical and Current Seasons
season_2021_22_players_understat <- import_all_players_understat("data/raw/2021-22/understat")
season_2022_23_players_understat <- import_all_players_understat("data/raw/2022-23/understat")
season_2023_24_players_understat <- import_all_players_understat("data/raw/2023-24/understat")


# Export ----
# season_2020_21_gws %>%
#   write_csv(here("data/processed/historical",
#                  "season_2020_21_gws.csv"))

season_2021_22_gws %>% 
  write_csv(here("data/processed/historical",
                 "season_2021_22_gws.csv"))

season_2022_23_gws %>% 
  write_csv(here("data/processed/historical",
                 "season_2022_23_gws.csv"))

season_2021_22_players_understat %>% 
  write_csv(here("data/processed/player_understat",
                 "season_2021_22_players_understat.csv"))

season_2022_23_players_understat %>% 
  write_csv(here("data/processed/player_understat",
                 "season_2022_23_players_understat.csv"))

season_2023_24_players_understat %>% 
  write_csv(here("data/processed/player_understat",
                 "season_2023_24_players_understat.csv"))

season_combined_fdr %>% 
  write_csv(here("data/processed/fdr",
                 "season_combined_fdr.csv"))

season_combined_teams %>% 
  write_csv(here("data/processed/team_id_name",
                 "season_combined_teams.csv"))