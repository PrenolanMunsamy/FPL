# Import Libraries ----
library(tidyverse)
library(here)

# Import Helper Functions ----
source(here("utils/import_all_gws.R"))
source(here("utils/cast_correct_dtypes.R"))
source(here("utils/add_opponent_team_name.R"))
source(here("utils/add_season.R"))
source(here("utils/preprocessing.R"))
source(here("utils/feature_engineering.R"))

# Import Data ----
# **** Corrected Player Names ----
player_name_fix <- readxl::read_xlsx(here("data",
                                          "player name alignment.xlsx"),
                                     sheet = 1,
                                     col_types = c("text","text","text")
                                     )

player_name_fix <-
  player_name_fix %>% 
  mutate(final_name = ifelse(is.na(final_name),name_copy,final_name)) %>% 
  select(-c(name_copy))

sum(is.na(player_name_fix$final_name))

# **** FDR ----
fdr_df <- read_csv(here("data/processed/fdr","season_combined_fdr.csv"))

# **** Team Names and IDs ----
team_name_id_df <- read_csv(here("data/processed/team_id_name","season_combined_teams.csv"))

# **** Historical Weekly Data ----
# season_2020_21_gws <- read_csv(here("data/processed/historical","season_2020_21_gws.csv"))
season_2021_22_gws <- read_csv(here("data/processed/historical","season_2021_22_gws.csv"))
season_2022_23_gws <- read_csv(here("data/processed/historical","season_2022_23_gws.csv"))

# Remove Duplicate Info ----
combined_df <- 
  bind_rows(#season_2020_21_gws,
            season_2021_22_gws,
            season_2022_23_gws) %>% 
  distinct(pick(-contains("GoalWeek")),.keep_all=TRUE)

# **** Understat Data ----
season_2021_22_players_understat <-
  read_csv(here("data/processed/player_understat",
                "season_2021_22_players_understat.csv"))

season_2022_23_players_understat <- 
  read_csv(here("data/processed/player_understat",
                "season_2022_23_players_understat.csv"))

season_2023_24_players_understat <- 
  read_csv(here("data/processed/player_understat",
                "season_2023_24_players_understat.csv"))

understat_combined <-
  bind_rows(season_2021_22_players_understat,
            season_2022_23_players_understat,
            season_2023_24_players_understat) %>% 
  distinct()

# Preprocessing ----
combined_df <- preprocessing(combined_df,
                             player_name_fix,
                             understat_combined,
                             fdr_df,
                             team_name_id_df)

# Add Lag and Aggregate Variables ----
combined_df <- feature_engineering(combined_df)

combined_df %>% 
  filter(season_x=="2021_22") %>% 
  select(YEAR,MONTH,DAY,name,contains("total_points")) %>% 
  select(-contains("grouped")) %>% 
  View()

# Export ----
combined_df %>%
  mutate(across(.cols = everything(),
                .fns = ~ ifelse(is.na(.x)|is.nan(.x),0,.x))) %>% 
  arrange(YEAR,MONTH,DAY,team,name) %>% 
  select(YEAR,MONTH,DAY,season_x,match_number,player_match_number,GoalWeek,name,everything()) %>% 
  write_csv(here("data/model",
                 "model_data.csv"))

