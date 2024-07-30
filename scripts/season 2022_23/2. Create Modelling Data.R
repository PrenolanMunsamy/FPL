# Import Libraries ----
library(tidyverse)
library(here)

# Import Helper Functions ----
source(here("scripts/season 2022_23","99. Constants.R"))
source(here("utils/import_all_gws.R"))
source(here("utils/cast_correct_dtypes.R"))
source(here("utils/add_opponent_team_name.R"))
source(here("utils/add_season.R"))
source(here("utils/preprocessing.R"))
source(here("utils/feature_engineering.R"))
source(here("utils/generate_seasons.R"))
source(here("utils","row_bind_pattern_objects.R"))

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
fdr_df <- read_csv(here(paste0("data/processed/fdr/season ",curr_season),
                        "season_combined_fdr.csv"))

# **** Team Names and IDs ----
team_name_id_df <- read_csv(here(paste0("data/processed/team_id_name/season ",curr_season),
                                 "season_combined_teams.csv"))

# **** Historical Weekly Data ----
seasons <- generate_seasons(first_season, curr_season)
seasons

season_combined_gws <- read_csv(here(paste0("data/processed/historical/season ",curr_season),
                                     "season_combined_gws.csv"))

# Remove Duplicate Info ----
combined_df <- 
  season_combined_gws %>% 
  distinct(pick(-contains("GoalWeek")),.keep_all=TRUE)

# **** Understat Data ----
understat_combined <-
  read_csv(here(paste0("data/processed/player_understat/season ",curr_season),
                "season_combined_players_understat.csv")) %>% 
  distinct()

# Preprocessing ----
combined_df <- preprocessing(combined_df,
                             player_name_fix,
                             understat_combined,
                             fdr_df,
                             team_name_id_df)

# Add Lag and Aggregate Variables ----
combined_df <- feature_engineering(combined_df)

# Export ----
combined_df %>%
  mutate(across(.cols = everything(),
                .fns = ~ ifelse(is.na(.x)|is.nan(.x),0,.x))) %>% 
  arrange(YEAR,MONTH,DAY,team,name) %>% 
  select(YEAR,MONTH,DAY,season_x,match_number,player_match_number,GoalWeek,name,everything()) %>% 
  write_csv(here(paste0("data/model/season ",curr_season),
                 "model_data.csv"))

