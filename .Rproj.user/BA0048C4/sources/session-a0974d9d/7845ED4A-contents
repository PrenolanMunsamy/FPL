# Import Libraries ----
library(tidyverse)
library(here)

# Import Helper Functions ----
source(here("season 2024_25","99. Constants.R"))
source(here("season 2024_25/utils/cast_correct_dtypes.R"))
source(here("season 2024_25/utils/add_season.R"))
source(here("season 2024_25/utils/import_all_gws.R"))
source(here("season 2024_25/utils/cast_correct_dtypes.R"))
source(here("season 2024_25/utils/add_opponent_team_name.R"))
source(here("season 2024_25/utils/add_season.R"))
source(here("season 2024_25/utils/preprocessing.R"))
source(here("season 2024_25/utils/feature_engineering.R"))
source(here("season 2024_25/utils/generate_seasons.R"))
source(here("season 2024_25/utils","row_bind_pattern_objects.R"))
source(here("season 2024_25/utils/fpl_lag_data.R"))
source(here("season 2024_25/utils/fpl_sum_all_previous.R"))
source(here("season 2024_25/utils/fpl_sum_n_previous.R"))
source(here("season 2024_25/utils/fpl_avg_n_previous.R"))
source(here("season 2024_25/utils/fpl_lag_team_data.R"))
source(here("season 2024_25/utils/fpl_sum_all_previous_team.R"))
source(here("season 2024_25/utils/fpl_sum_n_previous_team.R"))
source(here("season 2024_25/utils/fpl_std_dev_n_previous.R"))
source(here("season 2024_25/utils/fpl_avg_n_previous.R"))
source(here("season 2024_25/utils/fpl_std_dev_to_avg_ratio.R"))

# Import Data ----
# **** Corrected Player Names ----
player_name_fix <- 
  readxl::read_xlsx(here(paste0("season ",curr_season,"/data/player names"),
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
fdr_df <- 
  read_csv(here(paste0("season ",curr_season,"/data/fdr_previous_seasons"),
                "season_combined_fdr.csv"))

# **** Team Names and IDs ----
team_name_id_df <- 
  read_csv(here(paste0("season ",curr_season,"/data/team_id_name_previous_seasons"),
                "season_combined_teams.csv"))

# **** Historical Weekly Data ----
seasons <- generate_seasons(first_season, curr_season)
seasons

season_combined_gws <- 
  read_csv(here(paste0("season ",curr_season,"/data/gws_previous_seasons"),
                "season_combined_gws.csv"))

# Remove Duplicate Info ----
combined_df <- 
  season_combined_gws %>% 
  distinct(pick(-contains("GoalWeek")),.keep_all=TRUE)

# **** Understat Data ----
understat_combined <-
  read_csv(here(paste0("season ",curr_season,"/data/player_understat_previous_seasons"),
                "season_combined_players_understat.csv")) %>% 
  distinct()

# Preprocessing ----
combined_df <- 
  preprocessing(combined_df,
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
  write_csv(here(paste0("season ",curr_season,"/data/model"),
                 "model_data.csv"))

