library(tidyverse)
library(tidymodels)
library(here)

source(here("season 2024_25","99. Constants.R"))
source(here("season 2024_25/utils","create_data_for_initial_team.R"))
source(here("season 2024_25/utils/import_all_gws.R"))
source(here("season 2024_25/utils/cast_correct_dtypes.R"))
source(here("season 2024_25/utils/add_opponent_team_name.R"))
source(here("season 2024_25/utils/add_season.R"))
source(here("season 2024_25/utils/preprocessing.R"))
source(here("season 2024_25/utils/fpl_lag_data.R"))
source(here("season 2024_25/utils/fpl_sum_all_previous.R"))
source(here("season 2024_25/utils/fpl_sum_n_previous.R"))
source(here("season 2024_25/utils/fpl_avg_n_previous.R"))
source(here("season 2024_25/utils/feature_engineering.R"))
source(here("season 2024_25/utils/new_week_feature_engineering.R"))
source(here("season 2024_25/utils/import_historical_data.R"))
source(here("season 2024_25/utils/fpl_std_dev_n_previous.R"))
source(here("season 2024_25/utils/fpl_std_dev_to_avg_ratio.R"))
source(here("season 2024_25/utils/fpl_sum_all_previous_team.R"))
source(here("season 2024_25/utils/fpl_sum_n_previous_team.R"))

# gw <- 3

# Import Models ----
model_board <-
  pins::board_folder(here(paste0("season ",curr_season,"/models")),
                     versioned = TRUE)

gk_mod_reg <- 
  vetiver::vetiver_pin_read(model_board,
                            "xgb_gk_reg")

def_mod_reg <- 
  vetiver::vetiver_pin_read(model_board,
                            "xgb_def_reg")

mid_mod_reg <- 
  vetiver::vetiver_pin_read(model_board,
                            "xgb_mid_reg")

fwd_mod_reg <- 
  vetiver::vetiver_pin_read(model_board,
                            "xgb_fwd_reg")

# Import Data ----
# upcoming fdr ----
upcoming_fdr <-
  read_csv(here(paste0("season ",curr_season,"/data/fdr_current_season"),
                "fdr_upcoming_season.csv"))

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

current_season_fdr_df <- 
  read_csv(here(paste0("raw data/",str_replace(curr_season,coll("_"),"-")),
                "fixtures.csv"))

fdr_df <-
  bind_rows(fdr_df,
            current_season_fdr_df %>% mutate(season_x = curr_season))

##### NEED TO CHECK FROM HERE ###################################
# **** Team Names and IDs ----
prev_season_team_name_id_df <- 
  read_csv(here(paste0("season ",curr_season,"/data/team_id_name_previous_seasons"),
                "season_combined_teams.csv"))

curr_season_team_name_id_df <- 
  read_csv(here(paste0("season ",curr_season,"/data/team_id_name_current_seasons"),
                "curr_season_teams.csv"))

team_name_id_df <-
  bind_rows(prev_season_team_name_id_df,
            curr_season_team_name_id_df) %>% 
  distinct()

# **** Understat Data ----
prev_season_understat_combined <-
  read_csv(here(paste0("season ",curr_season,"/data/player_understat_previous_seasons"),
                "season_combined_players_understat.csv")) %>% 
  distinct()

curr_season_understat_combined <-
  read_csv(here(paste0("season ",curr_season,"/data/player_understat_current_season"),
                "curr_season_players_understat.csv")) %>% 
  distinct()

understat_combined <-
  bind_rows(prev_season_understat_combined,
            curr_season_understat_combined) %>% 
  distinct()

# **** Import Historical Data ----
prev_season_gws_df <- 
  import_historical_data(paste0("season ",curr_season,"/data/gws_previous_seasons"))

curr_season_gws_df <- 
  import_historical_data(paste0("season ",curr_season,"/data/gws_current_season_completed"))

historical_df <-
  bind_rows(prev_season_gws_df,curr_season_gws_df) %>% 
  filter(!is.na(name))

# Set Correct Data Types ----
historical_df <- cast_correct_dtypes(historical_df)

# Import Data to Predict ----
single_gw_df <- 
  import_all_gws(paste0("season ",curr_season,"/data/gws_current_season_upcoming"),
                 curr_season)

# Set Correct Data Types ----
single_gw_df <- cast_correct_dtypes(single_gw_df)
# str(single_gw_df)

# Add Season Variable ----
single_gw_df <- add_season(single_gw_df,curr_season)

# Feature Engineering ----
combined_df <- 
  bind_rows(historical_df,single_gw_df) %>% 
  mutate(across(.cols = -kickoff_time,
                .fns = ~ ifelse(GoalWeek==gw & season_x==curr_season & is.na(.x), 0, .x))) %>% 
  distinct(pick(-contains("GoalWeek")),.keep_all=TRUE)

combined_df <- preprocessing(combined_df,
                             player_name_fix,
                             understat_combined,
                             fdr_df,
                             team_name_id_df)

combined_df <- feature_engineering(combined_df)

combined_df <-
  combined_df %>%  
  left_join(upcoming_fdr %>%
              mutate(season_x = curr_season) %>% 
              select(kickoff_time,team,team_difficulty_avg_next_5))

# Data For Prediction ----
gk_single_gw_df <-
  combined_df %>% 
  filter(season_x==curr_season&GoalWeek==gw&position=="GK")

def_single_gw_df <-
  combined_df %>% 
  filter(season_x==curr_season&GoalWeek==gw&position=="DEF")

mid_single_gw_df <-
  combined_df %>% 
  filter(season_x==curr_season&GoalWeek==gw&position=="MID")

fwd_single_gw_df <-
  combined_df %>% 
  filter(season_x==curr_season&GoalWeek==gw&position=="FWD")

# Predictions ----
gk_single_gw_df <-
  augment(gk_mod_reg,gk_single_gw_df) %>% 
  select(kickoff_time,team,name,position,value,.pred,
         was_home,
         played_60_or_more_mins,
         contains("increase"),
         GoalWeek,
         contains("sum_last_5"),
         contains("ratio"),
         contains("consistency_seg"),
         team_difficulty_avg_next_5
  ) %>% 
  select(-contains("threat")) %>% 
  select(-contains("influence")) %>% 
  select(-contains("creativity"))

def_single_gw_df <-
  augment(def_mod_reg,def_single_gw_df) %>% 
  select(kickoff_time,team,name,position,value,.pred,
         was_home,
         played_60_or_more_mins,
         contains("increase"),
         GoalWeek,
         contains("sum_last_5"),
         contains("ratio"),
         contains("consistency_seg"),
         team_difficulty_avg_next_5
  ) %>% 
  select(-contains("threat")) %>% 
  select(-contains("influence")) %>% 
  select(-contains("creativity"))

mid_single_gw_df <-
  augment(mid_mod_reg,mid_single_gw_df) %>% 
  select(kickoff_time,team,name,position,value,.pred,
         was_home,
         played_60_or_more_mins,
         contains("increase"),
         GoalWeek,
         contains("sum_last_5"),
         contains("ratio"),
         contains("consistency_seg"),
         team_difficulty_avg_next_5
  ) %>% 
  select(-contains("threat")) %>% 
  select(-contains("influence")) %>% 
  select(-contains("creativity"))

fwd_single_gw_df <-
  augment(fwd_mod_reg,fwd_single_gw_df) %>% 
  select(kickoff_time,team,name,position,value,.pred,
         was_home,
         played_60_or_more_mins,
         contains("increase"),
         GoalWeek,
         contains("sum_last_5"),
         contains("ratio"),
         contains("consistency_seg"),
         team_difficulty_avg_next_5
  ) %>% 
  select(-contains("threat")) %>% 
  select(-contains("influence")) %>% 
  select(-contains("creativity"))

# Export Predictions ----
bind_rows(gk_single_gw_df,
          def_single_gw_df,
          mid_single_gw_df,
          fwd_single_gw_df) %>% 
  write_csv(here(paste0("season ",curr_season,"/data/predictions/",
                        "predicted_points_gw_",gw,".csv")))
