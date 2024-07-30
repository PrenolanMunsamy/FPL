library(tidyverse)
library(tidymodels)
library(here)

source(here("scripts/season 2022_23","99. Constants.R"))
source(here("utils","create_data_for_initial_team.R"))
source(here("utils/import_all_gws.R"))
source(here("utils/cast_correct_dtypes.R"))
source(here("utils/add_opponent_team_name.R"))
source(here("utils/add_season.R"))
source(here("utils/preprocessing.R"))
source(here("utils/fpl_lag_data.R"))
source(here("utils/fpl_sum_all_previous.R"))
source(here("utils/fpl_sum_n_previous.R"))
source(here("utils/fpl_avg_n_previous.R"))
source(here("utils/feature_engineering.R"))
source(here("utils/new_week_feature_engineering.R"))
source(here("utils/import_historical_data.R"))

# gw <- 2
# Import Models ----
gk_mod_reg <- 
  readRDS(here(paste0("models/season ",curr_season),
               "XGBMod_GK_Reg.rds"))

def_mod_reg <- 
  readRDS(here(paste0("models/season ",curr_season),
               "XGBMod_DEF_Reg.rds"))

mid_mod_reg <- 
  readRDS(here(paste0("models/season ",curr_season),
               "XGBMod_MID_Reg.rds"))

fwd_mod_reg <- 
  readRDS(here(paste0("models/season ",curr_season),
               "XGBMod_FWD_Reg.rds"))

# Import Data ----
# upcoming fdr ----
upcoming_fdr <-
  read_csv(here(paste0("data/processed/fdr/season ",curr_season),
                "upcoming_season.csv"))

# **** Corrected Player Names ----
player_name_fix <- readxl::read_xlsx(here("data",
                                          "player name alignment.xlsx"),
                                     sheet = 1,
                                     col_types = c("text","text","text"))

player_name_fix <-
  player_name_fix %>% 
  mutate(final_name = ifelse(is.na(final_name),name_copy,final_name)) %>% 
  select(-c(name_copy))

sum(is.na(player_name_fix$final_name))

# **** FDR ----
fdr_df <- read_csv(here(paste0("data/processed/fdr/season ",curr_season),
                        "season_combined_fdr.csv"))

current_season_fdr_df <- read_csv(here("data/raw/2022-23","fixtures.csv"))

fdr_df <-
  bind_rows(fdr_df,
            current_season_fdr_df %>% mutate(season_x = "2022_23"))

# **** Team Names and IDs ----
team_name_id_df <- read_csv(here(paste0("data/processed/team_id_name/season ",curr_season),
                                 "season_combined_teams.csv"))

# **** Understat Data ----
season_2021_22_players_understat <-
  read_csv(here(paste0("data/processed/player_understat/season ",curr_season),
                "season_2021_22_players_understat.csv"))

season_2022_23_players_understat <- 
  read_csv(here(paste0("data/processed/player_understat/season ",curr_season),
                "season_2022_23_players_understat.csv"))

understat_combined <-
  bind_rows(season_2021_22_players_understat,
            season_2022_23_players_understat) %>% 
  distinct()

# **** Import Historical Data ----
historical_df <- import_historical_data(paste0("data/processed/historical/season ",
                                               curr_season))
historical_df <-
  historical_df %>% 
  filter(!is.na(name))

# historical_df %>% filter(is.na(assists)) %>% View()

# historical_df %>% 
#   arrange(kickoff_time) %>% 
#   View()

# Set Correct Data Types ----
historical_df <- cast_correct_dtypes(historical_df)

# Import Data to Predict ----
single_gw_df <- 
  import_all_gws(paste0("data/processed/upcoming/season ",curr_season),curr_season)

# Set Correct Data Types ----
single_gw_df <- cast_correct_dtypes(single_gw_df)
# str(single_gw_df)

# Add Season Variable ----
single_gw_df <- add_season(single_gw_df,curr_season)

# Add Opponent Team Name ----
# single_gw_df <- add_opponent_team_name(single_gw_df)

# Add Opponent Team Name ----
# season_2023_24_teams <- read_csv(here("data/raw/2023-24","teams.csv"))
# season_2022_23_teams %>% View()

# single_gw_df <- 
#   single_gw_df %>% 
#   left_join(get(paste0("season_",curr_season,"_teams")) %>% 
#               select(id,name) %>% 
#               rename(opponent_team=id,
#                      opp_team_name=name))

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

# combined_df %>% 
#   filter(GoalWeek==gw & season_x==curr_season) %>% 
#   filter(minutes>0) %>% 
#   View()

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
  write_csv(here(paste0("data/predictions/season ",curr_season,"/gw",gw),
                 "predicted_points.csv"))