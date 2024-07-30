library(tidyverse)
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

gw <- 1

# Import Data ----
# **** Corrected Player Names ----
player_name_fix <- readxl::read_xlsx(here("data",
                                          "player name alignment.xlsx"),
                                     sheet = 1,
                                     col_types = c("text","text","text"))

player_name_fix <-
  player_name_fix %>% 
  mutate(final_name = ifelse(is.na(final_name),name_copy,final_name)) %>% 
  select(-c(name_copy))

# **** Understat Data ----
understat_combined <-
  read_csv(here(paste0("data/processed/player_understat/season ",curr_season),
                "season_combined_players_understat.csv")) %>%  
  arrange(YEAR,MONTH,DAY) %>% 
  distinct()

# FDR ----
fdr_df <- read_csv(here(paste0("data/processed/fdr/season ",curr_season),
                        "season_combined_fdr.csv"))

# Team Names and IDs ----
team_name_id_df <- read_csv(here(paste0("data/processed/team_id_name/season ",curr_season),
                                 "season_combined_teams.csv"))

# **** Team Data ----
# season_2020_21_teams <- read_csv(here("data/raw/2020-21","teams.csv"))
# season_2021_22_teams <- read_csv(here("data/raw/2021-22","teams.csv"))
# season_2022_23_teams <- read_csv(here("data/raw/2022-23","teams.csv"))
# season_2023_24_teams <- read_csv(here("data/raw/2023-24","teams.csv"))

# **** Previous Season Processed Data ----
previous_season_df <-
  read_csv(here(paste0("data/processed/historical/season ",curr_season),
                "season_combined_gws.csv")) %>%  
  filter(season_x==prev_season)

previous_season_df <-
  previous_season_df %>% 
  distinct(pick(-contains("GoalWeek")),.keep_all=TRUE)

# Feature Engineering ----
previous_season_df <- preprocessing(previous_season_df,
                                    player_name_fix,
                                    understat_combined,
                                    fdr_df,
                                    team_name_id_df)

previous_season_df <- feature_engineering(previous_season_df)

# Import GW1 Data for Current Season ----
new_season_df <-
  read_csv(here(paste0("data/raw/",
                       str_replace(curr_season,"_","-"),"/gws"),"gw1.csv"))

# Set Correct Data Types ----
new_season_df <- cast_correct_dtypes(new_season_df)

# Add Season Variable ----
new_season_df <- add_season(new_season_df,curr_season)

new_season_df <- preprocessing(new_season_df,
                               player_name_fix,
                               understat_combined,
                               fdr_df,
                               team_name_id_df)

# new_season_df <- feature_engineering(new_season_df)

data_for_initial_team <-
  create_data_for_initial_team(#previous_season_df=top_5_players,
                               previous_season_df=previous_season_df,
                               new_season_df=new_season_df)

data_for_initial_team %>% 
  write_csv(here(paste0("data/initial_team_data_",curr_season,".csv")))
