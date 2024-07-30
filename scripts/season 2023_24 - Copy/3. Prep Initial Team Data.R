library(tidyverse)
library(here)

source(here("scripts/99. Constants.R"))
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
  arrange(YEAR,MONTH,DAY) %>% 
  distinct()

# FDR ----
fdr_df <- read_csv(here("data/processed/fdr",
                        "season_combined_fdr.csv"))

# Team Names and IDs ----
team_name_id_df <- read_csv(here("data/processed/team_id_name",
                                 "season_combined_teams.csv"))

# **** Team Data ----
season_2020_21_teams <- read_csv(here("data/raw/2020-21","teams.csv"))
season_2021_22_teams <- read_csv(here("data/raw/2021-22","teams.csv"))
season_2022_23_teams <- read_csv(here("data/raw/2022-23","teams.csv"))
season_2023_24_teams <- read_csv(here("data/raw/2023-24","teams.csv"))

# **** Previous Season Processed Data ----
previous_season_df <-
  read_csv(here("data/processed/historical",
                paste0("season_",prev_season,"_gws.csv")))

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


# team and player reduction ----

team_fpl_ranking <-
  previous_season_df %>%
  filter(season_x==prev_season) %>%
  group_by(season_x,team) %>% 
  summarise(sum_total_points = sum(total_points,na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(season_x,desc(sum_total_points)) %>% 
  group_by(season_x) %>% 
  mutate(team_fpl_rank = row_number()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = season_x,
              values_from = c(sum_total_points,team_fpl_rank)) %>% 
  arrange(team) %>% 
  mutate(team_fpl_avg_rank = rowMeans(across(contains("rank")),na.rm = TRUE)) %>% 
  arrange(team_fpl_avg_rank)

team_fpl_top_10 <-
  team_fpl_ranking[1:10,]

top_5_players <-
  previous_season_df %>% 
  filter(team %in% team_fpl_top_10$team) %>% 
  group_by(season_x,team,position,name) %>% 
  summarise(player_sum_total_points = sum(total_points,na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(season_x,team,desc(player_sum_total_points)) %>% 
  group_by(season_x,team) %>% 
  mutate(top_5_player = ifelse(row_number()<=5,1,0)) %>% 
  ungroup() %>% 
  filter(top_5_player==1) %>% 
  filter(season_x==prev_season)

# Import GW1 Data for Current Season ----
new_season_df <-
  read_csv(here(paste0("data/raw/",str_replace(curr_season,"_","-"),"/gws"),"gw1.csv"))

new_season_df <-
  new_season_df %>% 
  distinct(pick(-contains("GoalWeek")),.keep_all=TRUE)

# Set Correct Data Types ----
new_season_df <- cast_correct_dtypes(new_season_df)

# Add Season Variable ----
new_season_df <- add_season(new_season_df,curr_season)

# # Add Opponent Team Name ----
# new_season_df <- 
#   new_season_df %>% 
#   left_join(get(paste0("season_",curr_season,"_teams")) %>% 
#               select(id,name) %>% 
#               rename(opponent_team=id,
#                      opp_team_name=name))

new_season_df <- preprocessing(new_season_df,
                               player_name_fix,
                               understat_combined,
                               fdr_df,
                               team_name_id_df)

new_season_df <- feature_engineering(new_season_df)

data_for_initial_team <-
  create_data_for_initial_team(#previous_season_df=top_5_players,
                               previous_season_df=previous_season_df,
                               new_season_df=new_season_df)

# data_for_initial_team <-
#   data_for_initial_team %>% 
#   filter(name %in% top_5_players$name)

data_for_initial_team %>% 
  write_csv(here(paste0("data/initial_team_data_",curr_season,".csv")))
