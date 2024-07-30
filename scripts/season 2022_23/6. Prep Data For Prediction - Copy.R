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

# gw <- 2
print(paste0("current season:",curr_season))
print(paste0("upcoming goal week:",gw))

# Import Data ----
# **** Team Data ----
season_2022_23_teams <- read_csv(here("data/raw/2022-23","teams.csv"))

# **** Team Strength Data ----
season_2022_23_fdr <- read_csv(here("data/raw/2022-23","fixtures.csv"))

# Move Upcoming GW to Folder and Process it ----
upcoming_gw <- 
  read_csv(here(paste0("data/raw/",str_replace(curr_season,"_","-"),"/gws"),
                paste0("gw",gw,".csv")))

# Set Correct Data Types ----
upcoming_gw <- cast_correct_dtypes(upcoming_gw)

# Add Season Variable ----
upcoming_gw <- add_season(upcoming_gw,curr_season)

# Add Opponent Team Name ----
upcoming_gw <- 
  upcoming_gw %>% 
  left_join(get(paste0("season_",curr_season,"_teams")) %>% 
              select(id,name) %>% 
              rename(opponent_team=id,
                     opp_team_name=name))

# Add Team Stength ----
upcoming_gw <-
  upcoming_gw %>% 
  mutate(team_a_name= ifelse(was_home==FALSE,team,opp_team_name),
         team_h_name = ifelse(was_home==TRUE,team,opp_team_name)) %>% 
  left_join(season_2022_23_fdr %>% 
              select(kickoff_time,team_a,team_h,team_a_difficulty,team_h_difficulty) %>% 
              left_join(season_2022_23_teams %>% 
                          select(id,name) %>% 
                          rename(team_a_name=name),
                        by = c("team_a"="id")) %>% 
              mutate(kickoff_time = as.Date(kickoff_time)),
            by = c("kickoff_time","team_a_name")) %>% 
  mutate(team_difficulty = case_when(team==team_a_name~team_h_difficulty,
                                     team==team_h_name~team_a_difficulty),
         opponent_difficulty = case_when(opp_team_name==team_a_name~team_h_difficulty,
                                         opp_team_name==team_h_name~team_a_difficulty),
         difficulty_ratio = team_difficulty/opponent_difficulty) %>% 
  select(-c(team_a_name,team_h_name,team_a_difficulty,team_h_difficulty))

# Export Upcoming GW to Processed Folder ----
# team_difficulty,opponent_difficulty,difficulty_ratio
upcoming_gw %>% 
  select(name,position,team,kickoff_time,opponent_team,value,was_home) %>% 
  write_csv(here(paste0("data/processed/upcoming/season ",curr_season),
                 paste0("gw",gw,".csv")))

# Move Previous GW file to Historical Folder ----
if (file.exists(here(paste0("data/processed/upcoming/season ",curr_season),
                     paste0("gw",gw-1,".csv")))) {
  
  # Remove the file
  file_removed <- file.remove(here(paste0("data/processed/upcoming/season ",curr_season),
                                   paste0("gw",gw-1,".csv")))
  
  # Check if the file was successfully removed
  if (file_removed) {
    message("The file was successfully removed.")
    } else {
      warning("The file could not be removed.")
    }
  
  } else {
    message("The file does not exist.")
}

# Move Previous GW to Historical Folder
previous_gw <- read_csv(here(paste0("data/raw/",str_replace(curr_season,coll("_"),"-"),"/gws"),
                             paste0("gw",gw-1,".csv")))

previous_gw <-
  previous_gw %>% 
  mutate(GoalWeek=gw-1)

# Set Correct Data Types ----
previous_gw <- cast_correct_dtypes(previous_gw)

# Add Season Variable ----
previous_gw <- add_season(previous_gw,curr_season)

# # Add Opponent Team Name ----
# previous_gw <- 
#   previous_gw %>% 
#   left_join(get(paste0("season_",curr_season,"_teams")) %>% 
#               select(id,name) %>% 
#               rename(opponent_team=id,
#                      opp_team_name=name))
# 
# # Add Team Stength ----
# previous_gw <-
#   previous_gw %>% 
#   mutate(team_a_name= ifelse(was_home==FALSE,team,opp_team_name),
#          team_h_name = ifelse(was_home==TRUE,team,opp_team_name)) %>% 
#   left_join(season_2022_23_fdr %>% 
#               select(kickoff_time,team_a,team_h,team_a_difficulty,team_h_difficulty) %>% 
#               left_join(season_2022_23_teams %>% 
#                           select(id,name) %>% 
#                           rename(team_a_name=name),
#                         by = c("team_a"="id")) %>% 
#               mutate(kickoff_time = as.Date(kickoff_time)),
#             by = c("kickoff_time","team_a_name")) %>% 
#   mutate(team_difficulty = case_when(team==team_a_name~team_a_difficulty,
#                                      team==team_h_name~team_h_difficulty),
#          opponent_difficulty = case_when(opp_team_name==team_a_name~team_a_difficulty,
#                                          opp_team_name==team_h_name~team_h_difficulty),
#          difficulty_ratio = team_difficulty/opponent_difficulty) %>% 
#   select(-c(team_a_name,team_h_name,team_a_difficulty,team_h_difficulty))

previous_gw %>% 
  write_csv(here(paste0("data/processed/historical/season ",curr_season),
                 paste0("gw",gw-1,".csv")))