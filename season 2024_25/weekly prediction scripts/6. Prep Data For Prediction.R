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

# gw <- 3

print(paste0("current season:",curr_season))
print(paste0("upcoming goal week:",gw))

# Import Data ----
# **** Team Data ----
curr_season_teams <- 
  read_csv(here(paste0("raw data/",str_replace(curr_season,coll("_"),"-")),
                "teams.csv"))

# **** Team Strength Data ----
curr_season_fdr <- 
  read_csv(here(paste0("raw data/",str_replace(curr_season,coll("_"),"-")),
                "fixtures.csv"))

# Move Upcoming GW to Folder and Process it ----
upcoming_gw <-
  read_csv(here(paste0("season ",curr_season,"/data/gws_current_season_upcoming"),
                paste0("gw",gw,".csv")))

# Set Correct Data Types ----
upcoming_gw <- cast_correct_dtypes(upcoming_gw)

# Add Season Variable ----
upcoming_gw <- add_season(upcoming_gw,curr_season)

# Add Opponent Team Name ----
upcoming_gw <- 
  upcoming_gw %>% 
  left_join(curr_season_teams %>% 
              select(id,name) %>% 
              rename(opponent_team=id,
                     opp_team_name=name))

# Add Team Strength ----
upcoming_gw <-
  upcoming_gw %>% 
  mutate(team_a_name= ifelse(was_home==FALSE,team,opp_team_name),
         team_h_name = ifelse(was_home==TRUE,team,opp_team_name)) %>% 
  left_join(curr_season_fdr %>% 
              select(kickoff_time,team_a,team_h,team_a_difficulty,team_h_difficulty) %>% 
              left_join(curr_season_teams %>% 
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
  write_csv(here(paste0("season ",curr_season,"/data/gws_current_season_upcoming"),
                 paste0("gw",gw,".csv")))

# Move Previous GW file to Historical Folder ----
if (file.exists(here(paste0("season ",curr_season,"/data/gws_current_season_upcoming"),
                     paste0("gw",gw-1,".csv")))) {
  
  # Remove the file
  file_removed <- 
    file.remove(here(paste0("season ",curr_season,"/data/gws_current_season_upcoming"),
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
previous_gw <- 
  read_csv("https://github.com/vaastav/Fantasy-Premier-League/tree/master/data/2024-25/gws/gw3.csv")

previous_gw %>% 
  write_csv(here(paste0("raw data/",str_replace(curr_season,coll("_"),"-"),"/gws"),
                 paste0("gw",gw-1,".csv")))

# previous_gw <- 
#   read_csv(here(paste0("raw data/",str_replace(curr_season,coll("_"),"-"),"/gws"),
#                 paste0("gw",gw-1,".csv")))

previous_gw <-
  previous_gw %>% 
  mutate(GoalWeek=gw-1)

# Set Correct Data Types ----
previous_gw <- cast_correct_dtypes(previous_gw)

# Add Season Variable ----
previous_gw <- add_season(previous_gw,curr_season)

previous_gw %>% 
  write_csv(here(paste0("season ",curr_season,"/data/gws_current_season_completed"),
                 paste0("gw",gw-1,".csv")))