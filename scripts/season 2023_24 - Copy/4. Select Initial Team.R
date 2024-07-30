# Load necessary libraries
#library(lpSolve)
library(tidyverse)
library(here)
library(ompr)
library(ompr.roi)
library(ROI)
library(ROI.plugin.glpk)

source(here("scripts/99. Constants.R"))
source(here("utils","initial_team_selection - roi.R"))
source(here("utils","initial_team_selection - total points.R"))
source(here("utils","team_rules_checks.R"))

df <- read_csv(here(paste0("data/initial_team_data_",curr_season,".csv")))

df %>% 
  rename(team=new_season_team,
         total_points = previous_season_total_points,
         value = new_season_value) %>% 
  arrange(desc(total_points)) %>% 
  View()

optimal_starting_team <-
  initial_team_selection_roi(df %>% 
                               filter(name != "harry kane") %>% 
                               filter(name != "emerson") %>% 
                               rename(team=new_season_team,
                                      total_points = previous_season_total_points,
                                      value = new_season_value))


team_rules_checks(optimal_starting_team)

# optimal_starting_team %>% 
#   group_by(position) %>% 
#   mutate(starting_11 = ifelse(row_number()==n(),"no","yes")) %>% 
#   ungroup() %>% 
#   View()

# Export Initial Team ----
optimal_starting_team %>%
  rename(previous_season_sum_total_points=total_points) %>% 
  select(team,name,position,value,previous_season_sum_total_points, starting_11) %>% 
  arrange(position) %>% 
  mutate(captain = ifelse(previous_season_sum_total_points==max(previous_season_sum_total_points),"captain","not captain")) %>% 
  write_csv(here(paste0("data/starting team/season ",curr_season),
                 paste0("season_",curr_season,"_gw1_team.csv")))