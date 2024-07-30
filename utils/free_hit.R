library(tidyverse)
library(here)

free_hit <- function(current_team,previous_weeks,upcoming_week){
  
  upcoming_week <-
    upcoming_week %>% 
    left_join(previous_weeks %>% 
                select(team,name,total_points)) %>% 
    filter(!is.na(total_points))
  
  View(upcoming_week)
  
  # apply_free_hit
  # need to get all previous data from this season
  # get upcoming week player values
  # run the optimisation to get the best team
  optimal_starting_team <-
    initial_team_selection_total_points(upcoming_week %>%
                                          filter(name != "emerson"))

    team_rules_checks(optimal_starting_team)

    # print("free hit active")

  current_team <- optimal_starting_team

  current_team_team <-
    current_team %>%
    mutate(captain = ifelse(total_points==max(total_points),"captain","not captain")) %>%
    mutate(key_player = 0) %>%
    rename(previous_season_sum_total_points=total_points) %>%
    select(team,name,position,value,previous_season_sum_total_points,starting_11,captain,key_player,.pred) %>%

    return(current_team)
}