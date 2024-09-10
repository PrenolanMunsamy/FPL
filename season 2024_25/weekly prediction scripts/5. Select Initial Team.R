# Load necessary libraries
#library(lpSolve)
library(tidyverse)
library(here)
library(ompr)
library(ompr.roi)
library(ROI)
library(ROI.plugin.glpk)

source(here("season 2024_25","99. Constants.R"))
source(here("season 2024_25/utils","initial_team_selection - roi.R"))
source(here("season 2024_25/utils","initial_team_selection - total points.R"))
source(here("season 2024_25/utils","initial_team_selection - total points best 11.R"))
source(here("season 2024_25/utils","team_rules_checks.R"))


fdr_df <- 
  read_csv(here(paste0("season ",curr_season,"/data/fdr_current_season"),
                       "fdr_upcoming_season.csv"))

df <- 
  read_csv(here(paste0("season ",curr_season,"/data/initial_team_data_",
                       curr_season,".csv")))

df %>% 
  rename(team=new_season_team,
         total_points = previous_season_total_points,
         value = new_season_value) %>% 
  arrange(desc(total_points)) %>% 
  left_join(fdr_df %>% 
              group_by(team) %>% 
              arrange(kickoff_time) %>% 
              slice_head(n=1) %>% 
              ungroup() %>% 
              select(team,team_difficulty_avg_next_5),
            by=c("team"="team")) %>% 
  View()

# optimal_starting_team <-
#   initial_team_selection_roi(df %>%
#                                filter(name != "harry kane") %>%
#                                filter(name != "emerson") %>%
#                                filter(name != "jarrad branthwaite") %>%
#                                filter(name != "julian alvarez") %>%
#                                filter(name != "pascal gross") %>%
#                                rename(team=new_season_team,
#                                       total_points = previous_season_total_points,
#                                       value = new_season_value))


optimal_starting_team <-
  initial_team_selection_total_points(df %>%
                                        filter(name != "harry kane") %>%
                                        filter(name != "emerson") %>%
                                        filter(name != "jarrad branthwaite") %>%
                                        filter(name != "julian alvarez") %>%
                                        filter(name != "pascal gross") %>%
                                        # filter(name != "phil foden") %>%
                                        # filter(name != "cole palmer") %>%
                                        # filter(previous_season_total_points>=125) %>%
                                        rename(team=new_season_team,
                                               total_points = previous_season_total_points,
                                               value = new_season_value))
                                        # left_join(fdr_df %>%
                                        #             group_by(team) %>%
                                        #             arrange(kickoff_time) %>%
                                        #             slice_head(n=1) %>%
                                        #             ungroup() %>%
                                        #             select(team,team_difficulty_avg_next_5),
                                        #           by=c("team"="team")) %>%
                                        # mutate(total_points = total_points))

# optimal_starting_team <-
#   initial_team_selection_total_points_best_11(df %>%
#                                         filter(name != "harry kane") %>%
#                                         filter(name != "emerson") %>%
#                                         filter(name != "jarrad branthwaite") %>%
#                                         filter(name != "julian alvarez") %>%
#                                         filter(name != "pascal gross") %>%
#                                         # filter(name != "phil foden") %>%
#                                         # filter(name != "cole palmer") %>%
#                                         # filter(previous_season_total_points>=125) %>% 
#                                         rename(team=new_season_team,
#                                                total_points = previous_season_total_points,
#                                                value = new_season_value) %>% 
#                                           left_join(fdr_df %>%
#                                                       group_by(team) %>%
#                                                       arrange(kickoff_time) %>%
#                                                       slice_head(n=1) %>%
#                                                       ungroup() %>%
#                                                       select(team,team_difficulty_avg_next_5),
#                                                     by=c("team"="team")) %>%
#                                           mutate(total_points = total_points))


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
  write_csv(here(paste0("season ",curr_season,"/data/starting_team"),
                 paste0("season_",curr_season,"_gw1_team.csv")))