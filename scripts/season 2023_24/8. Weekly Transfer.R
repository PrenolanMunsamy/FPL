library(tidyverse)
library(here)
library(ompr)
library(ompr.roi)
library(ROI)
library(ROI.plugin.glpk)

# source(here("scripts/season 2022_23","99. Constants.R"))
source(here("utils/find_substitute.R"))
source(here("utils/free_hit.R"))
source(here("utils/weekly_transfer.R"))
source(here("utils/import_all_gws.R"))
source(here("utils/preprocessing.R"))
source(here("utils/feature_engineering.R"))
source(here("utils","initial_team_selection - total points.R"))
source(here("utils","team_rules_checks.R"))

# gw <- 2

# Import Chip Metadata ----
metadata <- readxl::read_xlsx(here(paste0("data/metadata/season ",curr_season),
                                   "metadata.xlsx"),
                              sheet = 1)
  
# Import Current Team ----
current_team <- 
  read_csv(here(paste0("data/starting team/season ",curr_season),
                paste0("season_",curr_season,"_gw",gw-1,"_team.csv")))

current_team_copy <- current_team

current_team <-
  current_team %>% 
  select(-contains(".pred"))

current_team <- 
  current_team %>% 
  mutate(key_player = case_when(name=="erling haaland"~1,
                                name=="mohamed salah"~0,
                                name=="martin odegaard"~0,
                                .default = 0))
  
# Import Predictions for the current week ----
predictions_df <-
  read_csv(here(paste0("data/predictions/season ",curr_season,"/gw",gw),
                "predicted_points.csv"))

# Assuming 'predicted_points' is a data frame with columns: player_id, predicted_points
alpha <- 0.05  # Weight for the next fixture difficulty
beta <- 0.03   # Weight for the average difficulty of the next 5 fixtures

predictions_df <-
  predictions_df %>%
  mutate(.pred = .pred * (1 - beta * team_difficulty_avg_next_5)) %>% 
  select(-team_difficulty_avg_next_5)

predictions_df <-
  predictions_df %>%
  select(team,name,position,value,.pred,consistency_segments)

# Suggested Weekly Transfer ----
current_team <- weekly_transfer(current_team,predictions_df)
missing_player_count <-
  current_team %>% 
  filter(starting_11=="Yes") %>% 
  filter(.pred<0) %>% 
  nrow()

# Check for Free Hit ----
if (metadata$free_hit==0 & missing_player_count>=3){
  
  # Corrected Player Names
  player_name_fix <- readxl::read_xlsx(here("data",
                                            "player name alignment.xlsx"),
                                       sheet = 1,
                                       col_types = c("text","text","text"))
  
  player_name_fix <-
    player_name_fix %>% 
    mutate(final_name = ifelse(is.na(final_name),name_copy,final_name)) %>% 
    select(-c(name_copy))
  
  # Understat Data
  understat_combined <-
    read_csv(here(paste0("data/processed/player_understat/season ",curr_season),
                  "season_combined_players_understat.csv")) %>% 
    arrange(YEAR,MONTH,DAY) %>% 
    distinct()
  
  # FDR
  fdr_df <- 
    read_csv(here(paste0("data/processed/fdr/season ",curr_season),
                  "season_combined_fdr.csv"))
  
  # Team Names and IDs
  team_name_id_df <- 
    read_csv(here(paste0("data/processed/team_id_name/season ",curr_season),
                  "season_combined_teams.csv"))
  
  # import current season previous weeks data
  current_season_prev_gws <- 
    import_all_gws(here(paste0("data/processed/historical/season ",curr_season)),
                   curr_season)
  
  current_season_prev_gws <- preprocessing(current_season_prev_gws,
                                           player_name_fix,
                                           understat_combined,
                                           fdr_df,
                                           team_name_id_df)
  
  current_season_prev_gws <- feature_engineering(current_season_prev_gws)
  
  current_season_prev_gws <- 
    current_season_prev_gws %>% 
    group_by(team,name) %>% 
    summarise(total_points = sum(total_points,na.rm = TRUE)) %>%   
    ungroup()
  
  # obtain a brand new team that will be used only for the upcoming week
  # we will also make a copy of the previous weeks team to serve as the base team for the next games transfer
  current_team_free_hit <- free_hit(current_team,
                                    current_season_prev_gws,
                                    predictions_df %>% 
                                      select(team,name,position,value,.pred) %>% 
                                      distinct())
  
  current_team_free_hit %>%
    write_csv(here(paste0("data/starting team/season ",curr_season),
                   paste0("season_",curr_season,"_gw",gw,"_team_free_hit.csv")))

  current_team <- current_team_copy
  
  # change the status of free hit to 1, since it can only be used once a season
  metadata$free_hit <- 1
  metadata %>% openxlsx::write.xlsx(here(paste0("data/metadata/season ",curr_season),
                                         "metadata.xlsx"))
  
} else if (metadata$wild_card==0 & gw>=6 & gw<=19){
  # Corrected Player Names
  player_name_fix <- readxl::read_xlsx(here("data",
                                            "player name alignment.xlsx"),
                                       sheet = 1,
                                       col_types = c("text","text","text"))

  player_name_fix <-
    player_name_fix %>%
    mutate(final_name = ifelse(is.na(final_name),name_copy,final_name)) %>%
    select(-c(name_copy))

  # Understat Data
  understat_combined <-
    read_csv(here(paste0("data/processed/player_understat/season ",curr_season),
                  "season_combined_players_understat.csv")) %>%
    arrange(YEAR,MONTH,DAY) %>%
    distinct()

  # FDR
  fdr_df <- 
    read_csv(here(paste0("data/processed/fdr/season ",curr_season),
                  "season_combined_fdr.csv"))

  # Team Names and IDs
  team_name_id_df <- 
    read_csv(here(paste0("data/processed/team_id_name/season ",curr_season),
                  "season_combined_teams.csv"))

  # import current season previous weeks data
  current_season_prev_gws <-
    import_all_gws(here(paste0("data/processed/historical/season ",curr_season)),
                   curr_season)

  current_season_prev_gws <- preprocessing(current_season_prev_gws,
                                           player_name_fix,
                                           understat_combined,
                                           fdr_df,
                                           team_name_id_df)

  current_season_prev_gws <- feature_engineering(current_season_prev_gws)

  current_season_prev_gws <-
    current_season_prev_gws %>%
    group_by(team,name) %>%
    summarise(total_points = sum(total_points,na.rm = TRUE)) %>%
    ungroup()

  current_team_wild_card <- free_hit(current_team,
                                     current_season_prev_gws,
                                     predictions_df %>%
                                       select(team,name,position,value,.pred) %>%
                                       distinct())

  if (sum(current_team$.pred)<sum(current_team_wild_card$.pred)){
    current_team <- current_team_wild_card
    print(paste0("using wild_card in gw: ",gw))
  }
   
  # change the status of free hit to 1, since it can only be used once a season
  metadata$wild_card <- 1
  metadata %>% openxlsx::write.xlsx(here(paste0("data/metadata/season ",curr_season),
                                         "metadata.xlsx"))
}
  
# Export ----
current_team %>% 
  write_csv(here(paste0("data/starting team/season ",curr_season),
                 paste0("season_",curr_season,"_gw",gw,"_team.csv")))  
