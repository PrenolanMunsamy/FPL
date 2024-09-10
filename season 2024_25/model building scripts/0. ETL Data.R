# Import Libraries ----
library(tidyverse)
library(here)

# Import Helper Functions ----
source(here("season 2024_25","99. Constants.R"))
source(here("season 2024_25/utils/cast_correct_dtypes.R"))
source(here("season 2024_25/utils/add_season.R"))
source(here("season 2024_25/utils/import_all_gws.R"))
source(here("season 2024_25/utils/cast_correct_dtypes.R"))
source(here("season 2024_25/utils/add_opponent_team_name.R"))
source(here("season 2024_25/utils/add_season.R"))
source(here("season 2024_25/utils/import_all_players_understat.R"))
source(here("season 2024_25/utils/generate_seasons.R"))
source(here("season 2024_25/utils","row_bind_pattern_objects.R"))

# Import Data ----
# **** Historical GoalWeeks ----
seasons <- generate_seasons(first_season, curr_season)
seasons

for (season in seasons[1:length(seasons)-1]){
  print(season)
  
  var_name <- paste0("season_", season, "_gws")
  
  assign(var_name, import_all_gws(paste0("raw data/",
                                         str_replace(season,coll("_"),"-"),"/gws"),
                                  season))
}

season_combined_gws <- row_bind_pattern_objects("_gws")

# **** Team ID and Name data ----
# Include Historical Data only
for (season in seasons[1:length(seasons)-1]){
  print(season)
  
  var_name <- paste0("season_", season, "_teams")
  
  assign(var_name, 
         read_csv(here(paste0("raw data/",str_replace(season,coll("_"),"-")),
                       "teams.csv")
                            ) %>%
           mutate(season_x = season)
           
         )
}

season_combined_teams <- row_bind_pattern_objects("_teams")

# **** Team Strength Data ----
# Include Historical Data only
for (season in seasons[1:length(seasons)-1]){
  print(season)
  
  var_name <- paste0("season_", season, "_fdr")
  
  assign(var_name, 
         read_csv(here(paste0("raw data/",str_replace(season,coll("_"),"-")),
                       "fixtures.csv")) %>%
           mutate(season_x = season)
         )
}

season_combined_fdr <- row_bind_pattern_objects("_fdr")

# **** Player Understat Data ----
# Include Historical Data only
# [1:length(seasons)-1]
for (season in seasons[1:length(seasons)-1]){
  print(season)
  
  var_name <- paste0("season_", season, "_players_understat")
  
  assign(var_name, 
         import_all_players_understat(paste0("raw data/",
                                             str_replace(season,coll("_"),"-"),
                                             "/understat")))
}

season_combined_understat <- row_bind_pattern_objects("_understat")

season_combined_understat <-
  season_combined_understat %>% 
  distinct()

# Export ----
season_combined_gws %>% 
  write_csv(here(paste0("season ",curr_season,"/data/gws_previous_seasons"),
                 "season_combined_gws.csv"))

season_combined_understat %>% 
  write_csv(here(paste0("season ",curr_season,"/data/player_understat_previous_seasons"),
                 "season_combined_players_understat.csv"))

season_combined_fdr %>% 
  write_csv(here(paste0("season ",curr_season,"/data/fdr_previous_seasons"),
                 "season_combined_fdr.csv"))

season_combined_teams %>% 
  write_csv(here(paste0("season ",curr_season,"/data/team_id_name_previous_seasons"),
                 "season_combined_teams.csv"))