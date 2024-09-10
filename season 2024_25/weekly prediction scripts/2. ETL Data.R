# Import Libraries ----
library(tidyverse)
library(here)

# Import Helper Functions ----
source(here("season 2024_25","99. Constants.R"))
source(here("season 2024_25/utils/import_all_gws.R"))
source(here("season 2024_25/utils/cast_correct_dtypes.R"))
source(here("season 2024_25/utils/add_opponent_team_name.R"))
source(here("season 2024_25/utils/add_season.R"))
source(here("season 2024_25/utils/preprocessing.R"))
source(here("season 2024_25/utils/fpl_lag_data.R"))
source(here("season 2024_25/utils/fpl_sum_all_previous.R"))
source(here("season 2024_25/utils/fpl_sum_n_previous.R"))
source(here("season 2024_25/utils/fpl_avg_n_previous.R"))
source(here("season 2024_25/utils/import_all_players_understat.R"))
source(here("season 2024_25/utils/generate_seasons.R"))
source(here("season 2024_25/utils","row_bind_pattern_objects.R"))
 
# gw <- 3

# Import Data ----
# **** Team ID and Name data ----
# Include Current Season Only
for (season in curr_season){
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

# **** Player Understat Data ----
# Include Current Season Only
if (gw==1){
  
} else {
  for (season in curr_season){
    print(season)
    
    var_name <- paste0("season_", season, "_players_understat")
    
    assign(var_name, 
           import_all_players_understat(paste0("raw data/",
                                               str_replace(season,coll("_"),"-"),
                                               "/understat")))
  }
}

season_combined_understat <- row_bind_pattern_objects("_understat")

season_combined_understat <-
  season_combined_understat %>% 
  distinct()

# Export ----
season_combined_understat %>% 
  write_csv(here(paste0("season ",curr_season,"/data/player_understat_current_season"),
                 "curr_season_players_understat.csv"))

season_combined_teams %>% 
  write_csv(here(paste0("season ",curr_season,"/data/team_id_name_current_seasons"),
                 "curr_season_teams.csv"))