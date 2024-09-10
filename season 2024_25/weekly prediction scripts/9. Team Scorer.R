library(tidyverse)
library(here)

source(here("scripts/season 2022_23","99. Constants.R"))
source(here("utils/find_substitute.R"))

# **** Corrected Player Names ----
player_name_fix <- readxl::read_xlsx(here("data",
                                          "player name alignment.xlsx"),
                                     sheet = 1,
                                     col_types = c("text","text","text"))

player_name_fix <-
  player_name_fix %>% 
  mutate(final_name = ifelse(is.na(final_name),name_copy,final_name)) %>% 
  select(-c(name_copy))

# Initialize an empty dataframe to store the points tally for each game week
points_tally_df <- data.frame(gw = integer(), points_tally = integer())
result <- vector(mode = "list",length = 37)

# Loop over all game weeks
for (gw in c(1:38)) {
  # Import Latest Team
  current_team <- 
    read_csv(here(paste0("data/starting team/season ", curr_season),
                  paste0("season_",curr_season,"_gw", gw, "_team.csv")))
  
  # current_team <- 
  #   read_csv(here(paste0("data/starting team/season ", curr_season),
  #                 paste0("season_",curr_season,"_gw", 1, "_team.csv")))
  
  # Import Actual Points
  points_df <-
    read_csv(here(paste0("data/raw/", str_replace(curr_season, "_", "-"), "/gws"),
                  paste0("gw", gw, ".csv")))
  
  points_df <- 
    points_df %>% 
    select(name, total_points) %>% 
    mutate(name = stringi::stri_trans_general(name,"Latin-ASCII"),
           name = str_replace_all(name,coll("-")," "),
           name = tolower(name)) %>% 
    left_join(player_name_fix,
              by=c("name")) %>%
    mutate(final_name = ifelse(is.na(final_name),name,final_name)) %>%
    select(-name) %>%
    rename(name = final_name)
      
  
  # Add Points
  result[[gw]] <- current_team %>% 
    left_join(points_df %>% 
                mutate(name = stringi::stri_trans_general(name,"Latin-ASCII")) %>% 
                mutate(name = str_replace_all(name,coll("-")," ")) %>%
                mutate(name = tolower(name))) %>% 
    filter(starting_11 == "Yes") %>% 
    mutate(total_points = ifelse(captain=="captain",2*total_points,total_points))
  
  points_tally <- sum(result[[gw]]$total_points, na.rm = TRUE)
  
  # Append the points tally to the dataframe
  points_tally_df <- points_tally_df %>% 
    add_row(gw = gw, points_tally = points_tally)
}

# View the resulting dataframe
print(sum(points_tally_df$points_tally))

points_tally_df %>% 
  openxlsx::write.xlsx(here(paste0("data/points/season ",curr_season),"scores.xlsx"))

result %>% 
  openxlsx::write.xlsx(here(paste0("data/points/season ",curr_season),"scores_per_week.xlsx"))