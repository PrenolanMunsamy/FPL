library(tidyverse)
library(here)

source(here("scripts","99. Constants.R"))
source(here("utils/find_substitute.R"))
source(here("utils/weekly_transfer.R"))

# gw <- 2

# Import Current Team ----
current_team <- 
  read_csv(here(paste0("data/starting team/season ",curr_season),
                paste0("season_",curr_season,"_gw",gw-1,"_team.csv")))

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



# predictions_df <-
#   predictions_df %>%
#   mutate(pred_diff = .pred-total_points_avg_last_5,
#          .pred_grouped = case_when(.pred<=0 ~ 0,
#                                    .pred>0 & .pred<2 ~ 1,
#                                    .pred>=2 & .pred<3 ~ 2,
#                                    .pred>=3 & .pred<4 ~ 3,
#                                    .pred>=4 & .pred<5 ~ 4,
#                                    .pred>=5 & .pred<6 ~ 5,
#                                    .pred>=6 ~ 6)) %>%
#   filter(position!="GK") %>%
#   # filter(.pred_grouped >= 3) %>%
#   filter(total_points_std_last_5_to_avg_ratio>0) %>%
#   mutate(.pred_ratio = total_points_avg_last_5/.pred) %>%
#   filter(position=="FWD") %>%
#   arrange(desc(.pred))

predictions_df <-
  predictions_df %>%
  select(team,name,position,value,.pred,consistency_segments)

# tmp <- current_team %>%
#   left_join(predictions_df,by=c("team","name","position")) %>%
#   arrange(position,desc(.pred))
# 
# min(tmp$.pred)

# Suggested Weekly Transfer ----
current_team <- weekly_transfer(current_team,predictions_df)

# Export ----
current_team %>% 
  write_csv(here(paste0("data/starting team/season ",curr_season),
                 paste0("season_",curr_season,"_gw",gw,"_team.csv")))  
