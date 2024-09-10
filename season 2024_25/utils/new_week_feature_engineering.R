# Import Libraries ----
library(tidyverse)
library(here)

# Import Helper Functions ----
new_week_feature_engineering <- function(df){
  # Add Variables ----
  df <-
    df %>% 
    mutate(match_number=0,
           total_points=0,
           xP=0,
           assists=0,
           bonus=0,
           bps=0,
           clean_sheets=0,
           creativity=0,
           element=0,
           fixture=0,
           goals_conceded=0,
           goals_scored=0,
           ict_index=0,
           influence=0,
           minutes=0,
           own_goals=0,
           penalties_missed=0,
           penalties_saved=0,
           red_cards=0,
           round=0,
           saves=0,
           selected=0,
           team_a_score=0,
           team_h_score=0,
           threat=0,
           transfers_balance=0,
           transfers_in=0,
           transfers_out=0,
           was_home=0,
           yellow_cards=0)
  
  df <-
    preprocessing(df)
  
  # Add Lagged Variables ----
  vars_to_lag <-
    as.character(expression(value,
                            total_points,
                            assists,
                            bonus,
                            bps,
                            clean_sheets,
                            goals_conceded,
                            goals_scored,
                            ict_index,
                            influence,
                            creativity,
                            threat,
                            own_goals,
                            penalties_missed,
                            penalties_saved,
                            yellow_cards,
                            red_cards,
                            minutes,
                            match_result
    ))
  
  df <-
    fpl_lag_data(df,
                 vars_to_lag = vars_to_lag,
                 lag=1)
  
  df <-
    fpl_lag_data(df,
                 vars_to_lag = vars_to_lag,
                 lag=3)
  
  df <-
    fpl_lag_data(df,
                 vars_to_lag = vars_to_lag,
                 lag=5)
  
  # Add Aggregate Variables ----
  vars_to_aggreg <-
    as.character(expression(value,
                            total_points,
                            assists,
                            bonus,
                            bps,
                            clean_sheets,
                            goals_conceded,
                            goals_scored,
                            ict_index,
                            influence,
                            creativity,
                            threat,
                            own_goals,
                            penalties_missed,
                            penalties_saved,
                            yellow_cards,
                            red_cards,
                            minutes
    ))
  
  df <-
    fpl_sum_all_previous(df,
                         vars = vars_to_aggreg)
  
  df <-
    fpl_sum_n_previous(df,
                       vars = vars_to_aggreg,
                       window = 3)
  
  df <-
    fpl_sum_n_previous(df,
                       vars = vars_to_aggreg,
                       window = 5)
  
  df <-
    fpl_avg_n_previous(df,
                       vars = vars_to_aggreg,
                       window = 3)
  
  df <-
    fpl_avg_n_previous(df,
                       vars = vars_to_aggreg,
                       window = 5)
  
  return(df)
}
