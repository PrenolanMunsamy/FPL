# Import Libraries ----
library(tidyverse)
library(here)
library(stringr)

feature_engineering <- function(df){
  # Add Lagged Variables ----
  vars_to_lag <-
    as.character(expression(value,
                            total_points,
                            scored_more_than_2_points,
                            total_points_grouped,
                            minutes,
                            transfers_in,
                            transfers_out,
                            selected,
                            # played_less_than_60_mins,
                            # played_60_or_more_mins,
                            # scored_goal,
                            # made_assist,
                            # had_clean_sheet,
                            # made_3_or_more_saves,
                            # conceded_2_or_more_goals,
                            # got_yellow_card,
                            got_bonus,
                            ict_index,
                            influence,
                            creativity,
                            threat,
                            match_result,
                            shots,
                            starter_vs_sub,
                            xA,
                            key_passes,
                            npg,
                            npxG,
                            xGChain,
                            xGBuildup))
  
  df <-
    fpl_lag_data(df,
                 vars_to_lag = vars_to_lag,
                 lag=1)
  
  # df <-
  #   fpl_lag_data(df,
  #                vars_to_lag = vars_to_lag,
  #                lag=2)
  # 
  # df <-
  #   fpl_lag_data(df,
  #                vars_to_lag = vars_to_lag,
  #                lag=3)
  # 
  # df <-
  #   fpl_lag_data(df,
  #                vars_to_lag = vars_to_lag,
  #                lag=4)
  # 
  # df <-
  #   fpl_lag_data(df,
  #                vars_to_lag = vars_to_lag,
  #                lag=5)
  
  # Add Aggregate Variables ----
  vars_to_aggreg <-
    as.character(expression(value,
                            total_points,
                            scored_more_than_2_points,
                            # total_points_grouped,
                            minutes,
                            transfers_in,
                            transfers_out,
                            selected,
                            # played_less_than_60_mins,
                            # played_60_or_more_mins,
                            # scored_goal,
                            # made_assist,
                            # had_clean_sheet,
                            # made_3_or_more_saves,
                            # conceded_2_or_more_goals,
                            # got_yellow_card,
                            got_bonus,
                            ict_index,
                            influence,
                            creativity,
                            threat,
                            # match_result,
                            shots,
                            starter_vs_sub,
                            xA,
                            key_passes,
                            npg,
                            npxG,
                            xGChain,
                            xGBuildup))
  
  # form metrics
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

  # consistency metrics
  df <-
    fpl_avg_n_previous(df,
                       vars = vars_to_aggreg,
                       window = 5)

  df <-
    fpl_std_dev_n_previous(df,
                           vars = vars_to_aggreg,
                           window = 5)

  df <-
    fpl_std_dev_to_avg_ratio(df,
                             window = 5)


  # Team Level Variables ----
  team_level_df <-
    df %>%
    arrange(YEAR,MONTH,DAY,team,name) %>%
    group_by(YEAR,MONTH,DAY,team) %>%
    summarise(across(.cols = c(value,
                               total_points,
                               scored_more_than_2_points,
                               # total_points_grouped,
                               # played_less_than_60_mins,
                               # played_60_or_more_mins,
                               # scored_goal,
                               # made_assist,
                               #had_clean_sheet,
                               #made_3_or_more_saves,
                               #conceded_2_or_more_goals,
                               # got_yellow_card,
                               got_bonus,
                               ict_index,
                               influence,
                               creativity,
                               threat
                               #match_result
                               ),
                     .fns = ~ sum(.x,na.rm = TRUE),
                     .names = "team_{.col}")) %>%
    ungroup() %>%
    arrange(team,YEAR,MONTH,DAY) %>%
    group_by(team) %>%
    mutate(player_match_number=row_number()) %>%
    ungroup()

  vars_to_aggreg <-
    as.character(expression(team_value,
                            team_total_points,
                            team_scored_more_than_2_points,
                            # team_total_points_grouped,
                            # team_played_less_than_60_mins,
                            # team_played_60_or_more_mins,
                            # team_scored_goal,
                            # team_made_assist,
                            #team_had_clean_sheet,
                            #team_made_3_or_more_saves,
                            #team_conceded_2_or_more_goals,
                            # team_got_yellow_card,
                            team_got_bonus,
                            team_ict_index,
                            team_influence,
                            team_creativity,
                            team_threat
                            #team_match_result
                            ))

  team_level_df <-
    fpl_sum_all_previous_team(team_level_df,
                              vars = vars_to_aggreg)

  team_level_df <-
    fpl_sum_n_previous_team(team_level_df,
                            vars = vars_to_aggreg,
                            window = 3)

  team_level_df <-
    fpl_sum_n_previous_team(team_level_df,
                            vars = vars_to_aggreg,
                            window = 5)

  df <-
    df %>%
    left_join(team_level_df %>% select(-player_match_number),by=c("YEAR","MONTH","DAY","team")) %>%
    left_join(team_level_df %>%
                select(-player_match_number) %>%
                rename_with(~ paste0("opp_", .), starts_with("team_")),
              by=c("YEAR","MONTH","DAY","opp_team_name"="team"))

  # Adding Relative Performance Variables ----
  df <-
    df %>%
    mutate(rel_total_points_to_team_sum_last_5 = total_points_sum_last_5/team_total_points_sum_last_5,
           rel_total_points_to_opp_team_sum_last_5 = total_points_sum_last_5/opp_team_total_points_sum_last_5,
           rel_team_to_opp_total_points_sum_last_5 = team_total_points_sum_last_5/opp_team_total_points_sum_last_5) %>%
    mutate(across(.cols = c(rel_total_points_to_team_sum_last_5,
                            rel_total_points_to_opp_team_sum_last_5,
                            rel_team_to_opp_total_points_sum_last_5),
                  .fns = ~ ifelse(is.nan(.x)|is.infinite(.x),0,.x)))

  h2h_df <-
    df %>%
    select(name,opp_team_name,YEAR,MONTH,DAY,total_points) %>%
    arrange(name,opp_team_name,YEAR,MONTH,DAY) %>%
    group_by(name,opp_team_name) %>%
    mutate(h2h_total_points_prev_cume_sum = cumsum(total_points) - total_points) %>%
    #filter(row_number()!=n()) %>%
    ungroup()
  
  df <-
    df %>%
    left_join(h2h_df %>% select(-total_points))
  
  df <-
    df %>% 
    mutate(total_points_std_last_5_to_avg_ratio = ifelse(is.na(total_points_std_last_5_to_avg_ratio)|
                                                           is.infinite(total_points_std_last_5_to_avg_ratio),0,
                                                         total_points_std_last_5_to_avg_ratio)) %>% 
    group_by(season_x,GoalWeek) %>% 
    mutate(avg_total_points_avg_last_5 = mean(total_points_avg_last_5),
           avg_total_points_std_last_5 = mean(total_points_std_last_5_to_avg_ratio)) %>% 
    mutate(consistency_segments = case_when(total_points_avg_last_5>avg_total_points_avg_last_5 & total_points_std_last_5_to_avg_ratio>avg_total_points_std_last_5 ~ "seg_4",
                                            total_points_avg_last_5>avg_total_points_avg_last_5 & total_points_std_last_5_to_avg_ratio<avg_total_points_std_last_5 ~ "seg_2",
                                            total_points_avg_last_5<avg_total_points_avg_last_5 & total_points_std_last_5_to_avg_ratio<avg_total_points_std_last_5 ~ "seg_1",
                                            total_points_avg_last_5<avg_total_points_avg_last_5 & total_points_std_last_5_to_avg_ratio>avg_total_points_std_last_5 ~ "seg_3")) %>% 
    ungroup() %>% 
    mutate(consistency_segments = ifelse(is.na(consistency_segments),3,consistency_segments)) %>% 
    select(-c(avg_total_points_avg_last_5,avg_total_points_std_last_5))
  
  return(df)
}
