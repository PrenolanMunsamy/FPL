cast_correct_dtypes <- function(df){
  
  character_cols <-
    as.character(expression(name,
                            position,
                            team,
                            was_home))
  
  numeric_cols <-
    as.character(expression(xP,
                            assists,
                            bonus,
                            bps,
                            clean_sheets,
                            creativity,
                            element,
                            expected_assists,
                            expected_goal_involvements,
                            expected_goals,
                            expected_goals_conceded,
                            fixture,
                            goals_conceded,
                            goals_scored,
                            ict_index,
                            influence,
                            minutes,
                            opponent_team,
                            own_goals,
                            penalties_missed,
                            penalties_saved,
                            red_cards,
                            round,
                            saves,
                            selected,
                            starts,
                            team_a_score,
                            team_h_score,
                            threat,
                            total_points,
                            transfers_balance,
                            transfers_in,
                            transfers_out,
                            value,
                            yellow_cards,
                            GoalWeek,
                            difficulty_ratio
                            ))
  
  datetime_cols <-
    as.character(expression(kickoff_time))
  
  character_cols <- intersect(character_cols, names(df))
  numeric_cols <- intersect(numeric_cols, names(df))
  datetime_cols <- intersect(datetime_cols, names(df))
  
  df %>% 
    mutate(across(.cols = all_of(character_cols),
                  .fns = ~ as.character(.x))) %>%
    mutate(across(.cols = all_of(numeric_cols),
                  .fns = ~ as.numeric(as.character(.x)))) %>%
    mutate(across(.cols = all_of(datetime_cols),
                  .fns = ~ date(as.character(.x))))
}