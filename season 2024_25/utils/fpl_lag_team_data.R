# Lag Function ----
fpl_lag_team_data <- function(df, 
                              vars_to_lag, 
                              lag=1
                         ){
  
  df1 <- df
  
  df2 <- 
    df %>%  
    group_by(team,YEAR,MONTH,DAY) %>% 
    mutate(!!paste0("player_match_number","_lag_",lag):= player_match_number + lag) %>% 
    ungroup() %>% 
    select(team,
           paste0("player_match_number","_lag_",lag),
           all_of(vars_to_lag)) %>%
    rename_with(.fn = ~ paste0(.x,"_lag_",lag),
                .cols = all_of(vars_to_lag))
  
  join_cols <- c("player_match_number" = paste0("player_match_number","_lag_",lag))
  names(join_cols) <- "player_match_number"
  
  df1 %>% 
    left_join(df2,
              by=c("team",join_cols)) %>% 
    mutate(across(.cols = paste0(vars_to_lag,"_lag_",lag),
                  .fns = ~ ifelse(is.na(.x), 0, .x)))
}

