add_opponent_team_name <- function(df){
  team_dict <-
    df %>% 
    dplyr::select(team) %>% 
    distinct() %>%
    arrange(team) %>% 
    mutate(team_id = row_number()) %>% 
    rename(team_name = team)
  
  print(team_dict)
  
  df %>% 
    left_join(team_dict,
              by=c("opponent_team" = "team_id")) %>% 
    rename(opp_team_name = team_name)
}