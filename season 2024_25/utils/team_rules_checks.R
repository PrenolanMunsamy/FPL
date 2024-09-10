# checks
team_rules_checks <- function(optimal_team){
  
  error_count <- 0
  
  # Team Value Check
  team_value <- sum(optimal_team$value)
  
  if(team_value>100){
    print("team value is greater than the budget")
    error_count <- error_count + 1
  }
  
  # Team Count Check
  max_team_count <- 
    optimal_team %>% 
    count(team) %>% 
    summarise(max_team_count = max(n)) %>% 
    pull()
  
  if (max_team_count > 3){
    print("more than 3 players selected from one team")
    error_count <- error_count + 1
  }
  
  if (error_count==0){
    print("all rules passed")
  }
}