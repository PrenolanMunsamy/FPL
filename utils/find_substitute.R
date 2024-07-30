# Function to find the replacement
find_substitute <- function(current_players, candidate_players, budget_balance) {
  if (nrow(current_players)>=3){
    # Find the minimum predicted score among current players
    min_current_pred <- min(current_players$.pred,na.rm = TRUE)
    
    # make it hard for key players to be dropped
    cond_1 <- current_players %>% filter(.pred==min_current_pred) %>% select(key_player) %>% slice_sample(n=1) %>% pull()
    cond_2 <- current_players %>% filter(.pred==min_current_pred) %>% select(.pred) %>% slice_sample(n=1) %>% pull()
    # print(paste0("cond_1:",cond_1))
    # print(paste0("cond_2:",cond_2))
    if (cond_1==1 & cond_2<2){
      current_players <- current_players
    } else {
      current_players <-
        current_players %>% 
        filter(key_player==0)
      min_current_pred <- min(current_players$.pred,na.rm = TRUE)
    }
    
    # print(current_players %>% filter(.pred==min_current_pred))
    
    # Filter candidate players based on the criteria
    eligible_candidates <- 
      candidate_players %>%
      inner_join(current_players %>% 
                   filter(.pred==min_current_pred), 
                 by = "position", suffix = c(".cand", ".curr"),
                 relationship="many-to-many") %>%
      filter(value.cand <= (value.curr+budget_balance) & (.pred.cand - min_current_pred)>1) %>%
      mutate(pred_diff = .pred.cand - .pred.curr)  
  } else {
    
    # Filter candidate players based on the criteria
    eligible_candidates <- 
      candidate_players %>%
      inner_join(current_players, 
                 by = "position", suffix = c(".cand", ".curr"),
                 relationship="many-to-many") %>%
      filter(value.cand <= (value.curr+budget_balance)) %>%
      mutate(pred_diff = 0)
  }
  
  
  # print(eligible_candidates)
  # If no eligible candidates, return NULL
  if (nrow(eligible_candidates) == 0) {
    return(NULL)
  }
  
  # Select the best candidate to replace the corresponding current player
  best_candidate <- eligible_candidates %>%
    arrange(desc(.pred.cand),.pred.curr) %>%
    slice(1)
  
  return(best_candidate)
}