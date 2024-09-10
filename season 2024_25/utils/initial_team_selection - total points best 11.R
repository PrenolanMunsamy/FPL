initial_team_selection_total_points_best_11 <- function(df){
  
  # get 1 player per position with lowest cost
  # dont select these players from the top 6 teams from last season
  
  best_gk <-
    df %>%
    filter(position=="GK") %>%
    distinct() %>% 
    arrange(desc(value)) %>%
    slice_head(n=1)

  best_def <-
    df %>%
    filter(position=="DEF") %>%
    distinct() %>% 
    arrange(desc(value),desc(total_points)) %>%
    slice_head(n=1)

  best_fwd <-
    df %>%
    filter(position=="FWD") %>%
    distinct() %>% 
    arrange(desc(value),desc(total_points)) %>%
    slice_head(n=1)

  best_mid <-
    df %>%
    filter(position=="MID") %>%
    distinct() %>% 
    arrange(desc(value),desc(total_points)) %>%
    slice_head(n=1)

  print(best_gk)
  print(best_def)
  print(best_mid)
  print(best_fwd)

  best_players <-
    bind_rows(best_gk,
              best_def,
              best_mid,
              best_fwd
              )
  
  remaining_budget <- 100-sum(best_players$value)
  print(paste0("remaining_budget:",remaining_budget))
  
  # Adding an index column to use in the model
  filtered_data <- 
    df %>% 
    filter(!name %in% best_gk$name) %>%
    filter(!name %in% best_def$name) %>%
    filter(!name %in% best_mid$name) %>%
    filter(!name %in% best_fwd$name) %>%
    mutate(index=row_number())
  
  View(filtered_data)
  
  # Create the model
  model <- MIPModel() %>%
    # Add binary decision variables for each player
    add_variable(x[i], i = 1:nrow(filtered_data), type = "binary") %>%
    
    # Objective: Maximize total points
    set_objective(sum_expr(filtered_data$total_points[i] * x[i], i = 1:nrow(filtered_data)), "max") %>%
    
    # Objective: Maximize roi
    # set_objective(sum_expr(filtered_data$previous_season_roi[i] * x[i], i = 1:nrow(filtered_data)), "max") %>%
    
    # Constraint 1: Total value must be less than or equal to remaining budget
    add_constraint(sum_expr(filtered_data$value[i] * x[i], i = 1:nrow(filtered_data)) <= remaining_budget) %>% 
    
    # Constraint 2: Must select 11 players
    add_constraint(sum_expr(x[i], i = 1:nrow(filtered_data)) == 11) %>%
    
    # Constraint 3: Must select specific number of players in each position
    add_constraint(sum_expr(x[i], i = which(filtered_data$position == "GK")) == 1) %>%
    add_constraint(sum_expr(x[i], i = which(filtered_data$position == "DEF")) == 4) %>%
    add_constraint(sum_expr(x[i], i = which(filtered_data$position == "MID")) == 4) %>%
    add_constraint(sum_expr(x[i], i = which(filtered_data$position == "FWD")) == 2)
  
  # Constraint 4: Cannot select more than 3 players from the same team
  
  best_players_team_count <-
    best_players %>%
    count(team) %>%
    rename(team_name=team)
  
  # Add a constraint for each team
  for (team in unique(filtered_data$team)) {
    if (team %in% c(best_players_team_count$team_name)){

      team_max_count <-
        3 - (best_players_team_count %>%
        filter(team==team_name) %>%
        select(n) %>%
        pull())

    } else{
      team_max_count <- 3
    }

    model <- model %>%
      add_constraint(sum_expr(x[i], i = which(filtered_data$team == team)) <= team_max_count)
  }
  
  # Solve the model
  result <- model %>%
    solve_model(with_ROI(solver = "glpk", verbose = TRUE))
  
  # Get the solution
  solution <- 
    get_solution(result, x[i]) %>%
    filter(value == 1) %>%
    select(-value) %>% 
    left_join(filtered_data, by = c("i" = "index")) %>% 
    arrange(position,desc(total_points)) %>% 
    group_by(position) %>%
    mutate(starting_11 = case_when(position == "DEF" & row_number()>=n()~"no",
                                   position == "FWD" & row_number()>=n()~"no",
                                   position == "GK" & row_number()>=n()~"no",
                                   position == "MID" & row_number()>=n()~"no",
                                   .default = "Yes")) %>%
    ungroup()
  
  solution <-
    bind_rows(solution,
              best_players %>%
                mutate(starting_11 = "Yes"))

  return(solution)
  
}

