initial_team_selection_total_points <- function(df){
  
  # get 1 player per position with lowest cost
  # dont select these players from the top 6 teams from last season
  
  # lowest_gk <-
  #   df %>%
  #   # filter(!team %in% c("Liverpool","Man Utd","Man City","Arsenal","Spurs","Aston Villa","Newcastle")) %>%
  #   filter(position=="GK") %>%
  #   arrange(value,desc(total_points)) %>%
  #   slice_head(n=1)
  # 
  # lowest_def <-
  #   df %>%
  #   # filter(!team %in% c("Liverpool","Man Utd","Man City","Arsenal","Spurs","Aston Villa","Newcastle")) %>%
  #   filter(position=="DEF") %>%
  #   # filter(value>=5) %>%
  #   arrange(value,desc(total_points)) %>%
  #   slice_head(n=1)
  # 
  # lowest_fwd <-
  #   df %>%
  #   # filter(!team %in% c("Liverpool","Man Utd","Man City","Arsenal","Spurs","Aston Villa","Newcastle")) %>%
  #   filter(position=="FWD") %>%
  #   arrange(value,desc(total_points)) %>%
  #   slice_head(n=1)
  # 
  # lowest_mid <-
  #   df %>%
  #   # filter(!team %in% c("Liverpool","Man Utd","Man City","Arsenal","Spurs")) %>%
  #   filter(position=="MID") %>%
  #   # filter(value>=5) %>%
  #   arrange(value,desc(total_points)) %>%
  #   slice_head(n=1)
  # 
  # print(lowest_gk)
  # print(lowest_def)
  # print(lowest_mid)
  # print(lowest_fwd)
  # 
  # lowest_players <-
  #   bind_rows(lowest_gk,
  #             lowest_def,
  #             lowest_mid,
  #             lowest_fwd
  #             )
  
  # remaining_budget <- 100-sum(lowest_players$value)
  remaining_budget <- 100
  print(paste0("remaining_budget:",remaining_budget))
  
  # Adding an index column to use in the model
  filtered_data <- df %>% 
    mutate(index=row_number())
  
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
    
    # Constraint 2: Must select 15 players
    add_constraint(sum_expr(x[i], i = 1:nrow(filtered_data)) == 15) %>%
    
    # Constraint 2: Must select 11 players
    # add_constraint(sum_expr(x[i], i = 1:nrow(filtered_data)) == 11) %>% 
    
    # Constraint 3: Must select specific number of players in each position
    add_constraint(sum_expr(x[i], i = which(filtered_data$position == "GK")) == 2) %>%
    add_constraint(sum_expr(x[i], i = which(filtered_data$position == "DEF")) == 5) %>%
    add_constraint(sum_expr(x[i], i = which(filtered_data$position == "MID")) == 5) %>%
    add_constraint(sum_expr(x[i], i = which(filtered_data$position == "FWD")) == 3)
    # add_constraint(sum_expr(x[i], i = which(filtered_data$position == "GK")) == 1) %>%
    # add_constraint(sum_expr(x[i], i = which(filtered_data$position == "DEF")) == 4) %>%
    # add_constraint(sum_expr(x[i], i = which(filtered_data$position == "MID")) == 4) %>%
    # add_constraint(sum_expr(x[i], i = which(filtered_data$position == "FWD")) == 2)
  
  # Constraint 4: Cannot select more than 3 players from the same team
  # add_constraint(sum_expr(x[i], i = which(filtered_data$team == unique(filtered_data$team)[1])) <= 3)
  
  # lowest_players_team_count <-
  #   lowest_players %>% 
  #   count(team) %>% 
  #   rename(team_name=team)
  
  # Add a constraint for each team
  for (team in unique(filtered_data$team)) {
  #   if (team %in% c(lowest_players_team_count$team_name)){
  #     
  #     team_max_count <- 
  #       2 - (lowest_players_team_count %>% 
  #       filter(team==team_name) %>% 
  #       select(n) %>% 
  #       pull())
  #     
  #   } else{
  #     team_max_count <- 2
  #   }
  #   
    model <- model %>%
      add_constraint(sum_expr(x[i], i = which(filtered_data$team == team)) <= 2)
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
    # mutate(starting_11 = case_when(position == "DEF" & row_number()>=n()-1~"no",
    #                                position == "FWD" & row_number()>=n()~"no",
    #                                position == "GK" & row_number()>=n()~"no",
    #                                .default = "Yes")) %>%
    mutate(starting_11 = case_when(position == "DEF" & row_number()>=n()~"no",
                                   position == "FWD" & row_number()>=n()~"no",
                                   position == "GK" & row_number()>=n()~"no",
                                   position == "MID" & row_number()>=n()~"no",
                                   .default = "Yes")) %>%
    # mutate(starting_11 = case_when(position == "DEF" & row_number()>=n()-1~"no",
    #                                # position == "FWD" & row_number()>=n()~"no",
    #                                position == "GK" & row_number()>=n()~"no",
    #                                position == "MID" & row_number()>=n()~"no",
    #                                .default = "Yes")) %>%
    ungroup()
  
  # solution <-
  #   bind_rows(solution %>%
  #               mutate(starting_11 = "yes"),
  #             lowest_players %>%
  #               mutate(starting_11 = "no"))
  
  # print(solution)
  return(solution)
  
}

