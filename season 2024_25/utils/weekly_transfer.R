weekly_transfer <- function(current_team,
                            upcoming_week_predictions){
  
  current_team_copy <- current_team
  
  upcoming_week_predictions_orig <- upcoming_week_predictions
  
  upcoming_week_predictions <-
    upcoming_week_predictions %>% 
    select(-consistency_segments)
  
  upcoming_week_predictions <-
    upcoming_week_predictions %>% 
    group_by(name) %>% 
    mutate(.pred = mean(.pred,na.rm = TRUE)) %>% 
    ungroup() %>%
    distinct()
  
  # Get Team Count of Current Team ----
  # if any team already has 3 players, then we will not choose a substitute from
  # this team
  teams_with_3_players <-
    current_team %>% 
    count(team) %>% 
    filter(n==3) %>% 
    select(team) %>% 
    pull()
  
  # Add Predicted Points to Current Team ----
  current_team <-
    current_team %>%
    left_join(upcoming_week_predictions %>% 
                select(-value),
              by = c("team","name","position")) %>% 
    mutate(.pred = ifelse(is.na(.pred),-0.0001,.pred))
  
  # Remaining Budget ----
  budget_balance <- 100 - sum(current_team$value)
  
  # Get Best Player Per Position Within Budget ----
  candidates <-
    anti_join(
      upcoming_week_predictions %>% 
        filter(!team%in%(teams_with_3_players)),
      current_team,
      by = c("team","name","position")
    )
  
  # print(candidates)
  
  candidates <-
    candidates %>% 
    left_join(upcoming_week_predictions_orig) %>% 
    #filter(consistency_segments == "seg_2"|consistency_segments == "seg_4") %>% 
    select(-consistency_segments)
  
  # print(candidates)
  
  # Find Replacement ----
  player_count <- current_team %>% 
    filter(.pred<0) %>% 
    summarise(player_count = sum(ifelse(starting_11=="Yes",1,0))) %>% 
    select(player_count) %>% 
    pull()
  
  print(player_count)
  # player_count <- 11
  if(player_count<11 & player_count>0){
    replacement <-
      find_substitute(current_team %>%
                        filter(position!="GK") %>%
                        filter(.pred<0) %>% 
                        filter(starting_11=="Yes") %>% 
                        # mutate(roi = value/total_points_avg_last_5) %>% 
                        # mutate(roi = ifelse(is.na(roi)|is.nan(roi)|is.infinite(roi),0,roi)) %>% 
                        arrange(value) %>%
                        # arrange(previous_season_sum_total_points) %>%
                        slice(1) %>% 
                        select(-c(previous_season_sum_total_points,
                                  starting_11)),
                      candidates,
                      budget_balance)
  } else{
    replacement <-
      find_substitute(current_team %>%
                        filter(position!="GK") %>%
                        select(-c(previous_season_sum_total_points,
                                  starting_11)),
                      candidates,
                      budget_balance)
  }
    
  
  if (is.null(replacement)) {
    cat("No suitable replacement found.\n")
  } else {
    cat("Replace", replacement$name.curr, "with", replacement$name.cand, "\n")
  }
  
  current_team_gw <-
    current_team %>% 
    filter(position=="GK")
  
  current_team <-
    current_team %>% 
    filter(position!="GK")
  
  if (is.null(replacement)){
    current_team_3_5_2 <- 
      current_team %>% 
      bind_rows(current_team_gw) %>% 
      arrange(position,desc(.pred)) %>%
      group_by(position) %>% 
      mutate(starting_11 = case_when(position == "DEF" & row_number()>=n()-1~"no",
                                     position == "FWD" & row_number()>=n()~"no",
                                     position == "GK" & row_number()>=n()~"no",
                                     .default = "Yes")) %>% 
      ungroup() %>% 
      mutate(.pred = ifelse(is.na(.pred),0,.pred))
    
    current_team_4_4_2 <- 
      current_team %>% 
      bind_rows(current_team_gw) %>% 
      arrange(position,desc(.pred)) %>%
      group_by(position) %>% 
      mutate(starting_11 = case_when(position == "DEF" & row_number()>=n()~"no",
                                     position == "MID" & row_number()>=n()~"no",
                                     position == "FWD" & row_number()>=n()~"no",
                                     position == "GK" & row_number()>=n()~"no",
                                     .default = "Yes")) %>% 
      ungroup() %>% 
      mutate(.pred = ifelse(is.na(.pred),0,.pred))
    
    current_team_3_4_3 <- 
      current_team %>% 
      bind_rows(current_team_gw) %>% 
      arrange(position,desc(.pred)) %>%
      group_by(position) %>% 
      mutate(starting_11 = case_when(position == "DEF" & row_number()>=n()-1~"no",
                                     position == "MID" & row_number()>=n()~"no",
                                     position == "GK" & row_number()>=n()~"no",
                                     .default = "Yes")) %>% 
      ungroup() %>% 
      mutate(.pred = ifelse(is.na(.pred),0,.pred))
    
    max_formation_points <- max(sum(current_team_3_4_3 %>% filter(starting_11=="Yes") %>% select(.pred) %>% pull()),
                                sum(current_team_3_5_2 %>% filter(starting_11=="Yes") %>% select(.pred) %>% pull()),
                                sum(current_team_4_4_2 %>% filter(starting_11=="Yes") %>% select(.pred) %>% pull()),
                                na.rm = TRUE)
    
    if (sum(current_team_3_4_3 %>% filter(starting_11=="Yes") %>% select(.pred) %>% pull())==max_formation_points){
      current_team <- current_team_3_4_3
    } else if(sum(current_team_4_4_2 %>% filter(starting_11=="Yes") %>% select(.pred) %>% pull())==max_formation_points){
      current_team <- current_team_4_4_2
    } else if(sum(current_team_3_5_2 %>% filter(starting_11=="Yes") %>% select(.pred) %>% pull())==max_formation_points){
      current_team <- current_team_3_5_2
    }
    
  } else{
    current_team_3_5_2 <- 
      current_team %>% 
      filter(name!=replacement$name.curr) %>% 
      bind_rows(replacement %>% 
      select(!ends_with(".curr")) %>% 
      rename(team=team.cand,
             name=name.cand,
             value=value.cand,
             .pred=.pred.cand) %>%
             mutate(starting_11="Yes",
                    captain = "not captain")) %>%
      bind_rows(current_team_gw) %>% 
      arrange(position,desc(.pred)) %>%
      group_by(position) %>% 
      mutate(starting_11 = case_when(position == "DEF" & row_number()>=n()-1~"no",
                                     position == "FWD" & row_number()>=n()~"no",
                                     position == "GK" & row_number()>=n()~"no",
                                     .default = "Yes")) %>% 
      ungroup()%>% 
      select(-c(pred_diff)) %>% 
      mutate(.pred = ifelse(is.na(.pred),0,.pred))
    
    current_team_4_4_2 <- 
      current_team %>% 
      filter(name!=replacement$name.curr) %>% 
      bind_rows(replacement %>% 
                  select(!ends_with(".curr")) %>% 
                  rename(team=team.cand,
                         name=name.cand,
                         value=value.cand,
                         .pred=.pred.cand) %>%
                  mutate(starting_11="Yes",
                         captain = "not captain")) %>%
      bind_rows(current_team_gw) %>% 
      arrange(position,desc(.pred)) %>%
      group_by(position) %>% 
      mutate(starting_11 = case_when(position == "DEF" & row_number()>=n()~"no",
                                     position == "MID" & row_number()>=n()~"no",
                                     position == "FWD" & row_number()>=n()~"no",
                                     position == "GK" & row_number()>=n()~"no",
                                     .default = "Yes")) %>% 
      ungroup()%>% 
      select(-c(pred_diff)) %>% 
      mutate(.pred = ifelse(is.na(.pred),0,.pred))
    
    current_team_3_4_3 <- 
      current_team %>% 
      filter(name!=replacement$name.curr) %>% 
      bind_rows(replacement %>% 
                  select(!ends_with(".curr")) %>% 
                  rename(team=team.cand,
                         name=name.cand,
                         value=value.cand,
                         .pred=.pred.cand) %>%
                  mutate(starting_11="Yes",
                         captain = "not captain")) %>%
      bind_rows(current_team_gw) %>% 
      arrange(position,desc(.pred)) %>%
      group_by(position) %>% 
      mutate(starting_11 = case_when(position == "DEF" & row_number()>=n()-1~"no",
                                     position == "MID" & row_number()>=n()~"no",
                                     position == "GK" & row_number()>=n()~"no",
                                     .default = "Yes")) %>% 
      ungroup()%>% 
      select(-c(pred_diff)) %>% 
      mutate(.pred = ifelse(is.na(.pred),0,.pred))
    
    max_formation_points <- max(sum(current_team_3_4_3 %>% filter(starting_11=="Yes") %>% select(.pred) %>% pull()),
                                sum(current_team_3_5_2 %>% filter(starting_11=="Yes") %>% select(.pred) %>% pull()),
                                sum(current_team_4_4_2 %>% filter(starting_11=="Yes") %>% select(.pred) %>% pull()),
                                na.rm = TRUE)
    
    if (sum(current_team_3_4_3 %>% filter(starting_11=="Yes") %>% select(.pred) %>% pull())==max_formation_points){
      current_team <- current_team_3_4_3
    } else if(sum(current_team_4_4_2 %>% filter(starting_11=="Yes") %>% select(.pred) %>% pull())==max_formation_points){
      current_team <- current_team_4_4_2
    } else if(sum(current_team_3_5_2 %>% filter(starting_11=="Yes") %>% select(.pred) %>% pull())==max_formation_points){
      current_team <- current_team_3_5_2
    }
    }
  
  # only choose captains from midfielder or forwards
  mid_fwd_max <- current_team %>% 
    filter(position=="DEF"|position=="DEF") %>% 
    summarise(max_pred = max(.pred,na.rm = TRUE)) %>% 
    select(max_pred) %>% 
    pull()
  
  current_team <-
    current_team %>% 
    mutate(captain = ifelse(.pred==max(.pred,na.rm = TRUE) & starting_11=="Yes","captain","not captain"))
    # mutate(captain = ifelse(.pred==mid_fwd_max & starting_11=="Yes","captain","not captain"))
  
  print(current_team)
  return(current_team)
}