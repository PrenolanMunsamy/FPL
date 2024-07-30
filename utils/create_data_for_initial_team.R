create_data_for_initial_team <- 
  function(previous_season_df,new_season_df){
    
    new_season_df <-
      new_season_df %>% 
      select(team,name,position,value) %>%
      distinct() %>% 
      rename(new_season_team = team,
             new_season_value = value)
    
    print(paste0("new season rows:",nrow(new_season_df)))
    
    previous_season_df <-
      previous_season_df %>% 
      select(team,name,position,total_points,value) %>%
      group_by(name,position) %>% 
      summarise(full_season_total_points=sum(total_points),
                full_season_avg_value=mean(value,na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(previous_season_roi = full_season_total_points/full_season_avg_value) %>% 
      rename(#previous_season_team=team,
             previous_season_total_points=full_season_total_points,
             previous_season_avg_value=full_season_avg_value) %>% 
      distinct()
    
    print(paste0("previous season rows:",nrow(previous_season_df)))
    
    new_season_df %>% 
      inner_join(previous_season_df,
                 by=c("name","position"))
    
  }

