preprocessing <- function(gw_df,
                          fixed_player_names_df,
                          understat_df,
                          team_fdr_df,
                          team_name_id_df){
  
  # add opponent team names 
  gw_df <-
    gw_df %>%
    left_join(team_name_id_df %>% 
                select(season_x,id,name) %>% 
                rename(opponent_team=id,
                       opp_team_name=name)) 
  
  # add fdr for each fixture
  gw_df <-
    gw_df %>%
    mutate(team_a_name= ifelse(was_home=="False"|was_home==FALSE,team,opp_team_name),
           team_h_name = ifelse(was_home=="True"|was_home==TRUE,team,opp_team_name)) %>% 
    left_join(team_fdr_df %>%
                select(season_x,kickoff_time,team_a,team_h,team_a_difficulty,team_h_difficulty) %>%
                left_join(team_name_id_df %>%
                            select(season_x,id,name) %>%
                            rename(team_a_name=name),
                          by = c("season_x","team_a"="id")) %>%
                mutate(kickoff_time = as.Date(kickoff_time)),
              by = c("season_x","kickoff_time","team_a_name")) %>%
    mutate(team_difficulty = case_when(team==team_a_name~team_h_difficulty,
                                       team==team_h_name~team_a_difficulty),
           opponent_difficulty = case_when(opp_team_name==team_a_name~team_h_difficulty,
                                           opp_team_name==team_h_name~team_a_difficulty),
           difficulty_ratio = team_difficulty/opponent_difficulty) %>%
    select(-c(team_a_name,team_h_name,team_a_difficulty,team_h_difficulty))

  # add other variables
  gw_df <-
    gw_df %>%
    mutate(YEAR=year(kickoff_time),
           MONTH=month(kickoff_time),
           DAY=day(kickoff_time)) %>%
    mutate(name = stringi::stri_trans_general(name,"Latin-ASCII"),
           name = str_replace_all(name,coll("-")," "),
           name = tolower(name)) %>%
    arrange(name,YEAR,MONTH,DAY) %>%
    group_by(name) %>%
    mutate(player_match_number=row_number()) %>%
    mutate(value_increase_decrease = value-lag(value,1),
           value_increase_decrease = ifelse(is.na(value_increase_decrease),0,value_increase_decrease),
           value_increase_decrease = case_when(value_increase_decrease<0 ~ -1,
                                               value_increase_decrease==0 ~ 0,
                                               value_increase_decrease>0 ~ 1)) %>%
    ungroup() %>%
    mutate(value = value/10) %>%
    mutate(scored_more_than_2_points = ifelse(total_points>=2,1,0)) %>%
    mutate(total_points_grouped = case_when(total_points<=0~0,
                                            total_points==1~1,
                                            total_points==2~2,
                                            total_points>=3~3)) %>%
    mutate(was_home = ifelse((was_home==TRUE|was_home=="True"|was_home=="y"),"y","n")) %>%
    mutate(match_result = case_when(was_home=="y" & team_a_score>team_h_score~0,
                                    was_home=="n" & team_a_score>team_h_score~3,
                                    was_home=="y" & team_a_score<team_h_score~3,
                                    was_home=="n" & team_a_score<team_h_score~0,
                                    team_a_score==team_h_score~1)) %>%
    mutate(played_less_than_60_mins = ifelse(minutes>0 & minutes <60, 1, 0),
           played_60_or_more_mins = ifelse(minutes>=60, 1, 0),
           got_bonus = ifelse(bonus>0, 1, 0))

  season_match_number_df <-
    gw_df %>%
    select(team, YEAR, MONTH, DAY, season_x, opp_team_name) %>%
    distinct() %>%
    arrange(team, YEAR, MONTH, DAY) %>%
    group_by(team,season_x) %>%
    mutate(season_match_number=row_number()) %>%
    ungroup()

  match_number_df <-
    gw_df %>%
    select(team, YEAR, MONTH, DAY, season_x, opp_team_name) %>%
    distinct() %>%
    arrange(team, YEAR, MONTH, DAY) %>%
    group_by(team) %>%
    mutate(match_number=row_number()) %>%
    ungroup()

  opp_team_match_number_df <-
    gw_df %>%
    select(opp_team_name, YEAR, MONTH, DAY, season_x, team) %>%
    distinct() %>%
    arrange(opp_team_name, YEAR, MONTH, DAY) %>%
    group_by(opp_team_name) %>%
    mutate(opp_team_match_number=row_number()) %>%
    ungroup() %>%
    arrange(team, YEAR, MONTH, DAY) %>%
    select(YEAR,MONTH,DAY,season_x,team,opp_team_name,opp_team_match_number)

  gw_df <-
    gw_df %>%
    left_join(match_number_df) %>%
    left_join(season_match_number_df) %>%
    arrange(team, YEAR, MONTH, DAY, opp_team_name, name) %>%
    select(YEAR, MONTH, DAY, season_x, match_number,
           team, opp_team_name, name, position,
           value, total_points,
           everything()) %>%
    arrange(team,name,YEAR,MONTH,DAY) %>%
    group_by(team,name) %>%
    mutate(player_appearance_order=row_number()) %>%
    ungroup() %>%
    arrange(YEAR,MONTH,DAY,team,name)

  gw_df %>% View()
  
  gw_df <-
    gw_df %>%
    left_join(fixed_player_names_df,
              by=c("name")) %>%
    mutate(final_name = ifelse(is.na(final_name),name,final_name)) %>%
    left_join(understat_df %>%
                group_by(YEAR,MONTH,DAY,Player_Name) %>%
                summarise(across(.cols = -c(Player_Name_ID),
                                 .fns = ~ mean(.x,na.rm=TRUE))) %>%
                ungroup(),
              by=c("final_name"="Player_Name","YEAR","MONTH","DAY")) %>%
    select(-name) %>%
    rename(name=final_name) %>%
    mutate(across(.cols = c(shots,starter_vs_sub,xA,key_passes,npg,npxG,
                            xGChain,xGBuildup),
                  .fns = ~ ifelse(is.na(.x),0,.x))) %>%
    select(YEAR,MONTH,DAY,team,name,everything()) %>% 
    arrange(YEAR,MONTH,DAY,team,name)
  
  gw_df %>% View()
  
  gw_df
}