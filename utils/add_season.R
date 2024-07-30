add_season <- function(df,season){
  df %>% 
    mutate(season_x = season)
}