train_validation_test_split <- function(df, test_season, validation_season_match_number){
  
  # this function takes a dataframe that contains a season variable called season_x and another 
  # variable called season_match_number.
  # we split the input df as follows:
  # train set is equal to all seasons except the test season
  # validation set is equal to the first 19 matches in the test season
  # test set is equal to the last 19 matches in the test season
  
  
  # Train Test Split
  set.seed(1234)
  
  train_df <- 
    df %>% 
    filter(season_x!=test_season)
  
  test_df <- 
    input_df %>% 
    filter(season_x==test_season)
  
  validation_df <- 
    test_df %>% 
    filter(season_match_number<validation_season_match_number)
  
  test_df <-
    test_df %>% 
    filter(season_match_number>=validation_season_match_number)
  
  return(list(train=train_df, validation = validation_df, test = test_df))
}

