load_defender_data <- function(path){
  
  # Import Data ----
  input_df <- 
    read_csv(path)
  
  input_df_orig <- input_df
  
  # # Filter out first 3 match weeks ----
  # input_df %>% 
  #   filter(season_x=="2021-22") %>% 
  #   select(match_number,contains("_lag_")) %>% 
  #   View()
  # 
  # input_df <-
  #   input_df %>%
  #   filter(!(season_x=="2020-21" & match_number<=4))
  
  # Filter Players who Played in the previous matches ----
  # input_df <-
  #   input_df %>% 
  #   filter(minutes>0)
  
  # Filter on DEF ----
  input_df <-
    input_df %>% 
    filter(position=="DEF")  
}

