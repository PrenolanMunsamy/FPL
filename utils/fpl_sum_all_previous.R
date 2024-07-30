# Aggregate Function ----
fpl_sum_all_previous <- function(df, 
                                 vars) {
    
  df_out <- df
    
#  for (var in vars) {
    for (var in vars) {
      df_out <- df_out %>% 
        # arrange(team, name, match_number) %>%
        arrange(name, YEAR,MONTH,DAY) %>%
        #mutate(!!paste0(var,"_copy") := .data[[var]]) %>% 
        #mutate(!!paste0(var,"_copy") := ifelse(minutes==0,NA,.data[[paste0(var,"_copy")]])) %>% 
        # group_by(team, name) %>%
        group_by(name) %>%
        mutate(!!paste0(var, "_prev_cume_sum") := cumsum(.data[[paste0(var)]]) - .data[[paste0(var)]]) %>% 
        ungroup() 
        #select(-paste0(var,"_copy"))
      #mutate(!!paste0(var) := ifelse(is.na(.data[[var]]), 0, .data[[var]]))
    }
#  }
    
    
  df_out <-
    df_out %>% 
    mutate(across(.cols = contains("_last_"),
                  .fns = ~ifelse(is.na(.x),0,.x))) %>% 
    arrange(YEAR,MONTH,DAY,team,name)
  
    return(df_out)
}

