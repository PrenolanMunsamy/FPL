fpl_avg_n_previous <- function(df,
                               vars,
                               window) {
  
  df %>%
    # arrange(team, name, match_number) %>%
    # group_by(team, name) %>%
    arrange(name, player_match_number) %>%
    group_by(name) %>%
    mutate(across(.cols = all_of(vars),
                  .fns = ~ zoo::rollmeanr(lag(.x),
                                          k = window,
                                          fill = NA,
                                          na.rm = TRUE,
                                          align = "right"), 
                  .names = "{.col}_avg_last_{window}"
    )
    ) %>%
    ungroup() %>%
    mutate(across(contains("_avg_last_"), ~ ifelse(is.na(.x), 0, .x)))
  
}

