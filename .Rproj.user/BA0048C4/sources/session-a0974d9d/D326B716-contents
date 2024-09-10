# Import Libraries ----
library(tidyverse)
library(here)

# Import Helper Functions ----
source(here("season 2024_25","99. Constants.R"))
source(here("season 2024_25/utils/import_all_gws.R"))
source(here("season 2024_25/utils/cast_correct_dtypes.R"))
source(here("season 2024_25/utils/add_opponent_team_name.R"))
source(here("season 2024_25/utils/add_season.R"))
source(here("season 2024_25/utils/preprocessing.R"))
source(here("season 2024_25/utils/fpl_lag_data.R"))
source(here("season 2024_25/utils/fpl_sum_all_previous.R"))
source(here("season 2024_25/utils/fpl_sum_n_previous.R"))
source(here("season 2024_25/utils/fpl_avg_n_previous.R"))
source(here("season 2024_25/utils/import_all_players_understat.R"))
source(here("season 2024_25/utils","fpl_avg_fdr_n_next.R"))

# Import Data ----

# **** Team ID and Name data ----
# Current Season
curr_season_teams <- 
  read_csv(here(paste0("raw data/",str_replace(curr_season,"_","-")),
                "teams.csv"))

curr_season_teams <-
  curr_season_teams %>% 
  mutate(season_x = curr_season)

# **** Team Strength Data ----
curr_season_fdr <- 
  read_csv(here(paste0("raw data/",str_replace(curr_season,"_","-")),
                "fixtures.csv"))

curr_season_fdr <-
  curr_season_fdr %>% 
  mutate(season_x = curr_season) %>% 
  select(season_x,kickoff_time,team_a,team_h,team_a_difficulty,team_h_difficulty) %>%
  left_join(curr_season_teams %>%
              select(season_x,id,name) %>%
              rename(team_a_name=name),
            by = c("season_x","team_a"="id")) %>%
  mutate(kickoff_time = as.Date(kickoff_time)) %>% 
  mutate(YEAR = year(kickoff_time),
         MONTH = month(kickoff_time),
         DAY = day(kickoff_time)) %>% 
  arrange(team_a_name,YEAR,MONTH,DAY) %>% 
  left_join(curr_season_teams %>%
              select(season_x,id,name) %>%
              rename(team_h_name=name),
            by = c("season_x","team_h"="id"))

# Create a function to calculate the cumulative average of next values
calc_cumavg <- function(x, window) {
  n <- length(x)
  result <- numeric(n)
  for (i in seq_along(x)) {
    end_index <- min(n, i + window)
    result[i] <- mean(x[(i + 1):end_index], na.rm = TRUE)
  }
  result
}

a_team_fdr <-
  curr_season_fdr %>%
  arrange(team_a_name, YEAR, MONTH, DAY) %>%
  group_by(team_a_name) %>%
  mutate(across(.cols = team_a_difficulty, 
                .fns = ~ calc_cumavg(.x, 5),
                .names = "{.col}_avg_next_5")) %>%
  ungroup() %>%
  mutate(across(contains("_avg_next_"), ~ ifelse(is.na(.x), 0, .x))) %>% 
  arrange(kickoff_time,team_a_name,team_h_name)

h_team_fdr <-
  curr_season_fdr %>%
  arrange(team_h_name, YEAR, MONTH, DAY) %>%
  group_by(team_h_name) %>%
  mutate(across(.cols = team_h_difficulty, 
                .fns = ~ calc_cumavg(.x, 5),
                .names = "{.col}_avg_next_5")) %>%
  ungroup() %>%
  mutate(across(contains("_avg_next_"), ~ ifelse(is.na(.x), 0, .x))) %>% 
  arrange(kickoff_time,team_a_name,team_h_name)

team_fdr <-
  left_join(a_team_fdr,h_team_fdr)

team_fdr <-
  bind_rows(team_fdr %>% 
            select(kickoff_time,contains("team_a"),-contains("avg")) %>% 
            rename(team_id=team_a,
                   team = team_a_name,
                   team_difficulty = team_a_difficulty),
          team_fdr %>% 
            select(kickoff_time,contains("team_h"),-contains("avg")) %>% 
            rename(team_id=team_h,
                   team=team_h_name,
                   team_difficulty = team_h_difficulty)) %>%
  arrange(team, kickoff_time) %>%
  group_by(team) %>%
  mutate(across(.cols = team_difficulty, 
                .fns = ~ calc_cumavg(.x, 5),
                .names = "{.col}_avg_next_5")) %>%
  ungroup() %>%
  mutate(across(contains("_avg_next_"), ~ ifelse(is.na(.x), 0, .x))) %>% 
  arrange(team,kickoff_time)

# Export ----
team_fdr %>% 
  write_csv(here(paste0("season ", curr_season, "/data/fdr_current_season"),
                 "fdr_upcoming_season.csv"))