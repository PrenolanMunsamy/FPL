library(httr)
library(jsonlite)
library(here)
library(tidyverse)

source(here("season 2024_25/99. Constants.R"))

# URL for the FPL API
fpl_main_url <- "https://fantasy.premierleague.com/api/bootstrap-static/"
fpl_fixtures_url <- "https://fantasy.premierleague.com/api/fixtures/"

# Fetch the main data ----
fpl_main_response <- GET(fpl_main_url)
fpl_main_content <- content(fpl_main_response, as = "text")
fpl_main_data <- fromJSON(fpl_main_content)

# Extract relevant information ----
# **** Player Info ----
players_raw <- fpl_main_data$elements

# Select the desired columns
players_cleaned <- 
  players_raw %>% 
  select(element_type, now_cost, id, first_name, second_name, team)

# **** Team Info ----
teams_raw <- fpl_main_data$teams

# **** Add Team Info to Player Info ----
players_cleaned <- 
  players_cleaned %>% 
  left_join(teams_raw %>% 
              select(id,name),
            by = c("team"="id")) %>% 
  select(-team) %>% 
  rename(value=now_cost,
         team=name,
         position=element_type) %>%
  unite(name,first_name,second_name,sep=" ") %>% 
  select(name,position,team,value)

# Fetch Fixture data ----
fpl_fixture_response <- GET(fpl_fixtures_url)
fpl_fixture_content <- content(fpl_fixture_response, as = "text")
fpl_fixture_data <- fromJSON(fpl_fixture_content)

# Extract relevant information ----
fixtures_raw <- fpl_fixture_data

# Add Fixture Info to Player Info ----
players_cleaned <-
  players_cleaned %>% 
  left_join(fixtures_raw %>% 
              select(event,kickoff_time,team_a,team_h) %>% 
              filter(event==gw) %>% 
              pivot_longer(cols = -c(event,kickoff_time)) %>%
              rename(home_away=name) %>% 
              left_join(teams_raw %>% 
                          select(id,name),
                        by = c("value"="id")) %>% 
              select(-value) %>% 
              rename(team=name),
            by = "team")


# Add opponent team and finalise data ----
players_cleaned <- bind_rows(
  # handle home team case first
  players_cleaned %>% 
    left_join(teams_raw %>% 
                select(id,name),
              by = c("team"="name")) %>%
    filter(home_away=="team_h") %>% 
    left_join(fixtures_raw %>% 
                select(event,kickoff_time,team_a,team_h),
              by = c("kickoff_time","id"="team_h")) %>% 
    rename(opponent_team=team_a) %>% 
    select(name,position,team,kickoff_time,opponent_team,value,home_away),
  # handle away team case second
  players_cleaned %>% 
    left_join(teams_raw %>% 
                select(id,name),
              by = c("team"="name")) %>%
    filter(home_away=="team_a") %>% 
    left_join(fixtures_raw %>% 
                select(event,kickoff_time,team_a,team_h),
              by = c("kickoff_time","id"="team_a")) %>% 
    rename(opponent_team=team_h) %>% 
    select(name,position,team,kickoff_time,opponent_team,value,home_away)
) %>% 
  rename(was_home=home_away) %>% 
  mutate(was_home = ifelse(was_home=="team_h","TRUE","FALSE"),
         position = case_when(position==1~"GK",
                              position==2~"DEF",
                              position==3~"MID",
                              position==4~"FWD")) %>% 
  mutate(kickoff_time = date(kickoff_time))

# Export ----
players_cleaned %>% 
  write_csv(here(paste0("season ",curr_season,"/data/gws_current_season_upcoming"),
                 paste0("gw",gw,".csv")))