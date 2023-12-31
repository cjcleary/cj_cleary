### rushing over time using prf -------------------------
library(tidyverse)
library(rvest)

# define column names
nfl_rush_colnames_old <- c("rank", "player_name", "team", "age", 
                           "position", "games_played", "games_started", "rush_attempts",
                           "rush_yards", "rush_touchdowns", "rush_long", "rush_yards_per_attempt",
                           "rush_yds_per_game", "rush_fumbles")

nfl_rush_colnames_current <- c("rank", "player_name", "team", "age", "position", "games_played", 
                               "games_started", "rush_attempts", "rush_yards", "rush_touchdowns",
                               "rush_first_downs", "rush_success_rate", "rush_long", "rush_yards_per_attempt",
                               "rush_yds_per_game", "rush_fumbles")

# function for scrapping data
scrape_rush_data <- function(year) {
  
  # define url
  url <- paste0("https://www.pro-football-reference.com/years/", year, "/rushing.htm")
  
  x <- read_html(file.path(url)) %>% 
    html_table() %>% 
    as.data.frame()
  
  # logic because there was a change in the column counts after 1993
  if(year < 1993) {
    colnames(x) <- nfl_rush_colnames_old
    
    dat <- x %>% 
      # filter out column headers since they repeat every 30 rows
      filter(!grepl("Rk", rank)) %>% 
      # do not need rank
      select(-rank) %>% 
      relocate(position, .after = team) %>% 
      # clean up data and player names
      mutate(across(.cols = 5:13, as.numeric),
             team = nflreadr::clean_team_abbrs(team),
             player_name = str_remove(player_name, "\\*"),
             player_name = str_remove(player_name, "\\+")) 
  }
  
  # if year > 1994, change column names
  else {
    colnames(x) <- nfl_rush_colnames_current
    
    dat <- x %>% 
      filter(!grepl("Rk", rank)) %>% 
      select(-rank) %>% 
      relocate(position, .after = team) %>% 
      mutate(across(.cols = 5:15, as.numeric),
             team = nflreadr::clean_team_abbrs(team),
             player_name = str_remove(player_name, "\\*"),
             player_name = str_remove(player_name, "\\+")) 
  }
  
  # lastly, clean up add in year, make team a factor
  dat <- dat %>% 
    mutate(year = year, .before = player_name,
           team = fct(team),
           position = fct(position),
           player_name = nflreadr::clean_player_names(player_name))
  
  # because sportsref is smart and doesn't want their servers demolished by scrapers
  Sys.sleep(time = 5)
}

dat_2000s <- 2023 %>% 
  map(scrape_rush_data, .progress = T) %>% 
  bind_rows()

full_rushing_dat <- 2009:2023 %>% 
  map(possibly(scrape_rush_data), .progress = T)

bind_rows(full_rushing_dat)
scrape_rush_data(2003)


