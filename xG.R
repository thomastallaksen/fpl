# Calculate xG with data from understat.com

library(understatr)
library(fplr)
library(tidyverse)


# Fetching all player IDs
teams <- c("Arsenal", "Aston Villa", "Bournemouth", "Brighton", "Burnley", "Chelsea", "Crystal Palace", "Everton",
           "Leicester", "Liverpool", "Manchester City", "Manchester United", "Norwich", "Newcastle United", "Sheffield United", 
           "Southampton", "Tottenham", "Watford", "West Ham", "Wolverhampton Wanderers")    

player_ids <- function(x){
  player <- get_team_players_stats(team_name = x, year = 2019)%>%
    select(player_id, player_name, team_name)
  return(player)
}

IDs <- lapply(teams, player_ids)%>%
  bind_rows(.id = "ID")


# Matching IDs with all matches per player

xG_all_matches <- function(x){
  player <- get_player_matches_stats(x)
  return(player)
}

players_matches <- lapply(IDs$player_id, xG_all_matches)%>%
  bind_rows(.id = "id")

# Fetching round stats for FPL

player_detailed <- function(x){
    player <- fpl_get_player_current(x)
  return(player)
  }


player_list <- c(1:423, 427:562) 

players_all_rounds <- lapply(player_list, player_detailed)%>%
  bind_rows(.id = "id")

player_names <- fpl_get_player_all()%>%
  select(id, first_name, second_name)%>%
  mutate(player_name = paste(first_name, second_name, sep = " "))

player_points <- players_all_rounds%>%
  mutate(id = as.integer(id))%>%
  left_join(player_names)%>%
  mutate(date = as.Date(kickoff_time))%>%
  left_join(players_matches, by = c("player_name", "date"))



# xG vs goals this far

season <- function(x){
  player <- get_player_seasons_stats(x)
  return(player)
}

xG_seasons <- lapply(IDs$player_id, season)%>%
  bind_rows(.id = "id")






