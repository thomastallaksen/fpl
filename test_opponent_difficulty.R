library(fplr)
library(tidyverse)
library(data.table)

players <- fpl_get_players()
players <- players%>%
  select(id, status, first_name, second_name, team_name, position)


player_score <- function(x, y){
  player <- fpl_get_player_detailed(x)
  player_round <- player$history%>%
    select(round, total_points)%>%
    filter(round == y)
  return(player_round$total_points)
}  

players_round_1 <- players%>%
  mutate(round = 1)

players_round_1 <- players_round_1%>%
  rowwise()%>%
  mutate(points = player_score(id, round))





player_1 <- fpl_get_player_detailed(1)
number_of_rounds <- max(player_1$history$round)

players_duplicated <- players[rep(row.names(players), times = number_of_rounds), 1:6]
players_duplicated <- players_duplicated%>%
  arrange(id)
players_duplicated_table <- data.table(players_duplicated)
players_duplicated_table <- players_duplicated_table[ , round := 1:.N , by = "id"]
players_duplicated <- data.frame(players_duplicated_table)


