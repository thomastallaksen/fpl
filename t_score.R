library(fplr)
library(tidyverse)


players <- fpl_get_players()


# Defines functions

t_score <- function(x){
  player <- fpl_get_player_detailed(x)
  points_last_5 <- mean(tail(player$history$total_points, 5))
  difficulty_next_5 <- mean(head(player$fixtures$difficulty, 5))
  t_score <- points_last_5/difficulty_next_5
  return(t_score)
}

t_score_cpt <- function(x){
  player <- fpl_get_player_detailed(x)
  points_last_5 <- mean(tail(player$history$total_points, 5))
  difficulty_next_5 <- head(player$fixtures$difficulty, 1)
  t_score <- points_last_5/difficulty_next_5
  return(t_score)
}  

points_last_5 <- function(x){
  player <- fpl_get_player_detailed(x)
  points_last_5 <- mean(tail(player$history$total_points, 5))
  return(points_last_5)
}

difficulty_next_5 <- function(x){
  player <- fpl_get_player_detailed(x)
  difficulty_next_5 <- mean(head(player$fixtures$difficulty, 5))
  return(difficulty_next_5)
}


# Calculate rowwvise stats for the individual players using the functions

players <- players%>%
  rowwise()%>%
  mutate(difficulty_next_5 = difficulty_next_5(id))%>%
  mutate(t_score_cpt = t_score_cpt(id))%>%
  mutate(points_last_5 = points_last_5(id))%>%
  mutate(t_score = t_score(id))


# Select appropriate columns

players_utvalg <- players%>%
  select(second_name, first_name, team_name, position, now_cost,form, total_points, points_last_5, difficulty_next_5, t_score, t_score_cpt)%>%
  arrange(desc(t_score))


# Prints csv named with last round

player_1 <- fpl_get_player_detailed(1)
next_round <- max(player_1$history$round)+1

csvFileName <- paste("t_score csv/t_score_round_",next_round,".csv",sep="") 
write.csv(players_utvalg, file=csvFileName) 

