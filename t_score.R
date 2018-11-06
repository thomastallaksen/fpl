library(fplr)
library(tidyverse)
library(openxlsx)


players <- fpl_get_players()


# Definerer funksjoner

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


# Anvender funksjonene per spiller

players <- players%>%
  rowwise()%>%
  mutate(difficulty_next_5 = difficulty_next_5(id))%>%
  mutate(t_score_cpt = t_score_cpt(id))%>%
  mutate(points_last_5 = points_last_5(id))%>%
  mutate(t_score = t_score(id))


# Velger ut relevante stats 

players_utvalg <- players%>%
  select(first_name, second_name, team_name, now_cost, position, chance_of_playing_this_round, form, total_points, points_last_5, difficulty_next_5, t_score, t_score_cpt)%>%
  arrange(desc(t_score))


# Printer til excel
  
write.xlsx(players_utvalg, "t_score.xlsx")




