library(fplr)
library(tidyverse)
library(openxlsx)


t_score <- function(x){
  player <- fpl_get_player_detailed(x)
  points_last_5 <- mean(tail(player$history$total_points, 5))
  difficulty_next_5 <- mean(head(player$fixtures$difficulty, 5))
  t_score_result <- points_last_5/difficulty_next_5
  return(t_score_result)
}

players <- fpl_get_players()

players <- players%>%
  rowwise()%>%
  mutate(t_score = t_score(id))

players <- players%>%
  mutate(t_value = t_score/now_cost)

players_utvalg <- players%>%
  select(first_name, second_name, now_cost, position, t_score, t_value, chance_of_playing_this_round, selected_by_percent, form, total_points)%>%
  arrange(desc(t_score))
  
write.xlsx(players_utvalg, "t_score.xlsx")



