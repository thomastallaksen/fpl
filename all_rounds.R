library(fplr)
library(tidyverse)


players <- fpl_get_players()%>%
  select(id, web_name)%>%
  arrange(id)

player_1 <- fpl_get_player_detailed(1)
next_round <- max(player_1$history$round)
round_list <- 1:next_round

points <- function(x, y){
  player <- fpl_get_player_detailed(x)
  round_score <- player$history%>%
    select(total_points, round)%>%
    filter(round == y)%>%
    select(total_points)
  pull(round_score)
}

one_table_to_rule_them_all <- players%>%
  group_by(id, web_name)%>%
  expand(round = seq(round_list))%>%
  rowwise()%>%
  mutate(points = points(id, round))

round_1 <- players%>%
  select(id, web_name)%>%
  mutate(round = 1)%>%
  rowwise()%>%
  mutate(points = points(id, round))




