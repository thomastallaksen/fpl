library(fplr)
library(tidyverse)
library(openxlsx)

players <- fpl_get_players()
spi_teams <- read.csv(url("https://projects.fivethirtyeight.com/soccer-api/club/spi_global_rankings.csv"))


t_score_2 <- function(x){
  player <- fpl_get_player_detailed(x)
  position <- players%>%
    filter(id == x)%>%
    select(position)
  minutes_last_5 <- mean(tail(player$history$minutes, 5))
  next_5_opponents <- data.frame(tail(player$fixtures$opponent_name, 5))
  names(next_5_opponents) <- "name"
  next_5_spi <- next_5_opponents%>%
    left_join(spi_teams, by = "name")
  points_last_5 <- mean(tail(player$history$total_points, 5))
  difficulty_next_5 <- mean(head(player$fixtures$difficulty, 5))
  t_score_result <- points_last_5/difficulty_next_5
  return(t_score_result)
}
