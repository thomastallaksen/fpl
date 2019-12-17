library(fplr)
library(tidyverse)


players <- fpl_get_player_all()%>%
  select(id, web_name, total_points, points_per_game, form, now_cost, selected_by_percent, team)

# Defines functions

mean_points_last_5 <- function(x){
  player <- fpl_get_player_current(x)
  points_last_5 <- mean(tail(player$total_points, 5))
  return(points_last_5)
}

difficulty_next_5 <- function(x){
  player <- fpl_get_player_detailed(x)
  difficulty_next_5 <- mean(head(player$fixtures$difficulty, 5))
  return(difficulty_next_5)
}

spi_off_next_5 <- function(x){
  player <- fpl_get_player_detailed(x)
  fixtures <- player$fixtures%>%
  mutate(opponent = ifelse(is_home == TRUE, team_a, team_h))%>%
  left_join(teams_spi, by = c("opponent" = "id"))
  difficulty_next_5 <-  mean(head(fixtures$off, 5))
  return(difficulty_next_5)
  }

spi_def_next_5 <- function(x){
  player <- fpl_get_player_detailed(x)
  fixtures <- player$fixtures%>%
    mutate(opponent = ifelse(is_home == TRUE, team_a, team_h))%>%
    left_join(teams_spi, by = c("opponent" = "id"))
  difficulty_next_5 <-  mean(head(fixtures$def, 5))
  return(difficulty_next_5)
}

# Calculate rowwvise stats for the individual players using the functions

players_points <- players%>%
  mutate(mean_points_last_5 = lapply(id, mean_points_last_5))

players_off <- players_points %>%
  mutate(off_next_5 = lapply(id, spi_off_next_5))

players_def <- players_off %>%
  mutate(def_next_5 = lapply(id, spi_def_next_5))
