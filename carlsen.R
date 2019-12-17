library(fplr)
library(tidyverse)


# Fetching round stats for all players

player_detailed <- function(x){
  player <- fpl_get_player_current(x)
  return(player)
}

player_list <- c(1:423, 427:562) 

players_all_rounds <- lapply(player_list, player_detailed)%>%
  bind_rows(.id = "ID")

player_names <- fpl_get_player_all()%>%
  select(id, web_name)

player_points <- players_all_rounds%>%
  select(total_points, round, ID)%>%
  mutate(id = as.integer(ID))%>%
  left_join(player_names)%>%
  rename(round_points = total_points)%>%
  select(web_name, round, round_points)

vardy <- player_points%>%
  filter(web_name == "Vardy")%>%
  rename(vardy = round_points)%>%
  select(round, vardy)

# Points from captain choice vs. Vardy every round

web_name <- c("Salah", "De Bruyne", "Sterling", "Sterling", "Salah", "De Bruyne", "Sterling",
             "Sterling", "Abraham", "Sterling", "Sterling", "Abraham", "Mané", "Salah",
             "Mané", "Salah", "Salah")
round <- 1:17

carlsen <- data.frame(round, web_name)

carlsen <- carlsen%>%
  left_join(player_points)%>%
  left_join(vardy)%>%
  filter(round > 6)%>%
  summarise(Carlsen = sum(round_points), Vardy = sum(vardy))


# User stats

magnus_carlsen <- fpl_get_user_current(user_id = 1908330)

