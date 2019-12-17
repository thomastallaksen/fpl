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

web_name <- c("Salah", "Salah", "Deulofeu", "De Bruyne", "Salah", "De Bruyne", "Abraham", "Deulofeu", "Abraham",
              "Abraham", "Dunk", "Martial", "Kane", "Vardy", "Vardy", "Pukki", "Mané")

# Når han kapteiner dunk har han hatt Abraham de to foregående rundene. Dunk får 7 poeng, abraham 12. Kapper Walker-Peters i runde 13. Batshuayi i 14, 
# men ingen av dem kommer heldigvis inn. 

round <- 1:17

kalleklev <- data.frame(round, web_name)

kalleklev <- kalleklev%>%
  left_join(player_points)%>%
  left_join(vardy)%>%
  #filter(round > 6)%>%
  summarise(Carlsen = sum(round_points), Vardy = sum(vardy))


# User stats

helge_kalleklev <- fpl_get_user_current(user_id = 378991)

ggplot(helge_kalleklev, aes(x = event, y = overall_rank))+
  geom_line()+
  scale_y_reverse()+
  ggtitle("Kalleklevs rank")
