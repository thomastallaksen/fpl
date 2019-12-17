# Calculate wich players contibute points the most often


player_detailed <- function(x){
  player <- fpl_get_player_current(x)
  return(player)
}

player_list <- c(1:423, 427:562) 

players_all_rounds <- lapply(player_list, player_detailed)%>%
  bind_rows(.id = "ID")

player_names <- fpl_get_player_all()%>%
  select(id, web_name)

points_in_most_games <- players_all_rounds%>%
  select(total_points, round, ID)%>%
  mutate(id = as.integer(ID))%>%
  left_join(player_names)%>%
  rename(round_points = total_points)%>%
  filter(round_points > 3)%>%
  group_by(web_name)%>%
  summarise(Games = n(), Share = n())
