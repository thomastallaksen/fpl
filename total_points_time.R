# Total points over time

player_detailed <- function(x){
  player <- fpl_get_player_current(x)
  return(player)
}

player_list <- c(1:423, 427:562) 

players_all_rounds <- lapply(player_list, player_detailed)%>%
  bind_rows(.id = "ID")

best_last_round <- fpl_get_player_all()%>%
  mutate(position = ifelse(element_type == 1, "Goalkeeper", 
                    ifelse(element_type == 2, "Defender",
                    ifelse(element_type == 3, "Midfielder", 
                    ifelse(element_type == 4, "Forward", NA)))))%>%
  top_n(50, wt = total_points)%>%
  select(id, position, web_name)

plot <- players_all_rounds%>%
  mutate(id = as.integer(element))%>%
  filter(id %in% best_last_round$id)%>%
  left_join(best_last_round, by = c("element" = "id"))%>%
  group_by(element)%>%
  mutate(total = cumsum(total_points))

  ggplot(plot, aes(x = round, y = total, group = web_name, color = web_name))+
    geom_line()+
    facet_wrap(~position)+
    theme(legend.position = "none")+
    geom_text(data = subset(plot, round == max(round)), aes(label = web_name, colour = web_name, x = Inf, y = total), vjust = -0.3, hjust = 1, size = 3)
   