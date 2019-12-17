# Illustrate cost vs points
library(fplr)
library(tidyverse)
library(gridExtra)

fpl_get_player_all()%>%
  mutate(position = ifelse(element_type == 1, "Goalkeeper", 
                    ifelse(element_type == 2, "Defender",
                    ifelse(element_type == 3, "Midfielder", 
                    ifelse(element_type == 4, "Forward", NA)))))%>%
  mutate(rank = rank(desc(total_points)))%>%
  mutate(value = total_points/now_cost)%>%
  filter(rank <= 30)%>%
  ggplot(aes(x = now_cost, y = total_points, color = position))+
  geom_point(aes(size = value))+
  geom_text(aes(x = now_cost+0.2, label=web_name), size=4, hjust=0)+
  scale_size_continuous(range = c(1, 15))+
  theme(legend.position = "none")



# Test how many points the team picked from the chart over generates

best_team <- c("Vardy", "Rashford", "Abraham", "Mane", "De Bruyne", "Son", "Salah", "Lundstram", "Baldock", "Pereira", "Ramsdale")

round_one_cost <- function(x){
  player <- fpl_get_player_detailed(x)
  cost <- player$history%>%
    filter(round == 1)%>%
    select(value)
  return(cost$value[[1]])
}

fpl_get_player_all()%>%
  filter(web_name %in% best_team)%>%
  mutate(round_one_cost = map_dbl(id, round_one_cost))%>%
  summarise(Points = sum(total_points)+133, Cost = sum(round_one_cost))


# Points vs. cost last five rounds

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
  select(id, position, web_name, now_cost, team)

cumsum <- players_all_rounds%>%
  mutate(id = as.integer(element))%>%
  filter(id %in% best_last_round$id)%>%
  left_join(best_last_round, by = c("element" = "id"))%>%
  filter(round > 12)%>%
  group_by(element)%>%
  mutate(last5 = cumsum(total_points))%>%
  filter(round == 17)%>%
  ungroup()%>%
  top_n(25, wt = last5)


offensivt_neste5 <- cumsum%>%
  mutate(value = last5/now_cost)%>%
  left_join(teams_spi_next5, by = c("team" = "id"))%>%
  filter(position %in% c("Forward", "Midfielder"))%>%
  ggplot(aes(x = now_cost, y = last5, color = position))+
  geom_point(aes(size = def_next5))+
  geom_text(aes(x = now_cost+0.2, label=web_name), size=4, hjust=0)+
  scale_size_continuous(range = c(1, 15))+
  theme(legend.position = "none")+
  ggtitle("Angripere og midtbanespillere")

defensivt_neste5 <- cumsum%>%
  mutate(value = last5/now_cost)%>%
  left_join(teams_spi_next5, by = c("team" = "id"))%>%
  filter(position %in% c("Goalkeeper", "Defender"))%>%
  ggplot(aes(x = now_cost, y = last5, color = position))+
  geom_point(aes(size = -off_next5))+
  geom_text(aes(x = now_cost+0.2, label=web_name), size=4, hjust=0)+
  scale_size_continuous(range = c(1, 15))+
  theme(legend.position = "none")+
  ggtitle("Forsvarere og keepere")+
  scale_color_manual(values = c("#CC79A7", "#E7B800"))

grid.arrange(offensivt_neste5, defensivt_neste5, ncol = 2)

