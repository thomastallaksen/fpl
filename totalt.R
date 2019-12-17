library(fplr)
library(tidyverse)
library(zoo)

gameweeks <- fpl_get_player_detailed(1)

players_current <- fpl_get_player_all()%>%
  select(id, web_name, total_points, points_per_game, form, now_cost, selected_by_percent, team, element_type)

team_strength <- fpl_get_teams()%>%
  select(id, strength)

player_detailed <- function(x){
  player <- fpl_get_player_current(x)
  return(player)
}

player_list <- c(1:423, 427:562) 

players_all_rounds <- lapply(player_list, player_detailed)%>%
  bind_rows(.id = "ID")

players_all_rounds <- players_all_rounds%>%
  group_by(ID)%>%
  mutate(total = cumsum(total_points))


# Totalt vs. siste

players_all_rounds%>%
  select(round, total_points, total)%>%
  rename(round_points = total_points)%>%
  mutate(total_last_round = lag(total))%>%
  filter(round == 16)%>%
  ggplot(aes(x = total_last_round, y = round_points))+
  geom_point()

total_vs_last <- cor(last$round_points, last$total_last_round, use = "complete.obs")

# Plotter og regner korrelasjonen for snittpoeng ulike uker

round_points <- players_all_rounds%>%
  mutate(id = as.integer(ID))%>%
  rename(round_points = total_points)%>%
  select(id, round, round_points, minutes)%>%
  group_by(id)%>%
  arrange(id, round)%>%
  mutate(points_last1 = lag(rollmean(round_points, 1, fill=NA, align = "right")))%>%
  mutate(points_last2 = lag(rollmean(round_points, 2, fill=NA, align = "right")))%>%
  mutate(points_last3 = lag(rollmean(round_points, 3, fill=NA, align = "right")))%>%
  mutate(points_last4 = lag(rollmean(round_points, 4, fill=NA, align = "right")))%>%
  mutate(points_last5 = lag(rollmean(round_points, 5, fill=NA, align = "right")))%>%
  mutate(points_last6 = lag(rollmean(round_points, 6, fill=NA, align = "right")))%>%
  mutate(points_last7 = lag(rollmean(round_points, 7, fill=NA, align = "right")))%>%
  mutate(points_last8 = lag(rollmean(round_points, 8, fill=NA, align = "right")))%>%
  mutate(points_last9 = lag(rollmean(round_points, 9, fill=NA, align = "right")))%>%
  mutate(points_last10 = lag(rollmean(round_points, 10, fill=NA, align = "right")))%>%
  mutate(points_last11 = lag(rollmean(round_points, 11, fill=NA, align = "right")))%>%
  mutate(points_last12 = lag(rollmean(round_points, 12, fill=NA, align = "right")))%>%
  mutate(points_last13 = lag(rollmean(round_points, 13, fill=NA, align = "right")))%>%
  mutate(points_last14 = lag(rollmean(round_points, 14, fill=NA, align = "right")))%>%
  mutate(points_last15 = lag(rollmean(round_points, 15, fill=NA, align = "right")))%>%
  filter(minutes != 0)%>%
  select(-round)%>%
  gather(key = "last_rounds", value = "points" , -round_points, -id, -minutes)

ggplot(round_points, aes(x = points, y = round_points))+
  facet_wrap(~last_rounds)+
  geom_point()+
  geom_smooth(method = lm)



corr1 <- cor(players$round_points, players$points_last1, use = "complete.obs")
corr2 <- cor(players$round_points, players$points_last2, use = "complete.obs")
corr3 <- cor(players$round_points, players$points_last3, use = "complete.obs")
corr4 <- cor(players$round_points, players$points_last4, use = "complete.obs")
corr5 <- cor(players$round_points, players$points_last5, use = "complete.obs")
corr6 <- cor(players$round_points, players$points_last6, use = "complete.obs")
corr7 <- cor(players$round_points, players$points_last7, use = "complete.obs")
corr8 <- cor(players$round_points, players$points_last8, use = "complete.obs")
corr9 <- cor(players$round_points, players$points_last9, use = "complete.obs")
corr10 <- cor(players$round_points, players$points_last10, use = "complete.obs")
corr11 <- cor(players$round_points, players$points_last11, use = "complete.obs")
corr12 <- cor(players$round_points, players$points_last12, use = "complete.obs")
corr13 <- cor(players$round_points, players$points_last13, use = "complete.obs")
corr14 <- cor(players$round_points, players$points_last14, use = "complete.obs")
corr15 <- cor(players$round_points, players$points_last15, use = "complete.obs")






players <- players_all_rounds%>%
  select(ID, round, opponent_team, was_home, minutes, total_points)%>%
  mutate(id = as.integer(ID))%>%
  rename(round_points = total_points)%>%
  left_join(players_current, by = "id")%>%
  left_join(team_strength, by = c("opponent_team" = "id"))%>%
  left_join(teams_spi, by = c("team" = "id"))%>%
  left_join(teams_spi, by = c("opponent_team" = "id"))%>%
  mutate(attack = off_rank.x - def_rank.y)%>%
  mutate(defence = def_rank.x - off_rank.y)%>%
  filter(minutes != 0)

