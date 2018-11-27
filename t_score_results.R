library(fplr)
library(tidyverse)

round <- 13

player_points <- fpl_get_players()%>%
  select(id, event_points)

csvFileName <- paste("t_score csv/t_score_round_",round,".csv",sep="") 
t_score_round <- read_csv(csvFileName)

t_score_round <- t_score_round%>%
  left_join(player_points)

ggplot(t_score_round, aes(x = t_score, y = event_points))+
  geom_point()+
  stat_smooth(method=lm, se = FALSE)+
  geom_text(aes(label=second_name), size=4, vjust=2)

linearMod <- lm(event_points ~ t_score, data=t_score_round)

summary(linearMod)

ggplot(t_score_round, aes(x = mean_points_last_5, y = event_points))+
  geom_point()+
  stat_smooth(method=lm, se = FALSE)+
  geom_text(aes(label=second_name), size=4, vjust=2)

linearMod2 <- lm(event_points ~ mean_points_last_5, data=t_score_round)

summary(linearMod2)

ggplot(t_score_round, aes(x = t_score_cpt, y = event_points))+
  geom_point()+
  stat_smooth(method=lm, se = FALSE)+
  geom_text(aes(label=second_name), size=4, vjust=2)

linearMod3 <- lm(event_points ~ t_score_cpt, data=t_score_round)

summary(linearMod3)




