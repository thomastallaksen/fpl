library(fplr)
library(tidyverse)
library(gridExtra)

players <- fpl_get_players()
player_details <- fpl_get_player_detailed(player_id = list)
fixtures <- fpl_get_fixtures()

martin <- fpl_get_user_performance(user_id = 2622564)
thomas <- fpl_get_user_performance(user_id = 4452806)
phong <- fpl_get_user_performance(user_id = 4555888)
mina <- fpl_get_user_performance(user_id = 54323)

list <- (players$id)


players%>%
  select(first_name, second_name, status, value_form, ict_index, points_per_game)%>%
  arrange(desc(points_per_game))



#Sammenlikne rank

martin_rank <- martin$history%>%
  select(event, rank)%>%
  rename(rank_martin = rank)

thomas_rank <- thomas$history%>%
  select(event, rank)%>%
  rename(rank_thomas = rank)

sammenlikning_rank <- martin_rank%>%
  left_join(thomas_rank)%>%
  gather(key = name, value = rank, -1)
  
plot_rank <- ggplot(sammenlikning_rank, aes(x=event, y=rank, colour = name))+
  geom_line(size = 1)+
  ggtitle("Rank", subtitle = NULL)+
  scale_fill_grey()+
  scale_y_reverse()


#Sammenlikne totale poeng

martin_totpt <- martin$history%>%
  select(event, total_points)%>%
  rename(totpt_martin = total_points)

thomas_totpt <- thomas$history%>%
  select(event, total_points)%>%
  rename(totpt_thomas = total_points)

sammenlikning_totpt <- martin_totpt%>%
  left_join(thomas_totpt)%>%
  gather(key = name, value = total_points, -1)

plot_totpt <- ggplot(sammenlikning_totpt, aes(x=event, y=total_points, colour = name))+
  geom_line(size = 1)+
  ggtitle("Total points", subtitle = NULL)+
  scale_fill_grey()


#Sammenlikne poeng per runde

martin_pt <- martin$history%>%
  select(event, points)%>%
  rename(pt_martin = points)

thomas_pt <- thomas$history%>%
  select(event, points)%>%
  rename(pt_thomas = points)

phong_pt <- phong$history%>%
  select(event, points)%>%
  rename(pt_phong = points)

mina_pt <- mina$history%>%
  select(event, points)%>%
  rename(pt_mina = points)

sammenlikning_pt <- thomas_pt%>%
  left_join(mina_pt)%>%
  gather(key = name, value = points, -1)

ggplot(sammenlikning_pt, aes(x=event, y=points, fill = name))+
  geom_bar(stat="identity", width = 0.6, position = position_dodge(width=0.7))+
  geom_text(aes(label=points), position = position_dodge(width=0.7), vjust=-0.25)+
  ggtitle("Total points")


grid.arrange(plot_rank, plot_round)
