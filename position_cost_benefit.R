# Hvilket ledd lønner det seg å bruke penger på?
library(fplr)
library(tidyverse)

# Finner korrelasjonen mellom pris og totale poeng hittil
players <- fpl_get_player_all()%>%
  select(id, web_name, element_type, total_points, now_cost)%>%
  mutate(position = ifelse(element_type == 1, "Goalkeeper", 
                    ifelse(element_type == 2, "Defender",
                    ifelse(element_type == 3, "Midfielder", 
                    ifelse(element_type == 4, "Forward", NA)))))%>%
  mutate(value = total_points/now_cost)%>%
  arrange(element_type, desc(value))%>%
  mutate(rank = rank(desc(total_points)))%>%
  mutate(top20 = ifelse(rank <=20, 1, 0))

korrelasjoner <- players%>%
  group_by(element_type)%>%
  summarise(korrelasjon = cor(total_points, now_cost, use = "complete.obs"))

ggplot(players, aes(y = total_points, x = now_cost, color = top20))+
  facet_wrap(~element_type)+
  geom_point()+
  geom_smooth(method = lm)

players%>%
  filter(rank <= 30)%>%
  ggplot(aes(y = total_points, x = now_cost, color = position))+
  geom_point()+
  geom_text(aes(x = now_cost+0.1, label=web_name), size=4, hjust=0) 