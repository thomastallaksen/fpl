library(fplr)
library(tidyverse)

best_users <- c(1908330, 568651, 1229681, 1734934, 933902, 707074, 1032977, 151003, 2975622, 173155)

best_stats <- function(x){
  user_current <- fpl_get_user_current(x)
}

best_chips <- function(x){
  user_current <- fpl_get_user_chips(x)%>%
    group_by(name)%>%
    summarise(Antall = n())
}

user_current <- fpl_get_user_current(1908330)

mean_points <- mean(user_current$points)
mean_rank <- mean(user_current$rank)

api <- fpl_get_bootstrap()

best_of <- lapply(best_users, best_stats)%>%
  bind_rows(.id = "ID")%>%
  filter(event > 8)

chips <- lapply(best_users, best_chips)%>%
  bind_rows(.id = "ID")%>%
  spread(key = "name", value = "Antall")%>%
  mutate(ID = as.numeric(ID))%>%
  arrange(ID)

ggplot(best_of, aes(x = event, y = overall_rank, colour = ID))+
  geom_line()+
  scale_y_reverse()



