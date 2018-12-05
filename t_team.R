library(dplyr)

team <- data.frame(c(
  "dos Santos Patrício", 
  "Stekelenburg", 
  "Doherty", 
  "Robertson", 
  "Walker", 
  "Laporte", 
  "Dunk", 
  "Barkley", 
  "Martial", 
  "Sigurdsson", 
  "Sterling", 
  "de Andrade", 
  "Murray", 
  "Lacazette", 
  "Wilson"))

colnames(team) <- "Players"

t_team <- team%>%
  left_join(players_utvalg, by = c("Players" = "second_name"))
  