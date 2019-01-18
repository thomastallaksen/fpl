library(dplyr)

team <- data.frame(c(
  "Fabianski", 
  "Etheridge", 
  "Doherty", 
  "Robertson", 
  "Alexander-Arnold", 
  "Digne", 
  "Dunk", 
  "Son", 
  "Pereira Gomes", 
  "Sigurdsson", 
  "Pogba", 
  "de Andrade", 
  "Murray", 
  "Kane", 
  "Wilson"))

colnames(team) <- "Players"

t_team <- team%>%
  left_join(players_utvalg, by = c("Players" = "second_name"))

View(t_team)