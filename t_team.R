library(dplyr)

team <- data.frame(c(
  "Fabianski", 
  "Stekelenburg", 
  "Doherty", 
  "Robertson", 
  "Alexander-Arnold", 
  "Laporte", 
  "Dunk", 
  "Barkley", 
  "Martial", 
  "Sigurdsson", 
  "Sterling", 
  "de Andrade", 
  "Murray", 
  "Kane", 
  "Wilson"))

colnames(team) <- "Players"

t_team <- team%>%
  left_join(players_utvalg, by = c("Players" = "second_name"))
  