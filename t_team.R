library(dplyr)

team <- data.frame(c(
  "Fabianski", 
  "Etheridge", 
  "Rice", 
  "Robertson", 
  "Bamba", 
  "Digne", 
  "Wan-Bissaka", 
  "Camarasa", 
  "Salah", 
  "Bernado Silva", 
  "Pogba", 
  "Lingard", 
  "Deeney", 
  "Rashford", 
  "Aubameyang"))

colnames(team) <- "Players"

t_team <- team%>%
  left_join(players_utvalg, by = c("Players" = "second_name"))

View(t_team)