# fpl - t_score

Project for playing with stats for Fantasy Premier League, to learn R and have more fun with FPL. I rely heavily on the fplr package made by ewenme for fetching the stats. The t_score (Thomas score, duh!) is a score for picking the best players for next round. I will iterate on the model, so see below on what it currently consists of. I will run the script every round and update with new csv-sheets for next round. 

I would love comments on how to improve the model, or if anybody want to contribute to the code, feel free! 

t_score 1.0:
Mean of points from last five games, divided by mean of the difficulty of the next five opponents.
t_score_cpt looks only at the next game, and is for choosing captain. 

