# fpl t_score

Project for playing with stats for Fantasy Premier League in R. Uses the fplr package made by ewenme for fetching the stats.

The t_score (Thomas score, of course!) is a score for picking the best players for next round. See update below on what it currently is made of. I will run the script every round and update with new excel sheets for next round. 

I would love comments on how to improve the algorithm, or if anybody want to contribute to the code, feel free! 

t_score 1.0:
Mean of points from last five games, divided by mean of the difficulty of the next five opponents. 

