### TidyTuesday data - 2022-05-24 - made by Lorenzo Amabili ###

# Loading R libraries
library(readr)
library(tidyverse)
library(ggplot2)

# Loading the data
sevens = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-24/sevens.csv')

# Checking the data
head(sevens)

# Selecting only the data related to the World Series
sevens_world_series = sevens[which(sevens$tournament == 'World Series'), ][, c(2,4,5,7,8,9,13,14,15,16)]
head(sevens_world_series)

# Transforming the score variables in numeric variables
sevens_world_series$score_1 = as.numeric(sevens_world_series$score_1)
sevens_world_series$score_2 = as.numeric(sevens_world_series$score_2)

# Creating the variables for the winner scores and the loser scores
sevens_world_series$score_winner = apply(sevens_world_series[, 2:3], 1, max)
sevens_world_series$score_loser = apply(sevens_world_series[, 2:3], 1, min)
sevens_world_series = sevens_world_series[, -c(2,3)]
summary(sevens_world_series)

# Selecting only the data related to the finals
sevens_ws_reduced = sevens_world_series[which(sevens_world_series$stage == 'Final'), ]

# Formatting the time variable
sevens_ws_reduced['year'] = format(as.Date(sevens_ws_reduced$date, format="%d/%m/%Y"),"%Y")

# Creating the dot chart
dotchart(sevens_ws_reduced$score_winner, labels = sevens_ws_reduced$date, pch = 21, bg = "green", pt.cex = 1.5,
         xlim = range(sevens_ws_reduced$score_winner, sevens_ws_reduced$score_loser) + c(-4, 4))
points(sevens_ws_reduced$score_loser, 1:nrow(sevens_ws_reduced), col = "red", pch = 19, cex = 1.5)

invisible(sapply(1:nrow(sevens_ws_reduced), function(i) {
  segments(min(sevens_ws_reduced$score_winner[i], sevens_ws_reduced$score_loser[i]), i,
           max(sevens_ws_reduced$score_winner[i], sevens_ws_reduced$score_loser[i]), i, lwd = 1)
  text(min(sevens_ws_reduced$score_winner[i], sevens_ws_reduced$score_loser[i]) - 3, i,
       labels = sevens_ws_reduced$loser[i])
  text(max(sevens_ws_reduced$score_winner[i], sevens_ws_reduced$score_loser[i]) + 3, i,
       labels = sevens_ws_reduced$winner[i])
}))