### TidyTuesday data - 2022-05-31 - made by Lorenzo Amabili ###

# Loading R libraries
library(readr)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(gt)
library(scales)

# Loading the data
poll = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/poll.csv')
reputation = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/reputation.csv')

# Checking the data
head(poll)
head(reputation)
unique(reputation$company)
unique(reputation$industry)
unique(reputation$name)
min(reputation$score)
max(reputation$score)

# Formatting the data in order to create an heatmap
data = data.frame(reputation, model.matrix( ~ reputation$name - 1, reputation))

for(i in 6:12){
  for(j in 1:dim(data)[1]){
    data[j,i] = data[j,i] * data$score[j]
  }
}

colnames(data) = c("company", "industry", "name", "score", "rank", "citizenship", "culture", "ethics", "growth", "ps", "trust", "vision")
head(data)

data$score = round(data$score, 2)

# Grouping the data by industry
grouped_data = data %>% group_by(company) %>% summarise(industry = first(industry), citizenship = sum(citizenship), culture = sum(culture), ethics = sum(ethics),  growth = sum(growth),  ps = sum(ps), trust = sum(trust) , vision = sum(vision))
data_reduced = data[, -c(3, 5:12)]
grouped_data_reduced = data_reduced %>% group_by(company, industry) %>% summarise(score = mean(score))
data_scores = cbind(grouped_data, grouped_data_reduced[,3])
industry_data = data_scores %>% group_by(industry) %>% summarise(citizenship = mean(citizenship), culture = mean(culture), ethics = mean(ethics),  growth = mean(growth),  ps = mean(ps), trust = mean(trust) , vision = mean(vision), score = mean(score))

# Finding the mean score for each industry group
summary(industry_data[, -c(1,9)])
scores_means = c(0, 70.81, 74.37, 73.42, 76.67, 75.87, 73.29, 76.78, (70.81 + 74.37 + 73.42 + 76.67 + 75.87 + 73.29 + 76.78)/7 )

# Creating the final ordered data set
final_data = rbind(industry_data, scores_means)
final_data[dim(final_data)[1],1] = 'Means'
order_cols = colSums(final_data[, -c(1,9)])/dim(final_data)[1]
order_cols = c(1, order(-order_cols)+1, 9)

# Creating an heatmap 
final_data[order(-final_data$score), order_cols] %>%
  gt() %>%
  data_color(columns = 2:9, 
             colors = col_numeric(palette = c("#deebf7", "#9ecae1", "#3182bd"),
                                  domain = c(min(industry_data[2:9]), max(industry_data[2:9]))))