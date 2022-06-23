### TidyTuesday data - 2022-05-17 - made by Lorenzo Amabili ###

# Loading R libraries
library(tidyverse)
library(dplyr)
library(readxl)
library(fastDummies)

# Loading the data
data_votes = read_excel('/Users/lorenzoamabili/Library/Mobile Documents/com~apple~CloudDocs/Data_Science/Datasets/TidyTuesday/2022-05-17/eurovision_votes.xlsx')

# Checking the data 
head(data_votes)

# Renaming the columns
colnames(data_votes) = c("year", "final", "edition", "jury_televoting", "from", "to", "points", "duplicate")

# Selecting only the votes related to the finals and gave by the jury
data_votes_finals = data_votes[which(data_votes$final == "f") ,]
data_votes_finals_jury = data_votes_finals[which(data_votes_finals$jury_televoting == 'J'), ]

# Cleaning the data
data_votes_finals_jury[which(data_votes_finals_jury$to == "Netherlands") ,]$to = "The Netherlands"
data_votes_finals_jury[which(data_votes_finals_jury$from == "Netherlands") ,]$from = "The Netherlands"
data_votes_finals_jury[which(data_votes_finals_jury$to == "F.Y.R. Macedonia") ,]$to = "North Macedonia"
data_votes_finals_jury[which(data_votes_finals_jury$from == "F.Y.R. Macedonia") ,]$from = "North Macedonia"

# Aggregating the data to obtain the total amount of votes each country gave to each other country
data_votes_aggr = data_votes_finals_jury %>% group_by(from, to)  %>%
  summarise(points = sum(points))
data_votes_aggr

## Reformatting the data with the final goal to create an heatmap ##
# Creating dummy variables corresponding to the countries
dummies = dummy_cols(data_votes_aggr$to)
data_votes_aggr_from_to_dummies = data.frame(data_votes_aggr, dummies)

multiply <- function(data) {
  for (i in 1:nrow(data)) { 
    for (j in 5:ncol(data)) {
      data[i, j] <- data[i, j] * data[i,3]
    }
  }
  return(data)
}

final_data_votes_aggr_from_to = multiply(data_votes_aggr_from_to_dummies)
head(final_data_votes_aggr_from_to)
final_data_votes_aggr_from_to = final_data_votes_aggr_from_to[, -c(2,4)]

colnames(final_data_votes_aggr_from_to) 
colnames(final_data_votes_aggr_from_to) = c("from", "points", "Albania", "Armenia", "Australia", "Austria", "Azerbaijan","Belarus",
                                            "Belgium","Bosnia_Herzegovina", "Bulgaria","Croatia","Cyprus","Czech_Republic"      
                                            ,"Denmark","Estonia","Finland"             
                                            ,"France","Georgia","Germany","Greece"              
                                            ,"Hungary","Iceland","Ireland","Israel"              
                                            ,"Italy","Latvia","Lithuania","Luxembourg", "Malta"               
                                            ,"Moldova","Monaco", "Montenegro","Morocco", "North_Macedonia"     
                                            ,"Norway","Poland","Portugal","Romania"             
                                            ,"Russia","San_Marino","Serbia","Serbia_Montenegro", "Slovakia" 
                                            ,"Slovenia","Spain","Sweden","Switzerland"
                                            ,"The_Netherlands","Turkey","Ukraine","United_Kingdom", "Yugoslavia")

# Creating dummy variables corresponding to the countries receiving the votes
final_data_votes_aggr = final_data_votes_aggr_from_to %>% group_by(from)  %>%
  summarise(given_points = sum(points), Albania = sum(Albania), Armenia = sum(Armenia), Australia = sum(Australia), Austria = sum(Austria), Azerbaijan = sum(Azerbaijan),
            Belarus = sum(Belarus), Belgium = sum(Belgium), Bosnia_Herzegovina = sum(Bosnia_Herzegovina), Bulgaria = sum(Bulgaria), Croatia = sum(Croatia),
            Cyprus = sum(Cyprus), Czech_Republic = sum(Czech_Republic), Denmark = sum(Denmark), Estonia = sum(Estonia),
            Finland = sum(Finland), France = sum(France), Georgia = sum(Georgia), Germany = sum(Germany), Greece = sum(Greece),
            Hungary = sum(Hungary), Iceland = sum(Iceland), Ireland = sum(Ireland), Israel = sum(Israel), Italy = sum(Italy),
            Latvia = sum(Latvia), Lithuania = sum(Lithuania), Luxembourg = sum(Luxembourg), Malta = sum(Malta), Moldova = sum(Moldova),
            Monaco = sum(Monaco), Montenegro = sum(Montenegro), Morocco = sum(Morocco), North_Macedonia = sum(North_Macedonia), Norway = sum(Norway),
            Poland = sum(Poland), Portugal = sum(Portugal), Romania = sum(Romania), Russia = sum(Russia), San_Marino = sum(San_Marino),
            Serbia = sum(Serbia), Serbia_Montenegro = sum(Serbia_Montenegro), Slovakia = sum(Slovakia), Slovenia = sum(Slovenia), Spain = sum(Spain),
            Sweden = sum(Sweden), Switzerland = sum(Switzerland), The_Netherlands = sum(The_Netherlands), Turkey = sum(Turkey), Ukraine = sum(Ukraine),
            United_Kingdom = sum(United_Kingdom), Yugoslavia = sum(Yugoslavia))
head(final_data_votes_aggr)

# Adding a dummy variable for Andorra which gave votes but did not receive any to have the same countries on the x and y axis.
final_data_votes_aggr$Andorra = integer(nrow(final_data_votes_aggr))

# Ordering rows and columns based on the total amount of received and given votes
order_countries_to = colSums(final_data_votes_aggr[, -c(1,2)])
order_countries_to = order(-order_countries_to)
final_data_votes_aggr_ordered = final_data_votes_aggr[order(final_data_votes_aggr$given_points),]
final_data_countries = final_data_votes_aggr_ordered[, -c(1, 2)]
final_data_countries = final_data_countries[, order_countries_to]

# Plotting the final heatmap
heatmap(as.matrix(final_data_countries), Rowv = NA, Colv = NA, labRow = final_data_votes_aggr_ordered$from)