### TidyTuesday data - 2022-05-03 - made by Lorenzo Amabili ###

# Loading R libraries
library(readr)
library(tidyverse)
library(GGally)

# Loading the data
capacity = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/capacity.csv')
wind = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/wind.csv')
solar = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/solar.csv')
average_cost = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/average_cost.csv')

# Checking the data
head(capacity)
head(solar)
head(wind)
head(average_cost)

# Aggregating and merging the data related to the solar and wind energy
solar_aggr = solar %>%
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  group_by(year, month) %>%
  summarise(solar_mwh = mean(solar_mwh), solar_capacity = mean(solar_capacity))
head(solar_aggr)

wind_aggr = wind %>%
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  group_by(year, month) %>%
  summarise(wind_mwh = mean(wind_mwh), wind_capacity = mean(wind_capacity))
head(wind_aggr)

solar_wind = merge(solar_aggr, wind_aggr, by = c("year", 'month'), all = TRUE)
solar_wind$date = paste(solar_wind$month, solar_wind$year)
head(solar_wind)

# Visual exploration of the data
ggpairs(capacity[,2:7])
ggpairs(solar_wind[,3:6])
ggpairs(average_cost)

# Formatting the time variable
solar_wind$date = as.Date(paste(solar_wind$year, solar_wind$month, 01), "%Y %m %d")

# Plots
cols = c("Coal" = "#8dd3c7", "Gas" = "#ffffb3", "Nuclear" = "#fdb462", "Other" = "#bebada", "Solar" = "#fb8072", "Storage" = "#b3de69", "Wind" = "#80b1d3")
p1 = ggplot(capacity, aes(fill=type, y=total_gw, x=factor(year))) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = cols) + theme_bw() +
  annotate("text", x = 1, y = 340, size = 4, label = "324.57", color = "black") + 
  annotate("text", x = 7, y = 980, size = 4, label = "961.50", color = "black") 
p1

p2 = ggplot(solar_wind) +
  geom_point(aes(x=date, y=solar_mwh), color = '#fb8072', size = 3) +
  geom_smooth(aes(x=date, y=solar_mwh), color = '#fb8072', size = 3) + 
  geom_point(aes(x=date, y=wind_mwh), color = '#80b1d3', size = 3) + 
  geom_smooth(aes(x=date, y=wind_mwh) , color = '#80b1d3', size = 3) + 
  xlab("") +
  theme_bw() + theme(legend.position = "none")
p2











