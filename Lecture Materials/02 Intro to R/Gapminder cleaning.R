# Creating the data set for lecture 2: Intro to R

# Packages that meake me happy and changing working directory
library(tidyverse)
setwd('C:/Users/johnj/Dropbox/Julian/Academia/UIUC/Semester 11 Fall 2022/ECON474/Data/')

# Loading in the data
gapminder = read.csv('gapminder_all.csv')

# Pivoting longer and selecting years 2007
gapminder = gapminder %>%
  pivot_longer(cols = gdpPercap_1952:pop_2007,
               names_to = c('.value', 'year'),
               names_pattern = '(.*)_(.*)') %>%
  filter(year == max(year))

# Saving the data set
?write.csv
write.csv(gapminder, file = 'gapminder_2007.csv', row.names = FALSE)
