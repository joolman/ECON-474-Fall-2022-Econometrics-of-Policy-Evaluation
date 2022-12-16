
##=========================##
## ----------------------- ##
#### Probability Example ####
## ----------------------- ##
##=========================##
library(tidyverse)

# number of doors
doors = 1:3

# number of repetitions
n = 10000

# an empty placeholder object
results = tibble(prize = rep(NA, n),
                 guess = rep(NA, n),
                 switch = rep(NA, n))

# Experiments
set.seed(474)
for(i in 1:n){
  prize = sample(doors, 1)
  guess = sample(doors, 1)
  empty = setdiff(doors, c(prize, guess))[1]
  switch = setdiff(doors, c(guess, empty))[1]
  
  results$prize[i] = prize
  results$guess[i] = guess
  results$switch[i] = switch
}

results %>%
  summarise(stayers = mean(prize == guess),
            switches = mean(prize == switch))

##============================##
## -------------------------- ##
#### Expected Value Example ####
## -------------------------- ##
##============================##
# test E[wage_champaign | bachelor's degree, prime-age worker, employed] =
#         E[wage_chicago | bachelor's degree, prime-age worker, employed]
# Need a t-test
#
# Here, the null hypothesis is that the expected values are equal
#
# Data are from the American Community Survey (ACS) obtained from www.ipums.org

library(data.table)
acs = fread('C:/Users/johnj/Dropbox/Julian/Academia/UIUC/Semester 11 Fall 2022/ECON474/Data/acs 2019.csv')
acs

names(acs) = acs %>% names %>% tolower
acs


# expected wage in champaign the same as chicago conditional on stuff?

acs_trim = acs %>%
  filter(gq %in% c(1, 2) & # not living in group quarters (rooming houses, military barracks, prison, dorms)
           # https://usa.ipums.org/usa-action/variables/GQ#description_section
           educd == 101 & # bachelor's degree code
           # https://usa.ipums.org/usa-action/variables/educ#codes_section
           age >= 30 & age <= 55 &
           empstat == 1 &
           # https://usa.ipums.org/usa-action/variables/empstat#codes_section
           met2013 %in% c(16580, 16980)) %>%
           # https://usa.ipums.org/usa-action/variables/met2013#codes_section
  mutate(incwage = ifelse(incwage < 999998,
                          # https://usa.ipums.org/usa-action/variables/incwage#codes_section
                          incwage,
                          NA)) %>%
  na.omit()

# Note with survey data that the sampled individuals may not be representative of the 
# population of interest.
#
# Because we are interested in aggregated statistics (i.e. Champaign vs. Chicago) instead of
# individual comparisons (e.g. gender wage gap), we need to adjust how much emphasis
# we are putting on each individual by using the survey weights.
#
# This then adjusts the sample to become representative of the area.

acs_trim %>%
  group_by(met2013) %>%
  summarise(wrong_avg_wage = mean(incwage),
            avg_wage = weighted.mean(incwage, perwt))

# Creating ojects to perform a t test
champaign = acs_trim %>%
  filter(met2013 == 16580)
chicago = acs_trim %>%
  filter(met2013 == 16980)

?t.test # no weights possible
t.test(champaign$incwage, chicago$incwage)
# In this case the results are qualitiatvely true (although not necessarily)
# but the actual estimates and p-values are biased (wrong)

# Let's do this properly:
library(weights)
?wtd.t.test
wtd.t.test(x = champaign$incwage,
           weight = champaign$perwt,
           y = chicago$incwage,
           weighty = chicago$perwt)
