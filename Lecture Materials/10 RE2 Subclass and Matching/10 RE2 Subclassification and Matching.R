
##======================##
## -------------------- ##
#### 0) Preliminaries ####
## -------------------- ##
##======================##
library(tidyverse)

titanic = read.csv('C:/Users/johnj/Dropbox/Julian/Academia/UIUC/Semester 11 Fall 2022/ECON474/Data/titanic3.csv')

# Looking at the data
head(titanic); tail(titanic)
summary(titanic)


# cleaning the data
titanic = titanic %>%
  mutate(body = ifelse(is.na(body), 0, body),
         age = ceiling(age)) %>%
  na.omit()

##==========================##
## ------------------------ ##
#### 1) Subclassification ####
## ------------------------ ##
##==========================##

## --------------------------------------------- ##
#### 1.1) Simple differences in outcomes (SDO) ####
## --------------------------------------------- ##

# Creating treatment variable
titanic = titanic %>%
  # three ways to create treatment variable
  mutate(d = (pclass == 1)*1,
         d = ifelse(pclass == 1, 1, 0),
         d = case_when(pclass == 1 ~ 1,
                       TRUE ~ 0))

# simple differences in outcomes
sdo = titanic %>%
  group_by(d) %>%
  summarise(y_i = mean(survived)) %>%
  pivot_wider(names_from = d,
              values_from = y_i,
              names_prefix = 'y_') %>%
  mutate(sdo = y_1 - y_0) %>%
  pull(sdo)
  # select(sdo)
sdo
# textbook finds 35.4%

## --------------------------------- ##
#### 1.2) Probability weighted ATE ####
## --------------------------------- ##

# Creating a variable for
# - child
# - strata
titanic = titanic %>%
  mutate(child = (age < 13)*1,
         strata = case_when(sex == 'female' & child == 1 ~ 1,
                            sex == 'female' & child == 0 ~ 2,
                            sex == 'male' & child == 1 ~ 3,
                            TRUE ~ 4))

# The probability weighted average
w_ate = titanic %>%
  select(survived, d, sex, child) %>%
  mutate(wt_denom = n()) %>%
  group_by(sex, child) %>%
  mutate(wt_num = n(),
         wt = wt_num/wt_denom) %>%
  group_by(d, sex, child) %>%
  summarise(y_s = mean(survived),
            wt = unique(wt),
            .groups = 'drop') %>%
  pivot_wider(id_cols = wt,
              names_from = d,
              values_from = y_s,
              names_prefix = 'y_') %>%
  mutate(diff = y_1 - y_0) %>%
  summarise(w_ate = weighted.mean(diff, wt)) %>%
  pull(w_ate)
w_ate

# textbook finds 18.9%

## -------------------------------- ##
#### 1.3) Curse of dimensionality ####
## -------------------------------- ##
# Cannot perform same analysis on gender-age strata
titanic %>%
  group_by(d, sex, age) %>%
  summarise(survival_prob = mean(survived),
            n_group = n()) %>%
  pivot_wider(names_from = d,
              values_from = c(survival_prob, n_group)) %>%
  data.frame

# cannot do exact matching :(

##=================================##
## ------------------------------- ##
####) 2) Coarsend exact matching ####
## ------------------------------- ##
##=================================##

# motivation for next lectures approximate matching
# Here we will create age bins
hist(titanic$age)
max(titanic$age)

titanic = titanic %>%
  mutate(age_bins = cut(age, breaks = c(0, 10, 20, 30, 40, 50, 60, 80)))

# Creating matched values
match_values = titanic %>%
  group_by(d, sex, age_bins) %>%
  summarise(avg_survived = mean(survived)) %>%
  pivot_wider(id_cols = c(age_bins, sex),
              names_from = d,
              names_prefix = 'y_',
              values_from = avg_survived)

# adding on buddy values
titanic = left_join(titanic, match_values)

# ATT
att = titanic %>%
  filter(d == 1) %>%
  summarise(att = mean(survived - y_0)) %>%
  pull()
att

# ATE
titanic %>%
  mutate(diff = case_when(d == 1 ~ survived - y_0,
                          TRUE ~ y_1 - survived)) %>%
  summarise(ate = mean(diff))
