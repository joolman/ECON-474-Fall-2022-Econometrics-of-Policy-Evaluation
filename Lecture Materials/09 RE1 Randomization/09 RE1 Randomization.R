##======================##
## -------------------- ##
#### 0) Preliminaries ####
## -------------------- ##
##======================##
library(tidyverse)
library(fixest)
# install.packages('haven')
library(haven)
# install.packages('cowplot')
library(cowplot)


df = read_csv('C:/Users/johnj/Dropbox/Julian/Academia/UIUC/Semester 11 Fall 2022/ECON474/Data/Thornton 2008 main sample.csv')

##======================##
## -------------------- ##
#### 1) Balance Table ####
## -------------------- ##
##======================##
#
# Balance tables compare covariates that may either be confounders or are 
#   important predictors of treatment or outcomes
#
# Ideally, you want zero differences between treatment and control groups.
# If there is a difference in a variable that closes a backdoor path,
#   then you should include that variable to close the backdoor.
#
# Variables of interest
vrbls = c('got', # outcome - got HIV test
          'any', # treatment - any incentive
          'tinc', # treatment doseage - random incentive amount
          'distvct', # distance to VCT center
          'simaverage', # Simulate average distance to VCT
          'tb', # taken HIV test before interview
          'male', # male
          'age', # age
          'rumphi', # located in the Rumphi district
          'balaka', # located in the Balaka district
          'mar', # married
          'educ2004', # "preperiod education
          'land2004', # preperiod owned land
          'hiv2004', # preperiod HIV result
          'thinktreat', # Think treatment for HIV will be available in the next 5 years
          'hadsex12', # had sex in the last year
          'usecondom04') # used a condom in the last year

df %>%
  select(vrbls) %>%
  na.omit()
nrow(df)
# It is a problem that there is incomplete infomration for the "full sample" because
#   then you don't know if changes in results are due to including the variables
#   or dropping observations

# Checking out NA counts
?lapply
lapply(df[, vrbls], function(x) sum(is.na(x)))
temp = lapply(df[, vrbls], function(x) sum(is.na(x))) %>% unlist
temp[temp > 0]

# Creating the balance table.
# We need one column for:
#   1) treatment group averages
#   2) control group averages
#   3) difference in averages
#   4) p-value for difference significance

bal_tab = df %>%
  select(vrbls) %>%
  group_by(any) %>%
  # Summarizing with means ACROSS all variables besides the group/treatment variable `any`
  summarise(across(vrbls[vrbls != 'any'], mean, na.rm = TRUE)) %>%
  # Transposing the data by first pivoting longer then wider
  pivot_longer(!any, names_to = 'variable', values_to = 'm') %>%
  pivot_wider(values_from = m,
              names_from = any,
              names_prefix = 'D_') %>%
  # Reording the columns
  select(variable, D_1, D_0) %>%
  # calculate differences
  mutate(diff = D_1 - D_0)

bal_tab$pvalue = NA
for(v in vrbls[vrbls != 'any']){
  bal_tab$pvalue[bal_tab$variable == v] = t.test(df[[v]] ~ df$any)$p.value
}
bal_tab



##==========================================================##
## -------------------------------------------------------- ##
#### 2) Table 4: Incentive of Learning HIV status effects ####
## -------------------------------------------------------- ##
##==========================================================##

tab4_1 = feols(got ~ any + tb + male + age + rumphi + balaka, data = df, cluster = 'villnum')
tab4_2 = feols(got ~ any + tinc +
                 tb + male + age + rumphi + balaka, data = df, cluster = 'villnum')
tab4_3 = feols(got ~ any + tinc + I(tinc^2) + 
                 tb + male + age + rumphi + balaka, data = df, cluster = 'villnum')
tab4_4 = feols(got ~ any + tinc + I(tinc^2) + 
                 tb + distvct + I(distvct^2) + 
                 male + age + rumphi + balaka, data = df, cluster = 'villnum')
tab4_5 = feols(got ~ any + tinc + I(tinc^2) + 
                 tb + simaverage + over + 
                 male + age + rumphi + balaka, data = df, cluster = 'villnum')

etable(tab4_1, tab4_2, tab4_3, tab4_4, tab4_5)
dict = c(got = 'Got HIV Incentive',
         any = 'Any Incentive',
         villnum = 'Village')

etable(tab4_1, tab4_2, tab4_3, tab4_4, tab4_5,
       dict = dict,
       fitstat = c('n', 'r2'),
       se.below = TRUE,
       poly_dict = c('', '^2'))

##=================##
## --------------- ##
#### 3) Figure 3 ####
## --------------- ##
##=================##

# Figure 3 is plotting coefficient estimates from regressions
# So, let's run some regressions

## ---------------- ##
#### 3.1) Panel A ####
## ---------------- ##

# Create a factor variable `incentive` with figure labels to make plotting easier
# I am doing a factor because the default ording in ggplot2 of characters is
#   to put them in alphabetical order, which does not match the plot
# Factors force your specified order
df = df %>%
  mutate(incentive = ifelse(any == 0, 'No Incentive', 'Incentive'))
df$incentive = factor(df$incentive, levels = c('No Incentive', 'Incentive'))

# Regressions: note -1 removes the intercept
fit3_a = lm(got ~ incentive - 1, data = df)
summary(fit3_a)
coef(fit3_a)
confint(fit3_a)

df_fig3_a = data.frame(beta = coef(fit3_a),
                       group = factor(c('No Incentive', 'Incentive'),
                                      levels = c('No Incentive', 'Incentive'))) %>%
  # adding on the confidence interval data
  bind_cols(confint(fit3_a))

df_fig3_a

ggplot(df_fig3_a, aes(x = group, y = beta)) +
  geom_bar(stat = 'identity', # because we only have 1 value per group
           width = 0.4) +
  geom_errorbar(aes(ymin = `2.5 %`, ymax = `97.5 %`),
                width = 0.1) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1))

## ---------------- ##
#### 3.2) Panel B ####
## ---------------- ##

# creating bins of the continuous variable `tinc`
df$inc_group = df$tinc %>% cut(breaks = c(-1, 0, 0.1, 0.2, 0.3, 0.5, 1, 1.5, 2, 2.5, 3))
levels(df$inc_group)
levels(df$inc_group)[1] = '[0]'

# The missing -1 from the recording
fit3_b = lm(got ~ inc_group - 1, data = df)
summary(fit3_b)


df_fig3_b = data.frame(beta = coef(fit3_b),
                       group = factor(levels(df$inc_group),
                                      levels = levels(df$inc_group))) %>%
  # adding on the confidence interval data
  bind_cols(confint(fit3_b))

df_fig3_b

ggplot(df_fig3_b, aes(x = group, y = beta)) +
  geom_bar(stat = 'identity', # because we only have 1 value per group
           width = 0.4) +
  geom_errorbar(aes(ymin = `2.5 %`, ymax = `97.5 %`),
                width = 0.1) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1))

