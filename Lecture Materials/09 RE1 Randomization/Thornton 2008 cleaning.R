## Cleaning the replication data for Thornton (2008)
#
# Data are from: https://www.aeaweb.org/articles?id=10.1257/aer.98.5.1829
#
# This script largely follows the provided Stata replication code

##===================##
## ----------------- ##
#### Preliminaries ####
## ----------------- ##
##===================##

library(tidyverse)
library(haven) # read_dta() - Stata's specific data format is .dta

# loading in the data with haven
df = read_dta('Data/Thorton 2008 replication files/Thornton-HIV-Testing-Data.dta')
df

# This dataset comes with variable descriptions. Let's print them off:
#
# list apply the function that grabs the descriptions (label) from the data
lapply(df, function(x) attributes(x)$label)

# The variable `tinc` (amount of incentive) is adjusted to local currency:
# Exchange rate as of jan1 2005
df$tinc = df$tinc*0.009456

##==============##
## ------------ ##
#### Cleaning ####
## ------------ ##
##==============##
# Due to the design of the study having multiple early interviews, there is a main
# sample and an additional sample. We are only going to work with the main sample.
# That requires us to extract it.
#
# Firstly, we are going to create a few variables to help us split the samples
summary(select(df, m1out, m2out, survey2004))
attributes(df$m1out)$label
attributes(df$m2out)$label
attributes(df$survey2004)$label

df = df %>%
  mutate(sample_1998 = ifelse(is.na(m1out),
                              0,
                              (m1out == 2)*1),
         sample_2001 = ifelse(is.na(m2out), ### m2
                              0,
                              (m2out == 2)*1), ### m2
         sample_2004 = survey2004)

# The variables we will be using in the analysis:
df %>%
  select(test2004, age, villnum, tinc, distvct, hiv2004, followup_tested) %>%
  summary

# Some of the variables that are used to create the main sample are missing.
# For the ease of cleaning, we are going to 
#   create new variables 
#   from are the variables with missing values 
#   such that missing values are coded as 0 or -1
# to identify the main sample

df = df %>%
  mutate(test2004a = ifelse(is.na(test2004),
                            0,
                            test2004),
         hiv2004 = ifelse(is.na(hiv2004),
                          -1,
                          hiv2004),
         followup_tested = ifelse(is.na(followup_tested),
                                  -1,
                                  followup_tested),
         sample_main = (test2004a == 1 &
                          !is.na(age) & 
                          !is.na(villnum) &
                          !is.na(tinc) & 
                          !is.na(distvct) &
                          hiv2004 != -1 & 
                          followup_tested != 1)*1)

df_main = df %>% filter(sample_main == 1)

write_csv(df_main, 'Data/Thornton 2008 main sample.csv')