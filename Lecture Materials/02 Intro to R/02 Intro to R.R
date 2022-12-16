"Hello, world!"
'Hello, world!'
'"Hello, world!" exclaimed R.' # Bring R to life

##========================##
## ---------------------- ##
#### Classes of objects ####
## ---------------------- ##
##========================##

class('Hello') # in R, strings are called characters
class(1) # in R, floats are numerics
class(1L) # in R integers are integers
class('1')

# What happens if?
'1' * 2
1L*2
class(1L*2)

class(Inf)
Inf/Inf
class(NaN)

# Boolean and logic statements
1 == 1
class(TRUE) # in R, booleans are called logical
1 == 2
1 != 2 # ! negates

TRUE == 1
FALSE == 0
TRUE * 2
FALSE * 1e4

class(NA)

1 < 3
1 <= 3
1 %in% c(1, 2, 3)
!1 %in% c(1, 2, 3)

# NULL objects
class(NULL)


# Section 1 ----
# Section 2 ====
# Section 3 ####


##================================##
## ------------------------------ ##
#### Different Types of Objects ####
## ------------------------------ ##
##================================##
a = 7
b = 8
name4_1WeirD0.Example = a + b

the <- same <- value <- 474

econ = 'is awesome'

ls()
rm(name4_1WeirD0.Example)
ls()

# concatenation
multi = c(4, 7, 4)
multi + b
c(multi, b)
c('econ', 474)


#### Matices
# Let's create a 2x2 matrix with rows 1, 2, and 3, 4
matrix(1, 2,
       3, 4)
matrix(c(1, 2, 3, 4))
?matrix
matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
m = matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE)

# data.frame
data.frame(m)
df = data.frame(m)
df$X1
# Each column is a variable

# Indexing - i can row and j cannot
  # ith row and jth column
# 1st column, second row
df[2, 1]
df[2, ]
df[2, c('X1', 'X2')]
df$X2[1]

ex_df = data.frame(states = seq(1, 50),
                   year = 2020)
ex_df
head(ex_df)
head(ex_df, 1)
tail(ex_df)

##===========##
## --------- ##
#### Loops ####
## --------- ##
##===========##

for(i in c(1, 3, 6:10)){
  print(i)
}

20/3     # Regular division
20 %/% 3 # Integer division
20 %% 3  # moudulus (remainder)
?"+"

# Print only odd numbers
for(i in c(1, 3, 6:10)){
  if(i%%2 != 0){
    print(paste(i, 'is odd'))
  }
}

##=====================##
## ------------------- ##
#### Loading in Data ####
## ------------------- ##
##=====================##


gm = read.csv('C:\\Users\\johnj\\Dropbox\\Julian\\Academia\\UIUC\\Semester 11 Fall 2022\\ECON474\\Data\\gapminder_2007.csv')

list.files('C:\\Users\\johnj\\Dropbox\\Julian\\Academia\\UIUC\\Semester 11 Fall 2022\\ECON474\\Data')
list.files()
getwd()
setwd('C:\\Users\\johnj\\Dropbox\\Julian\\Academia\\UIUC\\Semester 11 Fall 2022\\ECON474\\Data')
list.files()
gm = read.csv('gapminder_2007.csv')

head(gm); tail(gm)
names(gm) # names of columns

unique(gm$continent)
class(gm$continent)
summary(gm)

gm[gm$continent == 'Oceania', ]
summary(gm[gm$continent == 'Oceania', ])

# A tangent on functions
awaken_R = function(){
  print("Hello, world!")
}
awaken_R()
double = function(anything_you_want_to_call_it){
  return(anything_you_want_to_call_it*2)
}
double(474)

# Plotting
mean(gm$gdpPercap)
median(gm$gdpPercap)
hist(gm$gdpPercap)
hist(log(gm$gdpPercap))

plot(lifeExp ~ gdpPercap, data = gm)
plot(lifeExp ~ gdpPercap, data = gm,
     xlab = 'GDP per Capita', ylab = 'Life Expectancy',
     main = 'GDPc vs. Life Expectancy 2007')
plot(lifeExp ~ gdpPercap, data = gm,
     xlab = 'GDP per Capita', ylab = 'Life Expectancy',
     main = 'GDPc vs. Life Expectancy 2007',
     pch = 16)

##==============##
## ------------ ##
#### Packages ####
## ------------ ##
##==============##
library(ggplot2)

ggplot(data = gm, aes(x = gdpPercap, y = lifeExp, 
                      color = continent, size = pop)) +
  geom_point(alpha = 0.5) +
  scale_size_continuous(guide = 'none') +
  labs(x = 'GDP per Capita', y = 'Life Expectancy',
       title = 'GDPc vs. Life Expectancy 2007') +
  # theme(text = element_text(size = 20)) + # comment out with Ctrl-Shift-C
  geom_text(aes(label = ifelse(pop >= quantile(pop, 0.90),
                               country, # TRUE
                               '')),
            size = 3, hjust = 0, vjust = 0, nudge_x = 0.1)
library(dplyr)

gm %>%
  mutate(econ = 474) %>%
  filter(continent == 'Africa') %>%
  select(country, year, gdpPercap, lifeExp)

gm %>%
  group_by(continent) %>%
  mutate(m_gdpc = mean(gdpPercap))
gm %>%
  group_by(continent) %>%
  summarize(m_gdpc = mean(gdpPercap))
