df = read_csv('C:/Users/johnj/Dropbox/Julian/Academia/UIUC/Semester 11 Fall 2022/ECON474/Data/RDD1 ACS5 2019 median income house value.csv')

df = df %>%
  rename(med_house_value = SE_A10036_001,
         med_hh_income = SE_A14006_001)

fig1 = ggplot(df, aes(x = log(med_house_value), y = log(med_hh_income))) +
  geom_point() +
  theme(text = element_text(size = 20)) +
  labs(x = 'log Median House Value ($)',
       y = 'log Median Household Income ($)',
       title = 'Tract-level Median Household Income versus Median Household Value',
       subtitle = 'Source: ACS5 2019')
fig1



ggplot(df, aes(x = log(med_house_value), y = log(med_hh_income))) +
  stat_summary_bin(fun = mean, bins = 20,
                   size = 1.5) +
  theme(text = element_text(size = 20)) +
  labs(x = 'log Median House Value ($)',
       y = 'log Median Household Income ($)',
       title = 'Tract-level Median Household Income versus Median Household Value',
       subtitle = 'Source: ACS5 2019') +
  ylim(layer_scales(fig1)$y$range$range)


ggplot(df, aes(x = log(med_house_value), y = log(med_hh_income))) +
  geom_bin_2d() +
  stat_summary_bin(fun = mean, bins = 20,
                   color = 'orange',
                   size = 1.5) +
  scale_fill_continuous(type = "viridis") +
  theme(text = element_text(size = 20)) +
  labs(x = 'log Median House Value ($)',
       y = 'log Median Household Income ($)',
       title = 'Tract-level Median Household Income versus Median Household Value',
       subtitle = 'Source: ACS5 2019')
