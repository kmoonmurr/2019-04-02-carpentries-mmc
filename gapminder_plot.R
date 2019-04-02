
library("dplyr")
library("tidyverse")

#READ IN THE DATA

gapminder <- readr::read_csv(here("data/gapminder/raw/gapminder_data.csv"))

mean(gapminder$gdpPercap [gapminder$continent=="Africa"])


year_country_gdp <-  gapminder %>% 
  select(gapminder, year, country, gdpPercap)

#Look at column names
colnames(gapminder)


dat1 <-  gapminder %>% 
  filter(continent == "Africa") %>% 
  select (lifeExp, country, year)

library(ggplot2)

#Make a plot of life expectancy for Africa
gapminder %>% 
  filter(continent == "Africa") %>% 
  ggplot(data = gapminder, aes(x = year, y = lifeExp, color = continent)) +
  geom_line() +
  facet_wrap( ~ country)

skimr::skim(gapminder)
str(gapminder)


# Create gap_wide
gap_wide <- gapminder %>%
  gather(key = 'key', value = 'value', c('pop', 'lifeExp', 'gdpPercap')) %>%
  mutate(year_var = paste(key, year, sep = '_')) %>%
  select(country, continent, year_var, value) %>%
  spread(key = 'year_var', value = 'value')



gap_long <- gap_wide %>%
  gather(obstype_year, obs_values, starts_with('pop'),
         starts_with('lifeExp'), starts_with('gdpPercap'))

gap_long <- gap_long %>% separate(obstype_year,into=c('obs_type','year'),sep="_") %>% 
  mutate(year=as.integer(year))

gap_normal <- gap_long %>% spread(obs_type,obs_values)
dim(gap_normal)



gap_temp <- gap_long %>% unite(var_ID,continent,country,sep="_")
str(gap_temp)



gap_temp <- gap_long %>%
  unite(ID_var,continent,country,sep="_", remove="FALSE") %>%
  unite(var_names,obs_type,year,sep="_", remove="FALSE")
str(gap_temp)



gap_wide_betterID <- separate(gap_wide_new,ID_var,c("continent","country"),sep="_", remove=FALSE)


gap_ludicrously_wide <- gap_long %>%
  unite(var_names,obs_type,year,country,sep="_") %>%
  spread(var_names,obs_values)
