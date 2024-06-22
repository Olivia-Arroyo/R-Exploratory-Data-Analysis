
library(tidyverse)
library(moderndive)
library(skimr)
library(gapminder)

#Exploratory Data Analysis for life expectancy and continent

gapminder2007 <- gapminder %>%
  filter(year == 2007) %>%
  select(country, lifeExp, continent, gdpPercap)


glimpse(gapminder2007)

gapminder2007 %>% 
  sample_n(size = 5)

gapminder2007 %>%
  select(lifeExp, continent) %>%
  skim()

ggplot(data = gapminder2007, aes(x = lifeExp)) +
  geom_histogram(binwidth = 5, color = "white") + 
  labs(x = "Life expectancy", y = "Number of countries",
       title = "Histogram of distribution of worldwide life expectancies") +
  facet_wrap(~continent, nrow = 2)

ggplot(data= gapminder2007, aes(x= continent, y= lifeExp)) +
  geom_boxplot() +
  labs(x = "Continent", y = "Life expectancy", 
       title = "Life expectancy by continent")

lifeExp_by_continent <- gapminder2007 %>%
  group_by(continent) %>%
  summarize(median = median(lifeExp),
            mean = mean(lifeExp))

lifeExp_model <- lm(lifeExp ~ continent, data= gapminder2007)
get_regression_table(lifeExp_model)

regression_points <- get_regression_points(lifeExp_model, ID = "country")
regression_points

regression_points %>%
  arrange(residual)

regression_points %>%
  arrange(desc(residual))

# Exploratory Data Analysis for GDP per capita and continent

gapminder2007 %>%
  select(gdpPercap, continent) %>%
  skim()

ggplot(data= gapminder2007, aes(x = continent, y = gdpPercap)) +
  geom_boxplot() + 
  labs(x = "Continent", y = "GDP per capita",
       title = "GDP per capita by continent") 

gdpPercap_model <- lm(gdpPercap ~ continent, data= gapminder2007)
get_regression_table(gdpPercap_model)

         