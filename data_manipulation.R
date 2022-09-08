library(tidyverse)
library(gapminder)


names(cars)  

ggplot(data = cars) + 
  aes(x = speed) + 
  aes(y = dist) + 
  geom_point() + 
  aes(color = speed)
  
names(pressure)

# This is my pressure

ggplot(data = pressure) +
  aes(x = temperature)

names(gapminder)

gapminder %>% 
  filter(continent == "Europe") %>% 
  filter(year == 2002) %>% 
  mutate(gdp_billions = pop*gdpPercap/1000000000) %>% 
  select(-lifeExp) %>% 
  arrange(-gdp_billions) ->
gapminder_europe_2002

q <- "yes"

p = 7
q = 9

p * q

data("gapminder")

