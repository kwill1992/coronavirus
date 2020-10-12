# Coronavirus
# Test modeling to show exponential growth with doubling time in 3, 6, or 12 days

#library
library(tidyverse)
library(lubridate)
library(gganimate)
library(scales)



# make dataframe and put data in it
# get 100 days
startDate <- as.Date(today())
Date <- seq(startDate, by="1 day", length.out = 100)
days <- seq(1:100)
Title <- "Double in 3 Days"
cv3 <- data.frame(days,Date,Title)
Title <- "Double in 6 Days"
cv6 <- data.frame(days,Date,Title)
Title <- "Double in 12 Days"
cv12 <- data.frame(days,Date,Title)

# cases if doubling time = 3 days
t3 <- 3
cv3$cases <- sapply(days, function(x) 2000*(2 ^ (x/t3)))
# cases if doubling time = 6 days
t6 <- 6
cv6$cases <- sapply(days, function(x) 2000*(2 ^ (x/t6)))
# cases if doubling time = 12 days
t12 <- 12
cv12$cases <- sapply(days, function(x) 2000*(2 ^ (x/t12)))

cv <- bind_rows(cv3,cv6,cv12)




p <- ggplot(cv,aes(x = Date, y = cases, color = Title)) +
    geom_line() +
    geom_point() +
    scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                   scientific = FALSE)) +
    transition_time(Date) +
    ease_aes('linear') +
    transition_reveal(Date) +
    view_follow()


anim <- animate(p,nframes = 100, renderer = gifski_renderer("cv5.gif"))



anim_save("test.gif",anim)

library(gapminder)

ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

library(gapminder)
library(gifski)
#gp <- gapminder
p <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent)) +
  geom_point() +
  scale_x_log10() +
  transition_states(year, 1, 5)
animate(p, nframes = 24, renderer = gifski_renderer("gganim.gif"))
animate(p)

> startDate <- as.Date("2012-01-01")
> xm <- seq(startDate, by="2 months", length.out=6)
> xm
[1] "2012-01-01" "2012-03-01" "2012-05-01" "2012-07-01"
[5] "2012-09-01" "2012-11-01"
