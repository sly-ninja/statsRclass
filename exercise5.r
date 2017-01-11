rm(list=ls())

library(dplyr)
library(gapminder)
data(gapminder)
head(gapminder)

x = unlist(gapminder %>% filter(year == 1952) %>% select(lifeExp))
hist(x)
# numeric_x = x[1]
# numeric_y = x[2]
# z = data.frame(x)

# filter(x, order =='country') %>% select(lifeExp)

# hist(x, xlab='country', ylab='life expectancy')

# plot(lifeExp ~ year, gapminder, subset = country == "Cambodia", type = "b")

# filter(mydata, order=='Primates') %>% select(sleep_total) %>% unlist

# tab <- data.frame(value=c(1, 2, 3, 4, 5), freq=c(2, 1, 4, 2, 1))
# vec <- rep(tab$value, tab$freq)


prop = function(q) {
  mean(x <= q)
}

prop(40)

qs = seq(from=min(x), to=max(x), length=20)
props = sapply(qs, prop)
plot(qs, props)
props = sapply(qs, function(q) mean(x <= q))
plot(ecdf(x))
