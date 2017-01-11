rm(list=ls())

library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download.file(url, destfile=filename)
x = unlist( read.csv(filename) )

set.seed(1)
averages = vector('numeric', 1000)
averages_two = vector('numeric', 1000)

for(i in 1:1000){
  # averages[i] = abs(mean( sample(x, 5) ) - mean(x))
  # averages_two[i] = abs(mean( sample(x, 50) ) - mean(x))
  averages[i] = mean(sample(x, 5))
  averages_two[i] = mean(sample(x, 50))
}

hist(averages)
hist(averages_two)

mean(averages)
mean(averages_two)

mean(averages_two < 23)
mean(averages_two > 25)
mean( averages50 < 25 & averages50 > 23)

pnorm(25,23.9,.43)
