rm(list=ls())
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

n = 1000
averages = vector('numeric', n)
set.seed(1)
for(i in 1:n){
  averages[i] = abs(mean( sample(x, 50) ) - mean(x))
}
mean ( averages > 1)

