library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x = ( read.csv(filename) ) 
y = unlist(x)
mean(y)

set.seed(1)
z = mean(sample(y, 5))
abs(mean(y) - z)

set.seed(5)
z = mean(sample(y, 5))
abs(mean(y) - z)
