library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv"
download.file(url, destfile=filename)
mydata = read.csv(filename)

mydata[12, "Bodyweight"]
mydata$Bodyweight[11]
length(mydata$Bodyweight)
fatdata <- mydata$Bodyweight[seq(13,24)]
mean(fatdata)

set.seed(1)
fatNums <- seq(13,24)
randomNum <- sample(fatNums, 1)
mydata$Bodyweight(randomNum)