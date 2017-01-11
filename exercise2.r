library(downloader)
url="https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- basename(url)
download.file(url,filename)

library(dplyr)

mydata <- read.csv(filename)

filterPrimates = filter(mydata, order=='Primates') %>% select(sleep_total) %>% unlist
nrow(filterPrimates)
mean(filterPrimates)


filterPrimates = filter(mydata, order=='Primates') %>% summarize(mean(sleep_total))
