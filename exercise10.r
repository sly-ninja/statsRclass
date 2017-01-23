library(downloader)
library(dplyr)
library(rafalib)

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download.file(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)

bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)


set.seed(1)
dat.ns = sample(bwt.nonsmoke, 25)
dat.s = sample(bwt.smoke, 25)

SE = sqrt( sd(dat.ns)^2/25 + sd(dat.s)^2/25 )
tval = ( mean(dat.ns) - mean(dat.s) ) / SE

result = t.test(dat.ns, dat.s)

2 * ( 1-pnorm(tval) )
2*pnorm(-abs(tval))

# SE to get confidence interval of 99%
N <- 25
qnorm(0.995)*sqrt( sd( dat.ns)^2/N + sd( dat.s)^2/N )
