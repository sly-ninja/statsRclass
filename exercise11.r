url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)

bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

set.seed(1)
dat.ns = sample(bwt.nonsmoke, 25)
dat.s = sample(bwt.smoke, 25)

# to get 99% confidence interval

Q = qt(.995, df=2*N-2)
Q*sqrt( sd( dat.ns)^2/N + sd( dat.s)^2/N )

qt(0.995, df=2*N-2)*sqrt( sd( dat.ns)^2/N + sd( dat.s)^2/N )


set.seed(1)
dat.ns = sample(bwt.nonsmoke, 5)
dat.s = sample(bwt.smoke, 5)
result = t.test(dat.ns, dat.s)
