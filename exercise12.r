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

N = 5

set.seed(1)
dat.ns = sample(bwt.nonsmoke, N)
dat.s = sample(bwt.smoke, N)


reject = function(N, alpha=0.01) {
  dat.s = sample(bwt.smoke, N)
  dat.ns = sample(bwt.nonsmoke, N)
  pval = t.test(dat.s, dat.ns)$p.value
  pval < alpha
}

# power: probability of rejecting the hypothesis
# when the alternative is true.
B = 10000
rejections = replicate(B, reject(N))
mean(rejections)

# run simluation with various values of N
Ns <- seq(30, 120, 30)
power = sapply(Ns, function(N){
  rejections = replicate(B, reject(N))
  mean(rejections)
})
Ns[ which.min( abs( power - .8) ) ] 
