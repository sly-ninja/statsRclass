library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv"
if(!file.exists("femaleMiceWeights.csv")) download(url,destfile=filename)
dat <- read.csv(filename)

control <- filter(dat,Diet=="chow") %>% select(Bodyweight) %>% unlist
treatment <- filter(dat,Diet=="hf") %>% select(Bodyweight) %>% unlist

diff <- mean(treatment) - mean(control)
print(diff)


# standard error
sd(control)/sqrt(length(control))

# standard error of the diff
se <- sqrt( 
  var(treatment)/length(treatment) + 
    var(control)/length(control) 
)

# t-statistic 
# CLT states that it is approximately normal
# with mean of 0 and standard deviation of 1
tstat <- diff/se 

# calculate p-value
# how often does a normally distributed random variable exceed diff?
# pnorm(a) - probability that a random variable following the standard normal distribution falls below a
# probability that it is larger than a, we simply use 1-pnorm(a)
righttail <- 1 - pnorm(abs(tstat)) 
lefttail <- pnorm(-abs(tstat))
pval <- lefttail + righttail
print(pval)

# t-distribution
library(rafalib)
mypar(1,2)

qqnorm(treatment)
qqline(treatment,col=2)

qqnorm(control)
qqline(control,col=2)

t.test(treatment, control)

# to see just the p-value
result <- t.test(treatment,control)
result$p.value


# die question
set.seed(1)
# .0424
n <- 100 
sides <- 6
p <- 1/sides

#.3956
n <- 5
p <- .5

# .9811
n <- 30
p <- .5

# .9723
n <- 30
p <- .01

# 1
n <- 100
p <- .01

y <- replicate(10000, {
  x = sample(1:sides, n, replace=TRUE)
  (mean(x==6) - p) / sqrt(p*(1-p)/n)
})

qqnorm(zs)
abline(0,1)#confirm it's well approximated with normal distribution
mean(abs(y) > 2)

# calculate sample average
X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
Y <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist

# the probability that our estimate is off by more than 2 grams from population mean
# given distribution is sqrt(12) * ( (sample mean - population mean) / standard deviation )
2 * ( 1-pnorm(2/sd(X) * sqrt(12) ) )

# null hypothesis example
varT = var(treatment)
varC = var(control)

SE = sqrt( varT/12 + varC/12 )

# t-statistic
tStat = ( mean(Y) - mean(X) ) / SE
result = t.test(treatment, control)
