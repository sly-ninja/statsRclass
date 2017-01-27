ttestgenerator = function(x) {
  cases = sample(controlPopulation, n)
  controls = sample(controlPopulation, n)
  tstat= (mean(cases) - mean(controls)) / sqrt(var(cases)/n + var(controls)/n))
  return(tstat)
}

### monte carlo simulation
ttests = replicate(1000, ttestgenerator(10))

hist(ttests)
qnorm(ttests)

Ps = (seq(0, 999) +0.5)/1000
qqplot(qt(Ps, df=2*3-2), ttests, xlim=c(-6, 6), ylim=c(-6, 6))
abline(0, 1)
qnorm(controlPopulation)


# generate data from simulation
# parametric monte carlo simulation
# creating population of 5000
# for rnorm, instead of sampling, 
# generating normally distribution random variables
controls = rnorm(5000, mean=24, sd=3.5)

ttestgenerator = function(n, mean=24, sd=3.5){
  cases = rnorm(n, mean, sd)
  controls = rnorm(n, mean, sd)
  tstat= (mean(cases) - mean(controls)) / sqrt(var(cases)/n + var(controls)/n)
  return(tstat)
}

ttests = replicate(1000, ttestgenerator(10))
qnorm(ttests)
abline(0, 1)


### question 2 ###
set.seed(1)
example = rnorm(5, 0, 1)
tstat = sqrt(5)*mean(example)/sd(example)

# determine if tstats follow t-dist with N-1 df
set.seed(1)

ttestgenerator = function(n, mean=0, sd=1){
  example = rnorm(5, 0, 1)
  tstat = sqrt(5)*mean(example)/sd(example)
  return(tstat)
}

ttests = replicate(1000, ttestgenerator(5))
mean(ttests > 2)


### question 3 ###
ttests = replicate(1000, ttestgenerator(5))
B=100
ps = seq(1/(B+1), 1-1/(B+1),len=B)
qqplot(qt(ps,df=4), ttests)
abline(0, 1)


# plotting for different sample sizes
library(rafalib)
mypar(3,2)

Ns<-seq(5,30,5)
B <- 1000
mypar(3,2)
LIM <- c(-4.5,4.5)
for(N in Ns){
  ts <- replicate(B, {
    X <- rnorm(N)
    sqrt(N)*mean(X)/sd(X)
  })
  ps <- seq(1/(B+1),1-1/(B+1),len=B)
  qqplot(qt(ps,df=N-1),ts,main=N,
         xlab="Theoretical",ylab="Observed",
         xlim=LIM, ylim=LIM)
  abline(0,1)
} 


### question 4 ###

Ns<-seq(5,30,5)
B <- 1000
mypar(3,2)
LIM <- c(-4.5,4.5)
for(N in Ns){
  ts <- replicate(B,{
    x <- rnorm(N)
    y <- rnorm(N)
    t.test(x,y, var.equal = TRUE)$stat
  })
  ps <- seq(1/(B+1),1-1/(B+1),len=B)
  qqplot(qt(ps,df=2*N-2),ts,main=N,
         xlab="Theoretical",ylab="Observed",
         xlim=LIM, ylim=LIM)
  abline(0,1)
}  

### question 5 ###
set.seed(1)
X =sample(c(-1,1), 15, replace=TRUE)
tstat <- sqrt(15)*mean(X) / sd(X)
ps=seq(1/(B+1), 1-1/(B+1), len=B) 
qqplot(qt(ps,df=14), X)

## question 5 solution
set.seed(1)
N <- 15
B <- 10000
tstats <- replicate(B,{
  X <- sample(c(-1,1), N, replace=TRUE)
  sqrt(N)*mean(X)/sd(X)
})
ps=seq(1/(B+1), 1-1/(B+1), len=B) 
qqplot(qt(ps,N-1), tstats, xlim=range(tstats))
abline(0,1)
# The population data is not normal thus the theory does not apply.
# We check with a Monte Carlo simulation. The qqplot shows a large tail. 
# Note that there is a small but positive chance that all the X are the same.
# In this case the denominator is 0 and the t-statistics is not defined


### question 6 ###
set.seed(1)
N <- 1000
B <- 10000
tstats <- replicate(B,{
  X= sample(c(-1,1), N, replace=TRUE)
  sqrt(N)*mean(X)/sd(X)
})
ps=seq(1/(B+1), 1-1/(B+1), len=B) 
qqplot(qt(ps,999), tstats, xlim=range(tstats))
abline(0,1)


### question 7 ###
set.seed(1)

medians = function(n, mean=0, sd=1){
  median(rnorm(5, 0, 1))
  median.result <- median(example)
}

## question 7 - solution
set.seed(1)
Ns <- seq(5,45,5)
library(rafalib)
mypar(3,3)
for(N in Ns){
  medians <- replicate(10000, median ( rnorm(N) ) )
  title <- paste("N=",N,", avg=",round( mean(medians), 2) , ", sd*sqrt(N)=", round( sd(medians)*sqrt(N),2) )
  qqnorm(medians, main = title )
  qqline(medians)
}
##there is an asymptotic result that says SD is sqrt(N*4*dnorm(0)^2)
