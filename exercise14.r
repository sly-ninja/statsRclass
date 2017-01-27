
### examples from book ###

# generate a null distribution by shuffling the data 1,000 times
# shuffle the cases and control labels and
# assume that the ensuing distribution approximates the null distribution
# null distributions created with permutations have longer tails
# than actual null distribution
# permutations result in conservative p-values

# samples assumed to be independent and 'exchangable':
# If there is hidden structure in your data, then permutation tests can result
# in estimated null distributions that underestimate the size of tails because 
# the permutations may destroy the existing structure in the original data.

dat=read.csv("femaleMiceWeights.csv")
library(dplyr)
control <- filter(dat,Diet=="chow") %>% select(Bodyweight) %>% unlist
treatment <- filter(dat,Diet=="hf") %>% select(Bodyweight) %>% unlist
obsdiff <- mean(treatment)-mean(control)

N <- 12
avgdiff <- replicate(1000, {
  all <- sample(c(control,treatment))
  newcontrols <- all[1:N]
  newtreatments <- all[(N+1):(2*N)]
  return(mean(newtreatments) - mean(newcontrols))
})
hist(avgdiff)
abline(v=obsdiff, col="red", lwd=2)

# the proportion of permutations with larger difference
(sum(abs(avgdiff) > abs(obsdiff)) + 1) / (length(avgdiff) + 1)


## same as above, but smaller data set ##
# when only have a few samples, can't do permutations
N <- 5
control <- sample(control,N)
treatment <- sample(treatment,N)
obsdiff <- mean(treatment)- mean(control)

avgdiff <- replicate(1000, {
  all <- sample(c(control,treatment))
  newcontrols <- all[1:N]
  newtreatments <- all[(N+1):(2*N)]
  return(mean(newtreatments) - mean(newcontrols))
})
hist(avgdiff)
abline(v=obsdiff, col="red", lwd=2)



### exercise 1 ###
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

N=10
set.seed(1)
nonsmokers <- sample(bwt.nonsmoke , N)
smokers <- sample(bwt.smoke , N)
obsdiff <- mean(smokers) - mean(nonsmokers)

avgdiff = replicate(1000, {
  all = sample(c(smokers,nonsmokers))
  nonsmokersstar = all[1:N]
  smokersstar = all[(N+1):(2*N)]
  return(mean(smokersstar) - mean(nonsmokersstar))
})

pvalue = (sum(abs(avgdiff) > abs(obsdiff)) + 1) / (length(avgdiff) + 1)


### exercise 2 ###
N=10
set.seed(1)
nonsmokers <- sample(bwt.nonsmoke , N)
smokers <- sample(bwt.smoke , N)
obsdiff <- median(smokers) - median(nonsmokers)

avgdiff = replicate(1000, {
  all = sample(c(smokers,nonsmokers))
  nonsmokersstar = all[1:N]
  smokersstar = all[(N+1):(2*N)]
  return(median(smokersstar) - median(nonsmokersstar))
})

pvalue = (sum(abs(avgdiff) > abs(obsdiff)) + 1) / (length(avgdiff) + 1)
