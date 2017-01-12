rm(list=ls())

library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- na.omit( read.csv(filename) )

x = unlist(filter(dat, Sex == 'M', Diet =='chow') %>% select(Bodyweight))
a = mean(x)
b = popsd(x)
c = sd(x)

mean(x < a+c) - mean(x < a-c)
mean(x < a+2*c) - mean(x < a-2*c)
mean(x < a+3*c) - mean(x < a-3*c)


# The average of this sample is our random variable. 
# We will use the replicate to observe 10,000 realizations of this random variable. 
# Set the seed at 1, generate these 10,000 averages. 
# Make a histogram and qq-plot of these 10,000 numbers against the normal distribution. 

y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
avgs <- replicate(10000, mean( sample(y, 25)))
# mypar gets around the suboptimal defaults of par
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)
