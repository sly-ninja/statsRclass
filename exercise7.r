library(downloader) 
library(dplyr)
library(rafalib)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- read.csv(filename) 

# remove lines that contain missing values
dat <- na.omit( dat )

x = unlist(filter(dat, Sex == 'M', Diet =='chow') %>% select(Bodyweight))
a = mean(x)

# rafalib to compute standard deviation
popsd(x)

set.seed(1)
b = mean(sample(x, 25))

y = unlist(filter(dat, Sex == 'M', Diet =='hf') %>% select(Bodyweight))
c = mean(y)
popsd(y)

set.seed(1)
d = mean(sample(y, 25))


# same exercise but with females
i = unlist(filter(dat, Sex == 'F', Diet =='chow') %>% select(Bodyweight))
q = mean(i)
set.seed(1)
r = mean(sample(i, 25))

j = unlist(filter(dat, Sex == 'F', Diet =='hf') %>% select(Bodyweight))
s = mean(j)
set.seed(1)
t = mean(sample(j, 25))

abs(abs(q-s) - abs(r-t))
