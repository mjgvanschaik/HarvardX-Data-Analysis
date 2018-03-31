# Exercises 1
babies <- read.table("data/inst/extdata/babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist
library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

set.seed(1)
n <- 25
dat.ns <- sample(bwt.nonsmoke, n)
dat.s <- sample(bwt.smoke, n)
diff <- mean(dat.ns)-mean(dat.s)
SED <- sqrt(var(dat.ns)/n + var(dat.s)/n)
(tval <- diff/SED)
2*(pnorm(-abs(tval)))
-qnorm(0.01/2)*SED

# exercises 2
# CLT not applying so use the t distr. for critical value:
SED <- sqrt(var(dat.ns)/n + var(dat.s)/n) # like before
-qt(0.01/2, df=2*n-2)*SED

set.seed(1)
n <- 5
dat.ns <- sample(bwt.nonsmoke, n)
dat.s <- sample(bwt.smoke, n)
test <- t.test(dat.ns, dat.s)
test$p.value

pwr <- vector("numeric", 40)
for(n in 10:50){
  pvals <- replicate(1000, t.test(sample(bwt.nonsmoke, n), 
                                   sample(bwt.smoke, n))$p.value)
  pwr[n] <- sum(pvals<=0.05)/10000
}

rep <- 10000
n <- 90
set.seed(1)
pwr <- vector("numeric", rep)
pvals <- replicate(rep, t.test(sample(bwt.nonsmoke, n),
                               sample(bwt.smoke, n))$p.value)
mean(pvals<=0.01)

# EXercises 3

pheno <- read.csv("data/inst/extdata/mice_pheno.csv")
controlPopulation <- unlist(read.csv("data/inst/extdata/femaleControlsPopulation.csv"))
ttestgenerator <- function(n){
  sample1 <- sample(controlPopulation, n)
  sample2 <- sample(controlPopulation, n)
  diff <- mean(sample1)-mean(sample2)
  SED <- sqrt(var(sample1)/n + var(sample2)/n)
  t <- diff/SED
  return(t)
}

ts <- replicate(10000, ttestgenerator(3))
hist(ts)
qqnorm(ts)
abline(0,1, col='red')

ps <- (seq(0, 999)+0.5)/1000
qqplot(qt(ps, df=4), ts)
abline(0, 1, col='red')

set.seed(1)
n <- 5
x <- rnorm(n)
(t <- sqrt(n)*mean(x)/sd(x))

set.seed(1)
b <- 1000
ts <- replicate(b, t.test(rnorm(n))$statistic)

sum(ts>2)/b
B=100
ps = seq(1/(B+1), 1-1/(B+1),len=B)
set.seed(1)
b <- 1000
n <- 3
ts <- replicate(b, t.test(rnorm(n))$statistic)
qqplot(qt(ps, df=n-1), ts)
abline(0, 1, col='red')

X <- sample(c(-1,1), 15, replace=TRUE)
set.seed(1)
ts <- replicate(1000, t.test(sample(c(-1,1), 1000, replace=TRUE))$statistic)
hist(ts)
qqplot(qt(ps, df=999), ts)
abline(0,1, col='red')

n <- 100
meds <- replicate(1000, median(rnorm(n)))
mean(meds)
sd(meds)
1/sqrt(n)
hist(meds)
qqplot(meds)
qqnorm(meds, )
abline(0, 1, col='red')

# Exercises 4
library(tidyverse)
babies <- read.table("data/inst/extdata/babies.txt", head=TRUE)
bwt.nonsmoke <- filter(babies, smoke=="0") %>% select(bwt) %>% unlist
bwt.smoke <- filter(babies, smoke=="1") %>% select(bwt) %>% unlist
N <- 10
set.seed(1)
nonsmokers <- sample(bwt.nonsmoke , N)
smokers <- sample(bwt.smoke , N)
obs <- median(smokers) - median(nonsmokers)

dat <- c(smokers,nonsmokers)
getnullval <- function(dat){
  shuffle <- sample( dat )
  smokersstar <- shuffle[1:N]
  nonsmokersstar <- shuffle[(N+1):(2*N)]
  median(smokersstar)-median(nonsmokersstar)
}
set.seed(1)
nullvals <- replicate(1000, getnullval(dat=dat))

sum(abs(nullvals)-abs(obs)>=0)/1000

# Exercises 5
d <- read.csv("data/assoctest.csv")
chisq.test(table(d))
fisher.test(table(d))
