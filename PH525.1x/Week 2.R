# Exercises 1
library(dplyr)
mice <- read.csv("data/inst/extdata/femaleMiceWeights.csv")
mice %>% group_by(Diet) %>% summarize_all(mean)
population <- unlist(read.csv("data/inst/extdata/femaleControlsPopulation.csv"))
mean(population)

set.seed(1)
s1 <- sample(population, 5)
abs(mean(s1)-mean(population))

set.seed(5)
s2 <- sample(population, 5)
abs(mean(s2)-mean(population))

# exercises 2
set.seed(1)
n <- 1000
smeans <- vector("numeric", n)
for(i in 1:n){
  s1 <- sample(population, 50)
  smeans[i] <- mean(s1)
}

val <- 1
sum(abs(smeans-mean(population))>val)/n

# exercises 3
# install.packages("gapminder")
library(gapminder)
data(gapminder)
head(gapminder)
x <- unlist(gapminder %>% filter(year==1952) %>% select(lifeExp))
hist(x)
(sum(x<=60)-sum(x<=40))/length(x)
prop = function(q) {
  mean(x <= q)
}
prop(40)
qs = seq(from=min(x), to=max(x), length=20)
props = sapply(qs, prop)
plot(qs, props)
props = sapply(qs, function(q) mean(x <= q))
plot(ecdf(x))

## Normal distribution
# Exercises 1
set.seed(1)
n <- 1000
smeans <- vector("numeric", n)
for(i in 1:n){
  s1 <- sample(population, 5)
  smeans[i] <- mean(s1)
}

set.seed(1)
n <- 1000
smeans2 <- vector("numeric", n)
for(i in 1:n){
  s1 <- sample(population, 50)
  smeans2[i] <- mean(s1)
}

par(mfrow=c(2,1))
hist(smeans, xlim=c(20, 30))
hist(smeans2, xlim=c(20, 30))

(sum(smeans2<=25)-sum(smeans2<=23))/length(smeans2)

pnorm(25, 23.9, 0.43) - pnorm(23, 23.9, 0.43)

# EXercises 2
pheno <- read.csv("data/inst/extdata/mice_pheno.csv")
pheno <- na.omit(pheno)

# for males
x <- pheno %>% filter(Diet=="chow", Sex=="M") %>% select(Bodyweight) %>% unlist
mean(x)
rafalib:::popsd(x)
set.seed(1)
s1 <- sample(x, 25)
mean(s1)
y <- pheno %>% filter(Diet=="hf", Sex=="M") %>% select(Bodyweight) %>% unlist
mean(y)
rafalib:::popsd(y)
set.seed(1)
s2 <- sample(y, 25)
mean(s2)
abs((mean(x)-mean(y))-(mean(s1)-mean(s2)))

# for females
x <- pheno %>% filter(Diet=="chow", Sex=="F") %>% select(Bodyweight) %>% unlist
mean(x)
rafalib:::popsd(x)
set.seed(1)
s1 <- sample(x, 25)
mean(s1)
y <- pheno %>% filter(Diet=="hf", Sex=="F") %>% select(Bodyweight) %>% unlist
mean(y)
rafalib:::popsd(y)
set.seed(1)
s2 <- sample(y, 25)
mean(s2)
abs((mean(x)-mean(y))-(mean(s1)-mean(s2)))

# Exercises 3
pnorm(1)-pnorm(-1)
pnorm(2)-pnorm(-2)
pnorm(3)-pnorm(-3)
# for males
x <- pheno %>% filter(Diet=="chow", Sex=="M") %>% select(Bodyweight) %>% unlist
xm <- mean(x)
xsd <- rafalib:::popsd(x)
(sum(abs(x-xm)<3*xsd))/length(x)

avgs <- replicate(10000, mean(sample(x, 25)))
par(mfrow=c(2,1))
hist(avgs)
qqnorm(avgs)
qqline(avgs)

# Exercises 4
dat <- read.csv("data/inst/extdata/femaleMiceWeights.csv")

set.seed(1)
m <- 10000
p <- 1/6
obs <- replicate(m, mean(sample(1:6, n, replace=TRUE)==6))
zs <- (obs-p)/sqrt(p*(1-p)/n)
sum(abs(zs)>2)/m

for(p in c(0.5, 0.01)){
  for(n in c(5, 30, 100)){
    set.seed(1)
    sides <- 1/p
    obs <- replicate(m, mean(sample(1:sides, n, replace=TRUE)==1))
    zs <- (obs-p)/sqrt(p*(1-p)/n)
    prop <- sum(abs(zs)>2)/m
    print(data.frame(p=p, n=n, prop=abs(prop-0.05)))
  }
}
set.seed(1)
X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
Y <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist
mean(X)
sd(X)
# use the CLT to...
se <- sd(X)/sqrt(length(X))
2*pnorm((mean(X)-2), mean(X), se)

SED <- sqrt(var(X)/12+var(Y)/12)
t <- (mean(Y)-mean(X))/SED
