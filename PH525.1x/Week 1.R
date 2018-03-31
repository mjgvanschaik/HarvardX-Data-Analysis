# Test 2
mydat <- read.csv("data/inst/extdata/femaleMiceWeights.csv")
mydat[12,2]
mydat$Bodyweight[11]
length(mydat)
dim(mydat)
lapply(split(mydat$Bodyweight, mydat$Diet), mean)

library(dplyr)
mydat %>% group_by(Diet) %>% summarize_all(mean)

set.seed(1)
i <- sample(13:24, 1)
mydat[i,]
 
# Test 3
msleep <- read.csv("data/inst/extdata/msleep_ggplot2.csv")
class(msleep)
nrow(msleep %>% filter(order=="Primates"))
primates <- msleep %>% subset(order=="Primates")
nrow(primates)
sleep_total <- msleep %>% filter(order=="Primates") %>% select(sleep_total)
class(sleep_total)
mean(unlist(sleep_total))
# better
msleep %>% filter(order=="Primates") %>% summarise_at("sleep_total", mean)
