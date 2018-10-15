

intrade08 <- read.csv("intrade08.csv")
pres08  <- read.csv("pres08.csv")

summary(intrade08)
summary(pres08)


intresults08 <- merge(intrade08, pres08, by = "state")
head(intresults08)

intresults08$DaysToElection <- as.Date("2008-11-04") - as.Date(intresults08$day)
intresults08$obama.intmarg <- intresults08$PriceD - intresults08$PriceR
intresults08$obama.actmarg <- intresults08$Obama - intresults08$McCain


latest08 <- intresults08[intresults08$DaysToElection == 1,]
int.fit08 <- lm(obama.actmarg ~ obama.intmarg, data = latest08)
coef(int.fit08)
summary(int.fit08)$r.squared
plot(latest08$obama.intmarg, latest08$obama.actmarg,
     xlab="Market's margin for Obama", ylab="Obama margin")
abline(int.fit08)




coef(int.fit08)[1] + coef(int.fit08)[2]*25


plot(latest08$obama.intmarg, latest08$obama.actmarg,
     xlab="Market's margin for Obama", ylab="Obama margin")
abline(int.fit08)
points(25,(coef(int.fit08)[1] + coef(int.fit08)[2]*25), col = "red")




stnames <- unique(intresults08$state.name)
recent <- subset(intresults08, subset=(DaysToElection <= 20) & (state.name==stnames[50]))
recent.mod <- lm(obama.intmarg ~ DaysToElection, data=recent)
plot(recent$DaysToElection, recent$obama.intmarg, xlab="Days to election", ylab="Market's Obama margin")
abline(recent.mod)



stnames <- unique(intresults08$state.name)
change <- rep(NA, length(unique(intresults08$state.name)))
names(change) <- unique(intresults08$state.name)

for(i in 1: length(unique(intresults08$state.name))){
    recent <- subset(intresults08, subset=(DaysToElection <= 20) & (state.name==stnames[i]))
    recent.mod <- lm(obama.intmarg ~ DaysToElection, data=recent)
    change[i] <- coef(recent.mod)[2]
}

hist(change)






###   q5
latest08 <- intresults08[intresults08$DaysToElection <8,]
average.Intrade <- tapply(latest08$obama.intmarg, latest08$state, mean)
true.margin <- tapply(latest08$obama.actmarg, latest08$state, mean)

int.fit08 <- lm(true.margin ~ average.Intrade)
coef(int.fit08)
summary(int.fit08)$r.squared


##### q6


data2012 <- read.csv("intresults12.csv")
data2012$DaysToElection <- as.Date("2008-11-06") - as.Date(data2012$day)
data2012$obama.intmarg <- data2012$PriceD - data2012$PriceR
data2012$obama.actmarg <- data2012$Obama - data2012$Romney

latest12 <- data2012[data2012$DaysToElection <8,]
average.Intrade12 <- tapply(latest12$obama.intmarg, latest12$state, mean, na.rm = T)
true.margin12 <- tapply(latest12$obama.actmarg, latest12$state, mean, na.rm = T)
prediction <- coef(int.fit08)[1] + coef(int.fit08)[2]*average.Intrade12
error <- true.margin12 - prediction
hist(error)
