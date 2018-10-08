                                        #x## Do changes in one's financial circumstances affect one's decision-making process and cognitive capacity? In an experimental study, researchers randomly selected a group of US respondents to be surveyed before their payday and another group to be surveyed after their payday. Under this design, the respondents of the Before Payday group are more likely to be financially strained than those of the After Payday group. The researchers were interested in investigating whether or not changes in people's financial circumstances affect their decision making and cognitive performance. Other researchers have found that scarcity induce an additional mental load that impedes cognitive capacity. This exercise is based on:

poverty <- read.csv("poverty.csv")
hist(poverty$cash, freq = FALSE, xlab = "Cash", main = "Distribution of Cash")

hist(poverty$accts_amt, freq = FALSE, xlab = "Checking/Savings Balance",
     main = "Distribution of Checking/Savings Balance")


### The distributions of the cash, and accts_amt variables are all highly skewed and possess many extreme values (outliers). These extreme values could pull our mean estimates upward, causing us to overestimate the mean amount of cash, as well as amount in checking and saving accounts for our sample. If these outliers are not evenly distributed between BeforeTreatment and AfterTreatment groups, we could also mischaracterize the effect of change in financial resources on decision-making. This would hamper the authors' ability to make a causal claim about this effect by hurting the internal validity of the survey experiment.


poverty$log_cash <- poverty$cash
poverty$log_cash[poverty$log_cash == 0] <- 1
poverty$log_cash <- log(poverty$log_cash)

poverty$log_accts_amt <- poverty$accts_amt
poverty$log_accts_amt[poverty$log_accts_amt == 0] <- 1
poverty$log_accts_amt <- log(poverty$log_accts_amt)

hist(poverty$log_cash, freq = FALSE, xlab = "log(Cash)",
     main = "Distribution of log(Cash) After Payday")

hist(poverty$log_accts_amt, freq = FALSE,
     xlab = "log(Checking/Savings Balance)",
     main = "Distribution of log(Checking/Savings)\n Balance After Payday")


### Taking the natural log of the three variables decreased the amount of variation, causing the histograms to look much less skewed. However, it is harder to interpret the values of the variables since everything is now on a log scale.


###Q2a

tapply(poverty$stroop_time, poverty$treatment, mean) -
    mean(poverty$stroop_time[poverty$treatment == "Before Payday"])


tapply(poverty$stroop_time, poverty$treatment, median) -
    median(poverty$stroop_time[poverty$treatment == "Before Payday"])

### Because mean and median cognitive scores are very similar for the two groups, the average and median treatment effects are close to zero.

par(mfrow = c(1,2))
plot(poverty$log_cash[poverty$treatment == "Before Payday"],
     poverty$stroop_time[poverty$treatment == "Before Payday"],
     ylab = "log(Average Response Time)", xlab = "log(Cash)", main = "Before Payday")

plot(poverty$log_cash[poverty$treatment == "After Payday"],
     poverty$stroop_time[poverty$treatment == "After Payday"],
     ylab = "log(Average Response Time)", xlab = "log(Cash)", main = "After Payday")

plot(poverty$log_accts_amt[poverty$treatment == "Before Payday"],
     poverty$stroop_time[poverty$treatment == "Before Payday"],
     ylab = "log(Average Response Time)", xlab = "log(Amount in Accounts)", main ="Before Payday")

plot(poverty$log_accts_amt[poverty$treatment == "After Payday"],
     poverty$stroop_time[poverty$treatment == "After Payday"],
     ylab = "log(Average Response Time)", xlab = "log(Amount in Accounts)",
     main = "After Payday")


### By looking at the scatter plots we can see that the relationship between financial circumstances (both cash on hand and amount in accounts), and cognitive performance (as indicated by the amount of time it took respondents to do the stroop test) is essentially flat for both before and after payday groups. This suggests that changes in financial resources do not affect cognitive performance.


### Q3

cashmean.diff <- mean(poverty$cash[poverty$treatment == "After Payday"], na.rm = TRUE) -
    mean(poverty$cash[poverty$treatment == "Before Payday"], na.rm = TRUE)

cashmed.diff <- median(poverty$cash[poverty$treatment == "After Payday"], na.rm = TRUE) -
    median(poverty$cash[poverty$treatment == "Before Payday"], na.rm = TRUE)

cash.diff <- c(cashmean.diff, cashmed.diff)
names(cash.diff) <- c("Mean Diff.in Cash", "Median Diff.in Cash")
cash.diff

### After payday, respondents report on average r round(cashmean.diff, 2) dollars more in cash holding (cash they had on hand) than Before payday respondents. However, the median difference in amount of cash reported is r round(cashmed.diff, 2) dollars. Since the mean is more sensitive to outliers, this relatively small difference between the two measures of central tendency is not surprising.

balancemean.diff <- mean(poverty$accts_amt[poverty$treatment == "After Payday"], na.rm = TRUE) -
    mean(poverty$accts_amt[poverty$treatment == "Before Payday"], na.rm = TRUE)

balancemed.diff <- median(poverty$accts_amt[poverty$treatment == "After Payday"], na.rm = TRUE) -
    median(poverty$accts_amt[poverty$treatment == "Before Payday"], na.rm = TRUE)

balance.diff <- c(balancemean.diff, balancemed.diff)
names(balance.diff) <- c("Mean Diff.in Balance", "Median Diff.in Balance")
balance.diff

### After payday, respondents report on average r round(-balancemean.diff, 2) dollars less in their checking and savings than before payday. The median difference, however, indicates that respondents have r round(balancemed.diff, 2) more dollars in their checking and savings account. This large diffence between the two values suggests the presence of influential outliers unevenly distributed between before and after payday groups.


### Q4

par(cex = 1.25)
qqplot(poverty$log_cash[poverty$treatment == "Before Payday"],
       poverty$log_cash[poverty$treatment == "After Payday"],
       xlab = "Before Payday", ylab = "After Payday",
       main = "QQ-plot Logged Cash Variable:\n Before vs. After Payday")
abline(0,1, col="red")

### espite the presence of some outliers, the qq-plot is relatively close to a 45 degree line. This suggests that there are no major differences in the distribution of cash held by respondents Before Payday and After Payday. This could be a problem for the authors' argument because cash is one of the measures of financial resources the authors use. If there is no variation in cash after payday anywhere in the distribution, the treatment did not occur.

par(cex = 1.25)
qqplot(poverty$log_accts_amt[poverty$treatment == "Before Payday"],
       poverty$log_accts_amt[poverty$treatment == "After Payday"],
       xlab = "Before Payday", ylab = "After Payday",
       main = "QQ-plot Logged Account Balance Variable:\n Before vs. After Payday")
abline(0, 1, col = "red")


### While after payday respondents in the lower 50 - 75 quantiles seem to have more money in checking and savings accounts than those before payday, the upper 25 - 50 quantiles of the qq-plot look relatively close to a 45 degree line. If these differences are systematically associated with a pre-treatment variable, this would suggest that there is a heterogeneous effect of payday on checking and savings account balances. This could be a problem for the authors' argument because checking and savings account balance is one of the measures of financial resources they use. If there is no difference in checking and savings account balances for Before Payday and After Payday, this means that treatment did not take. Given this heterogeneous treatment effect, we can only test the authors' hypotheses about the relationship between changes in financial resources and decision-making/cognitive performance for some of the respondents --- those most likely to be at the lower end of the accts_amt variable's distribution. Unfortunately, if those respondents are systematically different from respondents most likely to be at the upper end of the accts_amt variable's distribution, this could put the study's internal validity in question.


### Q5

great20k_diff <- mean(poverty$accts_amt[poverty$income_less20k == 0 & poverty$treatment == "After Payday"], na.rm = TRUE) -  mean(poverty$accts_amt[poverty$income_less20k == 0 & poverty$treatment == "Before Payday"], na.rm = TRUE)

less20k_diff <- mean(poverty$accts_amt[poverty$income_less20k == 1 & poverty$treatment == "After Payday"], na.rm = TRUE) - mean(poverty$accts_amt[poverty$income_less20k == 1 & poverty$treatment == "Before Payday"], na.rm = TRUE)
income_diffs <- c(great20k_diff, less20k_diff)

names(income_diffs) <- c("income > 20,000", "income < 20,000")

income_diffs

diff_diff <- great20k_diff - less20k_diff

diff_diff


### In this difference-in-difference design, we see that the effect of treatment on the amount in people's accounts is quite different for the lower income group compared to the higher income group. For the lower income group, we see that the effect of treatment is positive-- people in the After Payday group have a higher balance in their accounts than people in the Before Payday group (the difference is r round(less20k_diff, 2). But for the higher income group, the effect of treatment appears to be negative- those in the After Payday group have r round(abs(great20k_diff), 2) less than those in the Before Payday group. This brings into question the idea that the study successfully manipulated economic resources through their design, especially for higher income individuals.




install.packages("pollstR")
library(pollstR)
chart_name <- "2016-general-election-trump-vs-clinton"
polls2016 <- pollster_charts_polls(chart_name)[["content"]]




polls2016 <- as.data.frame(polls2016)
names(polls2016)

polls2016[1:3, c("Trump", "Clinton", "start_date", "end_date")]





class(polls2016$end_date)
polls2016$DaysToElection <-
    as.Date("2016-11-8") - polls2016$end_date


plot(polls2016$DaysToElection, polls2016$Clinton,
     xlab = "Days to the Election", ylab = "Support",
     xlim = c(550, 0), ylim = c(25, 65), pch = 19,
     col = "blue")
points(polls2016$DaysToElection, polls2016$Trump,
       pch = 20, col = "red")



plot(polls2016$DaysToElection, polls2016$Clinton, type = "l",
     xlab = "Days to the Election", ylab = "Support",
     xlim = c(550, 0), ylim = c(25, 65), pch = 19,
     col = "blue")
lines(polls2016$DaysToElection, polls2016$Trump,
      col = "red")


range(polls2016$DaysToElection)


for (i in c(1,2,3,4,5) {
    print(i)
}


values <- c(1, -1, 2)
results <- rep(NA, 3)
for (i in 1:3) {
    cat("iteration", i, "\n")
    results[i] <- log(values[i])
}




data <- subset(data, state == "TX")
for(i in unique(data$year)){
    sub.set <- subset(data, year == i)
    dems <- mean(sub.set$ideology_score[sub.set$party == "Democrat"])
    cat("Dem ideology", i, dems, "\n")
    repub <- mean(sub.set$ideology_score[sub.set$party == "Republican"])
    cat("Repub ideology", i, repub, "\n")
    cat("Polarization", i, (repub - dems), "\n")
}




days <- 500:26
window <- 7

Clinton.pred <- Trump.pred <- rep(NA, length(days))


for (i in 1:length(days)) {
    week.data <-
        subset(polls2016,
               subset = ((DaysToElection < (days[i] + window))
                   & (DaysToElection >= days[i])))
    Clinton.pred[i] <- mean(week.data$Clinton)
    Trump.pred[i] <- mean(week.data$Trump)
}



plot(days, Clinton.pred, type = "l", col = "blue",
     xlab = "Days to the Election", ylab = "Support",
     xlim = c(550, 0), ylim = c(25, 65))
lines(days, Trump.pred, col = "red")




days <- 500:26
window <- 14

Clinton.pred <- Trump.pred <- rep(NA, length(days))


for (i in 1:length(days)) {
    week.data <-
        subset(polls2016,
               subset = ((DaysToElection < (days[i] + window))
                   & (DaysToElection >= days[i])))
    Clinton.pred[i] <- mean(week.data$Clinton)
    Trump.pred[i] <- mean(week.data$Trump)
}


plot(days, Clinton.pred, type = "l", col = "blue",
     xlab = "Days to the Election", ylab = "Support",
     xlim = c(550, 0), ylim = c(25, 65))
lines(days, Trump.pred, col = "red")
text(400, 50, "Clinton", col = "blue")
text(400, 40, "Trump", col = "red")
text(200, 60, "party\n conventions")
abline(v = as.Date("2016-11-8") - as.Date("2016-7-28"),
       lty = "dotted", col = "blue")
abline(v = as.Date("2016-11-8") - as.Date("2016-7-21"),
       lty = "dotted", col = "red")
text(50, 30, "debates")
abline(v = as.Date("2016-11-8") - as.Date("2016-9-26"),
       lty = "dashed")
abline(v = as.Date("2016-11-8") - as.Date("2016-10-9"),
       lty = "dashed")




plot(days, Clinton.pred, type = "l", col = "blue",
     xlab = "Days to the Election", ylab = "Support",
     xlim = c(550, 0), ylim = c(25, 65))
lines(days, Trump.pred, col = "red")
text(400, 50, "Clinton", col = "blue")
text(400, 40, "Trump", col = "red")
text(200, 60, "party\n conventions")
abline(v = as.Date("2016-11-8") - as.Date("2016-7-28"),
       lty = "dotted", col = "blue")
abline(v = as.Date("2016-11-8") - as.Date("2016-7-21"),
       lty = "dotted", col = "red")
text(50, 30, "debates")
abline(v = as.Date("2016-11-8") - as.Date("2016-9-26"),
       lty = "dashed")
abline(v = as.Date("2016-11-8") - as.Date("2016-10-9"),
       lty = "dashed")
points(0,46.47, col = "red", pch = 15)
points(0,48.59, col = "blue", pch = 15)



#### subset to last week

last.week.data <- subset(polls2016, subset = DaysToElection < 15)

margin <- last.week.data$Clinton - last.week.data$Trump
true_margin <- 48.59 - 46.47

pred.error <- true_margin - margin

mean.error <- mean(pred.error)

rmse <- sqrt(mean(pred.error^2))







hist(margin, main = "Poll Prediction",
     xlab = "Predicted Clinton's margin of victory
(percentage points)")
abline(v = true_margin,
       lty = "dotted", col = "red")


average_error <- margin - true_margin
hist(average_error, main = "Poll Prediction Error",
     xlab = "Error in Predicted Clinton's margin of victory
(percentage points)")
