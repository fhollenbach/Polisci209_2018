### In class exercise on exlusionary attitudes
### Author: FLorian Hollenbach
### Date: Sep 7, 2018

### first we set a working directory
### this is where R will now look for anything to load or save

setwd("~/Documents/GitHub/Polisci209_2018/data/")#### you will need to change this to your directory on your compute

### now anytime we try to load data R will look in the directory we set above
### or if we save within the console, R will by default save it in that directory

### load data
### read.csv() is a function that reads csv files into R, the head = TRUE argument tells R that the first row is not actually data but has the variable names
boston <- read.csv("boston.csv", head = TRUE)
### our data is now assigned to the object name boston
### let's look at the dimension of the data
dim(boston)
### 123 rows, 14 columns

### what are the variables called?
names(boston)

### age Age of individual at time of experiment
### male Sex of individual, male (1) or female (0)
### income Income group in dollars (not exact income)
### white Indicator variable for whether individual identifies as white (1) or not (0)
### college Indicator variable for whether individual attended college (1) or not (0)
### usborn Indicator variable for whether individual is born in the US (1) or not (0)
### treatment Indicator variable for whether an individual was treated (1) or not (0)
### ideology Self-placement on ideology spectrum from Very Liberal (1) through Moderate (3) to Very Conservative (5)
### numberim.pre Policy opinion on question about increasing the number immigrants allowed in the country from Increased (1) to Decreased (5)
### numberim.post Same question as above, asked later
### remain.pre Policy opinion on question about allowing the children of undocumented immigrants to remain in the country from Allow (1) to Not Allow (5)
### remain.post Same question as above, asked later
### english.pre Policy opinion on question about passing a law establishing English as the official language from Not Favor (1) to Favor (5)
### english.post Same question as above, asked later



### take a look at the data
head(boston)
### the head function prints the first six rows of each variable in the data set

### the summary function can tell us more about each variable
summary(boston)

### remember, we can access individual variables with their name afteer $ sign or the column number
boston$age
boston[, 1]
### the colum is the second index, the row is the first
boston[1, ] ## gives the first row for all columns

### what is boston[1,1] going to show us?


### let's look at the overall mean income
mean(boston$income)
### average income in the sample

### logical operators, R can evaluate statements as TRUE or FALSE
### important operators <, >, ==, !=, >=, <=, also have & (and) and | (or) statements

4 > 3
4 == 3
4 < 3
4 != 3

4 > 3 & 7 > 5
4 > 3 & 7 < 5
4 > 3 | 7 < 5

### logical operators applied to a vector or dataframe are applied to each element separately

newAge <- boston$age[c(1:10)]
#### what does this do?
newAge
newAge < 40
newAge < 40 & newAge > 31


### TRUE and FALSE look like words but are actually coded as 1 and 0 in R
### This means you can make mathematical calculations on vectors that have TRUE and FALSE in them
### e.g.
vector <- c(T, F, T, F, T)
sum(vector) ### number of TRUE elements
mean(vector) ### share of TRUE elements

### can easily figure out number of people over 40 in data
(boston$age > 40)
sum(boston$age > 40)

### we can select certain elements of a vector with TRUE and FALSE statements
### only those elements with TRUE are selected, FALSE are left our
### example:
newAge
newAge[c(T, T, F, F, T, F, T, F, F, T)]

### better yet, select subset with condition
newAge > 40 ### shows whether TRUE or FALSE
newAge[newAge > 40] ### uses TRUE or FALSE to subset


### let's use subsetting to compare the group that was treated and the control group
income_treated <- boston$income[boston$treatment == 1]  ### subset whether group received treatment
income_control <- boston$income[boston$treatment == 0] ### no treatment

### let's compare the averages in both groups
mean(income_treated)
mean(income_control)
### pretty similar
mean(income_treated) - mean(income_control)
#### but mean is sensitive to outliers,
### let's use the median

### median: order observations in increasing order, median is the middle observations, i.e., 50% of observations are smaller, 50% larger
### if you have an even number of observations, median is the average of the middle two observations
median(income_treated)
median(income_control)



#### how would we calculate the average treatment effect on the number of immigrants question?
boston$numberim.pre ### attitude before experiment
boston$numberim.post ### attitude after experiment
boston$change <- boston$numberim.post - boston$numberim.pre


### we can also use the subset function to create a subset of a data set
treated <- subset(boston, treatment == 1)
summary(treated)
control <- subset(boston, treatment == 0)
summary(control)
### now we have two datasets, one for the control group, one for the treated

### let's compare some other variables
mean(treated$white)
mean(control$white)

summary(treated$ideology)
summary(control$ideology)


mean(treated$college)
mean(control$college)




#### or use the change variable already created
treated$change
control$change
### chage in attitude for treated group individuals
treated$change <- treated$numberim.post - treated$numberim.pre
### change in attitude for control group individuals
control$change <- control$numberim.post - control$numberim.pre

### how to we get the average treatment effect?
### average for treated group
mean(treated$change)
### oops something is wrong!
### we need to tell R to ignore the missing value (NA) in the calculation
mean(treated$change, na.rm = TRUE)


trt_change <- mean(treated$change, na.rm = TRUE)
ctrl_change <-  mean(control$change, na.rm = TRUE)


###Average treatment effect is the difference in change for the treated from the control
ATE <- trt_change - ctrl_change ## points on the 5 point scale, increase means more exclusionary
#### ATE is the difference in change of treated minus change in control
ATE

### do you think the effect could vary by education? maybe more so for people who didn't go to college?
### let's find out

treat.col.change <- mean(boston$change[boston$treatment == 1 & boston$college == 1], na.rm = TRUE)
### or split into multiple commands
treated.college <- boston$change[boston$treatment == 1 & boston$college == 1]
treated.col.change <- mean(treated.college, na.rm = T)

### now same for control
control.col.change <- mean(boston$change[boston$treatment == 0 & boston$college == 1], na.rm = TRUE)

### now we do treated and control for non college
treated.nocol.change <- mean(boston$change[boston$treatment == 1 & boston$college == 0], na.rm = TRUE)
control.nocol.change <- mean(boston$change[boston$treatment == 0 & boston$college == 0], na.rm = TRUE)

##### let's calculate the ATEs for college and no college

ATE.college <- treated.col.change - control.col.change
ATE.nocollege <- treated.nocol.change - control.nocol.change

ATE.college
ATE.nocollege

### difference:
ATE.college - ATE.nocollege


##### let's talk about some simple univariate statistics

###  quantiles allow us to split a variable into groups based on the magnitude of the variable
### example, median splits variable into two groups, based on the "middle value"
### i.e. 50% of observations are below and 50% above median
### mean and median both measure center of distribution, mean more sensitive to outliers

boston$income
mean(boston$income)
median(boston$income)

mean(c(boston$income, 10000000))
median(c(boston$income, 10000000))


### quartiles splits the data into more groups (4) based on
### first quartile (or lower quartile) is the value under which 25% of the observations fall, second quartile is 25% to median, third quartile (or upper quartile) is median to  75%. last is 75% to largest value


###summary gives quartiles
summary(boston$income)
### or
quantile(boston$income, prob = c(0, 0.25, 0.5, 0.75, 1))


### Interquartile range is the difference in values between 75th percentile and the 25th percentile, we use this to see how spread out the data is, how much variation there is
IQR(boston$income)
quantile(boston$income, prob = c(0, 0.25, 0.5, 0.75, 1))
135000 - 87500


### can also do deciles instead of quantiles
quantile(boston$income, prob = seq(0, 1, 0.1))


### standard deviation next

##### age by quantiles
## create age quartiles
qrt <- quantile(boston$age, probs = seq(from = 0, to = 1, by = (1 / 3)))


boston$age.qrt <- ifelse(boston$age <= qrt[2], 1,
                 ifelse(boston$age > qrt[2] & boston$age <= qrt[3], 2,
                 ifelse(boston$age > qrt[3], 3, NA)))

## treatmemnt group change by age
### tapply splits first argument, but factor in second argument and applies function in third argument to it, i.e. take the change variable, split it in groups by age quartile, calculate the mean
t.age.change <- tapply(boston$change[boston$treatment == 1], boston$age.qrt[boston$treatment == 1], mean, na.rm = TRUE)
## control group change by age
c.age.change <- tapply(boston$change[boston$treatment == 0], boston$age.qrt[boston$treatment == 0], mean, na.rm = TRUE)
ate.age <- t.age.change - c.age.change ### ATE for each age group
ate.age



#### standard deviation
sd(boston$income)

sqrt((sum((mean(boston$income, na.rm = T) - boston$income) ^ 2)) / length(boston$income))

dif <- (mean(boston$income, na.rm = T) - boston$income)
dif.squared <- dif ^ 2
dif.2.sum <- sum(dif.squared)
dif.mean <- dif.2.sum / length(boston$income)
sqrt(dif.mean)


### bar plot
### use age quartiles as factor variable
age.ptable <- prop.table(table(AGEgroups = boston$age.qrt, exclude = NULL))
barplot(age.ptable,
        names.arg = c("Young", "Middle Aged", "Old"),
        main = "",
        xlab = "age category",
        ylab = "Proportion of Subjects", ylim = c(0, 0.5))

#### histogram
hist(boston$age, freq = FALSE, xlab = "Age",
     main = "Distribution of Subjects's Age")


### boxplot
boxplot(income ~ treatment, data = boston,
        main = "Income by Treatment Status", ylab = "Income")
