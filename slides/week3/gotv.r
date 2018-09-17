setwd("path to working directory here") ### set working directory

social <- read.csv("social.csv") # load the data
summary(social) ## summary



#### we have four groups to compare (3 treatmnet, 1 control group)
#### would be a lot of work to calculate stuff for each group by hand

#### tapply
tapply(social$primary2006, social$messages, mean) ### applies function to subset by other variable
### calculate the mean of primary2006 (turn out or not) for each group in messages


#### to get the ATE, we compare to control

#### control group turnout
mean(social$primary2006[social$messages == "Control"])


### subtract control group turnout from others

tapply(social$primary2006, social$messages, mean) - mean(social$primary2006[social$messages == "Control"])

### look if groups are similar on other covars
tapply(social$age, social$messages, mean)

## back to slides
