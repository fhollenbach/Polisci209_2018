
data <- read.csv("~/Documents/GitHub/Polisci209_2018/slides/week5/bivariate_data.csv")

data <- subset(data, Year ==2010)
### how much missingness 
mean(is.na(data$PolityIV)==TRUE)
mean(is.na(data$GDP)==TRUE)
names(data)
### use subset to select only relevant variable
data <- subset(data, select = c("Entity","Country.code","GDP","PolityIV", "Child.Mortality"))
## remember how to subset to only keep rows that do not have missing data
data <- na.omit(data)

##simple plot
pdf("~/Documents/GitHub/Polisci209_2018/slides/week5/scatter_simple.pdf")
plot(data$GDP,data$Child.Mortality)
dev.off()


#so skewed, let's log
pdf("~/Documents/GitHub/Polisci209_2018/slides/week5/scatter_log_simple.pdf")
plot(log(data$GDP),data$Child.Mortality)
dev.off()


## add some labels, and nicer point symbols
pdf("~/Documents/GitHub/Polisci209_2018/slides/week5/scatter.pdf")
plot(log(data$GDP),data$Child.Mortality, pch = 16, col = "black", xlab = "logged GDP in PPP", ylab = "Child Mortality", main = "Income and Child Mortality")
dev.off()

## add special points for USA and Germany
pdf("~/Documents/GitHub/Polisci209_2018/slides/week5/scatter_points.pdf")
plot(log(data$GDP),data$Child.Mortality, pch = 16, col = "black", xlab = "logged GDP in PPP", ylab = "Child Mortality", main = "Income and Child Mortality")
points(log(data$GDP[data$Country.code == "USA"]), data$Child.Mortality[data$Country.code == "USA"], pch = 17, col = "red") ##USA
text(11, 16, "USA", col = "red")
points(log(data$GDP[data$Country.code == "DEU"]), data$Child.Mortality[data$Country.code == "DEU"], pch = 15, col = "gold") ##USA
text(10.2, 0, "GER", col = "gold")
dev.off()


cor(log(data$GDP),data$Child.Mortality, use = "pairwise")


cor(data[, c("GDP", "Child.Mortality", "PolityIV")], use = "pairwise.complete.obs")

### by hand
z_gdp <- (log(data$GDP) - mean(log(data$GDP), na.rm = T))/sd(log(data$GDP), na.rm =T)
z_CM <- (data$Child.Mortality - mean(data$Child.Mortality, na.rm = T))/sd(data$Child.Mortality, na.rm =T)
cor <- sum(z_gdp*z_CM)/(length(z_gdp)-1)
