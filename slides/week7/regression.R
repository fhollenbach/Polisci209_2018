
data <- read.csv("~/Documents/GitHub/Polisci209_2018/slides/week7/bivariate_data.csv")

data <- subset(data, Year ==2010)
### how much missingness 
mean(is.na(data$PolityIV)==TRUE)
mean(is.na(data$GDP)==TRUE)
names(data)
### use subset to select only relevant variable
data <- subset(data, select = c("Entity","Country.code","GDP","PolityIV", "Child.Mortality"))
## remember how to subset to only keep rows that do not have missing data
data <- na.omit(data)


## add some labels, and nicer point symbols
pdf("~/Documents/GitHub/Polisci209_2018/slides/week7/scatter_cor.pdf")
plot(log(data$GDP),data$Child.Mortality, pch = 16, col = "black", xlab = "logged GDP in PPP", ylab = "Child Mortality", main = "Income and Child Mortality")
text(11, 170, "Correlation = - 0.77")
dev.off()


cor(log(data$GDP), data$Child.Mortality, use = "pairwise.complete.obs")

N <- 100
x1 <- rnorm(N)
y1 <- x1/15 + rnorm(N)
y2 <- x1 + rnorm(N) / 2
y3 <- -x1 + rnorm(N)
y4 <- -2.5 + x1^2 + rnorm(N) / 10

pdf("~/Documents/GitHub/Polisci209_2018/slides/week7/correlations.pdf")
par(mfrow = c(2, 2), cex = 0.6)
plot(x1, y1, xlim = c(-3, 3), ylim = c(-3, 3), xlab = "", ylab = "",
     main = paste("(a) correlation =", round(cor(x1, y1), 2)))
plot(x1, y2, xlim = c(-3, 3), ylim = c(-3, 3), xlab = "", ylab = "",
     main = paste("(b) correlation =", round(cor(x1, y2), 2)))
plot(x1, y3, xlim = c(-3, 3), ylim = c(-3, 3), xlab = "", ylab = "",
     main = paste("(c) correlation =", round(cor(x1, y3), 2)))
plot(x1, y4, xlim = c(-3, 3), ylim = c(-3, 3), xlab = "", ylab = "",
     main = paste("(d) correlation =", round(cor(x1, y4), 2)))
dev.off()



lm <- lm(Child.Mortality ~ log(GDP), data = data)

pdf("~/Documents/GitHub/Polisci209_2018/slides/week7/scatter_lm.pdf")
plot(log(data$GDP),data$Child.Mortality, pch = 16, col = "black", xlab = "logged GDP in PPP", ylab = "Child Mortality", main = "Income and Child Mortality")
abline(lm, col = "red", lwd = 2)
dev.off()



pdf("~/Documents/GitHub/Polisci209_2018/slides/week7/scatter_lm_text.pdf")
plot(log(data$GDP),data$Child.Mortality, pch = 16, col = "black", xlab = "logged GDP in PPP", ylab = "Child Mortality", main = "Income and Child Mortality")
abline(lm, col = "red", lwd = 2)
text(7, 180, "y-intercept = 282.46", col = "red")
text(10, 150, "Slope  = -26.61", col = "red")
dev.off()
