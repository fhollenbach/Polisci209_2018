
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

### function to create curly braces
CurlyBraces <- function(x0, x1, y0, y1, pos = 1, direction = 1, depth = 1, color = "black") {

    a=c(1,2,3,48,50)    # set flexion point for spline
    b=c(0,.2,.28,.7,.8) # set depth for spline flexion point

    curve = spline(a, b, n = 50, method = "natural")$y * depth

    curve = c(curve,rev(curve))

    if (pos == 1){
        a_sequence = seq(x0,x1,length=100)
        b_sequence = seq(y0,y1,length=100)
    }
    if (pos == 2){
        b_sequence = seq(x0,x1,length=100)
        a_sequence = seq(y0,y1,length=100)
    }

                                        # direction
    if(direction==1)
        a_sequence = a_sequence+curve
    if(direction==2)
        a_sequence = a_sequence-curve

                                        # pos
    if(pos==1)
        lines(a_sequence,b_sequence, lwd=1.5, xpd=NA, col = color) # vertical
    if(pos==2)
        lines(b_sequence,a_sequence, lwd=1.5, xpd=NA, col = color) # horizontal

}


pdf("~/Documents/GitHub/Polisci209_2018/slides/week7/scatter_lm_resid.pdf")
plot(log(data$GDP),data$Child.Mortality, pch = 16, col = "black", xlab = "logged GDP in PPP", ylab = "Child Mortality", main = "Income and Child Mortality", xlim = c(6, 12))
abline(lm, col = "red", lwd = 3)# add regression line
abline(v = 0, lty = "dashed")
## misc
arrows(x0 = 0.1, y0 = -0.75, x1 = 0, y1 = coef(lm)[1], length = 0.1)
text(0.1, -0.8, "intercept")
text(0.1, -0.9, expression(hat(alpha)))
lines(rep(log(data$GDP)[44], 2), c(data$Child.Mortality[44], fitted(lm)[44]))
arrows(x0 = 11, x1 = log(data$GDP)[44]-0.01, y0 = 150,
       y1 = data$Child.Mortality[44] - 0.01, length = 0.1)
text(11, 153, "outcome")
text(11, 145, expression(y))
CurlyBraces(x0=log(data$GDP)[44], x1=log(data$GDP)[44], y0=data$Child.Mortality[44],
            y1=fitted(lm)[44], pos = 1, direction = 1, depth=0.35, color = "black")
text(10.9, (data$Child.Mortality[44] + fitted(lm)[44])/2 + 6, "residual")
text(10.9, (data$Child.Mortality[44] + fitted(lm)[44])/2-3, expression(hat(epsilon)))
arrows(x0 = 11.1, x1 = log(data$GDP)[44] + 0.01, y0 = 30, y1 = fitted(lm)[44] + 0.01,
       length = 0.1)
text(11.5, 45, expression(hat(y)))
text(11.5, 32, "predicted\n value")
dev.off()





pdf("~/Documents/GitHub/Polisci209_2018/slides/week7/scatter_lm_mean.pdf")
plot(log(data$GDP),data$Child.Mortality, pch = 16, col = "black", xlab = "logged GDP in PPP", ylab = "Child Mortality", main = "Income and Child Mortality", xlim = c(6, 12))
abline(lm, col = "red", lwd = 3)# add regression line
abline(v = 0, lty = "dashed")
## misc
arrows(x0 = 0.1, y0 = -0.75, x1 = 0, y1 = coef(lm)[1], length = 0.1)
text(0.1, -0.8, "intercept")
text(0.1, -0.9, expression(hat(alpha)))
lines(rep(log(data$GDP)[44], 2), c(data$Child.Mortality[44], fitted(lm)[44]))
arrows(x0 = 11, x1 = log(data$GDP)[44]-0.01, y0 = 150,
       y1 = data$Child.Mortality[44] - 0.01, length = 0.1)
text(11, 153, "outcome")
text(11, 145, expression(y))
CurlyBraces(x0=log(data$GDP)[44], x1=log(data$GDP)[44], y0=data$Child.Mortality[44],
            y1=fitted(lm)[44], pos = 1, direction = 1, depth=0.35, color = "black")
text(10.9, (data$Child.Mortality[44] + fitted(lm)[44])/2 + 6, "residual")
text(10.9, (data$Child.Mortality[44] + fitted(lm)[44])/2-3, expression(hat(epsilon)))
arrows(x0 = 11.1, x1 = log(data$GDP)[44] + 0.01, y0 = 30, y1 = fitted(lm)[44] + 0.01,
       length = 0.1)
text(11.5, 45, expression(hat(y)))
text(11.5, 32, "predicted\n value")
abline(v = mean(log(data$GDP)), lty = "dotted")
text(mean(log(data$GDP)) + 0.3, 182, expression(bar(x)))
text(mean(log(data$GDP)) + 0.5, 175, "mean of x")
abline(h = mean(data$Child.Mortality), lty = "dotted")
text(6.5, mean(data$Child.Mortality) + 3, "mean of y")
text(6.5, mean(data$Child.Mortality) - 5, expression(bar(y)))
points(mean(log(data$GDP)), mean(data$Child.Mortality), pch = 19)
dev.off()
