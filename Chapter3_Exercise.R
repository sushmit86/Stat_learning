library(MASS)
library(ISLR)
Auto = read.csv('Data/Auto.csv' ,header=T, na.strings="?")
Auto = na.omit(Auto)
Auto = na.omit(Auto)
View(Auto)
attach(Auto)

# Exercise 8 (a) 
lm.fit=lm(mpg~horsepower)

summary(lm.fit)

# Has an R squared of 60.59 % Both Intercept and horsepower is significant
# Also we see negative relationship b/w horsepower and mpg

#  (iv) mpg = 39.93 - 0.157845 * 98 = 24.46119

predict(lm.fit, data.frame(horsepower=c(98)), interval="confidence")
predict(lm.fit, data.frame(horsepower=c(98)), interval="prediction")
# 8 (c)
plot(horsepower,mpg)
abline(lm.fit, lw = 3 , col = "red")
# 8 (d)
par(mfrow=c(2,2))
plot(lm.fit)
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
# 9 (a)
par(mfrow=c(1,1))
pairs(Auto)
names(Auto)
cor(subset(Auto, select = -name))


