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
lm.fit1 = lm(mpg~.-name, data=Auto)
summary(lm.fit1)
# i.
# Yes, there is a relatioship between the
#predictors and the response by testing the null hypothesis of 
#whether all the regression coefficients are zero. The F -statistic is far from 1 (with a small p-value), indicating evidence against the null hypothesis.
# 
# ii.
# Looking at the p-values associated with each predictor’s 
#t-statistic, we see that displacement, weight, year, and origin 
#have a statistically significant relationship, while cylinders, horsepower, and acceleration do not.
# 
par(mfrow=c(2,2))
plot(lm.fit1)
plot(predict(lm.fit1), residuals(lm.fit1))
plot(predict(lm.fit1), rstudent(lm.fit1)) 
# Lot of possible outliers

lm.fit2 = lm(mpg ~ displacement + horsepower * displacement)
summary(lm.fit2)

lm.fit3 = lm(mpg~log(weight)+sqrt(horsepower)+acceleration+I(acceleration^2))
summary(lm.fit3)
par(mfrow = c(2,2))
plot(lm.fit3)
par(mfrow = c(1,1))
plot(predict(lm.fit3), rstudent(lm.fit3))


lm.fit3 = lm(log(mpg)~log(weight)+log(horsepower)+ log(acceleration) + log(weight))
summary(lm.fit3)
par(mfrow = c(2,2))

plot(lm.fit3)

plot(hatvalues(lm.fit3))
which.max(hatvalues(lm.fit3))


lm.fit4 = lm(log(mpg)~log(weight)+sqrt(horsepower)+acceleration+I(acceleration^2))
summary(lm.fit4)
par(mfrow = c(2,2))
plot(lm.fit4)
par(mfrow = c(1,1))
plot(predict(lm.fit4), rstudent(lm.fit4))

# log model follows a good regression model

# Exercise 10

summary(Carseats)
attach(Carseats)
lm.fit = lm(Sales~ Price + Urban + US)
summary(lm.fit)
# price is significant
# UrbanYes is not significant
# US yes is significant
# R^2 is very less

# Exercise 10 (e)
lm.fit2 = lm(Sales ~ Price + US)
summary(lm.fit2)
# Still R^2 is very less
confint(lm.fit2)
par(mfrow = c(2,2))
plot(lm.fit2)
plot(predict(lm.fit2), rstudent(lm.fit2))

# Exercise 11 (a)
set.seed(1)
x = rnorm(100)
y = 2 *x + rnorm(100)
lm.fit = lm(y~x + 0)
summary(lm.fit)
par(mfrow = c(2,2))
plot(lm.fit)
# Exercise 11 (b)
lm.fit2 = lm(x~y + 0)
summary(lm.fit2)
coef(lm.fit2)
# 11 (c)
# the t stat are same

# 11(c)  --> numerically
t_stat = sqrt(99) * sum(x*y)/sqrt(sum(x*x)*sum(y*y) - sum(x*y)^2) # = 18.73

names(lm.fit2)

#11(f)  -> can be shown both have t value = 18.73

# 12(a)  --> When the sum of squares is same

# 12(b)
set.seed(1)
x = rnorm(100)
y = 3*x

lm.fit = lm( y ~ x)
summary(lm.fit)

lm.fit2 = lm( x ~ y)
summary(lm.fit2)

# 12 (c)
set.seed(1)
x = rbeta(100, shape1 = 1, shape2 = 1) # using beta distribution
y = -sample(x,100)

lm.fit = lm( x ~ y + 0)

lm.fit2 = lm(y ~ x + 0)
names(lm.fit)
coef(lm.fit)
coef(lm.fit2)
# we can see they have the same coefficient

# Exerccise 13(a)


