library(ISLR)
library(vioplot)
# Exercise 10
head(Weekly)
summary(Weekly)
attach(Weekly)
par(mfrow=c(2,5))
boxplot(Lag1)
vioplot(Lag1)
boxplot(Lag2)
vioplot(Lag2)
boxplot(Lag3)
vioplot(Lag3)
boxplot(Lag4)
vioplot(Lag4)
boxplot(Lag5)
vioplot(Lag5)
cor(Weekly[, -9])
par(mfrow=c(1,1))
plot(Volume)
pairs(Weekly)
names(Weekly)
glm.fit = glm(Direction ~ .-Year -Today, data = Weekly, family = binomial())
summary(glm.fit)
# Lag 2 is significant at 5 % level
glm.probs = predict(glm.fit, type = "response")
glm.pred = rep("Down", length(glm.probs))
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction)
# Predicting large numbers of up where actually its going down
# only 11% correct when the market actually goes down
# Now compute the over all fraction of correct prediction
train = (Year < 2009)
Weekly_test = Weekly[!train,]
Direction_test = Direction[!train]
glm.fit2 = glm( Direction ~ Lag2, data = Weekly,  family = binomial, subset = train)
summary(glm.fit2)
glm.probs2 = predict(glm.fit2, Weekly_test, type = "response")
glm.pred2 = rep("Down", length(glm.probs2))
glm.pred2[glm.probs2 > 0.5] = "Up"


