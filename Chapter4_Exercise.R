library(ISLR)
library(MASS)
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
table(glm.pred2, Direction_test)
# Percentage correctly predicted = (9+56)/length(glm.probs2) = 62.5 %
# Percentage of correctly predicting down = 20.9%
# doing the lda fit
lda_fit = lda(Direction ~ Lag2 , data = Weekly, subset = train) 
lda_fit
plot(lda_fit)
lda_pred = predict(lda_fit, Weekly_test)
names(lda_pred)
lda_class = lda_pred$class
table(lda_class, Direction_test)
# Very close results with logistic regression
fit.qda <- qda(Direction ~ Lag2, data = Weekly, subset = train)
fit.qda
pred.qda <- predict(fit.qda, Weekly_test)
table(pred.qda$class, Direction_test)

# KNN = 1
train.X = as.matrix(Lag2[train])
test.X = as.matrix(Lag2[!train])
train.Direction = Direction[train]
set.seed(1)
pred.knn = knn(train.X, test.X, train.Direction, k = 1)
table(pred.knn, Direction_test)
# 50 % correct rate 
# (i) 
# Taking Lag1 + Lag2
glm.fit3 = glm(Direction ~ Lag2:Lag1 , data = Weekly, family = binomial, subset = train)
glm.prob3 = predict(glm.fit3, Weekly_test, type = "response")
pred.glm3 = rep("Down", length(glm.prob3))
pred.glm3[glm.prob3 > 0.5] = "Up"
table(pred.glm3, Direction_test)
# Almost predicts 60% of correct ups # not that with Down
# LDA with Lag2 interaction with Lag1
glm.lda3 = lda(Direction ~ Lag2:Lag1 , data = Weekly, subset = train)
glm.lda3
plot(glm.lda3)
lda_pred = predict(glm.lda3, Weekly_test)
names(lda_pred)
lda_class = lda_pred$class
table(lda_class, Direction_test)
mean(lda_class == Direction_test)
# QDA with sqrt(abs(Lag2))
qda_fit = qda(Direction ~ Lag2 + sqrt(abs(Lag2)), data = Weekly, subset = train)
pred.qda2 = predict( qda_fit, Weekly_test)
table(pred.qda2$class, Direction_test)
# 0.5769231
# K =2
set.seed(1)
pred.knn = knn(train.X, test.X, train.Direction, k = 2)
table(pred.knn, Direction_test)
# (18+ 25)/(length(pred.knn))
pred.knn = knn(train.X, test.X, train.Direction, k = 10)
table(pred.knn, Direction_test)
mean(pred.knn == Direction_test)
pred.knn = knn(train.X, test.X, train.Direction, k = 100)
mean(pred.knn == Direction_test)
# Question 11 (a)
attach(Auto)
head(Auto)
median(Auto$mpg)
mpg01 = rep(0, length(Auto$mpg))
mpg01[Auto$mpg > median(Auto$mpg)] = 1
mpg01
Auto_mpg01= data.frame(Auto,mpg01)
head(Auto_mpg01)
# 11(b)
pairs(Auto_mpg01)
par(mfrow = c(5,1))  
boxplot(horsepower~mpg01,main = "Horsepower vs mpg01")
boxplot(cylinders~mpg01 ,main = "Cylinders vs mpg01")
boxplot(weight~mpg01 , main = " weight vs mpg01")
boxplot(acceleration,mpg01, main = " accelaration vs mpg01")
boxplot(year, mpg01, main = "Year vs mpg01")
# there is realationship
# Later years have higher mpg01
#  11(c)
# training vs test 
head(Auto_mpg01) 
dim(Auto_mpg01)
train = (year%%2 ==0)
test = !train
Auto_mpg01.train = Auto_mpg01[train, ]
Auto_mpg01.test = Auto_mpg01[test,]
mpg01.test = mpg01[!train]
lda.fit = lda(mpg01 ~ horsepower+ cylinders + weight  + displacement, data = Auto_mpg01
              , subset = train)
lda.fit
pred.lda = predict(lda.fit, Auto_mpg01.test)
table(pred.lda$class, mpg01.test)
mean(pred.lda$class != mpg01.test)
# 12.6 %
pred.lda$class[1:10]
pred.lda$posterior[1:10]

qda.fit = qda(mpg01 ~ horsepower+ cylinders + weight  + displacement, data = Auto_mpg01
              , subset = train)
qda.fit
pred.qda = predict(qda.fit, Auto_mpg01.test)
names(pred.qda)
pred.qda$class[1:5]
pred.qda$posterior[1:5]
mean(pred.qda$class != mpg01.test)
# 13.1 % test error
# 11(f) -- Perform logistic Regression
glm.fit  = glm(mpg01 ~ horsepower+ cylinders + weight  + displacement, family = , binomial,
               data = Auto_mpg01 , subset = train)
summary(glm.fit)

# Lag 2 is significant at 5 % level
glm.probs = predict(glm.fit, Auto_mpg01.test,type = "response")
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
table(glm.pred, mpg01.test)
mean(glm.pred != mpg01.test)
# test error = 12.08%
# 11 (g) -- Perform KNN

# KNN = 1
train.X = cbind(cylinders, weight, displacement, horsepower)[train, ]
test.X = cbind(cylinders, weight, displacement, horsepower)[!train, ]
train.mpg01 = mpg01[train]
set.seed(1)
pred.knn = knn(train.X, test.X, train.mpg01, k = 1)
table(pred.knn, mpg01.test)
mean(pred.knn != mpg01.test)

# KNN = 10
train.X = cbind(cylinders, weight, displacement, horsepower)[train, ]
test.X = cbind(cylinders, weight, displacement, horsepower)[!train, ]
train.mpg01 = mpg01[train]
pred.knn = knn(train.X, test.X, train.mpg01, k = 10)
table(pred.knn, mpg01.test)
mean(pred.knn != mpg01.test)

# KNN = 100
train.X = cbind(cylinders, weight, displacement, horsepower)[train, ]
test.X = cbind(cylinders, weight, displacement, horsepower)[!train, ]
train.mpg01 = mpg01[train]
pred.knn = knn(train.X, test.X, train.mpg01, k = 100)
table(pred.knn, mpg01.test)
mean(pred.knn != mpg01.test)
# Test error = 14.2 %
# Exercise 12









