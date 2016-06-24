library(ISLR)
library(MASS)
library(class)
names(Smarket)
head(Smarket)
dim(Smarket)
summary(Smarket)
cor(Smarket[, -9])
attach(Smarket)
plot(Volume)
glm_fit = glm(Direction ~ Lag1 + 
                Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket , family =  binomial())
summary(glm_fit)
coef(glm_fit)
summary(glm_fit)$coef
contrasts(Direction)
glm_probs = predict(glm_fit, type = "response")
glm_probs[1:10]
glm_pred = rep("Down", 1250)
glm_pred[glm_probs > 0.5] = "Up"
addmargins(table(glm_pred, Direction))
mean(glm_pred == Direction)
train = (Year < 2005)
Smarket_2005 = Smarket[!train,]
dim(Smarket_2005)
Direction_2005 = Direction[!train]
glm_fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + 
                Lag4 + Lag5 + Volume, data = Smarket, family = binomial, subset = train)
glm_probs = predict(glm_fit, Smarket_2005,type = "response")
glm_pred = rep("Down",252)
glm_pred[glm_probs > 0.5] = "Up"
table(glm_pred, Direction_2005)
mean(glm_pred == Direction_2005)
mean(glm_pred != Direction_2005)
glm_fit = glm(Direction ~ Lag1 + Lag2 , data = Smarket, family = binomial, subset = train)
glm_probs = predict(glm_fit, Smarket_2005,type = "response")
glm_pred = rep("Down",252)
glm_pred[glm_probs > 0.5] = "Up"
table(glm_pred, Direction_2005)
mean(glm_pred == Direction_2005)
mean(glm_pred != Direction_2005)
# 106/(106+ 76)
predict(glm_fit, newdata = data.frame(Lag1= c(1.2,1.5), Lag2 = c(1.1,-0.8)), type = "response")
# Linear Discriminant analysis
lda_fit = lda(Direction ~ Lag1 + Lag2 , data = Smarket, subset = train) 
plot(lda_fit)
lda_pred = predict(lda_fit, Smarket_2005)
names(lda_pred)
lda_class = lda_pred$class
table(lda_class, Direction_2005)
mean(lda_class == Direction_2005)
lda_pred$class[1:10]
lda_pred$posterior[1:10]
lda_pred$x[1]
head(Smarket_2005)
sum(lda_pred$posterior[,1] >= 0.5)
sum(lda_pred$posterior[,1] < 0.5)
lda_pred$posterior[1:20,1]
lda_class[1:20]
## QDA
qda_fit = qda(Direction ~ Lag1 + Lag2 , data = Smarket , subset = train)
qda_fit
qda_predict = predict(qda_fit, Smarket_2005)
names(qda_predict)
table(qda_predict$class, Direction_2005)
mean(qda_predict$class== Direction_2005)
qda_predict$class[1:10]
qda_predict$posterior[1:10]
# Applying KNN
train.X = cbind(Lag1, Lag2)[train,]
test.X = cbind(Lag1,Lag2)[!train,]
train.Direction = Direction[train]
set.seed(1)
knn.pred = knn(train.X,test.X,train.Direction, k=1)
table(knn.pred, Direction_2005)
knn.pred = knn(train.X,test.X,train.Direction, k=3)
table(knn.pred, Direction_2005)
# Caravan Insurance Policy
dim(Caravan)
head(Caravan)
attach(Caravan)
summary(Purchase)
standardized.X = scale(Caravan[, -86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])
test = 1:1000
train.X = standardized.X[-test,]
test.X = standardized.X[test,]
train.Y = Purchase[-test]
test.Y = Purchase[test]
set.seed(1)
knn.pred = knn(train.X, test.X, train.Y, k=1)
mean(test.Y != knn.pred)
mean(test.Y != "No")
table(knn.pred,test.Y)
set.seed(1)
knn.pred = knn(train.X, test.X, train.Y, k=3)
table(knn.pred,test.Y)
knn.pred = knn(train.X, test.X, train.Y, k=5)
table(knn.pred,test.Y)
glm_fit = glm(Purchase ~. ,data = Caravan, family= binomial, subset = -test)
glm_probs = predict(glm_fit, Caravan[test,], type = "response")
glm_pred = rep("No",1000)
glm_pred[glm_probs > 0.5] = "Yes"
table(glm_pred, test.Y)

glm_pred = rep("No",1000)
glm_pred[glm_probs > 0.25] = "Yes"
table(glm_pred, test.Y)