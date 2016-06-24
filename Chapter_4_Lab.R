library(ISLR)
library(MASS)
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
