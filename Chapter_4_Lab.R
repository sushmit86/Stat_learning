library(ISLR)
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
