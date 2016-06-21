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