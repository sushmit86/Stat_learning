library(ISLR)
library(boot)

# exercise 2
pr = function(n) return(1 - (1 - 1/n)^n)
par(mfrow=c(1,1))
x = 1:100000
plot(x,pr(x))

store = rep(NA,10000)
for ( i in 1:10000){
  store[i] = sum(sample(1:100, rep = T)==4) > 0
}
mean(store)
# 
attach(Default)
summary(Default)
set.seed(1)
glm.fit = glm(default ~ income + balance ,data = Default, family = binomial)

validation_set_approach = function() {
  # i.
  train = sample(dim(Default)[1], dim(Default)[1]/2)
  # ii.
  glm.fit = glm(default ~ income + balance, data = Default, family = binomial, 
                subset = train)
  # iii.
  glm.pred = rep("No", dim(Default)[1]/2)
  glm.probs = predict(glm.fit, Default[-train, ], type = "response")
  glm.pred[glm.probs > 0.5] = "Yes"
  # iv.
  return(mean(glm.pred != Default[-train, ]$default))
}
validation_set_approach()
#(c)
validation_set_approach()
validation_set_approach()
validation_set_approach()
 # (d)
train = sample(dim(Default)[1], dim(Default)[1]/2)
glm.fit = glm(default ~ income + balance + student, data = Default, family = binomial, 
              subset = train)
glm.pred = rep("No", dim(Default)[1]/2)
glm.probs = predict(glm.fit, Default[-train, ], type = "response")
glm.pred[glm.probs > 0.5] = "Yes"
mean(glm.pred != Default[-train, ]$default)
# 6 
# 6 (a)
glm.fit = glm(default ~ income + balance , data = Default , family = binomial)
summary(glm.fit)$coeff 
# 6(c)
boot.fn = function(data, index)
return(coef(glm(default ~ income + balance , data = data,family=binomial,subset = index )))
# 6(d)
boot(Default,boot.fn, 1000)

# both have the same standard errors
# Exercise 7
attach(Weekly)
head(Weekly)
glm.fit = glm(Direction ~ Lag1 + Lag2, data =Weekly,family = binomial)
# 7(b)
Weekly_1 = Weekly[-1,]
glm.fit = glm(Direction ~ Lag1 + Lag2, data =Weekly_1,family = binomial)
#(b)
predict(glm.fit, Weekly[1,], type = "response") == Weekly[1,]$Direction
for (i in 1:dim(Weekly)[1] ){
  
  print(i)
}
