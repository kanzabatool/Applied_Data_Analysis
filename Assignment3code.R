# 1) Load Default data set. Do not forget to set a random seed before beginning your analysis. 

library(ISLR)
set.seed(1)
names(Default)
fix(Default)
attach(Default)

#a) Fit a logistic regression model that uses income and balance to predict default. 

glm.fit = glm(default~income+balance , data = Default, family=binomial)
summary(glm.fit)

#b) Using the validation set approach, estimate the test error of this model.

#i. Split the sample set into a training set and a validation set.

train = sample(1000, 500)

#ii. Fit multiple logistic regression model using only the training observations

glm.fit = glm(default~income+balance , data = Default, family=binomial, subset = train)

summary(glm.fit)

#iii. Prediction of model on validation

predict_probs = predict(glm.fit, type = "response", subset = -train)
predict_glm = rep("No", length(predict_probs))
predict_glm[predict_probs > 0.5] = "Yes"

#iv. Compute the validation set error

mean(predict_glm != Default[-train, ]$default)

# c) Repeat the process with 3 different splits

train1 = sample(1000, 400)
train2 = sample(1000, 600)
train3 = sample(1000, 700)

glm.train1 = glm(default~income+balance , data = Default, family=binomial, subset = train1)
glm.train2 = glm(default~income+balance , data = Default, family=binomial, subset = train2)
glm.train3 = glm(default~income+balance , data = Default, family=binomial, subset = train3)

predict_train1_probs = predict(glm.train1, type = "response", subset = -train1)
predict_train1 = rep("No", length(predict_train1_probs))
predict_train1[predict_train1_probs > 0.5] = "Yes"

predict_train2_probs = predict(glm.train2, type = "response", subset = -train2)
predict_train2 = rep("No", length(predict_train2_probs))
predict_train2[predict_train2_probs > 0.5] = "Yes"

predict_train3_probs = predict(glm.train3, type = "response", subset = -train3)
predict_train3 = rep("No", length(predict_train3_probs))
predict_train3[predict_train3_probs > 0.5] = "Yes"

mean(predict_train1 != Default[-train1, ]$default)  #0.0578125   - 400 training obs
mean(predict_train2 != Default[-train2, ]$default) #0.03574468 - 600 training obs
mean(predict_train3 != Default[-train3, ]$default) #0.044623 - 700 training obs

# a logistic regression model that predicts default using income, balance, and a dummy variable for student. 


glm.student = glm(default~income+balance+student , data = Default, family=binomial, subset = train)
summary(glm.student)

predict.student.probs = predict(glm.student, type = "response", subset = -train)
predict.student = rep("No", length(predict.student.probs))
predict.student[predict.student.probs > 0.5] = "Yes"

mean(predict.student != Default[-train, ]$default) # 0.0436 ~ 4.36%


#*********************    QUESTION 2  ****************************8

set.seed(1)
x = rnorm(100)     
y= x - 2*x^2+ rnorm(100)

plot(x,y)

library(boot)
Data <- data.frame(x, y)
glm.fit.q2 = glm(y ~ x, data = Data)

cv.error = rep(0,4)
for (i in 1:4){
  glm.fit.q2 = glm(y ~ poly(x,i), data = Data)
  cv.error[i] = cv.glm(Data, glm.fit.q2)$delta[1]
}
cv.error  #7.2881616 0.9374236 0.9566218 0.9539049


#d) another random seed
set.seed(2)
#glm.fit.q3 = glm(y ~ x, data = Data)
cv.error = rep(0,4)
for (i in 1:4){
  glm.fit.q2 = glm(y ~ poly(x,i), data = Data)
  cv.error[i] = cv.glm(Data, glm.fit.q2)$delta[1]
}
cv.error   #7.2881616 0.9374236 0.9566218 0.9539049


#f)
set.seed(1)
cv.error.kfold = rep(0,4)
for (i in 1:4){
  glm.fit.qf = glm(y ~ poly(x,i), data = Data)
  cv.error.kfold[i] = cv.glm(Data, glm.fit.qf, K=10)$delta[1]
}
cv.error.kfold #7.9009578 0.9311231 0.9668688 0.9273289
