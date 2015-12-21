#Logistic Regression
library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket)
cor(Smarket[,-9])
attach(Smarket)
plot(Volume)
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume ,
            data=Smarket ,family =binomial )
summary(glm.fit)
glm.probs=predict(glm.fit,type="response")
glm.probs[1:10]
#testing using the train set
glm.pred=rep("Down",1250)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction)
mean(glm.pred==Direction)

#testing using the test set
train=(Year<2005)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume ,
            data=Smarket ,family =binomial ,subset =train )
glm.probs=predict(glm.fit,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)

#testing with less predictors
glm.fit=glm(Direction~Lag1+Lag2 ,data=Smarket ,family =binomial, subset =train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)

#Linear Discriminant Analysis
library(MASS)
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit
