#Reading Denver, CO Listings CSV File Into R
Colorado=read.csv("CO Listings.csv",header=T)
#Names of variables in dataset
names(Colorado)
attach(COlorado)
#Making enjoy_stay, wifi, Pets & hot_tub categorical variables in R
enjoy_stay=as.factor(enjoy_stay)
contrasts(enjoy_stay)
wifi=as.factor(wifi)
contrasts(wifi)
Pets=as.factor(Pets)
contrasts(Pets)
hot_tub=as.factor(hot_tub)
contrasts(hot_tub)
#Creating train and test sets 
train=sample(1:nrow(Colorado),nrow(COlorado)/2)
Colorado.test=Colorado[-train,]
dim(Colorado.test)
enjoy_stay.test=enjoy_stay[-train]
#Logistic regression
glm.fit=gml(enjoy_stay~bathrooms+bedrooms+wifi+Pets+hot_tub,data=Colorado,subset=train,family=binomial)
summary(glm.fit)
glm.probs=predict(glm.fit,Colorado.test,type="response")
glm.pred=rep("No",2433)
glm.pred[glm.probs>.8]="Yes"
table(glm.pred,enjoy_stay.test)
mean(glmpred==ejnoy_stay.test) #Classification rate
mean(glm.pred!=enjoy_stay.test)
#Linear discriminant analysis (LDA)
library(MASS)
lda.fit=lda(enjoy_stay~bathrooms+bedrooms+wifi+Pets+hot_tub,data=Colorado,subset=train)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit,Colorado.test)
lda.class=lda.pred$class
table(lda.class,enjoy_stay.test)
#Quadratic discriminant analysis (QDA)
qda.fit=qda(enjoy_stay~bedrooms+bathrooms+wifi+Pets+hot_tub,data=Colorado,subset=train)
qda.fit
qda.class=predict(qda.fit,Colorado.test)$class
table(qda.fit,enjoy_stay.test)