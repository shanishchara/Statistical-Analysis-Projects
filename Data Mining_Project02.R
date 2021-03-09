library(tree)
library(randomForest)
library(gbm)
library(e1071)

data<-read.csv("classif1.txt")
name<-c("x1","x2","x3","x4","fraud")
colnames(data)<-name

# Creating train and test sets

set.seed(1)
train_num<-sample(1:length(data$x1),0.7*length(data$x1))
train<-data[train_num,]
test<-data[-train_num,]


# Trying classification method
# 
train$fraud<-as.factor(train$fraud)
# 
# test$fraud<-as.numeric(test$fraud)
test$fraud<-as.factor(test$fraud)
# 
# 
# train$fraud<-as.numeric(train$fraud)
# train$fraud[train$fraud!=1]<-0
# train$fraud[is.na(train$fraud)]<-0
# train$fraud<-as.factor(train$fraud)
# test$fraud<-as.numeric(test$fraud)
# test$fraud[test$fraud!=1]<-0
# test$fraud[is.na(test$fraud)]<-0
# test$fraud<-as.factor(test$fraud)

#train$dear<-NULL
#test$dear<-NULL


tree.train<-tree(fraud~.,train)
summary(tree.train)

plot(tree.train)
text(tree.train,pretty=0)

tree.test<-predict(tree.train,test,type = "class")
table(tree.test,test$fraud)

(2+7)/(224+7+2+179)

# Now let's try some pruning methods

cv.names_tree<-cv.tree(tree.train,FUN=prune.misclass)
names(cv.names_tree)

cv.names_tree$size
cv.names_tree$dev
cv.names_tree$k
cv.names_tree$method

par(mfrow=c(1,2))
plot(cv.names_tree$size,cv.names_tree$dev,type="b")
plot(cv.names_tree$k,cv.names_tree$dev,type="b")

prune.names_tree<-prune.misclass(tree.train,best=14)
plot(prune.names_tree)
text(prune.names_tree,pretty=0)

tree.test.pred<-predict(prune.names_tree,test,type = "class")
table(tree.test.pred,test$fraud)

#No difference because best was at size 14

# Attempting a bagging model

bag.email<-randomForest(fraud~.,data=train,mtry=length(colnames(data)),importance=TRUE)
bag.email

bag.yhat<-predict(bag.email,newdata=test)
plot(bag.yhat,test$fraud)
abline(0,1)
bag.err<-mean(bag.yhat!=test$fraud)

# Attempting a random forest model

pred<-ncol(train)-1

rf.email<-randomForest(fraud~.,data=train,mtry=round(sqrt(pred)),importance=TRUE)
rf.email

rf.yhat<-predict(rf.email,newdata=test)
plot(rf.yhat,test$fraud)
abline(0,1)
rf.err<-mean(rf.yhat!=test$fraud)
table(rf.yhat,test$fraud)


varImpPlot(rf.email)

# Excellent result of about .09%

# Attempting a boosting model

set.seed(1)

boost.email<-gbm(fraud~.,data=train,distribution = "bernoulli",n.trees=5000,interaction.depth = 4)
summary(boost.email)

par(mfrow=c(1,2))
plot(boost.email,i="rm")
plot(boost.email,i="lstat")

boost.yhat=predict(boost.email,newdata=test,n.trees=5000)
boost.err<-mean(boost.yhat!=test$fraud)

shrink.boost.email<-gbm(fraud~.,data=train,distribution = "bernoulli",n.trees=5000,interaction.depth = 4,
                        shrinkage = 0.2,verbose = F)
shrink.boost.yhat=predict(shrink.boost.email,newdata=test,n.trees=5000)
shrink.boost.err<-mean(shrink.boost.yhat!=test$fraud)

# Attempting a SVM

svmfit<-svm(fraud~.,data=train,kernel="linear",cost=10,scale=FALSE)
plot(svmfit,train)

set.seed(1)
tune.out<-tune(svm,fraud~.,data=train,kernel="linear",ranges=list(cost=c(1,5,10,100,200,300,350,400,500)))
summary(tune.out)

bestmod<-tune.out$best.model
summary(bestmod)

ypred<-predict(bestmod, subset(test,select = -fraud))
table(predict=ypred,truth=test$fraud)

# Trying a non linear SVM

svmfit<-svm(fraud~.,data=train,kernel="radial",gamma=1,cost=1)
plot(svmfit,train)

set.seed(1)
#tune.out<-tune(svm,fraud~.,data=train,kernel="radial",ranges= list(cost=c(0.1,1,2,5,10,100,300,500,1000)),gamma=c(0.5,1,2,3,4))

tune.out=tune(svm , fraud~., data=train, kernel ="radial",
              ranges =list(cost=c(0.1 ,1 ,10 ,100 ,1000),
                           gamma=c(0.5,1,2,3,4) ))
summary(tune.out)


table(truth=test$fraud,pred=predict(tune.out$best.model,newdata = subset(test,select = -fraud)))






