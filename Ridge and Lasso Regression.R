#Exploring the performance of ridge and lasso regression against standard OLS#

library(ISLR)
library(glmnet)
set.seed(2019)
x <- model.matrix(Apps ~.,College)[,-1]
y <- College$Apps
sample_data <- sample.int(nrow(College),floor(0.5*nrow(College)),replace = F)
x.train <- x[sample_data,]
x.test <- x[-sample_data,]
y.train <- y[sample_data]
y.test <- y[-sample_data]

#Ridge Model
set.seed(4630)
cv.out <- cv.glmnet(x.train,y.train,alpha=0)
bestlam<-cv.out$lambda.min
bestlam
plot(cv.out)
#Train ridge model with best lambda on the training data
ridge.mod<-glmnet(x.train,y.train,alpha=0,lambda=bestlam, thresh = 1e-14)
##Test MSE with best lambda
ridge.pred<-predict(ridge.mod,s=bestlam,newx=x.test)
mean((ridge.pred-y.test)^2)

#Lasso Model
set.seed(4630)
cv.out.l <- cv.glmnet(x.train,y.train,alpha=1)
bestlam.lasso<-cv.out.l$lambda.min
bestlam.lasso
plot(cv.out.l)
#Train lasso model with best lambda on the training data
lasso.mod<-glmnet(x.train,y.train,alpha=1,lambda=bestlam.lasso, thresh = 1e-14)
##Test MSE with best lambda
lasso.pred<-predict(lasso.mod,s=bestlam.lasso,newx=x.test)
mean((lasso.pred-y.test)^2)

#Comparing performance to a standard OLS model#
ols.mod <- glmnet(x.train,y.train,alpha=0,lambda=0, thresh = 1e-14)
ols.pred<-predict(ols.mod,newx=x.test)
mean((ols.pred-y.test)^2)

