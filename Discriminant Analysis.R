##install.packages("klaR")
##install.packages("ICS")

library(MASS) ##for lda function
library(klaR) ##for partimat function to produce 2-D partition plots
library(ICS) ##for multivariate normality tests

#######
##EDA##
#######

##look at first 10 samples
head(iris,10)

##scatterplot matrix of predictors, different colors for each species
pairs(iris[,1:4], col = c(1,2,3)[iris$Species], lower.panel=NULL)

##Assumption is that the predictors follow a MVN for each class, so need to assess the assumption for each species

##subset dataframe by species
species1<-iris[which(iris$Species=="setosa"),]
species2<-iris[which(iris$Species=="versicolor"),]
species3<-iris[which(iris$Species=="virginica"),]

##MVN tests for setosa
mvnorm.kur.test(species1[,1:4])
mvnorm.skew.test(species1[,1:4])

##MVN tests for versicolor
mvnorm.kur.test(species2[,1:4])
mvnorm.skew.test(species2[,1:4])

##MVN tests for virginica
mvnorm.kur.test(species3[,1:4])
mvnorm.skew.test(species3[,1:4])

set.seed(16)

##create training and test data
sample.data<-sample.int(nrow(iris), floor(.60*nrow(iris)), replace = F)
train<-iris[sample.data, ]
test<-iris[-sample.data, ]

#######
##LDA##
#######

##Carry out LDA on training data
lda.iris <- lda(Species ~ ., data=train)
##obtain ouput from LDA
lda.iris

##See 2-D groupings by LD1 and LD2
plot(lda.iris, col = as.integer(train$Species), main="Groupings of Species by LDs")

##See 1-D groupings by LD1
plot(lda.iris, dimen = 1, type = "b")

##boundaries based on 2 predictors
partimat(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=train, method="lda")

##predictions on training data. 
lda.train <- predict(lda.iris)
##Confusion matrix on training data. Rows represent actual value, cols represent pred value
table(train$Species,lda.train$class)

##predictions on test data. 
lda.test <- predict(lda.iris,test)
table(test$Species,lda.test$class)

## Overall accuracy
mean(test$Species == lda.test$class)

##posterior probabilities for first 5 observations of test data
head(lda.test$posterior,5)

#######
##QDA##
#######

qda.iris <- qda(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, train)
qda.iris 

##Boundaries based on 2 predictors
partimat(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=train, method="qda")

##predictions on test data
qda.test <- predict(qda.iris,test)
table(test$Species,qda.test$class)

## Overall accuracy
mean(test$Species == qda.test$class)