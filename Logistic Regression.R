#Reading in data and then subsetting the columns that we want
library(readxl)
wine1 <- read_excel('Data.xlsx')
wine2 <- wine1[,-c(5)]
data <- wine2[,c(3,4,5,8,9,10,16,17,18,20,22,30,33,34,35)]
#Creating the response variable
data$response <- ifelse(data$`Red Wine Grape New Acreage Planted by county (2018)`>data$`White Wine Grape New Acreage Planted by County (2018)`,1,0)
wine.df <- data[,-c(1,2)]
wine.df$Nonwhite.pct <- 100 - wine.df$`White (%)`
 wine.df<- wine.df[,-c(5)]
#Creating the train test split
set.seed(222)
sample_data <- sample.int(nrow(wine.df),floor(0.5*nrow(wine.df)),replace = F)
training.data <- wine.df[sample_data,]
test.data <- wine.df[-sample_data,]

#Training a logistic model
result <- glm(response ~ ., family = 'binomial',data = training.data)
summary(result)
result$fitted.values
#############
##ROC curve##
#############

##predicted classes/probabilities for test data based on training data
preds<-predict(result,newdata=test.data, type="response")
preds
table(test.data$response,preds > 0.5)
##need ROCR package to produce ROC curve
library(ROCR)

##produce the numbers associated with classification table
rates<-prediction(preds, test.data$response)

##store the true positive and false postive rates
roc_result<-performance(rates,measure="tpr", x.measure="fpr")

##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result, main="ROC Curve for Wine Data - Logistic")
lines(x = c(0,1), y = c(0,1), col="red")

#Computing the AUC
auc<-performance(rates, measure = "auc")
auc

#Using k fold cross validation to estimate error
library(boot)
set.seed(222)
two.fold<-cv.glm(training.data,result, K=10) ##k specified
two.fold$delta
result$rank




#Trying to find an improved model
wine2$Nonwhite.pct <- 1 - wine2$`White (%)`
wine2.train <- wine2[sample_data,]
wine2.test <- wine2[-sample_data,]
wine2$response <- ifelse(wine2$`Red Wine Grape New Acreage Planted by county (2018)`>wine2$`White Wine Grape New Acreage Planted by County (2018)`,1,0)
improved <- glm(response ~ Nonwhite.pct + `HD Index`,family = 'binomial',data = wine2.train)
summary(improved)

improved.preds <- predict(improved,newdata = wine2.test,type='response')
table(wine2.test$response,improved.preds>0.5)


#Calculating metrics for the improved model
##produce the numbers associated with classification table
improved.rates<-prediction(improved.preds, wine2.test$response)

##store the true positive and false postive rates
imp_roc_result<-performance(improved.rates,measure="tpr", x.measure="fpr")

##plot ROC curve and overlay the diagonal line for random guessing
plot(imp_roc_result, main="ROC Curve - Logistic Model 2")
lines(x = c(0,1), y = c(0,1), col="red")

#Computing the AUC
auc_imp<-performance(improved.rates, measure = "auc")
auc_imp

#Using k fold cross validation
library(boot)
set.seed(222)
two.fold.improved<-cv.glm(wine2.train,improved, K=10) ##k specified
two.fold.improved$delta
1
