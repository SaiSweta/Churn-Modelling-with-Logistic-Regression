#Setting the right working directory:

#Importing the dataset:
library(readxl)
data=read_excel("Cellphone-1.xlsx")

#Exploring the dataset:
str(data)
summary(data)

#Splitting the dataset into training & test set:
set.seed(3456)
library(caTools)
split=sample.split(data$Churn,SplitRatio = 0.7)
training_set=subset(data,split==TRUE)
test_set=subset(data,split==FALSE)

#Logistic Regression model:
model=glm(training_set$Churn~.,data=training_set[ ,2:11],family=binomial)

#Step 1:Overall Significance:
library(lmtest)
lrtest(model)

#Step 2:Pseudo R2:
library(pscl)
pR2(model)

#Step 3:Individual Coffiecients and significance:
summary(model)

#Variable Importance:
library(ggplot2)
library(lattice)
library(caret)
varImp(model)

#Step 4:Explanatory power of odds ratio:
exp(coef(model))

#Step 5:Confusion matrix:
predicted=floor(predict(model,type="response",newdata=test_set[ ,2:11])+0.5)
cm<-table(actual=test_set$Churn,predicted)
accuracy=(cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])*100

#Step 6:ROC plot:
library(rJava)
library(Deducer)
rocplot(model)

#Step 7:K-Fold Cross Validation:
library(lattice)
library(caret)
library(ggplot2)

folds=createFolds(training_set$Churn,k=10)
cv=lapply(folds,function(x){
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier=glm(formula=Churn ~ .,
                  data=training_fold,
                  family=binomial)
  newpred<-predict(classifier,type="response",newdata=test_fold)
  newpred<-(newpred>=0.5)
  cm_k<-table(actual=test_fold$Churn,newpred)
  acc = (cm_k[1,1] + cm_k[2,2]) / (cm_k[1,1] + cm_k[2,2] + cm_k[1,2] + cm_k[2,1])
  return(acc)
})

accuracy_k= mean((as.numeric(cv)))*100

          


