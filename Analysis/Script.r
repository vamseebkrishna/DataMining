#Classification using information-gain
library(caret)
library(dplyr)
library(rpart.plot)
library(rpart)

total_dataset = read.csv("census-adult.txt", sep = ",", header = FALSE, na.strings = " ?")
clean_dataset = na.omit(total_dataset)
colnames(clean_dataset) <- c("age", "workclass", "fnlwgt", "education", "education_num",
                             "marital_status", "occupation", "relationship", "race",
                             "sex", "capital_gain", "capital_loss", "hours_per_week",
                             "native_country", "income")
colnames(clean_dataset) <- make.names(colnames(clean_dataset))
set.seed(25)
new_index = sample(1:nrow(clean_dataset), 4000)
sample_datset = clean_dataset[new_index,]
train_index <- sample(1:nrow(sample_datset), 0.6 * nrow(sample_datset))
test_index <- setdiff(1:nrow(sample_datset), train_index)

train_set = sample_datset[train_index,]
test_set = sample_datset[test_index,]
# dim(train_set)
# dim(test_set)

dtree <- rpart(income~., data = train_set, method = 'class', parms = list(split = 'information'))

predict_set <- predict(dtree, test_set, type = "class")

rpart.plot(dtree)

confusion_set <- table(test_set$income, predict_set)

print(confusion_set)

recall <- confusion_set[1,1]/sum(confusion_set[1,1], confusion_set[2,1])
print(recall)
precision <- confusion_set[1,1]/sum(confusion_set[1,1], confusion_set[1,2])
print(precision)
F1_score <- (1+1^2)*precision*recall/((1^2*precision)+recall)
print(F1_score)

accuracy <- sum(diag(confusion_set))/sum(confusion_set)
print(accuracy)

printcp(dtree)

importance <- varImp(dtree)
print(importance)



#With-holding one of the attributes 'race' 

set.seed(25)
index = sample(1:nrow(clean_dataset), 4000)
sample_datset = clean_dataset[index,]

new_dataset = select(sample_datset, -c(race))
new_train_index <- sample(1:nrow(new_dataset), 0.7 * nrow(new_dataset))
new_test_index <- setdiff(1:nrow(new_dataset), new_train_index)

new_train_set = new_dataset[new_train_index,]
new_test_set = new_dataset[new_test_index,]


new_dtree <- rpart(income~., data = new_train_set, method = 'class')

new_predict_set <- predict(new_dtree, new_test_set, type = "class")

rpart.plot(new_dtree)

new_confusion_set <- table(new_test_set$income, new_predict_set)

print(new_confusion_set)

recall <- new_confusion_set[1,1]/sum(new_confusion_set[1,1], new_confusion_set[2,1])
print(recall)
precision <- new_confusion_set[1,1]/sum(new_confusion_set[1,1], new_confusion_set[1,2])
print(precision)
F1_score <- (1+1^2)*precision*recall/((1^2*precision)+recall)
print(F1_score)

accuracy <- sum(diag(new_confusion_set))/sum(new_confusion_set)
print(accuracy)


#Classification using GINI Index

library(caret)
library(dplyr)
library(rpart.plot)
library(rpart)

total_dataset = read.csv("census-adult.txt", sep = ",", header = FALSE, na.strings = " ?")
clean_dataset = na.omit(total_dataset)
colnames(clean_dataset) <- c("age", "workclass", "fnlwgt", "education", "education_num",
                             "marital_status", "occupation", "relationship", "race",
                             "sex", "capital_gain", "capital_loss", "hours_per_week",
                             "native_country", "income")
colnames(clean_dataset) <- make.names(colnames(clean_dataset))
set.seed(25)
index = sample(1:nrow(clean_dataset), 4000)
sample_datset = clean_dataset[index,]
train_index <- sample(1:nrow(sample_datset), 0.7 * nrow(sample_datset))
test_index <- setdiff(1:nrow(sample_datset), train_index)

train_set = sample_datset[train_index,]
test_set = sample_datset[test_index,]
# dim(train_set)
# dim(test_set)
dtree <- rpart(income~., data = train_set, method = 'class', parms = list(split = 'GINI'))

predict_set <- predict(dtree, test_set, type = "class")

rpart.plot(dtree)

confusion_set <- table(test_set$income, predict_set)

print(confusion_set)

recall <- confusion_set[1,1]/sum(confusion_set[1,1], confusion_set[2,1])
print(recall)
precision <- confusion_set[1,1]/sum(confusion_set[1,1], confusion_set[1,2])
print(precision)
F1_score <- (1+1^2)*precision*recall/((1^2*precision)+recall)
print(F1_score)

accuracy <- sum(diag(confusion_set))/sum(confusion_set)
print(accuracy)

printcp(dtree)

importance <- varImp(dtree)
print(importance)


#With-holding one of the attributes 'native_country'


set.seed(25)
index = sample(1:nrow(clean_dataset), 4000)
sample_datset = clean_dataset[index,]

new_dataset = select(sample_datset, -c(native_country))
new_train_index <- sample(1:nrow(new_dataset), 0.8 * nrow(new_dataset))
new_test_index <- setdiff(1:nrow(new_dataset), new_train_index)

new_train_set = new_dataset[new_train_index,]
new_test_set = new_dataset[new_test_index,]


new_dtree <- rpart(income~., data = new_train_set, method = 'class')

new_predict_set <- predict(new_dtree, new_test_set, type = "class")

rpart.plot(new_dtree)

new_confusion_set <- table(new_test_set$income, new_predict_set)

print(new_confusion_set)

recall <- new_confusion_set[1,1]/sum(new_confusion_set[1,1], new_confusion_set[2,1])
print(recall)
precision <- new_confusion_set[1,1]/sum(new_confusion_set[1,1], new_confusion_set[1,2])
print(precision)
F1_score <- (1+1^2)*precision*recall/((1^2*precision)+recall)
print(F1_score)

accuracy <- sum(diag(new_confusion_set))/sum(new_confusion_set)
print(accuracy)



#Now using the Naive-Bayes classification
library(e1071)

str(train_set)
naivebmodel <- naiveBayes(as.factor(income) ~., data = train_set)

modelPred <- predict(naivebmodel, test_set)

confusion_set <- table(modelPred, test_set$income)

print(confusion_set)


recall <- confusion_set[1,1]/sum(confusion_set[1,1], confusion_set[2,1])
print(recall)
precision <- confusion_set[1,1]/sum(confusion_set[1,1], confusion_set[1,2])
print(precision)
F1_score <- (1+1^2)*precision*recall/((1^2*precision)+recall)
print(F1_score)

accuracy <- sum(diag(confusion_set))/sum(confusion_set)
print(accuracy)



