#Chris Magnemi HW3 - Data Mining

#                  ----------------- Part 1 ------------------
tu <- read.csv('tuComplete.csv')

#convert eventResponse from numeric to factor
tu$eventResponse <- factor(tu$eventResponse)

#shuffle the rows
tuShuffled <- tu[sample(nrow(tu)),]

#trainig set, rows 1 to 2000
tuTrain <- tuShuffled[1:20000,]
#test set, rest of the rows
tuTest <- tuShuffled[20001:length(tu$eventResponse),]

#                ---------------- Part 2 ----------------------

#build probablistic models
#logistic regression
logit <- glm(eventResponse ~., data=tuTrain, family="binomial")
#Naive Bayes
library(e1071)
nb <- naiveBayes(eventResponse~., data=tuTrain)
#Decision tree
library(party)
tree <- ctree(eventResponse~., data=tuTrain)

#evaluate results using the test set
# logistic evaluation
logit_probs <- predict(logit,tuTest, type = "response")
#                 QUESTION -> need predicted class because logistic regressoin gives probabilites?
logit_preds <- ifelse(logit_probs > 0.5,1,0)
logit_cm <-table(logit_preds,tuTest$eventResponse)
logit_cm
logitAccuracy <- (logit_cm[1,1]+logit_cm[2,2])/sum(logit_cm)
logitAccuracy

# Naive Bayes evaluation
nb_preds <- predict(nb, tuTest)
nb_cm <- table(nb_preds, tuTest$eventResponse)
nb_cm
nb_accuracy <- (nb_cm[1,1] + nb_cm[2,2])/sum(nb_cm)
nb_accuracy

# Decision Tree evalutation
tree_preds <- predict(tree, tuTest)
tree_cm <- table(tree_preds, tuTest$eventResponse)
tree_cm
tree_accuracy <- (tree_cm[1,1]+tree_cm[2,2])/sum(tree_cm)
tree_accuracy

# the logistic & decision tree models are extremely close,
# but the decision tree is slightly more accurate by hundreths of a percent


# ------------- Part 3 -----------------

svmModel <- svm(eventResponse~.,data=tuTrain)
svm_preds <- predict(svmModel,tuTest)
svm_cm <- table(svm_preds, tuTest$eventResponse)
svm_cm
svm_accuracy <- (svm_cm[1,1]+svm_cm[2,2])/sum(svm_cm)
svm_accuracy

# the svm performs is extremely similar to the logistic and decision tree models,
# yet is slightly less accurate by a tenth of a percent

