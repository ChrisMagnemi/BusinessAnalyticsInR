#Assignment 2
tu <- read.csv("tuComplete.csv")
#Q1
# used a logistic model, since the response variable is binary
logit <- glm(eventResponse ~ responses + topics,data = tu, family="binomial")
summary(logit)
#Q2
logit2 <- glm(eventResponse ~ responses+topics+entropy+yearJoined+firstDayResponse+avgGap+avgStd+oneOrZeroResponses+
                responsesInDifferentTopics,data = tu, family="binomial")
summary(logit2)
#Q3
# the second model has the better fit with an AIC score of 9816.3, compared to 10296 in the first model
#Q4
# For the best model, the significant variables are:
# the intercept, responses, topics, yearJoined, avgStd, and oneOrZeroResponses
#Q5
sdTu <- as.data.frame(scale(tu[,c("responses", "topics","entropy", "yearJoined","firstDayResponse","avgGap","avgStd","oneOrZeroResponses","responsesInDifferentTopics")]), row.names=NULL)
sdTu$eventResponse <- tu$eventResponse
logit3 <- glm(eventResponse ~ responses+topics+entropy+yearJoined+firstDayResponse+avgGap+avgStd+oneOrZeroResponses+
                responsesInDifferentTopics,data = sdTu, family="binomial")
summary(logit3)
#the coefficients are 

head(tu$eventResponse)
test <- tu[tu$eventResponse==1,]
nrow(tu[tu$eventResponse==1,])
tu$probab <- (max(tu$firstDayResponse) - min(tu$firstDayResponse)) / max(tu$firstDayResponse)
