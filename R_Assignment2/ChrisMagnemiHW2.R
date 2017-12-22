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
logit2_normalizedVars <- glm(eventResponse ~ responses+topics+entropy+yearJoined+firstDayResponse+avgGap+avgStd+oneOrZeroResponses+
                responsesInDifferentTopics,data = sdTu, family="binomial")
summary(logit2_normalizedVars)
#------ Coefficients ----------
# the coefficients are different since the variables have been normalized, 
# but there is no significant reason for the value changes. They are different
# simply because the variable values changed. The regression is still the same.

#Q6
library(mfx)
logitmfx(eventResponse ~ responses+topics+entropy+yearJoined+firstDayResponse+avgGap+avgStd+oneOrZeroResponses+
           responsesInDifferentTopics,data = sdTu)
#------- Mfx explanations ---------
# note that each marginel effect is taken as all other things equal
# Independent var: effect explanation
# responses: for every unit increase in response, the probability of eventResponse being a 1 increases by 0.00363527
# topics: for every unit increase in topic, the probability of eventResponse being a 1 increases by 0.00326635
# entropy: for every unit increase in entropy, the probability of eventResponse being a 1 decreases by 0.00026877
# yearJoined: for every unit increase in this variable, the probability of eventResponse being a 1 decreases by 0.01588276
# firstDayResponse: for every unit increase in this variable, the probability of eventResponse being a 1 decreases by 0.00153766
# avgGap: for every unit increase in this variable, the probability of eventResponse being a 1 increases by 0.00136925
# avgStd: for every unit increase in this variable, the probability of eventResponse being a 1 decreases by 0.01058970
# oneOrZeroResponses: for every unit increase in this variable, the probability of eventResponse being a 1 decreases by 0.02380851
# responsesInDifferentTopics: for every unit increase in this variable, the probability of eventResponse being a 1 increases by 0.00078990

#Q7
#here I am running a Poisson regression
logit3 <- glm(responses ~ eventResponse+topics+entropy+yearJoined+avgGap+avgStd+oneOrZeroResponses+
                               responsesInDifferentTopics,data = tu, family="poisson")
summary(logit3)

#Q8
poissonmfx(responses ~ eventResponse+topics+entropy+yearJoined+avgGap+avgStd+oneOrZeroResponses+
             responsesInDifferentTopics,data = tu)
#------- Mfx explanations ---------
# note that each marginel effect is taken as all other things equal
# eventResponse: for ever unit increase in this variable, response will increase by 3.39233*10^-3
# topics: for every unit increase in this variable, response will decrease by 6.7119*10^-4
# entropy: for every unit increase in this variable, response will increase by 1.1553*10^-2
# yearJoined: for every unit increase in this variable, response will decrease by 4.8117*10^-4
# avgGap: for every unit increase in this variable, response will decrease by 9.4715*10^-5
# avgStd: for every unit increase in this variable, response will decrease by 1.7134*10^-3
# oneOrZeroResponses: for every unit increase in this variable, response will decrease by 3.5890*10^-2
# responssInDifferentTopics: for every unit increase in this variable, response will increase by 1.3813*10^-3










