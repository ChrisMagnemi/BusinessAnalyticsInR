#R Assigment 1
# ------ Variables and Assignments ---------
#Q1
chrisList <- c(14,12,40,50,67, 55, 32,89,200,250,210,209,199,206,256,276, 300, 350,333,290,265,237, 990, 1000,1500, 2000,4000,10000,20000)
#Q2
hist(chrisList, breaks = 100, col="red")
#Q3
chrisLogList <- log(chrisList)
#Q4
hist(chrisLogList, breaks = 100, col="red")
#Q5a
standardLogList <- scale(chrisLogList)
#Q5b
hist(standardLogList, breaks = 100, col = "red")
#Q6
meanStandardLogList = mean(standardLogList)
meanStandardLogList
stdStandardLogList = sd(standardLogList)
stdStandardLogList
#--------- Logic ------------
#Q1
X <- 5
#Q2
Y <- 10
#Q3
if (Y > X){
  X<-Y 
} else{
  Y<-X
}
#----------DataFrames -------
#Q1 
tudata <- read.csv("tuSample.csv")
head(tudata)
tail(tudata)
#Q2
entropyMean = mean(tudata$entropy)
entropyMean
entropystd = sd(tudata$entropy)
entropystd
#Q3
length(tudata$user)
length(unique(tudata$user))
#Q4
contributorsdf <- tudata[tudata$majorContributor == 1,]
#Q5
lurkersdf <- tudata[tudata$majorContributor == 0,]
#Q6
contributorsMeanNumResponses <- mean(contributorsdf$responses)
contributorsMeanNumTopics <- mean(contributorsdf$topics)
lurkersMeanNumResponses <- mean(lurkersdf$responses)
lurkersMeanNumTopics <- mean(lurkersdf$topics)
# Lurkers respond almost twice as much as contributors
# Contributors have only about .2 more topics than lurkers on average
#Q7
cleanlurkersdf <- lurkersdf[lurkersdf$responses <= ((lurkersMeanNumResponses) + (3*sd(lurkersdf$responses))),]
cleancontributorsdf <- contributorsdf[contributorsdf$responses <= contributorsMeanNumResponses + 3*sd(contributorsdf$responses),]
#Q8
finaldf <- rbind(cleancontributorsdf,cleanlurkersdf)
#Q9
finaldf$standardTopics <- scale(finaldf$topics)
#Q10
finaldf$normalTopics <- (finaldf$topics - min(finaldf$topics))/(max(finaldf$topics)-min(finaldf$topics))
#Q11
write.csv(finaldf, "ChrisMagnemiFinaldf.csv")
