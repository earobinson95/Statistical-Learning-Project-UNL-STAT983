install.packages("randomForest")
library(randomForest)
rfNews()

set.seed(220355)

#Import data
winequality <- read.csv("../data/winequality-all.csv")

#Drop quality variable
var.out <- !names(winequality) %in% c("quality")
winequality <- winequality[,var.out]

#Delineate testing and training data
n = dim(winequality)
n1 = round(n/3.333)
flag = sort(sample(1:n,n1))
winequalitytest = winequality[flag,]
winequalitytrain = winequality[-flag,]

#Create Loop

#Fit Random Forest Model with Training Data
model1 <- randomForest(qualityclass ~ ., method="class", data=winequalitytrain) 
model1
plot(model1)
varImpPlot(model1) 

#Tune the model
model1_tuned <- tuneRF(x = winequalitytrain[,-1], y = winequalitytrain$qualityclass, ntreeTry = 500, mtryStart=3, stepFactor = 1,
                       improve = 0.01, trace = FALSE)

#Obtain Misclassification
predTest <- predict(model1_tuned, winequalitytrain, type="class") 
mean(predTest == winequalitytrain$qualityclass) 
table(predTest, winequalitytrain$qualityclass) 
importance(model1_tuned)
varImpPlot(model1_tuned)

#Fit Random Forest Model with Test Data 
predTrain <- predict(model1_tuned, winequalitytest, type="class")
mean(predTrain == winequalitytest$qualityclass) 
table(predTrain, winequalitytest$qualityclass) 
importance(model1_tuned)
varImpPlot(model1_tuned)
