######Resampling Methods#####
winequality <- read.csv("C:/Users/AlisonPC/OneDrive - University of Nebraska-Lincoln/Documents/Statistical-Learning-Project-UNL-STAT983/data/winequality-all.csv")


### Oversampling Method ###
#SMOTE (NOT WORKING YET)
library(peformanceEstimation)
#not same package they used (doesn't exist in my version of R)
#Try reverting to an old version of R and use that?


#Code From Paper (Classification of Wine Quality with Imabalanced Data Set)
table(winequality $ binary_class) #0.06833923 from rare class (should be close to same for training)

#SMOTE algorithm is only for binary classification
#So what paper did was combined low and high into one category and normal in the other and then ran through algorithm
#Still did the three-way classification later with the classification techniques.

#Add Binary Classification variable
winequality_rare <- filter(winequality, qualityclass %in% c('Low', 'High'))
winequality_rare$binary_class <- 1
winequality_normal <- filter(winequality, qualityclass %in% c('Normal'))
winequality_normal$binary_class <- 0

winequality <- rbind(winequality_normal,winequality_rare)


#Generate a random training and testing data set
#defintely can change this later, just needed something for now
set.seed(44)
wine <- winequality[,-c(1:3)]
wine2 <- sort(sample(nrow(wine), nrow(wine)*0.5))
train_wine <- wine[wine2,]
table(train_wine $ binary_class) #0.06773399 in rare class (not bad)

#Attempt 1: library(peformanceEstimation)
names(wine)
form <- formula(binary_class ~ .)
SMOTEData <- smote(form,train_wine,k=4,perc.under = 100, perc.over = 100)


table(SMOTEData$binary_class) 

#currently it's not adding any to minority set, but its adding a ton to majority set

#Attempt 2 library(imbalance) - This just generate the samples we would add to data set 
#so numInstances = 100, samples minority class 100 times. Just add this to dataset?

SMOTEData <- mwmote(
  train_wine,
  numInstances = 100,
  kNoisy = 5,
  threshold = 5,
  cmax = 2,
  cclustering = 3,
  classAttr = "binary_class"
)
table(SMOTEData$binary_class) 

#Attempt 3 library(smotefamily) - in my opion this is the winner
#https://www.rdocumentation.org/packages/smotefamily/versions/1.3.1/topics/SMOTE

SMOTEData <- SMOTE(train_wine[,c(1:11)], train_wine[,12], K = 5, dup_size = 0)

data <- SMOTEData$data

table(data$class) 

### Undersampling Method ###