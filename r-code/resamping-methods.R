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
winequality_rare$rare_class <- 1
winequality_normal <- filter(winequality, qualityclass %in% c('Normal'))
winequality_normal$rare_class <- 0

winequality <- rbind(winequality_normal,winequality_rare)
write.csv(winequality, "winequality_binary.csv")

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

#Manual Method (How many would we want in the other group?)
#https://rstudio-pubs-static.s3.amazonaws.com/607601_57a11284917f4d79933f4c4db3d41713.html
#Pick how many we want in the majority class

df_wine_0_ind <- which(train_wine$binary_class == 0)
df_wine_1_ind <- which(train_wine$binary_class == 1)

### setting negative counts to be same as positive counts - so that the data is balanced
nsample <- 2000
pick_0 <- sample(df_wine_0_ind, nsample)

undersample_wine <- train_wine[c(df_wine_1_ind, pick_0), ]

table(undersample_wine$binary_class) 

#Can also use ovun.sample to do random oversampling and undersampling
#https://s3.amazonaws.com/assets.datacamp.com/production/course_8916/slides/chapter3.pdf


#################
###Functions for Resampling Methods
#################

#Undersampling

undersample <- function(train_df, nsample){
df_wine_0_ind <- which(train_df$rare_class == 0)
df_wine_1_ind <- which(train_df$rare_class == 1)
pick_0 <- sample(df_wine_0_ind, nsample)
undersample_wine <- train_df[c(df_wine_1_ind,pick_0),] #Final Data frame
return(undersample_wine)
}

train_un <- undersample(winequality,444)


#Oversampling (using smotefamily package)

oversample <- function(train_df, k){

  set.seed(44)
  wine<- sort(sample(nrow(winequality_normal), nrow(winequality_normal)*0.5))
  winequality_norm1 <- winequality_normal[wine,]
  winequality_norm2 <- winequality_normal[-wine,]
  wine_LN <- rbind(winequality_low,winequality_norm1)
  wine_HN <- rbind(winequality_high,winequality_norm2)
  SMOTEData1 <- SMOTE(wine_LN[,c(3:14)], wine_LN[,15], K = k, dup_size = 0)
  SMOTEData2 <- SMOTE(wine_HN[,c(3:14)], wine_HN[,15], K = k, dup_size = 0)
  oversample_df1 <- SMOTEData1$data #Final data frame
  oversample_df2 <- SMOTEData2$data #Final data frame
  oversample_df <- rbind(oversample_df1,oversample_df2)
  #table(oversample_df$class) have just to make sure it's all balancing out how I think 
  return(oversample_df)
}

#Just need the training data set and number of nearest neighbors
#This might take a while to run if have a bunch of different samples running
train.data <- oversample(winequality, 5)

#Should generate training and testing sets from winequality_binary.csv

######################333
#Do separately for low and high
winequality_low <- filter(winequality, qualityclass == "Low")
winequality_norm <- filter(winequality, qualityclass == "Normal")
winequality_high <- filter(winequality, qualityclass == "High")


set.seed(44)

wine<- sort(sample(nrow(winequality_norm), nrow(winequality_norm)*0.5))
winequality_norm1 <- winequality_norm[wine,]
winequality_norm2 <- winequality_norm[-wine,]

wine_LN <- rbind(winequality_low,winequality_norm1)
wine_HN <- rbind(winequality_high,winequality_norm2)

train.data_LH <- oversample(wine_LN, 5)
train.data_HN <- oversample(wine_HN, 5)

train <- rbind(train.data_HN,train.data_LH)

table(train$class)



###########NEED to Run this first - function references some of this stuff#########

winequality[winequality$type == "red",]$type = 1
winequality[winequality$type == "white",]$type = 0
winequality$type <- as.numeric(winequality$type)
winequality_low <- filter(winequality, qualityclass %in% c('Low'))
winequality_low$binary_class <- 0
winequality_normal <- filter(winequality, qualityclass %in% c('Normal'))
winequality_normal$binary_class <- 1
winequality_high <- filter(winequality, qualityclass %in% c('High'))
winequality_high$binary_class <- 2 
winequality <- rbind(winequality_normal,winequality_low, winequality_high)
winequality_rare <- filter(winequality, qualityclass %in% c('Low', 'High'))
winequality_rare$rare_class <- 1
winequality_normal <- filter(winequality, qualityclass %in% c('Normal'))
winequality_normal$rare_class <- 0
winequality <- rbind(winequality_normal, winequality_rare)


oversample <- function(train_df, k){
  set.seed(44)
  wine<- sort(sample(nrow(winequality_normal), nrow(winequality_normal)*0.5))
  winequality_norm1 <- winequality_normal[wine,]
  winequality_norm2 <- winequality_normal[-wine,]
  wine_LN <- rbind(winequality_low,winequality_norm1)
  wine_HN <- rbind(winequality_high,winequality_norm2)
  SMOTEData1 <- SMOTE(wine_LN[,c(3:14)], wine_LN[,15], K = k, dup_size = 0)
  SMOTEData2 <- SMOTE(wine_HN[,c(3:14)], wine_HN[,15], K = k, dup_size = 0)
  oversample_df1 <- SMOTEData1$data #Final data frame
  oversample_df2 <- SMOTEData2$data #Final data frame
  oversample_df <- rbind(oversample_df1,oversample_df2)
  #table(oversample_df$class) have just to make sure it's all balancing out how I think 
  return(oversample_df)
}

#Just need the training data set and number of nearest neighbors
#This might take a while to run if have a bunch of different samples running
train.data <- oversample(winequality, 5)

undersample <- function(train_df, nsample){
  df_wine_0_ind <- which(train_df$rare_class == 0)
  df_wine_1_ind <- which(train_df$rare_class == 1)
  pick_0 <- sample(df_wine_0_ind, nsample)
  undersample_wine <- train_df[c(df_wine_1_ind,pick_0),] #Final Data frame
  return(undersample_wine)
}

train_un <- undersample(winequality,444)
