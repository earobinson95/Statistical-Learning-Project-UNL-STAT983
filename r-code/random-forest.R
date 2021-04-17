#install.packages("randomForest")
install.packages("readr")
install.packages("tictoc")
install.packages("furrr")
install.packages("smotefamily")
library(randomForest)
library(tidyverse)
library(readr)
library(tictoc) 
library(furrr)
library(smotefamily)

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



#4.11.2021 New Approach

# IMPORT DATA, RELEVEL FACTOR COLUMNS, 
winequality <- read_csv("data/winequality-all.csv") %>%
  mutate(type = factor(type, levels = c("red", "white")),
         type01 = as.numeric(ifelse(type == "white", 0, 1)),
         qualityclass = factor(qualityclass, levels = c("Low", "Normal", "High")),
         under_class = ifelse(qualityclass == "Normal", 0, 1),
         over_class = ifelse(qualityclass == "Low", 0, 
                             ifelse(qualityclass == "Normal", 1, 2)))
colnames(winequality) <- make.names(names(winequality), unique=TRUE)
summary(winequality)

# FUNCTION FOR UNDERSAMPLING
undersample <- function(train_df, nsample){
  df_wine_0_ind <- which(train_df$under_class == 0)
  df_wine_1_ind <- which(train_df$under_class == 1)
  pick_0 <- sample(df_wine_0_ind, nsample)
  undersample_wine <- train_df[c(df_wine_1_ind,pick_0),] #Final Data frame
  undersample_wine <- undersample_wine %>%
    dplyr::select(qualityclass, type, fixed.acidity, volatile.acidity, citric.acid,
                  residual.sugar, chlorides, free.sulfur.dioxide, total.sulfur.dioxide,
                  density, pH, sulphates, alcohol)
  # table(undersample_wine$under_class) # have just to make sure it's all balancing out how I think
  return(undersample_wine)
}

# FUNCTION FOR OVERSAMPLING
oversample <- function(train_df, k){
  
  winequality_low <- filter(winequality, qualityclass %in% c('Low'))
  winequality_normal <- filter(winequality, qualityclass %in% c('Normal'))
  winequality_high <- filter(winequality, qualityclass %in% c('High'))
  
  wine <- sort(sample(nrow(winequality_normal), nrow(winequality_normal)*0.5))
  winequality_norm1 <- winequality_normal[wine,]
  winequality_norm2 <- winequality_normal[-wine,]
  
  wine_LN <- rbind(winequality_low,winequality_norm1)
  wine_HN <- rbind(winequality_high,winequality_norm2)
  SMOTEData1 <- SMOTE(wine_LN[,c("fixed.acidity", "volatile.acidity", "citric.acid",
                                 "residual.sugar", "chlorides", "free.sulfur.dioxide",
                                 "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol",
                                 "type01")], wine_LN[,"over_class"], K = k, dup_size = 0)
  SMOTEData2 <- SMOTE(wine_HN[,c("fixed.acidity", "volatile.acidity", "citric.acid",
                                 "residual.sugar", "chlorides", "free.sulfur.dioxide",
                                 "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol",
                                 "type01")], wine_HN[,"over_class"], K = k, dup_size = 0)
  oversample_df1 <- SMOTEData1$data #Final data frame
  oversample_df2 <- SMOTEData2$data #Final data frame
  oversample_df <- rbind(oversample_df1,oversample_df2) %>%
    mutate(type01 = round(type01)) %>%
    mutate(type = as.factor(ifelse(type01 == 0, "white","red")),
           qualityclass = ifelse(class == 0, "Low", 
                                 ifelse(class == "1", "Normal","High"))) %>%
    mutate(qualityclass = factor(qualityclass, levels = c("Low", "Normal", "High"))) %>%
    dplyr::select(qualityclass, type, fixed.acidity, volatile.acidity, citric.acid,
                  residual.sugar, chlorides, free.sulfur.dioxide, total.sulfur.dioxide,
                  density, pH, sulphates, alcohol)
  #table(oversample_df$class) have just to make sure it's all balancing out how I think 
  return(oversample_df)
}

# SET UP FUNCTION TO EVALUATE RANDOM FOREST
#Need to add mtry variability to functions for evaluation
rfFunc <- 
  function(df = winequality, samplingMethod = "none", nUndersample = 2000, kOversample = 5,
           trainPct = 0.7, importance = F, mtry = 4, ntree = 500, show.table = F){
    
    require(randomForest)
  
    # set up training/testing sets
    n <- nrow(df)
    train.index <- sample(seq(1,n), floor(n*trainPct), replace = F)
    
    # training
    train.data <-  df[train.index,] # Normal
    
    if(samplingMethod == "undersample"){
      train.data <- undersample(train.data, nUndersample) # Undersample
    }
    
    if(samplingMethod == "oversample"){
      train.data <- oversample(train.data, kOversample) # Oversample
    }
    
    train.data <- train.data %>%  
      dplyr::select(qualityclass, type, fixed.acidity, volatile.acidity, citric.acid,
                    residual.sugar, chlorides, free.sulfur.dioxide, total.sulfur.dioxide,
                    density, pH, sulphates, alcohol)
    # train.qualityclass <- train.data$qualityclass #I don't think I need this SJA
    # train.label        <- as.integer(train.data$qualityclass)-1 # label conversion
    # rf.train           <- list(data = train.data, label = train.label)

    # testing
    test.data  <- df[-train.index,] %>%     
      dplyr::select(qualityclass, type, fixed.acidity, volatile.acidity, citric.acid,
                    residual.sugar, chlorides, free.sulfur.dioxide, total.sulfur.dioxide,
                    density, pH, sulphates, alcohol)
    # test.qualityclass <- test.data$qualityclass
    # test.label        <- as.integer(test.data$qualityclass)-1 # label conversion
    # rf.test           <- list(data = test.data, label = test.label)

    # Fit that Random Forest!
    rf.fit <- randomForest(qualityclass ~ ., 
                           data=train.data,
                           method="class", 
                           ntree=ntree,
                           mtry = mtry,
                           importance=importance
                           )
    # rf.fit
    
    # Get that Prediction! I don't think you need to reshape, you want a vector of predicted values
    rf.pred = predict(rf.fit, newdata = test.data) 
    # length(rf.pred) # Should be nrow(test.data) x 1
    
    ### THIS IS SPECIFIC TO XGBOOST DON"T NEED - I think I need something from below, issue with dimensionality of pred SJA,
    # colnames(rf) = levels(train.qualityclass)
    # rf.pred$prediction = apply(rf.pred, 1, function(x) colnames(rf.pred)[which.max(x)])
    # rf.pred$label = levels(train.qualityclass)[test.label + 1]
    # rf.pred <- rf.pred %>%
    #   mutate(prediction = factor(prediction, levels = c("Low", "Normal", "High")),
    #          label = factor(label, levels = c("Low", "Normal", "High")))
    #####
    
    # Evaluate Prediction
    accuracy.all  <- mean(rf.pred==test.data$qualityclass) #this is not working SJA
    table    <- table(test.data$qualityclass, rf.pred)
    prop.table <- table/rowSums(table)
    accuracy.low    <- prop.table[1,1]
    accuracy.normal <- prop.table[2,2]
    accuracy.high   <- prop.table[3,3]
    accuracy <- cbind(accuracy.all, accuracy.low, accuracy.normal, accuracy.high)
    
    if(show.table){
      return(list(accuracy = accuracy, table = table, prop.table = prop.table)) 
    } else{
      return(accuracy) 
    }
    
  }

rf.none.results <- rfFunc(df = winequality, samplingMethod = "none", nUndersample = NA, kOversample = NA, trainPct = 0.7, ntree = 500, mtry = 4, show.table = T)
rf.none.results 

#not reviewed below SJA

rf.undersample.results <- rfFunc(df = winequality, samplingMethod = "undersample", nUndersample = 2000, kOversample = NA, trainPct = 0.7, ntree = 500, mtry = 4, show.table = T)
rf.undersample.results

rf.oversample.results <- rfFunc(df = winequality, samplingMethod = "oversample", nUndersample = NA, kOversample = 5, trainPct = 0.7, ntree = 500, mtry = 4, show.table = T)
rf.oversample.results

rf.results <- rbind(rf.none.results$accuracy,
                     rf.undersample.results$accuracy,
                     rf.oversample.results$accuracy)
rownames(rf.results) <- c("None", "Undersampling", "Oversampling")

rf.results %>%
  as.data.frame() %>%
  rownames_to_column("SampleMethod") %>%
  pivot_longer(cols = c("accuracy.all", "accuracy.low", "accuracy.normal", "accuracy.high"),
               names_to = "AccuracyGroup",
               values_to = "Accuracy") %>%
  mutate(SampleMethod = factor(SampleMethod, levels = c("Oversampling", "Undersampling", "None")),
         AccruacyGroup = factor(AccuracyGroup, levels = c("accuracy.all", "accuracy.low", "accuracy.normal", "accuracy.high"))) %>%
ggplot(aes(x = Accuracy, y = SampleMethod)) +
  geom_point() +
  facet_wrap(~ AccuracyGroup, ncol = 1) +
  theme_bw() +
  theme(aspect.ratio = 0.2) +
  scale_y_discrete("") +
  scale_x_continuous("Classification Rate", limits = c(0,1), breaks = seq(0,1,0.2), labels = scales::percent)

# MCMC FUNCTION WITH PARALLEL COMPUTING
rfMCMC <- 
  function(B = 5, samplingMethod = "none", nUndersample = 2000, kOversample = 5, trainPct = 0.7, ntree = 500, mtry = 4){
    require(furrr)
    
    # Create Parameter Grid
    mcmc.grid <- expand_grid(B = seq(1,B),
                             samplingMethod = samplingMethod,
                             nUndersample = nUndersample,
                             kOversample = kOversample,
                             trainPct = trainPct,
                             ntree = ntree,
                             mtry = mtry)
    
    # Obtain Accuracy
    accuracyList <- furrr::future_pmap(mcmc.grid[,-1], rfFunc)
    rfAccuracy <- matrix(unlist(accuracyList, use.names = TRUE), ncol = 4, nrow = nrow(mcmc.grid), byrow = T)
    colnames(rfAccuracy) <- colnames(accuracyList[[1]])
    
    # Summarize Accuracy
    results <- cbind(mcmc.grid, rfAccuracy) %>%
      pivot_longer(cols = c("accuracy.all", "accuracy.low", "accuracy.normal", "accuracy.high"),
                   names_to = "accuracyGroup",
                   values_to = "accuracy") %>%
      mutate(Method = "Random Forest") %>%
      dplyr::group_by(Method, accuracyGroup, samplingMethod, nUndersample, kOversample, ntree, mtry) %>%
      summarise(B = n(),
                mean  = mean(accuracy),
                lower = quantile(accuracy, probs = c(0.05)),
                upper = quantile(accuracy, probs = c(0.95))) %>%
      ungroup()
    
    return(results)
    
  }


# HYPERPARAMETER GRID SEARCH
tic()
rfMCMC.none.results <- rfMCMC(B = 50, samplingMethod = "none", nUndersample = NA, kOversample = NA,
                                trainPct = 0.7, ntree = 500, mtry = 4)
toc()
# rfMCMC.none.results

tic()
rfMCMC.undersample.results <- rfMCMC(B = 50, samplingMethod = "undersample", nUndersample = 2000, kOversample = NA,
                                      trainPct = 0.7, ntree = 500, mtry = 4)
toc()
# rfMCMC.undersample.results

tic()
rfMCMC.oversample.results <- rfMCMC(B = 50, samplingMethod = "oversample", nUndersample = NA, kOversample = 5,
                                    trainPct = 0.7, ntree = 500, mtry = 4)
toc()
# rfMCMC.oversample.results


rfMCMC.results <- rbind(rfMCMC.none.results,
                        rfMCMC.undersample.results,
                        rfMCMC.oversample.results)

rfMCMC.results %>%
  mutate(samplingMethod = factor(samplingMethod, levels = c("none", "undersample", "oversample")),
         accuracyGroup = factor(accuracyGroup, levels = c("accuracy.all", "accuracy.low", "accuracy.normal", "accuracy.high"))) %>%
  ggplot(aes(x = mean, y = samplingMethod)) +
  geom_point() +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.2) +
  facet_wrap(~accuracyGroup, ncol = 1) +
  theme_bw() +
  theme(aspect.ratio = 0.2) +
  scale_y_discrete("") +
  scale_x_continuous("Accuracy", limits = c(0,1), breaks = seq(0,1,0.2), labels = scales::percent) +
  ggtitle("Random Forest \nntree = 500; mtry = 4")
