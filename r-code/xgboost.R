# LOAD LIBRARIES
library(tidyverse)
library(readr)
library(xgboost)
library(Matrix)
library(tictoc) 
library(furrr)
library(smotefamily)

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

# SET UP FUNCTION TO EVALUATE XGBOOST
xgbFunc <- 
  function(df = winequality, samplingMethod = "none", nUndersample = 2000, kOversample = 5,
           trainPct = 0.7, max.depth, eta, nround = 2, nthread = 2, show.table = F){
  
    require(Matrix)
    require(xgboost)
    
    # set up training/testing sets
    n <- nrow(df)
    train.index <- sample(seq(1,n), floor(n*trainPct), replace = F)
    
    # create dgCMatrix for modeling
    
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
    train.datamatrix   <- sparse.model.matrix(qualityclass ~ ., data = train.data)[,-1]
    train.qualityclass <- train.data$qualityclass
    train.label        <- as.integer(train.data$qualityclass)-1 # label conversion
    xgb.train          <- list(data = train.datamatrix, label = train.label)
    
    # testing
    test.data         <- df[-train.index,] %>%     
      dplyr::select(qualityclass, type, fixed.acidity, volatile.acidity, citric.acid,
                    residual.sugar, chlorides, free.sulfur.dioxide, total.sulfur.dioxide,
                    density, pH, sulphates, alcohol)
    test.datamatrix   <- sparse.model.matrix(qualityclass ~ ., data = test.data)[,-1]
    test.qualityclass <- test.data$qualityclass
    test.label        <- as.integer(test.data$qualityclass)-1 # label conversion
    xgb.test          <- list(data = test.datamatrix, label = test.label)
    

    # fit xgboost model
    xgb.fit <- xgboost(data  = xgb.train$data,
                       label = xgb.train$label,
                       booster ="gbtree",
                       max.depth = max.depth,
                       eta = eta,
                       nround = nround,
                       nthread = nthread,
                       objective = "multi:softprob",
                       eval_metric = "mlogloss",
                       num_class = length(levels(train.qualityclass)),
                       verbose = 0
    )
    
    # predict
    xgb.pred = predict(xgb.fit, xgb.test$data, reshape = T) %>% as.data.frame()
    colnames(xgb.pred) = levels(train.qualityclass)
    xgb.pred$prediction = apply(xgb.pred, 1, function(x) colnames(xgb.pred)[which.max(x)])
    xgb.pred$label = levels(train.qualityclass)[test.label + 1]
    xgb.pred <- xgb.pred %>%
      mutate(prediction = factor(prediction, levels = c("Low", "Normal", "High")),
             label = factor(label, levels = c("Low", "Normal", "High")))
    
    # evaluated prediction
    accuracy <- mean(xgb.pred$prediction==xgb.pred$label)
    table    <- with(xgb.pred, table(prediction, label))
    
    if(show.table){
      return(list(accuracy = accuracy, table = table)) 
    } else{
      return(accuracy) 
    }
    
  }

xgbFunc(df = winequality, samplingMethod = "none", nUndersample = NA, kOversample = NA,
        trainPct = 0.7, max.depth = 20, eta = 0.0001, nround = 2, nthread = 2,
        show.table = T)

xgbFunc(df = winequality, samplingMethod = "undersample", nUndersample = 2000, kOversample = NA,
        trainPct = 0.7, max.depth = 20, eta = 0.0001, nround = 2, nthread = 2, show.table = T)

xgbFunc(df = winequality, samplingMethod = "oversample", nUndersample = NA, kOversample = 5,
        trainPct = 0.7, max.depth = 20, eta = 0.0001, nround = 2, nthread = 2, show.table = T)

# MCMC FUNCTION WITH PARALLEL COMPUTING
xgbMCMC <- 
  function(samplingMethod = "none", nUndersample = 2000, kOversample = 5,
           B = 5, trainPct = 0.7, max.depth, eta, nround = 2, nthread = 2){
    require(furrr)
    
    # Create Parameter Grid
    mcmc.grid <- expand_grid(B = seq(1,B),
                             samplingMethod = samplingMethod,
                             nUndersample = nUndersample,
                             kOversample = kOversample,
                             trainPct = trainPct,
                             max.depth = max.depth, 
                             eta = eta,
                             nround = nround, 
                             nthread = nthread)
    
    # Obtain Accuracy
    accuracyList <- furrr::future_pmap(mcmc.grid[,-1], xgbFunc)
    xgbAccuracy <- matrix(unlist(accuracyList, use.names = TRUE), nrow = nrow(mcmc.grid))
    
    # Summarize Accuracy
    results <- mcmc.grid %>%
      mutate(xgbAccuracy = xgbAccuracy) %>%
      group_by(samplingMethod, nUndersample, kOversample, max.depth, eta, nround, nthread) %>%
      summarise(B = n(),
                mean  = mean(xgbAccuracy),
                lower = quantile(xgbAccuracy, probs = c(0.05)),
                upper = quantile(xgbAccuracy, probs = c(0.95)))
    
    return(results)
    
  }

# HYPERPARAMETER GRID SEARCH
tic()
xgbMCMC.results <- xgbMCMC(samplingMethod = "none", nUndersample = NA, kOversample = NA,
                           trainPct = 0.7, B = 5, max.depth = c(10, 15, 20), eta = 1e-4, nround = 4, nthread = 2)
toc()
xgbMCMC.results %>% arrange(-mean)


tic()
xgbMCMC.results <- xgbMCMC(samplingMethod = "undersample", nUndersample = seq(500, 2000, 500), kOversample = NA,
                           trainPct = 0.7, B = 5, max.depth = 20, eta = 1e-4, nround = 4, nthread = 2)
toc()
xgbMCMC.results %>% arrange(-mean)

tic()
xgbMCMC.results <- xgbMCMC(samplingMethod = "oversample", nUndersample = NA, kOversample = 5,
                           trainPct = 0.7, B = 5, max.depth = 20, eta = 0.00001, nround = 4, nthread = 2)
toc()
xgbMCMC.results %>% arrange(-mean)


