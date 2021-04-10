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
         qualityclass = factor(qualityclass, levels = c("Low", "Normal", "High")),
         binary_class = ifelse(qualityclass == "Normal", 0, 1))
colnames(winequality) <- make.names(names(winequality), unique=TRUE)
summary(winequality)

# FUNCTION FOR UNDERSAMPLING
undersample <- function(train_df, nsample){
  df_wine_0_ind <- which(train_df$binary_class == 0)
  df_wine_1_ind <- which(train_df$binary_class == 1)
  pick_0 <- sample(df_wine_0_ind, nsample)
  undersample_wine <- train_df[c(df_wine_1_ind,pick_0),] #Final Data frame
  #table(undersample_wine$binary_class) have just to make sure it's all balancing out how I think
  return(undersample_wine)
}

oversample <- function(train_df, k){
  require(smotefamily)
  SMOTEData <- SMOTE(train_df[,c(4:14)], train_df[,15], K = k, dup_size = 0)
  oversample_df <- SMOTEData$data #Final data frame
  #table(oversample_df$class) have just to make sure it's all balancing out how I think 
  return(oversample_df)
}

poo <- oversample(winequality, k = 5) # does not give you the quality class or wine type???

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
    
    train.data <- train.data %>% select(-binary_class, -quality)  
    train.datamatrix   <- sparse.model.matrix(qualityclass ~ ., data = train.data)[,-1]
    train.qualityclass <- train.data$qualityclass
    train.label        <- as.integer(train.data$qualityclass)-1 # label conversion
    xgb.train          <- list(data = train.datamatrix, label = train.label)
    
    # testing
    test.data         <- df[-train.index,] %>% select(-binary_class, -quality)
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
    table <- with(xgb.pred, table(prediction, label))
    
    if(show.table){
      return(list(accuracy = accuracy, table = table)) 
    } else{
      return(accuracy) 
    }
    
  }

xgbFunc(df = winequality, samplingMethod = "none", nUndersample = 2000, kOversample = 5,
        trainPct = 0.7, max.depth = 20, eta = 0.0001, nround = 2, nthread = 2,
        show.table = T)

xgbFunc(df = winequality, samplingMethod = "undersample", nUndersample = 2000, kOversample = 5,
        trainPct = 0.7, max.depth = 20, eta = 0.0001, nround = 2, nthread = 2, show.table = T)

# Does not work right now...
xgbFunc(df = winequality, samplingMethod = "oversample", nUndersample = 2000, kOversample = 5,
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
xgbMCMC.results <- xgbMCMC(samplingMethod = "overample", nUndersample = NA, kOversample = seq(4,6,1),
                           trainPct = 0.7, B = 5, max.depth = 20, eta = 1e-4, nround = 4, nthread = 2)
toc()
xgbMCMC.results %>% arrange(-mean)


