# LOAD LIBRARIES
library(tidyverse)
library(readr)
library(xgboost)
library(Matrix)
library(tictoc) 
library(furrr)

# IMPORT DATA, RELEVEL FACTOR COLUMNS, 
winequality <- read_csv("data/winequality-all.csv") %>%
  mutate(type = factor(type, levels = c("red", "white")),
         qualityclass = factor(qualityclass, levels = c("Low", "Normal", "High")))
winequality$quality = NULL # we don't need this column
colnames(winequality) <- make.names(names(winequality), unique=TRUE)
summary(winequality)

# SET UP FUNCTION TO EVALUATE XGBOOST
xgbFunc <- 
  function(trainPct = 0.7, max.depth, eta, nround = 2, nthread = 2){
  
    # label conversion
    qualityclass = winequality$qualityclass
    label = as.integer(winequality$qualityclass)-1
    
    # set up training/testing sets
    n <- nrow(winequality)
    train.index <- sample(seq(1,n), floor(n*trainPct), replace = F)
    
    # create dgCMatrix for modeling
    # training
    train.data  <- sparse.model.matrix(qualityclass ~ ., data = winequality[train.index,])[,-1]
    train.label <- label[train.index]
    xgb.train   <- list(data = train.data, label = train.label)
    # testing
    test.data  <- sparse.model.matrix(qualityclass ~ ., data = winequality[-train.index,])[,-1]
    test.label <- label[-train.index]
    xgb.test   <- list(data = test.data, label = test.label)
    
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
                       num_class = length(levels(qualityclass)),
                       verbose = 0
    )
    
    # predict
    xgb.pred = predict(xgb.fit, xgb.test$data, reshape = T) %>% as.data.frame()
    colnames(xgb.pred) = levels(qualityclass)
    xgb.pred$prediction = apply(xgb.pred, 1, function(x) colnames(xgb.pred)[which.max(x)])
    xgb.pred$label = levels(qualityclass)[test.label + 1]
    xgb.pred <- xgb.pred %>%
      mutate(prediction = factor(prediction, levels = c("Low", "Normal", "High")),
             label = factor(label, levels = c("Low", "Normal", "High")))
    
    # evaluated prediction
    accuracy <- mean(xgb.pred$prediction==xgb.pred$label)
    table <- with(xgb.pred, table(prediction, label))
    
    # return(list(accuracy = accuracy, table = table))
    return(accuracy)
    
  }

# xgbFunc(max.depth = 20, eta = 0.0001)

# MCMC FUNCTION WITH PARALLEL COMPUTING
xgbMCMC <- 
  function(B = 5, trainPct = 0.7, max.depth, eta, nround = 2, nthread = 2){
    
    # Create Parameter Grid
    mcmc.grid <- expand_grid(B = seq(1,B), 
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
      group_by(max.depth, eta, nround, nthread) %>%
      summarise(B = n(),
                mean  = mean(xgbAccuracy),
                lower = quantile(xgbAccuracy, probs = c(0.05)),
                upper = quantile(xgbAccuracy, probs = c(0.95)))
    
    return(results)
    
  }

# HYPERPARAMETER GRID SEARCH
tic()
xgbMCMC.results <- xgbMCMC(B = 50, max.depth = 20, eta = 1e-4, nround = 4, nthread = 2)
toc()

xgbMCMC.results %>% arrange(-mean)

