# use source(r-code/regression-evaluation.R) to have function in your script file

regEval <- 
  function(y, yhat){
  MSE = mean((y-yhat)^2)
  MAE = mean(abs(y-yhat))
  return(MSE = MSE, MAE = MAE)
  }

# DESIGNATE TESTING AND TRAINING DATASETS
n = dim(winequality)[1];      ### total number of observations
n1 = round(n/10);     ### number of observations randomly selected for testing data
set.seed(7406);   ### set the seed for randomization
flag = sort(sample(1:n, n1));
wineregtrain = winequality[-flag,];    
wineregtest  = winequality[flag,];

###  DESCRIPTIVES  ###

# PEARSON AND SPEARMAN CORRELATION
pearson <- cor.test(wineregtrain$density, wineregtrain$pH, method="pearson")
spearman <- cor.test(wineregtrain$density, wineregtrain$pH, method="spearman")



(wineregtrain$`fixed acidity`, wineregtrain$`citric acid`, wineregtrain$`residual sugar`,
    wineregtrain$chlorides, wineregtrain$`free sulfur dioxide`, wineregtrain$`total sulfur dioxide`,
    wineregtrain$density, wineregtrain$pH, wineregtrain$sulphates, wineregtrain$alcohol)

# T- AND MANN-WHITNEY TESTS



#What is our main independent variable of interest and hypothesis?
#Stratify and compare

###  MODELING  ###

# VARIABLE SELECTION: RANDOM FOREST and GINI INDEX; VIF

# LINEAR REGRESSION

# POLYNOMIAL REGRESSION

# SPLINES


