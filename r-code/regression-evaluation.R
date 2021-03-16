# use source(r-code/regression-evaluation.R) to have function in your script file
install.packages("ranger")
install.packages("leaps")
library(ranger)
library(leaps)

# DESIGNATE TESTING AND TRAINING DATASETS
n = dim(winequality)[1];      ### total number of observations
n1 = round(n/10);     ### number of observations randomly selected for testing data
set.seed(7406);   ### set the seed for randomization
flag = sort(sample(1:n, n1));
wineregtrain = winequality[-flag,];    
wineregtest  = winequality[flag,];

###  DESCRIPTIVES  ###

#What is our main independent variable of interest and hypothesis?
#Categorize, stratify, and compare

# PROC UNIVARIATE in SAS

# PEARSON AND SPEARMAN CORRELATION
pearson <- cor.test(wineregtrain$density, wineregtrain$pH, method="pearson")
spearman <- cor.test(wineregtrain$density, wineregtrain$pH, method="spearman")

(wineregtrain$`fixed acidity`, wineregtrain$`citric acid`, wineregtrain$`residual sugar`,
    wineregtrain$chlorides, wineregtrain$`free sulfur dioxide`, wineregtrain$`total sulfur dioxide`,
    wineregtrain$density, wineregtrain$pH, wineregtrain$sulphates, wineregtrain$alcohol)

# T- AND MANN-WHITNEY TESTS or KRUSKAL-WALLIS AND ANOVA


###  MODELING  ###


# VARIABLE SELECTION: RANDOM FOREST and GINI INDEX
rfselection <- ranger(quality ~ density + pH, #+  `fixed acidity` + `citric acid` + `residual sugar` + chlorides +
                    #`free sulfur dioxide` + `total sulfur dioxide` + density + sulphates + alcohol,
                    data = wineregtrain, num.trees = 500, mtry = 1, importance = 'impurity')
rfselection$variable.importance #print gini index

# BEST FIVE
leapselection <- regsubsets(quality ~ ., data = wineregtrain, nbest = 100, really.big = TRUE)
leapmodel <- summary(leapselection)$which;
leapmodelsize <- as.numeric(attr(leapselection, "dimnames")[[1]])
leapmodelrss <- summary(leapselection)$rss
op2 <- which(leapselection == 5) #error

# LINEAR REGRESSION
model1 <- lm(quality ~ ., data=wineregtrain)

# VARIABLE SELECTION: VIF
VIF <- NULL
X <- model.matrix(model1)
for (i in 2:14) VIF <- cbind(VIF, 1/(1-summary(lm(X[,i] ~ X[,-i]))$r.squared))
colnames(VIF) <- colnames(training)[1:13]
VIF #what should our cutoff be?


regEval <- 
  function(y, yhat){
    MSE = mean((y-yhat)^2)
    MAE = mean(abs(y-yhat))
    return(MSE = MSE, MAE = MAE)
  }

# POLYNOMIAL REGRESSION

# SPLINES


