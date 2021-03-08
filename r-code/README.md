# R-code

### data-management.R
+ Used to clean data.
+ Read in and combine *winequality-red.csv* and *winequality-white.csv*.
+ Create variables
    + type (red / white)
    + class *we should check papers to see how we want to break apart these categories*
        + (1) "good" if quality >= 6.5
        + (0) "bad" if quality < 6.5
+ Export cleaned data set for modeling to [*winequality-all.csv*](https://github.com/earobinson95/Statistical-Learning-Project-UNL-STAT983/blob/main/data/winequality-all.csv).

### winequality-exploration.Rmd/html
+ Provides data description and summary statistics.
+ Addresses distribution of responses (quality & class).
+ Explore multi-colinearity.

### regression-evaluation.R
+ Function for calculating prediction errors
    + Mean square error (MSE)
    + Mean absolute error (MAE)

### classification-evaluation.R
+ Function for calculating classification errors
    + Contingency table 
    + Misclassification rate