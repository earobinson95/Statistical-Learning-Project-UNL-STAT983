# Data

See [*winequality-all.csv*](https://github.com/earobinson95/Statistical-Learning-Project-UNL-STAT983/blob/main/data/winequality-all.csv)

## Background

The two datasets are related to red and white variants of the Portuguese "Vinho Verde" wine. For more details, consult: http://www.vinhoverde.pt/en/ or the reference [Cortez et al., 2009]. Due to privacy and logistic issues, only physicochemical (inputs) and sensory (the output) variables are available (e.g. there is no data about grape types, wine brand, wine selling price, etc.). These data sets can be viewed as classification or regression tasks. The classes are ordered and not balanced (e.g. there are munch more normal wines than excellent or poor ones). Outlier detection algorithms could be used to detect the few excellent or poor wines. Also, we are not sure if all input variables are relevant. So it could be interesting to test feature selection methods. Additionally, several of the attributes may be correlated, thus it makes sense to apply some sort of feature selection.

## Data summary
+ Response: quality (score between 0 and 10)
+ red wine - 1599; white wine - 4898
+ 11 predictor variables
    - fixed acidity
    - volatile acidity
    - citric acid
    - residual sugar
     chlorides
    - free sulfur dioxide
    - total sulfur dioxide
    - density
    - pH
    - sulphates
    - alcohol

## Resources

[UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/Wine+Quality)

[Kaggle](https://www.kaggle.com/uciml/red-wine-quality-cortez-et-al-2009)

P. Cortez, A. Cerdeira, F. Almeida, T. Matos and J. Reis. Modeling wine preferences by data mining from physicochemical properties. In Decision Support Systems, Elsevier, 47(4):547-553. ISSN: 0167-9236. http://dx.doi.org/10.1016/j.dss.2009.05.016.

+ In the above reference, two datasets were created, using red and white wine samples. The inputs include objective tests (e.g. PH values) and the output is based on sensory data (median of at least 3 evaluations made by wine experts). Each expert graded the wine quality between 0 (very bad) and 10 (very excellent). Several data mining methods were applied to model these datasets under a regression approach. The support vector machine model achieved the best results. Several metrics were computed: MAD, confusion matrix for a fixed error tolerance (T), etc. Also, we plot the relative importances of the input variables (as measured by a sensitivity analysis procedure).*
