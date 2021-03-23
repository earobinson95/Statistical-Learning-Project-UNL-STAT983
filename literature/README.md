# Literature Review

#### [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/Wine+Quality)

#### [Kaggle](https://www.kaggle.com/uciml/red-wine-quality-cortez-et-al-2009)

#### Modeling wine preferences by data mining from physicochemical properties

+ P. Cortez, A. Cerdeira, F. Almeida, T. Matos and J. Reis. Modeling wine preferences by data mining from physicochemical properties. In Decision Support Systems, Elsevier, 47(4):547-553. ISSN: 0167-9236. http://dx.doi.org/10.1016/j.dss.2009.05.016.

+ Two data sets were created, using red and white wine samples. The inputs include objective tests (e.g. PH values) and the output is based on sensory data (median of at least 3 evaluations made by wine experts). Each expert graded the wine quality between 0 (very bad) and 10 (very excellent). Several data mining methods were applied to model these data sets under a regression approach. The support vector machine model achieved the best results. Several metrics were computed: MAD, confusion matrix for a fixed error tolerance (T), etc. Also, we plot the relative importance of the input variables (as measured by a sensitivity analysis procedure).'

+ Methods used: Regression, Variable selection, Model selection, Support vector machines (with more that 2 categories???), Neural networks

+ Predicted classes 3 - 9, did not split into "good" vs "bad".

+ REC and ROC curves. Variable Importance.

+ SVM outperforms multiple regression and neural network methods.

#### Using data mining for wine quality assessment

+ Cortez, P., Teixeira, J., Cerdeira, A., Almeida, F., Matos, T., & Reis, J. (2009, October). Using data mining for wine quality assessment. In International Conference on Discovery Science (pp. 66-79). Springer, Berlin, Heidelberg. http://dx.doi.org/10.1007/978-3-642-04747-3_8

+ Methods used: Ordinal Regression, Support Vector Machines, Variable and Model Selection

+ I think this is the same paper as above...a little more clear though


#### Classification of Wine Quality with Imbalanced Data

+ G. Hu, T. Xi, F. Mohammed and H. Miao, "Classification of wine quality with imbalanced data," 2016 IEEE International Conference on Industrial Technology (ICIT), Taipei, Taiwan, 2016, pp. 1712-1217, doi: 10.1109/ICIT.2016.7475021. (https://ieeexplore.ieee.org/abstract/document/7475021/citations#citations)
+ Seems to be using the white wine data set, so something we can try with both data sets. Also they classified the wines into three different categories: high, normal, poor. 
+ Methods used: Decision tree, adaptive boosting, random forest, and SMOTE for unbalancedness
+ They used AdaBoost, we could use XGBoost?
+ We conducted classification experiments that showed that the random forest model outperformed the other two models in terms of classification errors. AKA, Random Forests Rock!
+ 4535 **normal quality** wines and 363 **high/low quality** wines. (Low Quality: 3, 4; Normal: 5, 6, 7; High Quality: 8, 9)


#### Selection of Important Features and predicting wine quality using Machine Learning Techniques

+ Y. Gupta, "Selection of important features and predicting wine quality using Machine learning techniques," In Prcedia Compute Science, 125:305-312. ISSN 1877-0509. https://doi.org/10.1016/j.procs.2017.12.041.
+ Same Data Set
+ Methods Used: Linear Regression, neural network, support vector machine

#### Classification of River Water Quality using Multivariate Analysis (so not wine, but seems like the same idea from the abstract)
+ Azhar S., A. Aris, M. Yusoff, M. Ramli, H. Juahir, "Classification of Water Quality using Mulitvariate Analysis", In Procedia Environmental Sciences (2015), 30:79-84. ISSN 1878-0296.https://doi.org/10.1016/j.proenv.2015.10.014.     
+ Methods Used: Cluster Analysis, Discriminant Analysis, Prinicpical Component Analysis (so seem to be more unsupervised learning, but maybe something to incorporate?)

#### Support Vector Regression

+ https://rpubs.com/richkt/280840
