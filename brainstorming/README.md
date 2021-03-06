# Statistical Learning Project Brainstorming

### Neural Backed Decision Trees

+  <https://bair.berkeley.edu/blog/2020/04/23/decisions/>
+ Appears to be an image classification problem.
+ Trees: have good interpretability.
+ Neural Networks: have good accuracy in prediction.
+ COMBINE! matches neural network accuracy while preserving the interpretability of a decision tree. *this is kinda cool*
+ [Neural Backed Decision Trees](https://research.alvinwan.com/neural-backed-decision-trees/)
+ Could compare accuracy of NN, Trees, and NBDT?
+ Example is in Python. I beleive Python tends to do better with computer vision. But maybe we can find a data set that is already prepared? I breifly glanced on their github but everything was .py python files so I didn't have much luck initially.
+ [Official Paper](https://arxiv.org/pdf/2004.00221.pdf)

### Evaluation of head and neck mucosal melanoma and proposal of a novel stage grouping system

+ See *Melanoma NCDB proposal + SA.docx*

+ Predictor Variables

    + Age
    + Gender
    + Race
    + Tumor Size
    + Tumor Location
    + Extent of Invation
    + Nodal Disease
    + Metastatic Disease
    + Treatment Modality
    + Margin Status

+ Response Variables (*How do we obtain these outcomes? Multivariate?*)

    + Overall survival
    + Disease specific survival
    + Disease free survival
    + Metastasis free survival
    + Nodal/regional failure rate

+ Recursive Partitioning Analysis (RPA)

    + Goal: Establish a stage grouping system.
    + An Introduction to Recursive Partitioning: Rationale, Application and Characteristics of Classification and Regression Trees, Bagging and Random Forests](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2927982/)
    + I believe RPA is just a fancy term for tree based models. We could compare the different kinds (Random forest, bagging, etc.)?

### Predict Droughts Using Meteorological and Soil Data

+ https://www.kaggle.com/cdminix/us-drought-meteorological-data
+ The goal for this dataset is to try and see if droughts can be predicted using only meteorolical data
+ Classification Problem with 5 levels of drought
+ Each entry is drought level at a specific point in time in a county in the US
+ Already has it broken down into training, validation, and testing, but they did so by year (something to maybe reconsider?)
+ Has 18 Meteorological Predictor Variables (description given in kaggle)
+ Imbalanced data set (most entries in lowest level of drought index) - resampling?
+ Haven't looked for papers, but I'm sure we would be able to find some on this topic
+ Has a UNL Connection in the acknowledgment section!

### Wine Quality

+ <http://archive.ics.uci.edu/ml/datasets/Wine+Quality>
+ P. Cortez, A. Cerdeira, F. Almeida, T. Matos and J. Reis. Modeling wine preferences by data mining from physicochemical properties. In Decision Support Systems, Elsevier, 47(4):547-553, 2009.
+ Output variable (based on sensory data):
    - quality (score between 0 and 10)
+ Predictor Variables:
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

