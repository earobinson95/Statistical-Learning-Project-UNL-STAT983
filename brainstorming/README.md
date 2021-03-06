# Statistical Learning Project Brainstorming

### Neural Backed Decision Trees

+ https://bair.berkeley.edu/blog/2020/04/23/decisions/
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
    + [An Introduction to Recursive Partitioning: Rationale, Application and Characteristics of Classification and Regression Trees, Bagging and Random Forests](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2927982/)
    + I believe RPA is just a fancy term for tree based models. We could compare the different kinds (Random forest, bagging, etc.)?
