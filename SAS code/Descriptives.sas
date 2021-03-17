PROC IMPORT OUT= WORK.Wine 
            DATAFILE= "C:\Users\sja38343\Desktop\Files to Email Laptop\S
TAT 983\Project\winequality-all.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

PROC CONTENTS DATA=Wine; RUN;

*Re-code Variables;
DATA Wine; SET Wine;
IF Quality IN (3,4) 	THEN Quality_New = 1;
IF Quality IN (5,6,7) 	THEN Quality_New = 2;
IF Quality IN (8,9) 	THEN Quality_New = 3;
RUN;

**********************
***  Descriptives  ***
**********************;

TITLE 'Continuous Variables - Overall';
PROC UNIVARIATE DATA=Wine; 
VAR alcohol chlorides citric_acid density fixed_acidity free_sulfur_dioxide pH quality residual_sugar sulphates total_sulfur_dioxide volatile_acidity;
HISTOGRAM;
RUN; TITLE;

TITLE 'Discrete Variables - Overall';
PROC FREQ DATA=Wine;
TABLES class type;
RUN; TITLE;

PROC SORT DATA=Wine; BY Quality_New; RUN;

TITLE 'Continuous Variables - By Categorical Wine Quality';
*Evidence of skewness;
PROC UNIVARIATE DATA=Wine; 
BY Quality_New;
VAR alcohol chlorides citric_acid density fixed_acidity free_sulfur_dioxide pH residual_sugar sulphates total_sulfur_dioxide volatile_acidity;
HISTOGRAM;
RUN; TITLE;

TITLE 'Continuous Variables - By Categorical Wine Quality Kruskal Wallis Test with Dwass Steel Critchlow Fligner Post-Hoc Testing';
PROC NPAR1WAY DATA=Wine WILCOXON DSCF;
CLASS Quality_New;
VAR alcohol chlorides citric_acid density fixed_acidity free_sulfur_dioxide pH residual_sugar sulphates total_sulfur_dioxide volatile_acidity;
RUN; TITLE;

TITLE 'Discrete Variables - By Categorical Wine Quality';
PROC FREQ DATA=Wine;
TABLES (class type)*Quality_New / CHISQ EXACT CL;
RUN; TITLE;





