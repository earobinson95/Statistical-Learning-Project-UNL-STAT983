*Project: Medullary Thyroid Ped vs. Adult Cases;
*PI: Jonathan Gootee;
*Date: 01.23.2020;
*SJA;

*Data import;
LIBNAME SASFiles 'Z:\NCDB';

*Data Cleaning;
DATA thyroid; SET SASFiles.Medullary_Thyroid;
	*Age;
	IF (AGE = . OR AGE = 999) THEN DELETE; 
	*Race;
	IF (RACE = . OR RACE = 99) THEN DELETE; 
	IF (RACE = 1) 					 THEN WHITE = 1; ELSE WHITE = 0;
	IF (RACE = 2) 					 THEN BLACK = 1; ELSE BLACK = 0;
	IF (WHITE = 0 AND BLACK = 0) 	 THEN OTHER = 1; ELSE OTHER = 0;
	*Ethnicity;
	IF (SPANISH_HISPANIC_ORIGIN = . OR SPANISH_HISPANIC_ORIGIN = 9) THEN DELETE;
	IF (SPANISH_HISPANIC_ORIGIN = 1) THEN HISPANIC = 1; ELSE HISPANIC = 0;
	*Combine Race and Ethnicity;
	IF (WHITE = 1 AND HISPANIC = 1)  THEN RACE_ETHN = 1;
	IF (WHITE = 1 AND HISPANIC = 0)  THEN RACE_ETHN = 2;
	IF BLACK = 1 					 THEN RACE_ETHN = 3;
	IF OTHER = 1					 THEN RACE_ETHN = 4;
	LABEL RACE_ETHN = "RACE_ETH: 1=HW, 2=NHW, 3=Black, 4=Other";
	IF RACE_ETHN = 1 THEN HW = 1; ELSE HW = 0;
	IF RACE_ETHN = 2 THEN NHW = 1; ELSE NHW = 0;
	*Biological Sex;
	IF (SEX = . OR SEX > 2) THEN DELETE; 
	IF (SEX = 1) THEN MALE = 1; 
	IF (SEX = 2) THEN MALE = 0;
	*Charlson/Deyo Score;
	IF (CDCC_TOTAL_BEST = 0) THEN CDCC0 = 1; ELSE CDCC0 = 0;
	IF (CDCC_TOTAL_BEST = 1) THEN CDCC1 = 1; ELSE CDCC1 = 0;
	IF (CDCC_TOTAL_BEST = 2) THEN CDCC2 = 1; ELSE CDCC2 = 0;
	IF (CDCC_TOTAL_BEST = 3) THEN CDCC3 = 1; ELSE CDCC3 = 0;
	*Stage;
	IF (ANALYTIC_STAGE_GROUP = 0 OR ANALYTIC_STAGE_GROUP = 8 OR ANALYTIC_STAGE_GROUP = 9) THEN DELETE;
	*Metastasis;
/*	IF TNM_CLIN_M = '88' THEN DELETE;*/
	IF (TNM_CLIN_M = 'c1' OR TNM_CLIN_M = 'c1A' OR TNM_CLIN_M = 'c1B') THEN METS_CLIN = 1; ELSE METS_CLIN = 0;
/*	IF TNM_PATH_M = '88' THEN DELETE;*/
	IF (TNM_PATH_M = 'p1' OR TNM_PATH_M = 'p1A' OR TNM_PATH_M = 'p1B') THEN METS_PATH = 1; ELSE METS_PATH = 0;
	*Surgery;
	IF (RX_SUMM_SURG_PRIM_SITE = 99 OR RX_SUMM_SURG_PRIM_SITE = .) THEN DELETE;
	IF RX_SUMM_SURG_PRIM_SITE = 0 THEN SURGERY = 0; ELSE SURGERY = 1;
	*Radiation;
	IF (RX_SUMM_RADIATION = 9 OR RX_SUMM_RADIATION = .) THEN DELETE;
	IF RX_SUMM_RADIATION = 0 THEN RAD = 0; ELSE RAD = 1;
	IF (RX_SUMM_SURGRAD_SEQ = 9 OR RX_SUMM_SURGRAD_SEQ = .) THEN DELETE;
	IF (RX_SUMM_SURGRAD_SEQ = 3 OR RX_SUMM_SURGRAD_SEQ = 4) 							THEN ADJUV = 1;
	IF (RX_SUMM_SURGRAD_SEQ = 2 OR RX_SUMM_SURGRAD_SEQ = 5 OR RX_SUMM_SURGRAD_SEQ = 6) 	THEN ADJUV = 0;
	IF RAD = 1 AND ADJUV = 1 THEN ADJ_RAD = 1;
	IF RAD = 1 AND ADJUV = 0 THEN ADJ_RAD = 0;
	IF RAD = 0 		THEN RAD_FINAL = 0;
	IF ADJ_RAD = 0 	THEN RAD_FINAL = 1;
	IF ADJ_RAD = 1 	THEN RAD_FINAL = 2;
	LABEL RAD_FINAL="0=No Rad, 1=Non-Adj Rad, 2=Adj Rad";
	IF RAD_FINAL = . THEN DELETE;
	*Death;
	IF (PUF_VITAL_STATUS = .) THEN DELETE;
	IF (PUF_VITAL_STATUS = 0) THEN DEAD = 1; ELSE DEAD = 0;
	*Label Time Variable;
	IF (DX_LASTCONTACT_DEATH_MONTHS = .) THEN DELETE;
	LABEL DX_LASTCONTACT_DEATH_MONTHS="Months from Diagnosis";
RUN;

proc freq data=thyroid; tables histology; run;

**********************
***  Descriptives  ***
**********************;

TITLE 'Frequences of Pediatric and Adult Patients';
PROC FREQ DATA=thyroid;
TABLES agegroup*RAD_FINAL;
RUN;

**************************************
***   Proportionality of Hazards   ***
**************************************;

TITLE1 "Age Group: Test PH Assumption Log-Negative-Log"; 
PROC LIFETEST DATA=thyroid NOTABLE PLOTS=(LLS);
	STRATA agegroup;
	TIME DX_LASTCONTACT_DEATH_MONTHS*DEAD(0);
RUN; TITLE1;

