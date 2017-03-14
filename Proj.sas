proc import datafile="C:\Users\adity\OneDrive\Documents\UIC\IDS 575\Project\Finalv1.0.csv" 
dbms=csv  out=Credit_data REPLACE ;
	 run;


	 proc means data=Credit_Data n mean;
	 run;

	 proc contents data=Credit_Data;
	 run;

	 proc univariate data=Credit_Data;
	 var LIMIT_BAL;
	 run;

	 ods graphics on;
proc anova data=Credit_Data ;
TITLE "Limit bal vs SEX";
CLASS SEX EDUCATION;
MODEL LIMIT_BAL = SEX EDUCATION;
MEANS EDUCATION SEX/ SCHEFFE;
run ; 
ods graphics off;


	 ods graphics on;
proc anova data=Credit_Data ;
TITLE "Limit bal vs SEX";
CLASS MARRIAGE;
MODEL LIMIT_BAL = MARRIAGE;
MEANS MARRIAGE/ SCHEFFE;
run ; 
ods graphics off;
