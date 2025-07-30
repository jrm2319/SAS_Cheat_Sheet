******************************************************
**	SAS Cheat Sheets    							**
**  Date: 07/28/2025								**
**  Name: Jasmin Martinez                           **
******************************************************;

** ODS Graphics for KM survival plot **;
**u63509268 > Coding 1**;
ods graphics on;
proc lifetest data=data1 method=KM alpha=0.05 maxtime=28 plots=survival(cl);
  time time*status(0); 
run;
ods graphics off;

*** Example of Cox Model **;
proc phreg data=framingham;
  class dbp_c(ref=first)/param=ref;
  model followup*chdfate(0) = dbp_c /risklimits covb 
                                     ties=EFRON;
  title "Model 1: DBP_c";
run;

*** Test the Proportionality assumption**;
**u63509268 > Coding 5**;
ods graphics on;
proc phreg data=recid;
  class fin(ref='0') race(ref='0') wexp(ref='0') mar(ref='0') paro(ref='0')/param=ref;
  model week*arrest(0) = fin age race wexp mar paro 
                         prio/ties=efron;
  assess PH / resample;
run;
ods graphics off;

*** fit the multinomial Logistic model**;
**u63509268 > Coding 6**;
proc logistic data=school;
  freq count;
  class school(ref='1') program (ref='regular')/param=ref;
  model style(ref='class')=school program / link=glogit;
run;

*** Ordinal Logistic regression**; 
**u63509268 > Coding 6**;
ods graphics on;
proc logistic data=cheese;
  freq count;
  class additive (ref='A')/param=ref;
  model rating = additive;
  oddsratio additive;
  effectplot / polybar;
run;
ods graphics off;

**Poisson regression**;
**u63509268 > Coding 7**;
proc genmod data=insure;
  class car(ref='small') age(ref='1')/param=ref;
  model Y = car age /link=log dist=poi 
                     offset=log_N type3;
run;

*** Negative binomial regression **;
**u63509268 > Coding 7**;
proc genmod data=homicide;
  class race(ref='White')/param=ref;
  model response = race/link=log dist=negbin;
  freq count;
run;

*** GEE for binary data **; 
**u63509268 > Coding 8**; 
proc genmod data=wheeze descending;
  class child city smoke age/param=ref ref=first;
  model wheeze = smoke age city/dist=bin link=logit
                                 type3 wald;
  repeated subject = child/type=AR(1) within=age;
run;

*** GEE for count data **; 
**u63509268 > Coding 8**; 
proc genmod data=seizure;
  class subject;
  model seize = trt v trt*v/dist=poi link=log 
                            offset=log_n;
  repeated subject=subject/type=cs modelse;
run;

***generate spaghetti plots by gender**;
**u63509268 > Coding 9**; 
proc sgpanel data=dental NOAUTOLEGEND;
  title 'Spaghetti plot of distance by gender';
  panelby gender;
  series x=age y=distance/group=id BREAK
                          LINEATTRS=(COLOR=gray
                          PATTERN=1 THICKNESS=1) ;
  reg x=age y=distance/LINEATTRS=(COLOR=red
                       PATTERN=1 THICKNESS=3)
                       MARKERATTRS=(COLOR=gray);
run;

*** random intercept and slope model**;
**u63509268 > Coding 9**; 
proc mixed data=dental method=ml;
  class gender id;
  model distance = age gender gender*age/s;
  random int age/type=un subject=id g;
run;

*** Calculate the crude OR and RR**;
**u62250266 > Lab I Code**; 
proc freq data=CVD order=data;
	table FAMHX*CVD / chisq relrisk; *RELRISK requests the OR and RR and their 95% CI's. You want
									  RELRISK Column 1 ("Yes"). CHISQ provides a quick measure of
									  association;
	format FAMHX yn. CVD yn.;
run;

proc sort data=CVD;
	by descending STATIN descending OBESE; *Get the other variables in the correct order;
run;

proc freq data=CVD order=data;
	table STATIN*CVD / chisq relrisk;
	format STATIN yn. CVD yn.;
run;

proc freq data=CVD order=data;
	table OBESE*CVD / chisq relrisk;
	format OBESE yn. CVD yn.;
run;

/* Create a temporary data set from the permanent  chs03 data in our library.*/
**u62250266 > Lab 2 Code**; 
data chs03;
set epi3.chs03;

/*Macros*/
	/*Print and sort data to understand it*/
		PROC PRINT DATA=sashelp.class;
		RUN;
		
		PROC SORT DATA=sashelp.class OUT=class;
		BY sex;
		RUN;

	/*To get the female and male values */
		DATA Mclass;
		SET sashelp.class;
		WHERE sex = 'F';
		RUN; 
		
		DATA Mclass;
		SET sashelp.class;
		WHERE sex = 'M';
		RUN; 
	/*Creating the Macro variable*/;
		/*%LET macro_name = value*/;
		%LET x = sashelp.class; *since we are using 'sashelp.class' a lot, we cna create a macro so that we don't have to write it as often*/
		
	/*Using Macros*/;
		PROC PRINT data=&x; /*to call on macros, you use '&macro_name'*/
		RUN;
		
		PROC SORT DATA=&x OUT=class;
		BY sex;
		RUN;
		
		DATA Mclass;
		SET &x;
		WHERE sex = 'F';
		RUN; 
		
		DATA Mclass;
		SET &x;
		WHERE sex = 'M';
		RUN; 
		
	/*Creating another macro and macro variable*/
		/*These are the datasets we want, but there has to be an easier way to do this.*/
		PROC PRINT DATA=sashelp.class;
		RUN;
		
		PROC PRINT DATA=sashelp.air; 
		RUN; 
		
		PROC PRINT DATA=sashelp.cars;
		RUN;
		
		PROC PRINT DATA=sashelp.shoes;
		RUN; 
		
		/*Create a macro to make this easier*/
		%macro print; /*the name here is 'print'*/
		PROC PRINT DATA=sashelp.class;
		RUN;
		%mend; 
		
		/*Call the macro*/
		%print;
		
		/*Using key word parameters*/
			/*Above, the only thing that is changing is the parameter (in this case the dataset you want to pull from sashelp), 
			therefore there is an easier way to do this using macros with a parameter.*/
		%macro print(dname=); /*'dname' is the name given to the parameter*/
		PROC PRINT DATA=&dname; /*here we are linking the parameter using an &*/
		RUN;
		%mend; 
		
		%print(dname=sashelp.class);
		%print(dname=sashelp.cars);
		%print(dname=sashelp.shoes);
			/*Above is example of KEYWORD PARAMETER*/

		/*Another example of how to do this*/
		%macro print(dname); 
		PROC PRINT DATA=&dname; 
		RUN;
		%mend; 
		
		%print(sashelp.class);
		%print(sashelp.cars);
		%print(sashelp.shoes);
			/*Above is example of POSITIONAL PARAMETER*/
		
	/*Macro programming for subsetting data*/
		/*This is what we want*/
		DATA class1;
		SET sashelp.class;
		WHERE sex = 'F';
		RUN; 
		
		/*Let's assign a Macro to this*/
		%macro sub(d=,r=,var=,val=,);
		DATA &d;
		SET &r;
		WHERE &var = &val;
		RUN; 
		%mend;	
		
		/*Let's use the macro we just created*/
		%sub(d=class2, r=sashelp.class, var=sex, val='M');
		%sub(d=class3, r=sashelp.class, var=sex, val='F');
		
	/*Macro programming for creating libraries*/
		/*Traditional SAS Code*/
			LIBNAME lib '/home/u63745106/sasuser.v94';
			RUN;
		/*Assign Macro programming for library creation*/
			 /*in positional parameter: (Parameter1, Parameter2,...
				in keyword parameter: (Parameter1=Value1, Parameter2=Value2,...)*/
			%macro dv(name=,path=);
			LIBNAME &name &path;
			RUN;
			%mend;
			
			%dv (name=lib2, path='/home/u63745106/sasuser.v94');
			
	/*Macro porgrammming for frequency procedure*/
		/*Traditional SAS code*/
			PROC FREQ DATA=sashelp.class;
			TABLE age;
			RUN; 
			
			PROC SORT DATA=sashelp.class OUT=class7; 
			BY sex;
			RUN; 
			
			PROC FREQ DATA=class7;
			TABLE age; 
			BY sex; 
			RUN; 
		/*Macros code*/
			%macro sexfreq(dat=, table=, by=);
			PROC FREQ DATA=&dat;
			TABLE &table; 
			BY &by; 
			RUN; 
			%mend;
			
			%sexfreq(dat=class7, table=age, by=sex);
			
			%macro sexsort(dat=, by=); 
			PROC SORT DATA=&dat;
			BY &by;
			RUN;
			%mend;
			
			%sexsort(dat=class7, by=sex);
			
	/*Macro porgrammming for mean procedure*/;
		/*Traditional SAS code*/;
			%sexsort(dat=class7, by=sex);
			PROC SORT DATA=sashelp.class OUT=class1223; BY SEX; RUN; 
			
			PROC MEANS DATA=class1223;
			BY SEX;
			VAR height weight;
			RUN;
		/*Macros code*/
			/*This allows us to do the sorting, frequency, and mean all in one go*/
			%macro sexfreq2 (R= ,OP=, BYVAR=, TVAR=, AVAR1=, AVAR2=);
			PROC SORT DATA=&R OUT=&OP; BY &BYVAR; RUN;
			
			PROC FREQ DATA=&OP;
			TABLE &TVAR;
			BY &BYVAR; 
			RUN;
			
			PROC MEANS DATA=&OP;
			BY &BYVAR;
			VAR &AVAR1 &AVAR2; 
			RUN; 
			
			%mend;
			
			%sexfreq2 (R=sashelp.class,OP=class, BYVAR=sex, TVAR=age, AVAR1=height, AVAR2=weight);

	/*SAS Macros-Conditional and iterative statement*/
		/*Using conditional statements and lopps using macro statements such as %IF, %THEN, %ELSE and 
		%DO, %END. These macro statements should alwasys be called inside the macro.*/
		
		/*Conditional processing: used when we want to execute a piece of code based on the output of 
		single or multiple conditions.*/
		
		%macro sexfreq2 (C=, R= ,OP=, BYVAR=, TVAR=, AVAR1=, AVAR2=);
		
		%IF &C=S %THEN %DO;
			PROC SORT DATA=&R OUT=&OP; BY &BYVAR; RUN;
		%END; /*the PROC SORT will only be completed if the IF/THEN statement is satified*/
			
		%ELSE %IF &C=F %THEN %DO;
			PROC FREQ DATA=&OP;
			TABLE &TVAR;
			BY &BYVAR; 
			RUN;
		%END;
			
		%ELSE %IF &C=M %THEN %DO;
			PROC MEANS DATA=&OP;
			BY &BYVAR;
			VAR &AVAR1 &AVAR2; 
			RUN; 
		%END;

		%mend;
			
		%sexfreq2 (C=M, R=sashelp.class,OP=class, BYVAR=sex, TVAR=age, AVAR1=height, AVAR2=weight);
					/*Here, we used C+M tf the PROC MEAN function will be printed. It will all be executed.*/
					
	/*SAS Macro Functions*/
		/*
		%PUT = writes text or macro variable infomration to the SAS log. To display the automatically created macro variable.
		%PUT_AUTOMATIC_ = to display macro variables defined by a user
		%PUT_USER_ = to display global macro variable
		%PUT_GLOBAL_ : to display loacl macro variables 
		%PUT_LOCAL_ = to display value assigned to macro variables
		*/		
		
		%LET X=20;
		%LET y=5;
		%LET z=&x*&y;
		
		%PUT &z; /*this will NOT run the calculation! It will just run exactly what you put in as the Z value.*/
		
		%LET X=20;
		%LET y=5;
		%LET z=%eval(&x*&y); /* %eval can ONLY be used on whole numbers, decimals do not work.*/
		
		%PUT &z; /*Now the calculation is running!*/
		
		%LET X=20.34;
		%LET y=5.21;
		%LET z=%sysevalf(&x*&y); /* %sysevalf can be used with decimal values*/
		
		%PUT &z; /*Now the calculation is running!*/
		
		%LET x=10
		%LET y= %STR (R &x ; Chandana); /* %STR removes the meaning of the special characters, for example the ';' in this case*/

		%PUT &y; 
		
		
		
		
		
		
		
		