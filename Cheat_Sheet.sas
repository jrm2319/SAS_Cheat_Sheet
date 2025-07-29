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
