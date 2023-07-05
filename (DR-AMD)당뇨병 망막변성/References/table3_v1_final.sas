/* CAUTION:: EXECUTE AFTER RUNNING table12.sas */



proc freq data=all;
tables A1CB2;
run;
data allprog;  /* needed for 2-step progression outcomes */
set all;
if bdr<3;
run;


/* ------------------------------------------------------ */
/* --------------TABLE 3 UPPER ROW--------------------- */
/* ------------  UNWEIGHTED PROCEDURES --------------- */
/* ------------------------------------------------------ */

/* table 3 crude composite outcome */
proc phreg data=all covs(aggregate) ;
  class patient_id cat(ref='0')  bad
 /descending;
 model FU_duration*bad(0) =  cat /rl;
 ID patient_id;
run;
/* table 3 crude PROG outcome */
/*prog : 2-step progression of DR */
proc phreg data=allprog covs(aggregate) ;
  class patient_id cat(ref='0')  bad
 /descending;
 model FU_duration*prog(0) =  cat/rl;
 ID patient_id;
run;
/* table 3 crude PDR outcome */
/*PDR : Progression to PDR */
proc phreg data=all covs(aggregate) ;
  class patient_id cat(ref='0')  bad
 /descending;
 model FU_duration*pdr(0) =  cat/rl;
 ID patient_id;
run;


/* table 3 unweighted multivariable coxph composite outcome */
proc phreg data=all covs(aggregate) ;
  class patient_id cat(ref='0')   bad
sex
bdr
HTN
curr_smoker
/descending;
  model FU_duration*bad(0) =  cat
age
sex
bdr
HTN
curr_smoker/rl;
 ID patient_id;
run;
/* table 3 unweighted multivariable coxph PROG outcome */
proc phreg data=allprog covs(aggregate) ;
  class patient_id cat(ref='0')   bad
sex
bdr
HTN
curr_smoker
/descending;
  model FU_duration*prog(0) =  cat
age
sex
bdr
HTN
curr_smoker/rl;
 ID patient_id;
run;
/* table 3 unweighted multivariable coxph PDR outcome */
proc phreg data=all covs(aggregate) ;
  class patient_id cat(ref='0')   bad
sex
bdr
HTN
curr_smoker
/descending;
  model FU_duration*pdr(0) =  cat
age
sex
bdr
HTN
curr_smoker/rl;
 ID patient_id;
run;

/* ------------------------------------------------------ */
/* --------------TABLE 3 UPPER ROW--------------------- */
/* ------------  UNWEIGHTED PROCEDURES END----------- */
/* ------------------------------------------------------ */




/* --------------------------------------------------------------- */
/* ----  ESTIMATING PROPENSITIES FOR TREATMENT AND MEDIATOR --- */
/* --------------- ALL EYES WITH NONMISSING A1C (all2) ------------- */
/* --------------------------------------------------------------- */

  
/* PS weight calculated withoug a1c value */
/* pna: marginal probability of treatment */
/* exposure pred null */
/*
proc logistic data=all descending;
 class patient_id cat bdr
DM_type 
HTN
Sex
past_smoker /descending;
model cat(event='1')=;
output out=all predicted=pna;
run;
*/
/* pda: propensity score */
/* exposure pred denominator */
/*
proc logistic data=all descending;
 class patient_id cat bdr
DM_type 
HTN
Sex
past_smoker
curr_smoker
/descending;
model cat(event='1')=
age
agesq
sex
bdr
DM_type
DM_duration 
/*A1C_baseline*/
/*AMD_grade*/
/*
HTN
past_smoker
curr_smoker
;
output out=all predicted=pda;
run;
*/
/* wa: relative PS (just for monitoring), not used in main analysis */
/*
data all;
set all;
wa=pna/pda;
run;
proc univariate data=all;
var wa pna pda;
run; 
*/


/* all2: discard subjects with missing a1cs*/
data all2;
set all;
if avg_A1C>0;
if  A1C_baseline>0;
run;
/* pna: marginal probability of treatment */
/* exposure pred null */
proc logistic data=all2 descending;
 class patient_id cat bdr
DM_type 
HTN
Sex
past_smoker /descending;
model cat(event='1')=;
output out=all2  predicted=pna1;
run;
/* pda: propensity score */
/* exposure pred denominator */
proc logistic data=all2 descending;
 class patient_id cat bdr
DM_type 
HTN
Sex
past_smoker
ever_smoker
curr_smoker
/descending;
model cat(event='1')=
age
agesq
sex
bdr
DM_type
DM_duration 
A1c_baseline 
HTN
past_smoker
curr_smoker;
output out=all2  predicted=pda1;
run;
/* (generalized) propensity score for the mediator (Avg_a1c) */
proc glm data=all2;
class cat
sex
bdr
DM_type
/*Avg_a1c */
/*AMD_grade*/
HTN
past_smoker
curr_smoker
patient_id;
model Avg_a1c=
cat  
age
agesq
sex
bdr
DM_type
DM_duration 
A1c_baseline 
HTN
past_smoker
curr_smoker; 
output out=all2 student=rden; /* studentized residual of the model */
run; quit;
proc glm data=all2;
class cat  ;
model Avg_a1c=cat  ; output out=all2 student=rnum; 
run; quit;
*Obtain probabilities from normal p.d.f.; 
data all2; set all2;
/* standardized IPTW for estimating IPTW-Cox-based HR */
if cat=1 then wa1=pna1/pda1; else  wa1=(1-pna1)/(1-pda1);
/* generalized propensity score  */
pnum = exp(-.5*(rnum**2))/2.506; 
/* generalized propensity score  */
pden = exp(-.5*(rden**2))/2.506; 
wm1=pnum/pden;
w=wa1*wm1;
if w>0;
run;
proc univariate data=all2;
var  w;
run;



/* --------------------------------------------------------------- */
/* ----  ESTIMATING PROPENSITIES FOR TREATMENT AND MEDIATOR --- */
/*-------- ALL EYES WITH NONMISSING A1C and BDR < 3 (allprog2) ----- */
/* --------------------------------------------------------------- */

/* not used (full data)  */
/*
proc logistic data=allprog descending;
 class patient_id cat bdr
DM_type 
HTN
Sex
past_smoker /descending;
model cat(event='1')=;
output out=allprog predicted=pna;
run;
*/
/* exposure pred denominator */
/*
proc logistic data=allprog descending;
 class patient_id cat bdr
DM_type 
HTN
Sex
past_smoker
curr_smoker
/descending;
model cat(event='1')=
age
agesq
sex
bdr
DM_type
DM_duration 
*/
/*A1C_baseline*/
/*AMD_grade*/
/*
HTN
past_smoker
curr_smoker;;
output out=allprog predicted=pda;
run;
data allprog;
set allprog;
wa=pna/pda;
run;
proc univariate data=allprog;
var wa pna pda;
run;
*/

data allprog2;
set allprog;
if avg_A1C>0;
if  A1C_baseline>0;
run;
/* pna: marginal probability of treatment */
/* exposure pred null */
proc logistic data=allprog2 descending;
 class patient_id cat bdr
DM_type 
HTN
Sex
past_smoker /descending;
model cat(event='1')=;;
output out=allprog2  predicted=pna1;
run;
/* pda: propensity score */
/* exposure pred denominator */
proc logistic data=allprog2 descending;
 class patient_id cat bdr
DM_type 
HTN
Sex
past_smoker
ever_smoker
curr_smoker
/descending;
model cat(event='1')=
age
agesq
sex
bdr
DM_type
DM_duration 
A1c_baseline 
HTN
past_smoker
curr_smoker;
output out=allprog2  predicted=pda1;
run;
/* (generalized) propensity score for the mediator (Avg_a1c) */
proc glm data=allprog2;
class cat
sex
bdr
DM_type
/*Avg_a1c */
/*AMD_grade*/
HTN
past_smoker
curr_smoker
patient_id;
model Avg_a1c=
cat  
age
agesq
sex
bdr
DM_type
DM_duration 
A1c_baseline 
HTN
past_smoker
curr_smoker;
output out=allprog2 student=rden; /* studentized residual of the model */
run; quit;
proc glm data=allprog2;
class cat  ;
model Avg_a1c=cat  ; output out=allprog2 student=rnum; run; quit;
*Obtain probabilities from normal p.d.f.; 
data allprog2; set allprog2;
if cat=1 then wa1=pna1/pda1; else  wa1=(1-pna1)/(1-pda1);
pnum = exp(-.5*(rnum**2))/2.506; 
pden = exp(-.5*(rden**2))/2.506; 
wm1=pnum/pden;
w=wa1*wm1;

if w>0;
run;
proc univariate data=allprog2;
var  w;
run;



/* --------------------------------------------------------------- */
/* ------------------- TABLE 3 LOWER ROW ----------------------  */ 
/*------ -------------------- WEIGHTED COX --------------------- */
/* ------------------------------------------------------------- */

/*composite total effect*/
proc phreg data=all2 covsandwich;
  class patient_id cat(ref='0') 
 bad
 /descending;
 model FU_duration*bad(0) =  cat /rl;
 ID patient_id ;
 weight wa1;
run;
/*progression total effect*/
proc phreg data=allprog2 covsandwich;
  class patient_id cat(ref='0') 
 prog
 /descending;
 model FU_duration*prog(0) =  cat  /rl;
 ID patient_id;
 weight wa1;
run;
/*pdr total effect*/
proc phreg data=all2 covsandwich;
  class patient_id cat(ref='0') 
 pdr
 /descending;
 model FU_duration*pdr(0) =  cat  /rl;
 ID patient_id;
 weight wa1;
run;



/* --------------------------------------------------------------- */
/* ------------------- TABLE 3 LOWER ROW ----------------------  */ 
/* ------------------- MEDIATION ANALYSIS ----------------------- */
/* ------------------------------------------------------------- */


/* ------------------------------------------------------------- */
/* create a table 'newmydata' for mediation analysis for the composite outcome and PDR outcome */
/* ------------------------------------------------------------- */
DATA newmydata; SET all2;
catStar = 0; Mtemp =.;
IF cat=catStar then Mtemp = avg_A1C;
OUTPUT; 
catStar = 1; Mtemp =.; 
IF cat=catStar then Mtemp = avg_A1C; 
OUTPUT;
RUN;
DATA newMyData; SET newMyData; 
Atemp = cat; 
RUN;
PROC GENMOD DATA=newmydata; 
CLASS sex   Atemp bdr
DM_type HTN
past_smoker
curr_smoker;
MODEL Mtemp = Atemp age sex
agesq
bdr
DM_type
DM_duration 
A1c_baseline 
HTN
past_smoker
curr_smoker
/ERROR=N; 
output OUT=newmydata 
P = predM;
RUN;
DATA newMyData; 
SET newMyData; 
weightDIR = PDF('normal', avg_A1C, predM, 1.2083);
RUN;
DATA newMyData; SET newMyData; 
Atemp = catStar; 
RUN;
PROC GENMOD DATA=newmydata; 
CLASS sex   Atemp bdr
DM_type HTN
past_smoker
curr_smoker;
MODEL Mtemp = Atemp age sex
agesq
bdr
DM_type
DM_duration 
A1c_baseline 
HTN
past_smoker
curr_smoker
/ERROR=N; 
output OUT=newmydata 
P = predMStar;
RUN;
DATA newMyData; SET newMyData; 
weightINDIR = PDF('normal', avg_A1C, predMStar, 1.2083); RUN;
DATA newMyData; SET newMyData; w2 = weightINDIR/weightDIR;
ww=wa1*w2;
RUN;
proc univariate data=newmydata;
var ww;
run;
/*
proc phreg data=newMyData covsandwich;
  class patient_id cat(ref='0') 
catstar(ref='0') bad
 /descending;
 model FU_duration*bad(0) =  cat catstar /rl;
random patient_id /dist=gamma;
 ID cnt;
 weight ww;
run;*/


/*composite nde nie*/
proc phreg data=newMyData covsandwich;
  class patient_id cat(ref='0') 
catstar(ref='0') bad
 /descending;
 model FU_duration*bad(0) =  cat catstar /rl;

 ID patient_id;
 weight ww;
run;
/*
proc phreg data=newMyData covsandwich;
  class patient_id cat(ref='0') 
 bad
 /descending;
 model FU_duration*bad(0) =  cat  /rl;
 ID patient_id;
 weight ww;
run;

proc phreg data=newMyData covsandwich;
  class patient_id cat(ref='0') 
 bad
 /descending;
 model FU_duration*bad(0) =  cat  AVG_A1C/rl;
 ID patient_id;
 weight ww;
run;

proc phreg data=newMyData covsandwich;
  class patient_id cat(ref='0') 
 bad
 /descending;
 model FU_duration*bad(0) =  cat  AVG_A1C cat*AVG_A1C /rl;
 ID patient_id;
 weight ww;
run;
*/

/* prog nde nie --> below */

/*pdr nde nie*/
proc phreg data=newMyData covsandwich;
  class patient_id cat(ref='0') 
catstar(ref='0') pdr
 /descending;
 model FU_duration*pdr(0) =  cat catstar /rl;

 ID patient_id;
 weight ww;
run;



/* ------------------------------------------------------------- */
/* create a table 'newmydata' for mediation analysis for the 2-step progression outcome */
/* ------------------------------------------------------------- */

DATA newmydata; SET allprog2;
catStar = 0; Mtemp =.;
IF cat=catStar then Mtemp = avg_A1C;
OUTPUT; 
catStar = 1; Mtemp =.; 
IF cat=catStar then Mtemp = avg_A1C; 
OUTPUT;
RUN;
DATA newMyData; SET newMyData; 
Atemp = cat; 
RUN;
PROC GENMOD DATA=newmydata; 
CLASS sex   Atemp bdr
DM_type HTN
past_smoker
curr_smoker;
MODEL Mtemp = Atemp age sex
agesq
bdr
DM_type
DM_duration 
A1c_baseline 
HTN
past_smoker
curr_smoker
/ERROR=N; 
output OUT=newmydata 
P = predM;
RUN;
DATA newMyData; 
SET newMyData; 
weightDIR = PDF('normal', avg_A1C, predM, 1.2083);
RUN;
DATA newMyData; SET newMyData; 
Atemp = catStar; 
RUN;
PROC GENMOD DATA=newmydata; 
CLASS sex   Atemp bdr
DM_type HTN
past_smoker
curr_smoker;
MODEL Mtemp = Atemp age sex
agesq
bdr
DM_type
DM_duration 
A1c_baseline 
HTN
past_smoker
curr_smoker
/ERROR=N; 
output OUT=newmydata 
P = predMStar;
RUN;
DATA newMyData; SET newMyData; 
weightINDIR = PDF('normal', avg_A1C, predMStar, 1.2083); RUN;
DATA newMyData; SET newMyData; w2 = weightINDIR/weightDIR;
ww=wa1*w2;
RUN;

/*progression nde nie*/
proc phreg data=newMyData covsandwich;
  class patient_id cat(ref='0') 
catstar(ref='0') prog
 /descending;
 model FU_duration*prog(0) =  cat catstar /rl;

 ID patient_id;
 weight ww;
run;
