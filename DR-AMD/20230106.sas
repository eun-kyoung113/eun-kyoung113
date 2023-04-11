/***** 231006 ******/
/**** Sensitivity analysis ****/
/**** Run the PHREG Procedure ****/
libname dir1 "C:\Users\stat\OneDrive - 숙명여자대학교\석사연구\당뇨병_망막변성_연구자료\sas code\My_data";
run;

DATA PHREG_data;
SET dir1.PHREG_data;
IF A1c_Result="13.2 Results Repeated" then A1c_Result="13.2";
ELSE IF A1c_Result= "8.9 repeated" then A1c_Result="8.9";
IF Patient_ID = "DEBE" then DELETE;
A1c_Result2=INPUT(A1c_Result,12.);
RUN;

/* pna: marginal probability of treatment */
/* exposure pred null */
proc logistic data=PHREG_data descending;
 class patient_id cat bdr
DM_type 
HTN
Sex
past_smoker /descending;
model cat(event='1')=;
output out=PHREG_data  predicted=pna1;
run;
/* pda: propensity score */
/* exposure pred denominator */
proc logistic data=PHREG_data descending;
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
A1c_Result2 
HTN
past_smoker
curr_smoker;
output out=PHREG_data  predicted=pda1;
run;

data dir1.PHREG_data2; 
set PHREG_data;
/* standardized IPTW for estimating IPTW-Cox-based HR */
if cat=1 then wa1=pna1/pda1; else  wa1=(1-pna1)/(1-pda1);
run;

data allprog2;  /* needed for 2-step progression outcomes */
set PHREG_data;
if bdr<3;
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
A1c_Result2 
HTN
past_smoker
curr_smoker;
output out=allprog2  predicted=pda1;
run;

data dir1.allprog2; 
set allprog2;
if cat=1 then wa1=pna1/pda1; else  wa1=(1-pna1)/(1-pda1);
run;

/******* Model 1-1 ********/
/*composite total effect*/
proc phreg data=dir1.PHREG_data2 covsandwich;
  class patient_id cat(ref='0') 
 bad Eye
 /descending;
 model (start, stop)*bad(0) =  cat /rl;
 ID patient_id Eye;
 weight wa1;
run;

/*composite total effect -- Eye : numeric variable */
proc phreg data=dir1.PHREG_data2 covsandwich;
  class patient_id cat(ref='0') 
 bad Eye_num
 /descending;
 model (start, stop)*bad(0) =  cat /rl;
 ID patient_id Eye_num;
 weight wa1;
run;

/*progression total effect*/
proc phreg data=dir1.allprog2 covsandwich;
  class patient_id cat(ref='0') 
 prog Eye
 /descending;
 model (start, stop)*prog(0) =  cat  /rl;
 ID patient_id Eye;
 weight wa1;
run;

/*progression total effect -- Eye : numeric variable */
proc phreg data=dir1.allprog2 covsandwich;
  class patient_id cat(ref='0') 
 prog Eye_num
 /descending;
 model (start, stop)*prog(0) =  cat  /rl;
 ID patient_id Eye_num;
 weight wa1;
run;

/*pdr total effect*/
proc phreg data=dir1.PHREG_data2 covsandwich;
  class patient_id cat(ref='0') 
 pdr Eye
 /descending;
 model (start, stop)*pdr(0) =  cat  /rl;
 ID patient_id Eye;
 weight wa1;
run;

/*pdr total effect -- Eye : numeric variable */
proc phreg data=dir1.PHREG_data2 covsandwich;
  class patient_id cat(ref='0') 
 pdr Eye_num
 /descending;
 model (start, stop)*pdr(0) =  cat  /rl;
 ID patient_id Eye_num;
 weight wa1;
run;

/******* Model 1-2 ********/
/*composite total effect*/
proc phreg data=dir1.PHREG_data2 covsandwich;
  class patient_id cat(ref='0') 
 bad
 /descending;
 model (start, stop)*bad(0) =  cat /rl;
 ID patient_id Replication;
 weight wa1;
run;

/*progression total effect*/
proc phreg data=dir1.allprog2 covsandwich;
  class patient_id cat(ref='0') 
 prog
 /descending;
 model (start, stop)*prog(0) =  cat  /rl;
 ID patient_id Replication;
 weight wa1;
run;

/*pdr total effect*/
proc phreg data=dir1.PHREG_data2 covsandwich;
  class patient_id cat(ref='0') 
 pdr
 /descending;
 model (start, stop)*pdr(0) =  cat  /rl;
 ID patient_id Replication;
 weight wa1;
run;
