/***** 231004 ******/
/** Sensitivity analysis***/
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

data allprog2;  /* needed for 2-step progression outcomes */
set PHREG_data;
if bdr<3;
run;

/****** Model 2 ******/
/*composite total effect*/
proc phreg data=PHREG_data covsandwich;
class patient_id cat(ref='0')  
bad
bdr
DM_type 
HTN
Sex
past_smoker
ever_smoker
curr_smoker
/descending;
model (start, stop)*bad(0) =
cat
age
agesq
sex
bdr
DM_type
DM_duration 
A1c_Result2 
HTN
past_smoker
curr_smoker
/ rl;
 ID patient_id;
run;

/*progression total effect*/
proc phreg data=allprog2 covsandwich;
  class patient_id cat(ref='0')  prog bdr
DM_type 
HTN
Sex
past_smoker
ever_smoker
curr_smoker
 /descending;
model (start, stop)*prog(0) =  
cat
age
agesq
sex
bdr
DM_type
DM_duration 
A1c_Result2 
HTN
past_smoker
curr_smoker
/ rl;
 ID patient_id;
run;

/*pdr total effect*/
proc phreg data=PHREG_data covsandwich;
  class patient_id cat(ref='0')  bad bdr
DM_type 
HTN
Sex
past_smoker
ever_smoker
curr_smoker
/descending;
model (start, stop)*pdr(0) =  cat
age
agesq
sex
bdr
DM_type
DM_duration 
A1c_Result2 
HTN
past_smoker
curr_smoker
/ rl;
 ID patient_id;
run;



/****** Model 3 ******/
/*composite total effect*/
proc phreg data=PHREG_data covsandwich;
class patient_id cat(ref='0')  
bad
bdr 
HTN
Sex
curr_smoker
/descending;
model (start, stop)*bad(0) =
cat
age
sex
bdr
A1c_Result2 
HTN
curr_smoker
/ rl;
 ID patient_id;
run;

/*progression total effect*/
proc phreg data=allprog2 covsandwich;
  class patient_id cat(ref='0')  prog bdr 
HTN
Sex
curr_smoker
 /descending;
model (start, stop)*prog(0) =  
cat
age
sex
bdr
A1c_Result2 
HTN
curr_smoker
/ rl;
 ID patient_id;
run;

/*pdr total effect*/
proc phreg data=PHREG_data covsandwich;
  class patient_id cat(ref='0')  pdr
bdr 
HTN
Sex
curr_smoker
/descending;
model (start, stop)*pdr(0) =  cat
cat
age
sex
bdr
A1c_Result2  
HTN
curr_smoker
/ rl;
 ID patient_id;
run;

