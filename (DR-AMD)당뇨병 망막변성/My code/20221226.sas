/***** 221226 ******/
/** Change the format of data type***/
/**** Run the PHREG Procedure ****/
libname dir1 "C:\Users\stat\OneDrive - 숙명여자대학교\석사연구\당뇨병_망막변성_연구자료\sas code\My_data";
run;

/* Import data_ver2 */
/* Data Cleaning */
DATA newmydata;
SET dir1.Newmydata;
RUN;

proc sort data=newmydata;
by Patient_ID Eye;
run;

/** Make cumsum FU_Duration variable & Define (start, stop] time format **/
DATA newmydata_ver0;
SET newmydata;
by Patient_ID Eye;
retain cumulative_FU_Duration;
if first.Eye then do;
	cumulative_FU_Duration=FU_Duration;
	start=0;
END;
else cumulative_FU_Duration = cumulative_FU_Duration + FU_Duration;
run;

PROC IML;
USE newmydata_ver0 var {ID Eye_num start cumulative_FU_Duration Replication}; 
READ ALL into result[colname=varNames];

DO i=1 TO NROW(result);
	IF result[i,"start"] = . THEN result[i , "start"]=result[(i-1), "cumulative_FU_Duration"];
END;

create newdata_start FROM result[colname=varNames];
APPEND FROM result;
CLOSE newmydata_ver0;
QUIT;

proc sql;
create table newmydata_ver1 as select * from newmydata join newdata_start
on (newmydata.ID = newdata_start.ID)
and (newmydata.Eye_num = newdata_start.Eye_num)
and (newmydata.Replication = newdata_start.Replication);
quit;

DATA dir1.PHREG_data;
SET newmydata_ver1;
RENAME cumulative_FU_Duration=stop;
run;

/* A1c_Result variable quality check */
/*proc sql;
select Patient_ID, Eye, Replication, A1c_Result, A1c_Result2 from PHREG_data
where A1c_Result2=.;
quit;

proc sql;
select Patient_ID, Eye, Replication, A1c_Result from PHREG_data
where Patient_ID = 'DEBE';
quit; */

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

data PHREG_data2; 
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

data allprog2; 
set allprog2;
if cat=1 then wa1=pna1/pda1; else  wa1=(1-pna1)/(1-pda1);
run;
/******* Model 1 ********/
/*composite total effect*/
proc phreg data=PHREG_data2 covsandwich;
  class patient_id cat(ref='0') 
 bad
 /descending;
 model (start, stop)*bad(0) =  cat /rl;
 ID patient_id;
 weight wa1;
run;

/*progression total effect*/
proc phreg data=allprog2 covsandwich;
  class patient_id cat(ref='0') 
 prog
 /descending;
 model (start, stop)*prog(0) =  cat  /rl;
 ID patient_id;
 weight wa1;
run;

/*pdr total effect*/
proc phreg data=PHREG_data2 covsandwich;
  class patient_id cat(ref='0') 
 pdr
 /descending;
 model (start, stop)*pdr(0) =  cat  /rl;
 ID patient_id;
 weight wa1;
run;
