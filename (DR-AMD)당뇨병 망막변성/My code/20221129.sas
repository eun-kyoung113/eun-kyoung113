/* Import Birth table */
libname dir1 "C:\Users\stat\OneDrive - 숙명여자대학교\석사연구\당뇨병_망막변성_연구자료\sas code\My_data";
run;

DATA birth;
SET dir1.birth;
run;

/*Excel file related A1c record import */
FILENAME REFFILE 'C:\Users\stat\OneDrive - 숙명여자대학교\석사연구\당뇨병_망막변성_연구자료\sas code\My_data\AMD DR Pts A1c Rlts2.xlsx';

PROC IMPORT DATAFILE=REFFILE OUT=A1c
	dbms=xlsx  REPLACE;
	SHEET = sheet1;
	GETNAMES=YES;
	/* Date variable format */
	format Date_of_baseline_exam mdyampm24. Date_of_follow_up_exam mdyampm25. A1c_Create_TimeStamp mdyampm25.;
RUN;

/*Unique MRN(Patient ID) check in A1c table */
proc sql;
create table check1 as select distinct mrn from A1c;
quit;

/* Birth table, A1c excel file joint */
/* "mrn" is joint key */
proc sql;
create table Birth_A1c as select Birth.PatientID, A1c.mrn, A1c.Date_of_baseline_exam_2, A1c.Date_of_follow_up_exam_2, A1c.A1c_Create_TimeStamp_2, A1c.A1c_Result, A1c.Replication
from Birth join A1c 
on Birth.MRN=A1c.mrn;
quit;

DATA dir1.Birth_A1c;
SET Birth_A1c;
RENAME PatientID = Patient_ID;
RUN;

/*Unique MRN(Patient ID) check in Birth_A1c table */
proc sql;
create table check2 as select distinct mrn
from Birth_a1c;
quit;

/* Make all2 & Store all2 data table */
data dramd;
set dir1.dramd4;
if category="Dronly" then cat=0;
else if  category="AMDandDR" then cat=1;
if curr_smoker<0 then curr_smoker=9;
if past_smoker<0 then past_smoker=9;
if curr_smoker=1 or past_smoker=1 then ever_smoker=1;
else if curr_smoker=0 and past_smoker=0 then ever_smoker=0;
else ever_smoker=9;

if eye="OD" then side=1;
else if eye="OS" then side=2;
else side=3;
if baselinedr="mild" then bdr=1;
else if baselinedr="mod" then bdr=2;
else if baselinedr="severe" then bdr=3;
else bdr=4;
if bdr=3 then bdr2=2;
else bdr2=bdr;
nside=side;
if patient_id="BOER"  then A1C_baseline=.;

if prog_to_pdr="no" then pdr=0; else pdr=1;
if v2_step_prog="no" then prog=0; else prog=1;
if pdr=1 or prog=1 then bad=1; else bad=0;
cnt+1; /*if first.patient_id then cnt=1*/; 

if amd_grade=0 then AMDG=1;
else AMDG=amd_grade;

A1cdiff=A1c_baseline -Avg_a1C;
if AMDG=4 then AMDG2=3;
else AMDG2=AMDG;
agesq=age*age;
smkcat=curr_smoker*cat;
A1ccat=A1C_baseline*cat;
/*if A1c_baseline>0 then A1CB=1; else A1CB=0;
if avg_A1c>0 then A1CA=1; else A1CA=0;
if A1cb=0 and a1ca=1 then A1c_base2=avg_a1c; else A1c_base2=A1c_baseline*/;
if patient_id="KALA"  then exccr=1;
else if patient_id="DAMU"  then exccr=1;
else exccr=0;

run;
data dramdexc;
set dramd;
if exccr=0;
run;
proc rank data=dramdexc group=2 out=med;
   var age  A1c_baseline  Avg_a1C   DM_duration ;
   ranks ageb A1CB  A1CA DMD;
run;
 proc sort data=med;
 by cnt;
 run;
proc sort data=dramdexc;
 by cnt;
 run;
data all;
   merge dramdexc med; by cnt;


/*** change quintile categories into scores */
array old {4}  ageb A1CB A1CA  DMD;
array new {4} ageb2 A1CB2  A1CA2  DMD2; 

   do i=1 to 4;
      new {i}= old{i}+1;
end;

if A1c_baseline<0 then  A1CB2=9;
if Avg_A1C <0 then  A1CA2=9;


run;
data allprog;
set all;
if bdr<3;
run;
/* PS weight  for composite and PDR*/
 /* exposure pred null */
proc logistic data=all descending;
 class patient_id cat bdr
DM_type 
HTN
Sex
past_smoker /descending;
model cat(event='1')=;
output out=all predicted=pna;
run;
/* exposure pred denominator */

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
HTN
past_smoker
curr_smoker
;;

output out=all predicted=pda;
run;
data all;
set all;
wa=pna/pda;
run;
proc univariate data=all;
var wa pna pda;
run;
data dir1.all2;
set all;
if avg_A1C>0;
if  A1C_baseline>0;
run;




