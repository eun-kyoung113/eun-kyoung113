/** Exposure Define **/
libname dir1 "/userdata12/room111/data_out/data_store";
run;

DATA screening_08;
SET dir1.screening_08;
KEEP PERSON_ID HME_DT BMI;
RUN;

DATA screening_0915;
SET dir1.screening_09_15;
KEEP PERSON_ID HME_DT BMI;
RUN;

DATA screening_1617;
SET '/userdata12/room111/data_source/1619/nhis_heals_gj_1617.sas7bdat';
KEEP PERSON_ID HME_DT BMI;
RUN;

DATA screening_19;
SET '/userdata12/room111/data_source/1619/nhis_heals_gj_1819.sas7bdat';
IF PERSON_ID NOT = '';
KEEP PERSON_ID HME_DT BMI;
RUN;

DATA ex_screening_08;
SET dir1.ex_screening_08;
KEEP PERSON_ID HME_DT BMI;
RUN;

DATA ex_screening_0915;
SET dir1.ex_screening_09_15;
KEEP PERSON_ID HME_DT BMI;
RUN;

DATA ex_screening_1617;
SET '/userdata12/room111/data_source/1619/nhis_heals_lj_1617.sas7bdat';
IF PERSON_ID NOT = '';
KEEP PERSON_ID HME_DT BMI;
RUN;

/*** All Data Binding ***/
DATA screening_total;
SET screening_08 screening_0915 screening_1617 screening_19 ex_screening_08 ex_screening_0915 ex_screening_1617;
RUN;

/** Joint with Study population **/
proc sql;
create table Exposure_BMI as select a.PERSON_ID, a.HME_DT, b.HME_DT, b.BMI
from dir1.total_study_pop2 as a inner join screening_total as b
on a.PERSON_ID = b.PERSON_ID
and a.HME_DT = b.HME_DT
and not b.BMI in (.);
quit; 

/** Check **/
proc sql;
create table check_ID as select distinct PERSON_ID from Exposure_bmi;
quit;

DATA check;
SET Exposure_BMI;
ID = 1;
RUN;

proc sql;
create table check_num as select PERSON_ID, SUM(ID) as check from check
group by PERSON_ID;
quit;

DATA need_check;
SET check_num;
IF check > 1;
RUN;

proc sql;
create table check_HME_DT as select b.PERSON_ID, a.HME_DT 
from dir1.total_study_pop2 as a, need_check as b
where a.PERSON_ID = b.PERSON_ID;
quit;

proc sql;
create table check_record as select * from screening_total as a, check_HME_DT as b
where a.PERSON_ID = b.PERSON_ID
and a.HME_DT = b.HME_DT;
quit; 

proc sort data=check_record;
by PERSON_ID;
RUN;

/** Get Unique Pair **/
proc sql;
create table Exposure2 as select distinct PERSON_ID, BMI from Exposure_BMI;
quit;

/** Add Category of BMI **/
DATA dir1.BMI_Exposure;
SET Exposure2;
IF BMI < 18.5 THEN Category='Underweighted';
ELSE IF BMI < 25 THEN Category='Moderate';
ELSE IF BMI <30 THEN Category='Overweight';
ELSE Category = 'Obesity';
RUN;

/** Summary & Histogram **/
proc sort data=dir1.BMI_Exposure;
by Category;
RUN;

proc means data=dir1.BMI_Exposure;
var BMI;
by Category;
RUN;

proc univariate data=dir1.BMI_EXPOSURE;
var BMI;
by Category;
RUN;

proc sgplot data=dir1.BMI_Exposure;
histogram BMI / binstart=10 binwidth=0.5 showbins
group=Category transparency=0.5;
xaxis label = "BMI of Study Population";
density bmi / group=Category transparency=0.5;
run;

/****************************/
/*** BMI & WAIST trajetory ***/
libname dir1 "/userdata12/room111/data_out/data_store";
run;

DATA screening_08;
SET dir1.screening_08;
KEEP PERSON_ID HME_DT BMI WAIST;
RUN;

DATA screening_0915;
SET dir1.screening_09_15;
KEEP PERSON_ID HME_DT BMI WAIST;
RUN;

DATA screening_1617;
SET '/userdata12/room111/data_source/1619/nhis_heals_gj_1617.sas7bdat';
KEEP PERSON_ID HME_DT BMI WAIST;
RUN;

DATA screening_19;
SET '/userdata12/room111/data_source/1619/nhis_heals_gj_1819.sas7bdat';
IF PERSON_ID NOT = '';
KEEP PERSON_ID HME_DT BMI WAIST;
RUN;

DATA ex_screening_08;
SET dir1.ex_screening_08;
KEEP PERSON_ID HME_DT BMI WAIST;
RUN;

DATA ex_screening_0915;
SET dir1.ex_screening_09_15;
KEEP PERSON_ID HME_DT BMI WAIST;
RUN;

DATA ex_screening_1617;
SET '/userdata12/room111/data_source/1619/nhis_heals_lj_1617.sas7bdat';
IF PERSON_ID NOT = '';
KEEP PERSON_ID HME_DT BMI WAIST;
RUN;

/*** All Data Binding ***/
DATA screening_total;
SET screening_08 screening_0915 screening_1617 screening_19 ex_screening_08 ex_screening_0915 ex_screening_1617;
RUN;

/** Joint with Study population **/
proc sql;
create table BMI_track as select a.PERSON_ID, b.HME_DT, b.BMI
from dir1.total_study_pop2 as a inner join screening_total as b
on a.PERSON_ID = b.PERSON_ID
and a.HME_DT <= b.HME_DT
and not b.BMI in (.);
quit; 

proc sql;
create table WAIST_track as select a.PERSON_ID, b.HME_DT, b.WAIST
from dir1.total_study_pop2 as a inner join screening_total as b
on a.PERSON_ID = b.PERSON_ID
and a.HME_DT <= b.HME_DT
and not b.WAIST in (.);
quit; 

proc sort data=BMI_track;
BY PERSON_ID HME_DT;
RUN;

proc sort data=WAIST_track;
BY PERSON_ID HME_DT;
RUN;

proc sql;
create table check_BMI as select distinct PERSON_ID from BMI_track;
quit;

proc sql;
create table check_WAIST as select distinct PERSON_ID from WAIST_track;
quit;

DATA dir1.BMI_trajectory;
MERGE BMI_track dir1.BMI_EXPOSURE(DROP=BMI);
BY PERSON_ID;
RUN;

DATA dir1.WAIST_trajectory;
MERGE WAIST_track dir1.BMI_EXPOSURE(DROP=BMI);
BY PERSON_ID;
RUN;

proc sort data=dir1.BMI_trajectory;
by Category;
RUN;

proc means data=dir1.BMI_trajectory;
var BMI;
by Category;
RUN;

proc univariate data=dir1.BMI_trajectory;
var BMI;
by Category;
RUN;

DATA BMI_check;
SET dir1.BMI_trajectory;
HME_DT2=input(HME_DT,YYMMDD10.);
DROP HME_DT;
RUN;

proc sgplot data=BMI_check;
WHERE (Category='Underweighted');
scatter x=HME_DT2 y=BMI;
refline 18.5;
RUN;

proc sgplot data=BMI_check;
WHERE (Category='Moderate');
scatter x=HME_DT2 y=BMI;
refline 18.5;
refline 25;
RUN;

proc sgplot data=BMI_check;
WHERE (Category='Overweight');
scatter x=HME_DT2 y=BMI;
refline 25;
refline 30;
RUN;

proc sgplot data=BMI_check;
WHERE (Category='Obesity');
scatter x=HME_DT2 y=BMI;
refline 30;
RUN;

proc sort data=dir1.WAIST_trajectory;
by Category;
RUN;

proc univariate data=dir1.WAIST_trajectory;
var WAIST;
by Category;
RUN;

DATA WAIST_check;
SET dir1.WAIST_trajectory;
HME_DT2=input(HME_DT,YYMMDD10.);
DROP HME_DT;
RUN;

proc sgplot data=WAIST_check;
histogram WAIST / binstart=10 binwidth=0.5 showbins
group=Category transparency=0.5;
xaxis label = "WAIST trajectory of Study Population";
density WAIST / group=Category transparency=0.5;
run;


/************************************/
/* Check the number of recording BMI */
DATA BMI;
SET dir1.BMI_TRAJECTORY;
check=1;
RUN;

proc sort data=BMI;
BY PERSON_ID HME_DT;

proc sql;
create table BMI_N as select PERSON_ID, SUM(check) as BMI_check from BMI
group by PERSON_ID;
quit;

proc univariate data=BMI_N;
var BMI_check;
RUN;

proc sgplot data=BMI_N;
histogram BMI_check / binstart=1 binwidth=1 showbins;
xaxis label = "The number of recording BMI";
run;

/**************/
DATA WAIST;
SET dir1.WAIST_TRAJECTORY;
IF Missing(WAIST)=0; 
check=1;
RUN;

proc sort data=WAIST;
BY PERSON_ID HME_DT;

proc sql;
create table WAIST_N as select PERSON_ID, SUM(check) as WAIST_check from WAIST
group by PERSON_ID;
quit;

proc univariate data=WAIST_N;
var WAIST_check;
RUN;

proc sgplot data=WAIST_N;
histogram WAIST_check / binstart=1 binwidth=1 showbins;
xaxis label = "The number of recording WAIST";
run;


/**************************************/
/******* BMI Trajectory data split *******/
libname dir1 "/userdata12/room111/data_out/data_store";
run;

DATA BMI_traj1;
SET dir1.BMI_TRAJECTORY;
IF HME_DT<='20101231';
RUN;

DATA BMI_traj2;
SET dir1.BMI_TRAJECTORY;
IF HME_DT>='20110101';
RUN;
