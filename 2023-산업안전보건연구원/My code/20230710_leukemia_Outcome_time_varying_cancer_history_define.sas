/* Define time-varying cancer history variable */

LIBNAME dir 'E:\koshri-23\data';
LIBNAME mine 'E:\koshri-23\eklee\data';

/* Get distinct INDI_ID who have cancer history for leukemia cancer outcome */
proc sql;
create table temp as select distinct INDI_ID from mine.final_leukemia_no2
where leukemia_cancer_history=1;
quit;

/* Get record who is in temp tbl */
proc sql;
create table check_cancer as select distinct INDI_ID, fdx1 from mine.My_raw
where INDI_ID in (select * from temp);
quit;

/* joint counting-process format for leukemia cancer outcome tbl and check_cancer tbl */
DATA final_leukemia_no2;
SET mine.final_leukemia_no2;
DROP leukemia_cancer_history start stop;
RUN;

proc sql;
create table mine.temp2 as select * from final_leukemia_no2 as a join check_cancer as b
on a.INDI_ID = b.INDI_ID;
quit;

/* Define "between_yes" variable */
DATA temp3;
SET mine.temp2;
IF (fdx1> ECNY_DT) and (fdx1 < OUT_DT) then between_yes=1;
ELSE between_yes=0;
run;

/* Get records who have "between_yes" = 1*/
proc sql;
create table yes_tbl as select * from temp3
where INDI_ID in (select distinct INDI_ID from temp3 where between_yes=1);
quit;

/* Plus the obs cancer history for leukemia_cancer outcome */
DATA yes_1;
SET yes_tbl;
IF between_yes=1;
run;

DATA first last;
SET yes_1;
ECNY_DT2 = ECNY_DT;
OUT_DT2 = fdx1;
DROP ECNY_DT OUT_DT;
RENAME ECNY_DT2 = ECNY_DT OUT_DT2 = OUT_DT;
Output first;
ECNY_DT2 = fdx1;
OUT_DT2 = OUT_DT;
DROP ECNY_DT OUT_DT;
RENAME ECNY_DT2 = ECNY_DT OUT_DT2 = OUT_DT;
Output last;
run;

proc sql;
create table except_yes as select * from yes_tbl
where NO2 not in (select NO2 from yes_1);
quit;

DATA yes_all;
SET except_yes first last;
DROP between_yes;
run;

proc sort data = yes_all;
BY INDI_ID ECNY_DT OUT_DT;
run;

/* Get all records who don't have "between_yes" = 1 */
proc sql;
create table check_ID as select * from temp3
where INDI_ID not in (select distinct INDI_ID from yes_tbl);
quit;

/* MERGE data who have cancer history for leukemia cancer outcome */
DATA mine.final_history_yes;
SET yes_all check_ID(DROP = between_yes);
IF ECNY_DT >= fdx1 then leukemia_cancer_history=1;
ELSE leukemia_cancer_history=0;
run;

/* MERGE all data */
proc sql;
create table final_history_not as select * from mine.final_leukemia_no2
where INDI_ID not in (select distinct INDI_ID from mine.final_history_yes);
quit;

DATA final_history_not2;
SET final_history_not;
leukemia_cancer_history=0;
run;

DATA mine.final_time_varying_cancer;
SET mine.final_history_yes(DROP = fdx1) final_history_not(DROP = start stop);
run;

proc sort data = mine.final_time_varying_cancer;
BY INDI_ID ECNY_DT OUT_DT;
run;

/* Add (Start, Stop] and "NO3" variable */
DATA mine.final_leukemia_time_varying_NO3;
SET mine.final_time_varying_cancer;
BY INDI_ID ECNY_DT OUT_DT;
RETAIN first;
IF first.INDI_ID then first = ECNY_DT;
start = (input(ECNY_DT, yymmdd8.) - input(first, yymmdd8.))/365.25;
stop = (input(OUT_DT, yymmdd8.) - input(first, yymmdd8.))/365.25;
NO3 = _n_;
DROP first;
run;

