/***** 221209 ******/
/** FU_Duration2 Redefine & Event variable cleaning ***/
libname dir1 "C:\Users\stat\OneDrive - 숙명여자대학교\석사연구\당뇨병_망막변성_연구자료\sas code\My_data";
run;

/* Import data_ver2 */
/* Data Cleaning */
DATA data;
SET dir1.data_ver2;
FU_Duration2=.;
DROP FU_duration A1c_Baseline avg_A1c;
RUN;

proc sort data=data;
by Patient_ID Eye;
run;

DATA data2;
SET data;
BY Patient_ID Eye;
IF last.Eye then FU_Duration2 = (Date_of_follow_up_exam_2 - A1c_Create_TimeStamp_2)/365.25;
run;


/* FU_Duration variable redefine */
PROC IML;
USE data2 var {Patient_ID Eye Date_of_baseline_exam_2 A1c_Create_TimeStamp_2 Replication FU_Duration2}; 
READ ALL into result[colname=varNames];

DO i=1 TO NROW(result);
	IF result[i,"FU_Duration2"] = . THEN result[i , "FU_Duration2"]=(result[(i+1), "A1c_Create_TimeStamp_2"]-result[i, "A1c_Create_TimeStamp_2"])/365.25;
END;

create data_FU_Duration FROM result[colname=varNames];
APPEND FROM result;
CLOSE data2;
QUIT;

DATA data_FU_Duration;
SET data_FU_Duration;
RENAME FU_Duration2 = FU_Duration;
run;

proc sort data=data;
by Date_of_baseline_exam_2 A1c_Create_TimeStamp_2 Replication;
run;

proc sort data=data_FU_Duration;
by Date_of_baseline_exam_2 A1c_Create_TimeStamp_2 Replication;
run;

DATA mydata;
MERGE data data_FU_Duration;
BY Date_of_baseline_exam_2 A1c_Create_TimeStamp_2 Replication;
DROP FU_Duration2;
run;

/* FU_Duration variable quality check */
proc sql;
select * from mydata
where FU_Duration=0;
quit;

proc sql;
select Patient_ID, Eye, Date_of_baseline_exam_2, Date_of_follow_up_exam_2, A1c_Create_TimeStamp_2, Replication, FU_Duration, A1c_Result from dir1.newdata
where Patient_ID in ("BASA", "DEBE", "DORJO", "MIDA");
quit;



/****************************************************/
/*Related Event variable(bad, prog, pdr) cleaning */
proc sql;
create table check_event as select Patient_ID,  Eye, SUM(bad) as bad_check, SUM(prog) as prog_check, SUM(pdr) as pdr_check from mydata
group by Patient_ID, Eye;
quit;

proc sort data=check_event;
by Patient_ID Eye;
run;

proc sort data=mydata;
by Patient_ID Eye;
run;

DATA event_data;
MERGE mydata check_event;
BY Patient_ID Eye;
run;

proc sort data=event_data;
by Patient_ID Eye bad prog pdr;
run;


DATA newmydata;
SET event_data;
BY Patient_ID Eye bad prog pdr;

IF (bad_check>=1) & (last.Eye)  then bad=1;
ELSE bad=0;

IF (prog_check>=1) & (last.Eye)  then prog=1;
ELSE prog= 0;

IF (pdr_check>=1) & (last.Eye)  then pdr=1;
ELSE pdr=0;
DROP bad_check prog_check pdr_check;
RUN;


/* Data check */
/*proc sql;
create table event_table as select Patient_ID, Eye, Replication, bad, prog, pdr  from newmydata
where bad=1 | prog=1 | pdr=1;
quit;*/


/* FU_Duration negative obs check -> delete */
proc sort data=newmydata;
by Patient_ID Eye;
run;

DATA dir1.newdata;
SET newmydata;
BY Patient_ID Eye;
IF (last.Eye) & (FU_Duration<0) then DELETE;
run; 

/* check */
proc sql;
select distinct Patient_ID from dir1.newdata
where FU_duration<0;
quit;
