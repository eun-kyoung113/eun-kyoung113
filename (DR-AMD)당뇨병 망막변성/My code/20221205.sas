/***** 221205  ~ 221206 ******/
libname dir1 "C:\Users\stat\OneDrive - 숙명여자대학교\석사연구\당뇨병_망막변성_연구자료\sas code\My_data";
run;

/* Import Birth table */
DATA Birth_A1c;
SET dir1.Birth_A1c;
run;

/* Data quality check */
/*proc sql;
create table check_unique as select * from Birth_A1c
where A1c_Create_TimeStamp_2 - Date_of_baseline_exam_2 <0;
quit; */

/* Import all2 table */
DATA all2;
SET dir1.all2;
run;

/* Check Intersection PERSON_ID */
/*proc sql;
create table dir1.check_all2_Birth_A1c as select * from all2
where Patient_ID not in (select  Patient_ID from Birth_A1c);
quit;*/

/* all2 table & Birth_A1c table joint (joint key is "Patient_ID") */
proc sql;
create table dir1.data_ver2 as select * from all2 join Birth_A1c
on all2.Patient_ID = Birth_A1c.Patient_ID;
quit;

/* Data Cleaning */
DATA data;
SET dir1.data_ver2;
FU_Duration2=.;
DROP FU_duration A1c_Baseline avg_A1c;
RUN;

/* Date_of_follow_up_exam_2 <= A1c_Create_TimeStamp_2 check */
/*proc sql;
create table check_time as select Patient_ID, Date_of_baseline_exam_2, Date_of_follow_up_exam_2, A1c_Create_TimeStamp_2, Replication from data
where Date_of_follow_up_exam_2 <= A1c_Create_TimeStamp_2;
quit; */

proc sort data=data;
by Patient_ID;
run;

/*Event time check */
/*proc sql;
create table check_event as select Patient_ID, pdr, prog, bad, Replication from data
where (pdr=1) or (prog=1) or (bad=1);
quit;*/

/* FU_Duration variable redefine */
PROC IML;
USE data var {Patient_ID Eye Date_of_baseline_exam_2 A1c_Create_TimeStamp_2 Replication FU_Duration2}; 
READ ALL into result[colname=varNames];

DO i=1 TO NROW(result);
	IF result[i,"Replication"]>1 THEN result[i,"FU_Duration2"]=(result[i,"A1c_Create_TimeStamp_2"] - result[(i-1),"A1c_Create_TimeStamp_2"])/365.25;
    ELSE result[i,"FU_Duration2"]=(result[i,"A1c_Create_TimeStamp_2"] - result[i,"Date_of_baseline_exam_2"])/365.25;
END;

DO i=1 TO NROW(result);
	IF result[i,"FU_Duration2"] = 0 THEN result[i,"FU_Duration2"] = result[(i-1),"FU_Duration2"];
END;

create data_FU_Duration FROM result[colname=varNames];
APPEND FROM result;
CLOSE data;
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

DATA dir1.mydata;
MERGE data data_FU_Duration;
BY Date_of_baseline_exam_2 A1c_Create_TimeStamp_2 Replication;
DROP FU_Duration2;
run;


