/***** 221219 ******/
/** FU_Duration2 Redefine & Event variable cleaning ver1.1***/
/*** Change the code order (FU_Duration all --> Event) ***/
libname dir1 "C:\Users\stat\OneDrive - 숙명여자대학교\석사연구\당뇨병_망막변성_연구자료\sas code\My_data";
run;

/* Import data_ver2 */
/* Data Cleaning */
DATA data;
SET dir1.data_ver2;
FU_Duration2=.;
DROP FU_duration A1c_Baseline avg_A1c;
RUN;

/*Give unique ID for each Patient_ID */
proc sort data=data;
by Patient_ID;
run;

DATA data_ver0;
SET data;
BY Patient_ID;
retain ID 0;
if first.Patient_ID then ID=ID+1;

IF Eye="OD" then Eye_num=1;
ELSE IF Eye="OS" then Eye_num=2;
run;

/* FU_Duration variable redefine */
proc sort data=data_ver0;
by Patient_ID Eye;
run;

DATA data2;
SET data_ver0;
BY Patient_ID Eye;
IF last.Eye then FU_Duration2 = (Date_of_follow_up_exam_2 - A1c_Create_TimeStamp_2);
run;

PROC IML;
USE data2 var {ID Eye_num Date_of_baseline_exam_2 A1c_Create_TimeStamp_2 Replication FU_Duration2}; 
READ ALL into result[colname=varNames];

DO i=1 TO NROW(result);
	IF result[i,"FU_Duration2"] = . THEN result[i , "FU_Duration2"]=(result[(i+1), "A1c_Create_TimeStamp_2"]-result[i, "A1c_Create_TimeStamp_2"]);
END;

create data_FU_Duration FROM result[colname=varNames];
APPEND FROM result;
CLOSE data2;
QUIT;

DATA data_FU_Duration;
SET data_FU_Duration;
RENAME FU_Duration2 = FU_Duration;
run;

proc sql;
create table mydata_ver0 as select * from data2 join data_FU_Duration
on (data2.ID = data_FU_Duration.ID)
and (data2.Eye_num = data_FU_Duration.Eye_num)
and (data2.Replication = data_FU_Duration.Replication);
quit;

/* FU_Duration variable quality check */
proc sql;
select * from mydata_ver0
where FU_Duration=0;
quit;

proc sql;
select Patient_ID, Eye, Date_of_baseline_exam_2, Date_of_follow_up_exam_2, A1c_Create_TimeStamp_2, Replication, FU_Duration, A1c_Result from mydata_ver0
where Patient_ID in ("BASA", "DEBE", "DORJO", "MIDA", "MYBR");
quit;

/* Change the replication number */
DATA newdata;
SET mydata_ver0;
Replication2=Replication;
RUN;

proc sql;
create table N_group as select distinct ID, Eye_num, count(*) as N_count from newdata
group by ID, Eye_num;
quit;

proc sort data=newdata;
by ID Eye_num;
run;

proc sort data=N_group;
by ID Eye_num ;
run;

DATA new_add_N;
MERGE newdata N_group;
BY ID Eye_num;
ow_n = N_count-Replication;
RUN;

PROC IML;
USE new_add_N var {ID Eye_num Date_of_baseline_exam_2 A1c_Create_TimeStamp_2 Replication Replication2 N_count ow_n FU_Duration}; 
READ ALL into result2[colname=varNames];

DO i=1 TO NROW(result2);
	IF (result2[i,"FU_Duration"] = 0) & (result2[i,"ID"] =result2[(i+1),"ID"]) &  (result2[i,"Eye_num"] =result2[(i+1),"Eye_num"]) THEN do;
 	result2[(i+1) , "Replication2"]=result2[i, "Replication2"];
	result2[((i+2):(i+1+result2[(i+1),"ow_n"])),"Replication2"] = result2[((i+2):(i+1+result2[(i+1),"ow_n"])),"Replication"]-1;
	END;
END;
create newdata_ver1_2 FROM result2[colname=varNames];
APPEND FROM result2;
CLOSE new_add_N;
QUIT;

proc sort data=newdata;
by ID Eye_num Replication;
run;

proc sort data=newdata_ver1_2;
by ID Eye_num Replication;
run;

DATA new;
MERGE newdata newdata_ver1_2;
BY ID Eye_num Replication;
RUN;

/* check */
proc sql;
select Patient_ID, Eye, Date_of_baseline_exam_2, Date_of_follow_up_exam_2, A1c_Create_TimeStamp_2, Replication, Replication2, FU_Duration, A1c_Result from new
where Patient_ID in ("BASA", "DEBE", "DORJO", "MIDA", "MYBR");
quit;

DATA new_ver1;
SET new;
DROP Replication N_count ow_n;
RENAME Replication2 = Replication;
RUN;

/* Delete record which FU_Duration = 0 */
DATA new_ver2;
SET new_ver1;
IF FU_Duration = 0 then DELETE;
RUN;

/*check*/
proc sql;
select distinct Patient_ID from new_ver2
where FU_duration=0;
quit;

/* Check the record which FU_Duration <1 and FU_Duration >-1 */
proc sort data=new_ver2;
by Patient_ID Eye;
run;

DATA new_ver3;
SET new_ver2;
BY Patient_ID Eye;
IF (last.Eye) & (FU_Duration<1) & (FU_Duration>-1);
KEEP Patient_ID Eye Date_of_baseline_exam_2 Date_of_follow_up_exam_2 A1c_Create_TimeStamp_2 Replication FU_Duration;
RUN;

proc sql;
select distinct Patient_ID from new_ver3;
quit;


DATA new_ver4;
SET new_ver2;
BY Patient_ID Eye;
IF (last.Eye) & (FU_Duration<1) & (FU_Duration>-1) then DELETE;
FU_Duration=FU_Duration/365.25;
RUN;

/* Check Is there another negative value of FU_Duration */ 
/*proc sql;
select * from new_ver4
where FU_Duration<0;
quit;*/
/* No record */

/****************************************************/
/*Related Event variable(bad, prog, pdr) cleaning */
proc sql;
create table check_event as select Patient_ID,  Eye, SUM(bad) as bad_check, SUM(prog) as prog_check, SUM(pdr) as pdr_check from new_ver4
group by Patient_ID, Eye;
quit;

proc sort data=check_event;
by Patient_ID Eye;
run;

proc sort data=new_ver4;
by Patient_ID Eye;
run;

DATA event_data;
MERGE new_ver4 check_event;
BY Patient_ID Eye;
DROP FU_Duration2;
run;

proc sort data=event_data;
by Patient_ID Eye bad prog pdr;
run;


DATA dir1.newmydata;
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
select Patient_ID, Eye, Replication, bad, prog, pdr  from newmydata
where bad=1 | prog=1 | pdr=1;
quit;*/


