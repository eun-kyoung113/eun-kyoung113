LIBNAME dir 'E:\koshri-23\data';
LIBNAME mine 'E:\koshri-23\eklee\data';

proc sort data = mine.company_info;
BY INDI_ID ECNY_DT OUT_DT;
run;

proc sort data = mine.N1_raw;
BY INDI_ID;
run;

/* OutcomeÀÌ ¹éÇ÷º´ÀÎ Data generate */
DATA mine.leukemia_raw;
MERGE mine.company_info mine.N1_raw;
BY INDI_ID;
IF leukemia=1 then do;
	Outcome_Category="leukemia_cancer";
	Outcome_date = leukemia_date;
end;
ELSE IF Death=1 then do;
	Outcome_Category="Death";
	Outcome_date = DTH_DATE;
end;
ELSE DO;
	Outcome_Category="last_follow";
	Outcome_date = '20181231';
end;
IF (Outcome_date> ECNY_DT) and (Outcome_date < OUT_DT) then between_yes=1;
ELSE between_yes=0;
DROP DTH_DATE Death lung_cancer lung_cancer_date lung_cancer_history leukemia leukemia_date;
run;

/* "leukemia" tbl check */
proc sql;
create table check1 as select distinct Outcome_Category from mine.leukemia_raw
where between_yes=1;
quit;

proc sql;
create table check2 as select INDI_ID, ECNY_DT, OUT_DT, Outcome_Category, Outcome_date from mine.leukemia_raw
where INDI_ID not in (select distinct INDI_ID from mine.leukemia_raw where between_yes=1) and Outcome_Category='leukemia_cancer';
quit;

/** Counting-process format data generate **/
/* Get all INDI_ID's record who have "between_yes=1" */
proc sql;
create table between_yes_all as select * from mine.leukemia_raw 
where INDI_ID in (select distinct INDI_ID from mine.leukemia_raw where between_yes=1);
quit;

proc sort data = between_yes_all;
BY INDI_ID ECNY_DT OUT_DT;
run;

/* first.INDI_ID + between_yes=1 */
DATA first_yes;
SET between_yes_all;
BY INDI_ID ECNY_DT OUT_DT;
IF first.INDI_ID and between_yes=1;
run;

DATA temp_yes last_yes;
SET first_yes;
start = ECNY_DT;
stop = Outcome_date;
Output last_yes;
DROP ECNY_DT OUT_DT between_yes;
RENAME start = ECNY_DT stop = OUT_DT;
run;

/* between_yes=1 & have multiple records for each INDI_ID */
proc sql;
create table between_yes_much as select * from between_yes_all
where INDI_ID not in (select distinct INDI_ID from first_yes);
quit;

proc sort data = between_yes_much;
by INDI_ID ECNY_DT OUT_DT;
run;

DATA temp2 last;
SET between_yes_much;
BY INDI_ID ECNY_DT OUT_DT;
start = lag(OUT_DT);
stop=ECNY_DT;
NO2=put('',8.);

IF first.INDI_ID and between_yes=0 then do;
start = ECNY_DT;
stop = OUT_DT;
NO2=NO;
end;
Output temp2;

IF between_yes=1 then do;
	start = ECNY_DT;
	stop = Outcome_date;
	NO2=NO;
	Output last;
end;
DROP ECNY_DT OUT_DT NO between_yes;
RENAME NO2 = NO start = ECNY_DT stop = OUT_DT;
run;

DATA mine.final_between_yes;
SET between_yes_all last_yes temp2 last;
IF ECNY_DT = OUT_DT and NO = '' then delete;
IF OUT_DT > Outcome_date then delete;
DROP NEW_NY between_yes;
run;

proc sort data=mine.final_between_yes noduprecs;
BY INDI_ID ECNY_DT OUT_DT;
run;

/* Dataset clean */
proc delete data=work.between_yes_much;
run;

proc delete data = work.check1;
run;

proc delete data = work.check2;
run;

proc delete data = work.First_yes;
run;

proc delete data = work.last;
run;

proc delete data = work.last_yes;
run;

proc delete data = work.temp2;
run;

proc delete data = work.temp_yes;
run;

/* Get all INDI_ID's record who don't have "between_yes=1" */
proc sql;
create table between_no_all as select * from mine.leukemia_raw 
where INDI_ID not in (select distinct INDI_ID from between_yes_all);
quit;

proc sort data = between_no_all;
by INDI_ID ECNY_DT OUT_DT;
run;

/* Get all INDI_ID's record who have only just one replication */
proc sql;
create table counts as select INDI_ID, count(*) as obs_count  from between_no_all
group by INDI_ID;
quit;

/* between_yes=0 & first.INDI_ID */
DATA first_no;
MERGE between_no_all counts;
BY INDI_ID;
IF obs_count = 1;
DROP between_yes obs_count;
run;

DATA temp_no last_no;
SET first_no;
start = OUT_DT;
stop = Outcome_date;
NO2=put('',8.);;
Output last_no;
DROP ECNY_DT OUT_DT NO;
*DROP Outcome_date;
RENAME NO2 = NO start = ECNY_DT stop = OUT_DT;
run;

DATA no_first;
SET first_no last_no;
IF ECNY_DT = OUT_DT and NO = '' then delete;
run;

proc sort data=no_first noduprecs;
BY INDI_ID ECNY_DT OUT_DT;
run;

/* between_yes=0 & have much replication records */
proc sql;
create table between_no_much as select * from between_no_all
where INDI_ID not in (select distinct INDI_ID from first_no);
quit;

proc sort data = between_no_much;
by INDI_ID ECNY_DT OUT_DT;
run;

DATA temp2 last;
SET between_no_much;
BY INDI_ID ECNY_DT OUT_DT;
start = lag(OUT_DT);
stop=ECNY_DT;
NO2=put('',8.);
IF first.INDI_ID then do;
start = ECNY_DT;
stop = OUT_DT;
NO2=NO;
end;
Output temp2;
IF last.INDI_ID then do;
	start = OUT_DT;
	stop = Outcome_date;
	NO2=put('',8.);
	Output last;
end;
DROP NO ECNY_DT OUT_DT between_yes;
RENAME NO2 = NO start = ECNY_DT stop = OUT_DT;
run;

DATA mine.final_between_no;
SET between_no_much(drop = between_yes) no_first temp2 last;
IF ECNY_DT = OUT_DT and NO = '' then delete;
IF OUT_DT > Outcome_date then delete;
DROP NEW_NY;
run;

proc sort data = mine.final_between_no noduprecs;
by INDI_ID ECNY_DT OUT_DT;
run;

/* Data rbind */
DATA mine.final_format;
SET mine.final_between_yes mine.final_between_no;
IF NO='' then do;
	OUT_CZ='';
	BIZ_INDUTY10='';
	JSSFC_CD='';
	JSSFC_NO='';
	INDDIS_NO='';
	ENROL_NO='';
	BIZ_NM='';
	BIZ_NO='';
	BIZ_ADDRESS='';
	BIZ_ZIP='';
end;
IF OUT_DT = Outcome_date and Outcome_Category="leukemia_cancer" then Event=1;
ELSE Event=0;
RUN;

proc sort data = mine.final_format;
BY INDI_ID ECNY_DT OUT_DT;
run;

DATA mine.final_leukemia_NO2;
SET mine.final_format;
BY INDI_ID ECNY_DT OUT_DT;
No2 = _n_;
RETAIN first;
IF first.INDI_ID then first = ECNY_DT;
start = (input(ECNY_DT, yymmdd8.) - input(first,yymmdd8.))/365.25;
stop = (input(OUT_DT, yymmdd8.) - input(first,yymmdd8.))/365.25;
DROP first;
run;
