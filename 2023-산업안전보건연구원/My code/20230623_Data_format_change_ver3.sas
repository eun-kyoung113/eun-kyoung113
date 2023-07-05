LIBNAME dir 'E:\koshri-23\data';
LIBNAME mine 'E:\koshri-23\eklee\data';

/* Get distinct INDI_ID who have "between_yes=1" */
proc sql;
create table between_yes as select distinct INDI_ID from mine.lung_temp
where between_yes=1;
quit;

/* Get all INDI_ID's record who have "between_yes=1" */
proc sql;
create table between_yes_all as select b.* from between_yes as a join mine.lung_temp as b
on a.INDI_ID = b.INDI_ID;
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
NO2=NO;
Outcome_Category2=Outcome_Category;
IF Outcome_Category2='lung_cancer' then Event=1;
ELSE Event=0;
Output last_yes;
KEEP INDI_ID NO2 start stop Outcome_Category Outcome_date Event;
RENAME NO2 = NO start = ECNY_DT stop = OUT_DT;
run;

/* not first.INDI_ID + between_yes=1 */
proc sql;
create table not_first as select * from between_yes_all as a except select * from first_yes;
quit;
 
proc sort data = not_first;
BY INDI_ID ECNY_DT OUT_DT;
run;

DATA temp2 last;;
SET not_first;
BY INDI_ID ECNY_DT OUT_DT;
start = lag(OUT_DT);
stop=ECNY_DT;
NO2=put('',8.);
Event=0;
IF first.INDI_ID and between_yes=0 then do;
start = ECNY_DT;
stop = OUT_DT;
NO2=NO;
Event=0;
end;
Output temp2;
IF between_yes=1 then do;
	start = ECNY_DT;
	stop = Outcome_date;
	NO2=NO;
	Outcome_Category2=Outcome_Category;
	IF Outcome_Category2='lung_cancer' then Event=1;
	ELSE Event=0;
	Output last;
end;
KEEP INDI_ID NO2 start stop Outcome_Category Outcome_date Event;
RENAME NO2 = NO start = ECNY_DT stop = OUT_DT;
run;

DATA final;
SET between_yes_all last_yes temp2 last;
IF ECNY_DT = OUT_DT and NO = '' then delete;
KEEP INDI_ID NO ECNY_DT OUT_DT Outcome_Category Outcome_date Event;
run;

DATA final2;
SET final;
IF Event ^= 1 then Event = 0;
IF OUT_DT > Outcome_date then delete;
run;

proc sort data = final2 nodupkey;
by INDI_ID ECNY_DT OUT_DT;
run;

proc sql;
create table mine.between_yes_info as select * from final2 as a join (select INDI_ID, SEX ,BYEAR, Entry, Cancer_History from mine.N1_data as b)
on a.INDI_ID = b.INDI_ID;
quit;

proc sort data = mine.between_yes_info;
BY INDI_ID ECNY_DT OUT_DT;
run;


/* Get all INDI_ID's record who don't have "between_yes=1" */
proc sql;
create table between_no as select distinct INDI_ID from mine.lung_temp except select * from between_yes;
quit;

proc sql;
create table between_no_all as select b.* from between_no as a join mine.lung_temp as b
on a.INDI_ID = b.INDI_ID;
quit;

proc sort data = between_no_all;
BY INDI_ID ECNY_DT OUT_DT;
run;

/* Only one record */
proc sql;
create table counts as select INDI_ID, count(*) as obs_count  from between_no_all
group by INDI_ID;
quit;

/* 관측치 개수가 1인 그룹의 데이터 추출 */
data first_no;
merge between_no_all counts;
by INDI_ID;
if obs_count = 1;
run;

DATA temp_no last_no;
SET first_no;
start = ECNY_DT;
stop = Outcome_date;
NO2=NO;
Outcome_Category2=Outcome_Category;
IF Outcome_Category2='lung_cancer' then Event=1;
ELSE Event=0;
Output last_no;
KEEP INDI_ID NO2 start stop Outcome_Category Outcome_date Event;
RENAME NO2 = NO start = ECNY_DT stop = OUT_DT;
run;

DATA temp2 last;
SET between_no_all;
BY INDI_ID ECNY_DT OUT_DT;
start = lag(OUT_DT);
stop=ECNY_DT;
NO2=put('',8.);
Event=0;
IF first.INDI_ID then do;
start = ECNY_DT;
stop = OUT_DT;
NO2=NO;
Event=0;
end;
Output temp2;
IF last.INDI_ID then do;
	start = OUT_DT;
	stop = Outcome_date;
	NO2=put('',8.);
	Outcome_Category2=Outcome_Category;
	IF Outcome_Category2='lung_cancer' then Event=1;
	ELSE Event=0;
	Output last;
end;
KEEP INDI_ID NO2 start stop Outcome_Category Outcome_date Event;
RENAME NO2 = NO start = ECNY_DT stop = OUT_DT;
run;

DATA final;
SET between_no_all temp2 last;
KEEP INDI_ID NO ECNY_DT OUT_DT Outcome_Category Outcome_date Event;
run;

proc sort data = final nodupkey;
by INDI_ID NO ECNY_DT OUT_DT;
run;

proc sql;
create table info_merge as select * from final as a join (select INDI_ID, SEX ,BYEAR, Entry, Cancer_History from mine.N1_data as b)
on a.INDI_ID = b.INDI_ID;
quit;
