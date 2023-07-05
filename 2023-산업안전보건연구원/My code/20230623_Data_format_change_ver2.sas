LIBNAME dir 'E:\koshri-23\data';
LIBNAME mine 'E:\koshri-23\eklee\data';

/* Change data format for one person -- "between_yes"가 모두 0인 객체 대상 */
DATA temp;
SET mine.lung_temp;
IF INDI_ID = '1000000757032';
DROP SEX BYEAR Entry Cancer_History;
run;

proc sort data = temp;
BY INDI_ID ECNY_DT OUT_DT;
run;

DATA temp2 last;
SET temp;
BY INDI_ID ECNY_DT;
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
SET temp temp2 last;
KEEP INDI_ID NO ECNY_DT OUT_DT Outcome_Category Outcome_date Event;
run;

proc sort data = final nodupkey;
by INDI_ID ECNY_DT OUT_DT;
run;

proc sql;
create table info_merge as select * from final as a join (select INDI_ID, SEX ,BYEAR, Entry, Cancer_History from mine.N1_data as b)
on a.INDI_ID = b.INDI_ID;
quit;

/* Change data format for one person -- "between_yes"가 1이 존재하는  객체 대상 */
DATA temp;
SET mine.lung_temp;
IF INDI_ID = '1000000786132';;
DROP SEX BYEAR Entry Cancer_History;
run;

proc sort data = temp;
BY INDI_ID ECNY_DT OUT_DT;
run;

DATA temp2 last;
SET temp;
BY INDI_ID ECNY_DT;
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
SET temp temp2 last;
IF ECNY_DT = OUT_DT and NO = '' then delete;
KEEP INDI_ID NO ECNY_DT OUT_DT Outcome_Category Outcome_date Event;
run;

proc sort data = final nodupkey;
by INDI_ID ECNY_DT OUT_DT;
run;

DATA final2;
SET final;
IF Event ^= 1 then Event = 0;
IF OUT_DT > Outcome_date then delete;
run;

proc sql;
create table info_merge2 as select * from final2 as a join (select INDI_ID, SEX ,BYEAR, Entry, Cancer_History from mine.N1_data as b)
on a.INDI_ID = b.INDI_ID;
quit;

/* Final data rbind */
DATA total;
SET info_merge info_merge2;
IF Event ^ = 1 then Event=0;
run;

proc sort data = total;
BY INDI_ID ECNY_DT OUT_DT;
run;

/* 사업장 정보와 연결 */
DATA mine.company_info;
SET dir.New_raw;
DROP fdx1-fdx6 tcode1-tcode6 mcode1-mcode6 method1-method6 icd10_1-icd10_6 seer_grp1-seer_grp6 REAL_AUTH_CODE1 DTH_DATE1-DTH_DATE3 BYEAR BMONTH BDAY AGE DURATION;
run;

proc sort data = mine.company_info;
BY INDI_ID NO ECNY_DT OUT_DT;
run;

proc sort data = total;
BY INDI_ID NO ECNY_DT OUT_DT;
run;

proc sql;
create table total2 as select * from total as a left join mine.company_info as b
on a.INDI_ID  = b.INDI_ID and a.NO = b.NO;
quit;

proc sort data = total2;
BY INDI_ID ECNY_DT OUT_DT;
run;
