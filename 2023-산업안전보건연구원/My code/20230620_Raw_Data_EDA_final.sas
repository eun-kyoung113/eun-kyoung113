/* Raw Data EDA */

LIBNAME dir 'E:\koshri-23\data';

/* 출생연도가 1940 ~ 1999년인 사람들의 데이터 필터링 */
/* DROP the unneeded variable */
DATA cohort;
SET dir.Db3 (DROP = AGE DURATION DREGDATE REAL_AUTH_CODE2 DTH_POS MRR_STATUS DTH_EDU DTH_AGE NATIONALITY_CLASS_F NATIONALITY_F CODE_103 CODE_56 
                                 DTH_JOB NEW_DTH_CODE1 NEW_DTH_CODE2);
WHERE (input(BYEAR, 4.) BETWEEN 1940 AND 1999) and (input(BMONTH,2.) between 1 and 12) and (input(BDAY, 2.) between 1 and 31);
run;

/* Check N */
proc sql;
create table check_N as select distinct INDI_ID from dir.Db3;
quit;

proc sql;
create table change_N as select distinct INDI_ID from cohort;
quit;

/* ECNY_DT filtering */
DATA cohort2;
SET cohort;
IF (ECNY_DT = OUT_DT) or (ECNY_DT>'20181231') THEN delete;
RUN;

/* Data quality check */
proc sql;
	select count(*) as null_count from cohort2
	where INDI_ID is null or INDI_ID = "1000000000001" or length(INDI_ID) <>13 ; /* count : 0 */
quit;

/* 상실일, 취득일의 입력 방식이 옳지 않은 데이터 check */
proc sql;
create table temp as select * from cohort2
where length(ECNY_DT) <> 8 or (not missing(OUT_DT) and  length(OUT_DT) <> 8);
quit;

/* 상실일, 취득일의 입력 방식이 옳지 않은 데이터 delete + OUT_DT가 결측인 관측치 대체 */
DATA cohort3;
SET cohort2;
IF length(ECNY_DT) ^= 8 or (not missing(OUT_DT) and length(OUT_DT) ^= 8) then delete;
IF OUT_DT = '' then OUT_DT = '20181231';
RUN;

/* Define "AGE", "DURATION" variable + IF DURATION < 0 delete */
DATA cohort4;
SET cohort3;
AGE = round((input(ECNY_DT, yymmdd8.) - input(cats(BYEAR,BMONTH,BDAY), yymmdd8.))/365.25, .01);
DURATION = round((input(OUT_DT, yymmdd8.) - input(ECNY_DT, yymmdd8.))/365.25, .01);
IF DURATION < 0  then delete;
RUN;

/* Dataset clean */
proc delete data=work.change_n; 
run;

proc delete data = work.check_n;
run;

proc delete data = work.temp;
run;

proc delete data = work.cohort;
run;

proc delete library=work data=cohort2-cohort3; 
run;

/* Duplicate ROW delete */
proc sort data = cohort4 noduprecs;
BY INDI_ID;
run;

/* Duplicate ROW 존재하는지 확인 */
proc sql;
create table check1 as select * from cohort4
where INDI_ID = '1000001273396';
quit;

/* 최초고용보험등록일자 전에 암 발병한 사람 추출 */
proc sort data = cohort4;
by INDI_ID ECNY_DT;
run;
 
DATA cohort5 exclude;
SET cohort4;
BY INDI_ID ECNY_DT;
IF first.INDI_ID;
IF (not missing(fdx1) and length(fdx1) ^= 8) or  (not missing(fdx2) and length(fdx2) ^= 8) or (not missing(fdx3) and length(fdx3) ^= 8)
or  (not missing(fdx4) and length(fdx4) ^= 8) or (not missing(fdx5) and length(fdx5) ^= 8)  or  (not missing(fdx6) and length(fdx6) ^= 8)  then delete; /* 21개 */

IF not missing(fdx1) and input(ECNY_DT, yymmdd8.) > min(input(fdx1, yymmdd8.), input(fdx2, yymmdd8.), input(fdx3, yymmdd8.), input(fdx4, yymmdd8.)
																						  , input(fdx5, yymmdd8.), input(fdx6, yymmdd8.)) then output exclude;
ELSE output cohort5;
run;

/* Result check */
DATA temp;
SET exclude;
KEEP INDI_ID ECNY_DT fdx1-fdx6;
run;

proc sql;
create table dir.new_raw as select a.* from cohort4 as a join cohort5 as b
on a.INDI_ID = b.INDI_ID;
quit;
