/****************************************************************************************************/
/*                                                                     What TO DO                                                                                   */
/************************************************************************************************** */
/* 1. Data cleaning                                                                                                                                                 */
/* 2  Data quality check                                                                                                                                          */
/* - 이상한 관측치가 존재하는 객체의 반복 측정치 모두 제거하는 방식 선택                                                                */
/* - 특정 조건 만족하는 “관측치”만 제거(정제 통해 충분히 객체의 정보 보존할 수 있으므로 “관측치” 제거 방식을 선택) */
/* 3. 추가 제외조건 적용 (최초 고용보험 등록일 전에 암 이력이 있는 객체 제외)                                                          */
/* 4. 추가 전처리(Data 살펴보았을 때 발견된 이상 case 정제)                                                                                   */
/* - 사망 사건이 발생한 객체 중, 사망일이 상실일보다 과거 시점인 관측치가 존재함. (행정 상의 문제로 판단됨)            */
/*    → 이 경우, 고용보험 상실일을 사망일로 변경                                                                                                      */
/* - 암 진단 일자(fdx1 ~ fdx6)과 사망 일자(DTH_DATE1 ~ DTH_DATE3) 중 달력에 존재하지 날짜를 가지는 객체          */ 
/*    모집단에서 추가 제외                                                                                                                                       */
/***************************************************************************************************/

LIBNAME dir 'E:\koshri-23\data';

/* 0. 원 데이터 불러오기 */
DATA cohort;
SET dir.Db3 (DROP = AGE DURATION DREGDATE REAL_AUTH_CODE2 DTH_POS MRR_STATUS DTH_EDU DTH_AGE NATIONALITY_CLASS_F NATIONALITY_F CODE_103 CODE_56 
                                 DTH_JOB NEW_DTH_CODE1 NEW_DTH_CODE2 BIZ_INDUTY8 BIZ_INDUTY9);
run;

proc sql;
	select count(distinct indi_id) as indi_id_count
	from cohort;;
quit;

/* 1.DB 코딩 오류 및 분석 대상에 포함되지 않는 자료 제거 */
/* 1-1. ENCY_DT(취득일), OUT_DT(상실일) 관련 코딩 오류 제거 */
/* 1-1-1. ECNY_DT가 결측이거나  ECNY_DT, OUT_DT가 8글자가 아닌 사람 제거 */
proc sql;
	create table cohort2 as
	select *
	from cohort
	where indi_id not in (
		select indi_id
		from cohort
		where missing(ECNY_DT) or  length(ECNY_DT) <> 8 or (not missing(out_dt) and length(out_dt) <> 8)
	);
quit;

/* 1-1-2. ECNY_DT - OUT_DT ≤ 0 인 사람 제거 */
proc sql;
	create table cohort as
	select *
	from cohort2
	where indi_id not in (
		select indi_id
		from cohort2
		where (not missing(out_dt)) and (input(out_dt, best12.) - input(ecny_dt, best12.) <= 0)
	);
quit;

/* 1-2. 생년월일 값이 잘못 입력된 데이터 삭제 */
/* 1-2-1. 생년월일 변수(BYEAR, BMONTH, BDAY)이 하나라도 결측 이고, 월이 1-12 사이가 아니고, 일이 1-31 사이가 아닌 사람 삭제 */
proc sql;
	create table cohort2 as
	select *
	from cohort
	where indi_id  not in (
	select indi_id
	from cohort
	where missing(byear) or missing(bmonth) or missing(bday) or not (input(bmonth,2.) between 1 and 12) or  not (input(bday, 2.) between 1 and 31)
	);
quit;

/* 1-2-2. 생년월일이 ENCY_DT보다 미래시점인 사람 삭제 */
proc sql;
	create table cohort as
	select *
	from cohort2
	where indi_id not in (
	select indi_id
	from cohort2
	where ECNY_DT <= cats(byear, bmonth, bday)
	);
quit;

/*1-2-3. 생년월일이 달력상에 존재하지 않는 날짜로 입력(ex. 1994.13.31 / 1999.04.31.)된 사람 삭제 */
/* 문자형으로 정의되어 있는 변수들을 날짜형으로 바꾸면 달력상에 존재하지 않는 날짜는 결측으로 표시됨을 이용 */
data tmp;
	set cohort (keep = indi_id byear bmonth bday);
	tmp_val = input(cats(byear, bmonth, bday), yymmdd10.);
	if tmp_val = . then output tmp;
run;

proc sql;
	create table cohort2 as
	select *
	from cohort
	where indi_id not in (
	select indi_id
	from tmp);
quit;

/* 1-3. 중복 제거 */
proc sort data = cohort2 noduprecs;
BY INDI_ID ECNY_DT;
run;

/* 1-4. 사업장 정보가 부족한 사람 제거 */
/* 사업장 정보 부족 ↔ 파견직을 뜻할 수 있음을 인지하고 있음 */
proc sql;
	create table cohort as
	select *
	from cohort2
	where indi_id not in (
		select indi_id
		from cohort2
		where BIZ_INDUTY10 = '' or JSSFC_CD = '' or JSSFC_NO = '' or BIZ_ZIP = '' or BIZ_NO = ''
	);
quit;

/* 1-5. 암 발생 일자 관련 변수 코딩 오류 제거 */
/* 암 발생 일자 또한 날짜형 변수이므로 YYYYMMDD 형태이어야 함. 즉, 변수 길이가 “8”이어야 한다. */
proc sql;
	create table cohort2 as
	select *
	from cohort
	where indi_id in (
		select indi_id
		from cohort
		where ((fdx1 = ''  or length(fdx1) = 8)) and
				  ((fdx2 = ''  or  length(fdx2) = 8)) and
					((fdx3 = ''  or length(fdx3) = 8)) and
					((fdx4 = ''  or length(fdx4) = 8)) and
					((fdx5 = ''  or length(fdx5) = 8)) and
					((fdx6 = ''  or length(fdx6) = 8))				
	);
quit;

/* 1-6. 사망 일자 관련 변수 코딩 오류 제거 */
/* 사망 연도나 월이 달력상에 존재하지 않는 값을 가지는 객체 기록 제외 */
proc sql;
	create table cohort as
	select *
	from cohort2
	where indi_id in (
	select indi_id
	from cohort2
	where (missing(DTH_DATE1) and missing(DTH_DATE2) and missing(DTH_DATE3)) or ((input(DTH_DATE1, 4.) between 1995 and 2023) and (input(DTH_DATE2, 2.) between 1 and 12) and (input(DTH_DATE3, 2.) between 1 and 31))
	);
quit;

/*Data quality check + “AGE”(입사 시 연령), “DURATION”(근속연수) 변수 재정의 (이상한 관측치가 존재하는 객체의 반복 측정치 모두 제거하는 방식 선택)
: 데이터를 살펴보니, 입사 시 연령, 근속연수 값이 음수 혹은 200 이상 등 이상치가 존재하여 새로 정의하는 것이 낫다고 판단. */

/* 2. 결측값 대체 및 변수 재정의 */
/* 2-1. out_dt 결측값 대체 */
/* out_dt가 결측 이라는 것은 현시점 근무를 하고 있다는 것을 뜻하나, 추적 종료 시점을 2018.12.31.로 정하였으므로 모두 2018.12.31.로 값 대체 */
data cohort2;
set cohort;
if out_dt='' then out_dt='20181231';
run;

/* 2-2. out_dt가 존재할 수 없는 날짜를 가지는 사람 삭제 */
/* 문자형으로 정의되어 있는 변수들을 날짜형으로 바꾸면 달력상에 존재하지 않는 날짜는 결측으로 표시됨을 이용 */
data tmp;
	set cohort2 (keep = indi_id out_dt);
	tmp_val = input(out_dt, yymmdd10.);
	if tmp_val = . then output tmp;
run;

proc sql;
	create table cohort as
	select *
	from cohort2
	where indi_id not in (
	select indi_id
	from tmp);
quit;

/*2-3. age, duration 변수 재생성*/
/* AGE : (취득일 ? 생년월일)/365.25 & DURATION = (상실일 ? 취득일)/365.25 방식으로 정의한 후, 소수점 둘째자리에서 반올림 */
data cohort2;
set cohort;
AGE = round( ( input(ECNY_DT, yymmdd10.) - input(cats(BYEAR, BMONTH, BDAY), yymmdd10.) )/365.25,  .01);
DURATION = round( ( input(OUT_DT, yymmdd10.) - input(ECNY_DT, yymmdd10.) ) /365.25, .01);
run;

proc sql;
	select count(distinct indi_id) as indi_id_count
	from cohort2;;
quit;

/* 특정 조건 만족하는 “관측치”만 제거(정제 통해 충분히 객체의 정보 보존할 수 있으므로 “관측치” 제거 방식을 선택) */

/* 3. 분석 대상에 포함되지 않는 자료 제거 */
/* 3-1. ECNY_DT가 ‘20181231’ 이후인 데이터(행) 삭제 */
/* 가지고 있는 암 DB가 2018.12.31.까지만 기록되어 있으므로 이후의 관측치는 삭제 */
data cohort;
	set cohort2;
	if ECNY_DT < '20181231';
run;

/* out_dt가 2018.12.31. 이후인 데이터의 값을 ‘20181231’로 변경 */
/* 추적 종료 시점을 2018.12.31.로 설정하였으므로  2019년부터의 상실일을 모두 2018.12.31.로 바꿈. */
data cohort2;
set cohort;
if out_dt > '20181231' then out_dt='20181231';
run;

proc sql;
	select count(distinct indi_id) as indi_id_count
	from cohort2;
quit;

/* 3-2. 출생연도를 기준으로 분석 대상 제한(1940 - 1999) */
/* “BYEAR” 별 관측치 개수 Histogram 살펴본 결과, 출생연도가 1940-1999년도 사이인 객체들의 반복 측정치가 높은 빈도를 가짐을 확인함 */
data cohort;
	set cohort2;
	where not missing(BYEAR) and input(BYEAR, 4.) BETWEEN 1940 AND 1999;
run;

proc sql;
	select count(distinct indi_id) as indi_id_count
	from cohort;;
quit;

/* 3-3. 근무기록 중 중첩이 있는 개인 제거 */
/* two-job 가지는 객체들 아예 제거함으로써 Data 간단하게 바꿈 */
/* “중첩” ↔ “ECNY_DT” 기준으로 정렬했을 때 이전 반복 측정치의 상실일이 현 반복 측정치의 취득일보다 미래시점이다. */
data del_list;
	set cohort;
	by indi_id;
	retain prev_out_dt;
	if first.indi_id then prev_out_dt = .;
	if not missing(prev_out_dt) and ecny_dt < prev_out_dt then output;
	prev_out_dt = out_dt;
run;

proc sql;
	create table cohort2 as
	select *
	from cohort
	where indi_id not in (
	select indi_id
	from del_list
);
quit;

proc sql;
	select count(distinct indi_id) as indi_id_count
	from cohort2;
quit;

/* 추가 제외조건 적용 (최초 고용보험 등록일 전에 암 이력이 있는 객체 제외) */
/* 3-4. 최초입사 전 암발병자 제거 */
 proc sort data = cohort2;
by INDI_ID ECNY_DT;
run;

/* 첫 암 진단 일자 변수가 “fdx1”임을 이용해 fdx1이 결측이 아닌 객체 중, 최초 고용보험 등록일과 fdx1 ~ fdx6의 최솟값 비교 */
DATA del_list;
SET cohort2;
BY INDI_ID ECNY_DT;
IF first.INDI_ID;
IF not missing(fdx1) and input(ECNY_DT, yymmdd8.) > min(input(fdx1, yymmdd8.), 
input(fdx2, yymmdd8.), input(fdx3, yymmdd8.), input(fdx4, yymmdd8.)
, input(fdx5, yymmdd8.), input(fdx6, yymmdd8.)) then output del_list;
run;

proc sql;
	create table cohort as
	select *
	from cohort2
	where indi_id not in (
	select indi_id
	from del_list
);
quit;

proc sql;
	select count(distinct indi_id) as indi_id_count
	from cohort;
quit;

/*추가 전처리(Data 살펴보았을 때 발견된 이상 case 정제) 
/* 사망 사건이 발생한 객체 중, 사망일이 상실일보다 과거 시점인 관측치가 존재함. (행정 상의 문제로 판단됨) 
  	→ 이 경우, 고용보험 상실일을 사망일로 변경*/
/* 암 진단 일자(fdx1 ~ fdx6)과 사망 일자(DTH_DATE1 ~ DTH_DATE3) 중 달력에 존재하지 날짜를 가지는 객체 모집단에서 추가 제외*/

/* 5. 추가 전처리 */
/* 5-1. 사망일이 상실일보다 과거 시점인 경우 상실일의 값을 사망일로 대체*/
DATA cohort2;
SET cohort;
DTH_DATE = cats(DTH_DATE1, DTH_DATE2, DTH_DATE3);
IF (not missing(DTH_DATE) and DTH_DATE < OUT_DT) then OUT_DT = DTH_DATE;
DROP DTH_DATE;
RUN;

/* 정제한 Data 저장 */
data dir.preproc1;
	set cohort2;
run;

/* DTH DATE or fdx1-fdx6 달력상에 존재하지 않는 값 있는지 확인 */
/* 문자형으로 정의되어 있는 변수들을 날짜형으로 바꾸면 달력상에 존재하지 않는 날짜는 결측으로 표시됨을 이용 */
DATA check_fdx;
SET dir.Preproc1;
IF not missing(fdx1) ;
fdx1_2 = input(fdx1, yymmdd10.);
IF fdx1_2 = . then output; /* count : 0 */
run;

DATA check_fdx2;
SET dir.Preproc1;
IF not missing(fdx2) ;
fdx2_2 = input(fdx2, yymmdd10.);
IF fdx2_2 = . then output; /* count : 1 */
run;

DATA check_fdx3;
SET dir.Preproc1;
IF not missing(fdx3) ;
fdx3_2 = input(fdx3, yymmdd10.);
IF fdx3_2 = . then output; /* count : 0 */
run;

DATA check_fdx4;
SET dir.Preproc1;
IF not missing(fdx4) ;
fdx4_2 = input(fdx4, yymmdd10.);
IF fdx4_2 = . then output; /* count : 0 */
run;

DATA check_fdx5;
SET dir.Preproc1;
IF not missing(fdx5) ;
fdx5_2 = input(fdx5, yymmdd10.);
IF fdx5_2 = . then output; /* count : 0 */
run;

DATA check_fdx6;
SET dir.Preproc1;
IF not missing(fdx6) ;
fdx6_2 = input(fdx6, yymmdd10.);
IF fdx6_2 = . then output; /* count : 0 */
run;

DATA check_DTH;
SET dir.Preproc1;
IF not missing(DTH_DATE1) and not missing(DTH_DATE2) and not missing(DTH_DATE3) ;
DTH_DATE = input(cats(DTH_DATE1, DTH_DATE2, DTH_DATE3), yymmdd10.);
IF DTH_DATE = . then output;
run;

/* dir.Preproc1에서 check_fdx2에서 추출된 객체 제외한 뒤, “dir.Preproc1_new” data 다시 저장 */
proc sql;
create table dir.Preproc1_new as select * from dir.Preproc1
where INDI_ID not in (select INDI_ID from check_fdx2);
quit;
