/****************************************************************/
/*                                            What TO DO                                             */
/****************************************************************/
/* 1. "Entry" Dummy variable 생성                                                            */
/* 2. 후에 Data grouping 위해 "출생연도", "성별" information 가져오기       */
/* 3. Outcome Define - 폐암 / 백혈병 발병 여부 + Outcome 별 진단일자   */
/* 4. 폐암 / 백혈병 별 개인 암 이력 변수 정의                                            */
/***************************************************************/

LIBNAME dir 'E:\koshri-23\data';
LIBNAME mine 'E:\koshri-23\eklee\data';

/* AGE, DURATION variable Data quality re-check */
/* AGE, DURATION 변수 값 중 음수이거나 혹은 입사 시 연령이 100세 초과인 경우 존재하는지 확인 */
proc sql;
create table check1 as select INDI_ID, NO, ECNY_DT, OUT_DT, BYEAR, BMONTH, BDAY, AGE, DURATION from dir.Preproc1_new
where AGE < 0 or AGE >100 or DURATION < 0; /* count : 0 */
quit;

/* 전처리한 데이터에서 Demographic Information, Outcome, 개인 암 이력 정의하기 위해 필요한 변수만 가져오기 */
DATA mine.my_raw;
SET dir.Preproc1_new;
KEEP INDI_ID NO SEX ECNY_DT OUT_DT fdx1 icd10_1 fdx2 icd10_2 fdx3 icd10_3 fdx4 icd10_4 fdx5 icd10_5 fdx6 icd10_6
		 DTH_DATE1-DTH_DATE3 BYEAR BMONTH BDAY;
RUN;

/* INDI_ID와 ECNY_DT 기준으로 정렬한 뒤, 각 객체별 첫 번째 측정치만 가져옴으로써 출생연도(BYEAR, BMONTH, BDAY), 성별(SEX), 첫 번째 고용보험 등록일(ECNY_DT) 변수 GET */
/* 후에 사망 여부 & 사망 일자 파악하기 위해 "cats" 함수 이용해 DTH_DATE1 ~ DTH_DATE3 변수값 연결(사망일자 의미) - "DTH_DATE" 변수 생성 */
proc sort data = mine.my_raw;
BY INDI_ID ECNY_DT;
run;

DATA temp;
SET mine.my_raw(DROP = NO);
BY INDI_ID ECNY_DT;
IF first.INDI_ID;
BYEAR2= input(BYEAR, 12.);
BMONTH2 = input(BMONTH, 12.);
BDAY2 = input(BDAY, 12.);
DTH_DATE = cats(DTH_DATE1, DTH_DATE2, DTH_DATE3);
DROP BYEAR BMONTH BDAY DTH_DATE1-DTH_DATE3;
RENAME BYEAR2=BYEAR BMONTH2=BMONTH BDAY2=BDAY;
run;

/* 최초 고용보험 일자(ECNY_DT) 변수 이용해 "Entry" 범주형 변수 생성 + 사망일자(DTH_DATE) 변수의 결측 여부 이용해 사망 여부 변수("Death") 정의 */
/* "Entry" + "Death" + "DTH_DATE" + "SEX" + "INDI_ID" + "BYEAR" 변수 가져와 "Demographic_Info" tbl 생성 */
DATA mine.Demographic_Info;
SET temp;
first_year = substr(ECNY_DT, 1, 4);
IF first_year <= '1999' then Entry_1999 = 1;
ELSE Entry_1999 = 0;

IF first_year > '1999' and  first_year <= '2004' then Entry_2004 = 1;
ELSE Entry_2004=0;

IF first_year > '2004' and first_year <= '2009' then Entry_2009 = 1;
ELSE Entry_2009=0;

IF first_year > '2009' and first_year <= '2014'  then Entry_2014 =1;
ELSE Entry_2014 = 0;

IF first_year > '2014' then Entry_2018=1;
ELSE Entry_2018=0;

IF not missing(DTH_DATE) then Death=1;
ELSE Death=0;
KEEP INDI_ID BYEAR SEX Entry_1999 Entry_2004 Entry_2009 Entry_2014 Entry_2018 DTH_DATE Death;
RUN; 

/* Outcome(폐암 / 백혈병 발병 유무 + 진단일자) + Cancer History(Outcome 별 개인 암 이력 변수) Define한 "Cancer" tbl 생성 */
DATA mine.Cancer;
SET temp;
/* lung cancer */
IF substr(icd10_1,1,3) in ("C33",  "C34") then do;
	lung_cancer=1;
	lung_cancer_date=fdx1;
end;
ELSE IF substr(icd10_2,1,3) in ("C33",  "C34") then do;
	lung_cancer=1;
	lung_cancer_date=fdx2;
end; 
ELSE IF substr(icd10_3,1,3) in ("C33",  "C34") then do;
	lung_cancer=1;
	lung_cancer_date=fdx3;
end; 
ELSE IF substr(icd10_4,1,3) in ("C33",  "C34") then do;
	lung_cancer=1;
	lung_cancer_date=fdx4;
end; 
ELSE IF substr(icd10_5,1,3) in ("C33",  "C34") then do;
	lung_cancer=1;
	lung_cancer_date=fdx5;
end; 
ELSE IF substr(icd10_6,1,3) in ("C33",  "C34") then do;
	lung_cancer=1;
	lung_cancer_date=fdx6;
end; 
ELSE lung_cancer=0;

/* leukemia(백혈병) */
IF substr(icd10_1,1,3) in ("C91", "C92", "C93", "C94", "C95") then do;
	leukemia=1;
	leukemia_date=fdx1;
end;
ELSE IF substr(icd10_2,1,3) in ("C91", "C92", "C93", "C94", "C95") then do;
	leukemia=1;
	leukemia_date=fdx2;
end; 
ELSE IF substr(icd10_3,1,3) in ("C91", "C92", "C93", "C94", "C95") then do;
	leukemia=1;
	leukemia_date=fdx3;
end; 
ELSE IF substr(icd10_4,1,3) in ("C91", "C92", "C93", "C94", "C95") then do;
	leukemia=1;
	leukemia_date=fdx4;
end; 
ELSE IF substr(icd10_5,1,3) in ("C91", "C92", "C93", "C94", "C95") then do;
	leukemia=1;
	leukemia_date=fdx5;
end; 
ELSE IF substr(icd10_6,1,3) in ("C91", "C92", "C93", "C94", "C95") then do;
	leukemia=1;
	leukemia_date=fdx6;
end; 
ELSE leukemia=0;

/* Cancer History */
IF not missing(fdx1) and lung_cancer=0 then Lung_Cancer_History=1;
ELSE Lung_Cancer_History=0;
IF not missing(fdx1) and leukemia=0 then Leukemia_Cancer_History=1;
ELSE Leukemia_Cancer_History=0;

KEEP INDI_ID lung_cancer lung_cancer_date leukemia leukemia_date Lung_Cancer_History Leukemia_Cancer_History;
RUN;

/* "Demographic_Info" tbl과 "Cancer" tbl "INDI_ID" 변수 기준으로 MERGE 해 "N1_raw" tbl 생성 */
/* Counting-process format 형성 때 사용할 tbl */
proc sql;
create table mine.N1_raw as select * from mine.Demographic_Info as a join mine.Cancer as b
on a.INDI_ID = b.INDI_ID;
quit;
