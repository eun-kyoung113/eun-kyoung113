LIBNAME dir 'E:\koshri-23\data';
LIBNAME mine 'E:\koshri-23\eklee\data';
LIBNAME dir1 'L:\';
LIBNAME mine1 'D:\';

DATA Num_Db3;
SET dir1.Db3;
BYEAR2=input(BYEAR, 16.);
BMONTH2=input(BMONTH, 16.);
BDAY2=input(BDAY, 16.);
IF OUT_DT = ' ' then OUT_DT = '20181231';
ECNY_DT_2=input(ECNY_DT,yymmdd10.);
OUT_DT_2=input(OUT_DT,yymmdd10.);
AGE2=input(AGE, 12.);

/* DURATION 변수 새로 정의 */
DURATION2=(OUT_DT_2-ECNY_DT_2)/365.25; 

DROP BYEAR BMONTH BDAY AGE DURATION ECNY_DT_2 OUT_DT_2;
RENAME BYEAR2=BYEAR BMONTH2=BMONTH BDAY2=BDAY AGE2=AGE DURATION2=DURATION;
RUN;

/* ②, ④, ⑤, ⑥ - Extract INDI_ID which need to be excluded */
PROC SQL;
create exclude_ID as select distinct INDI_ID from dir.Num_Db3
where (INDI_ID = '1000000000001') or (BYEAR not between 1940 and 1999) or 
(BMONTH not between 1 and 12) or (BDAY not between 1 and 31) or 
(AGE not between 1 and 99) or (DURATION < 0) or (DURATION <> '');
quit;

/* ③ */
DATA temp;
SET dir.Num_Db3(KEEP = INDI_ID NO ECNY_DT fdx1-fdx6);
RUN;

PROC SORT DATA = temp;
BY INDI_ID ECNY_DT;
RUN;

DATA temp2;
SET temp;
BY INDI_ID ECNY_DT;
IF first.INDI_ID; /* 최초 고용보험 등록 일자 추출 */
IF ECNY_DT > fdx1 or ECNY_DR > fdx2 or ECNY_DT > fdx3 or ECNY_DT > fdx4 or ECNY_DT > fdx5
   ECNY_DT > fdx6;
KEEP INDI_ID;
RUN;

/* Exclude ID Merge */
DATA Exclude;
SET exclude_ID temp2;
RUN;

/* Extract Included ID */
PROC SQL;
create table include_ID as select distinct INDI_ID from dir.Num_Db3
except select * from Exclude;
quit;

/* Delete Obs which need to be excluded and ⑦ */
PROC SQL;
create table dir.Raw_temp as select a.* from dir.Num_Db3 as a join dir.include_ID as b
on a.INDI_ID = b.INDI_ID and a.ECNY_DT <> a.OUT_DT and a.ECNY_DT < a.OUT_DT and 
a.ECNY_DT < '20190101';
quit;
