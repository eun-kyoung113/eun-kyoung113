/*********************************************************************************/
/*                                                               What TO DO                                                       */
/********************************************************************************/
/* 1. ��� �߻� Outcome ���, time-invariant covariate���� ��time-varying�� covariate�� ��ȯ */
/*  �� �����͵� counting-process format �� ����                                                                 */
/*******************************************************************************/

LIBNAME dir 'E:\koshri-23\data';
LIBNAME mine 'E:\koshri-23\eklee\data';

/* 1. ������ ������ ��mine.final_lung_plus_no2�� tbl���� lung_cancer_history=1, ��, ���� �� �̷��� �����ϴ� ��ü�� distinct INDI_ID�� ù �� ���� ����(fdx1)�������� */
/* Get distinct INDI_ID who have cancer history for lung cancer outcome */
proc sql;
create table temp as select distinct INDI_ID from mine.final_lung_plus_no2
where lung_cancer_history=1;
quit;

/* Get record who is in "temp" tbl */
proc sql;
create table check_cancer as select distinct INDI_ID, fdx1 from mine.My_raw
where INDI_ID in (select * from temp);
quit;

/* 2. Cancer history�� �ִ� ��ü���� ��� �ݺ� ����ġ �������� */
/* - ������ ������ ��� �߻��� Outcome�� counting-process format�� ��check_cancer�� tbl joint */

/* joint counting-process format for lung cancer outcome tbl and check_cancer tbl */
DATA final_lung_no2;
SET mine.final_lung_plus_no2;
DROP lung_cancer_history start stop;
RUN;

proc sql;
create table mine.temp2 as select * from final_lung_no2 as a join check_cancer as b
on a.INDI_ID = b.INDI_ID;
quit;

/* 3. ��between_yes�� ����(�� �ݺ� ����ġ�� (�����, �����) ���̿� �� ���� ���ڰ� ���ԵǴ��� ��Ÿ���� ����) ���� */ 
/* ���� �� �̷��� �����ϴ� ��ü�� ���� ������� ������.("temp2" tbl) */
/* ������ ������ ������� ���� */
DATA temp3;
SET mine.temp2;
IF (fdx1> ECNY_DT) and (fdx1 < OUT_DT) then between_yes=1;
ELSE between_yes=0;
run;

/* 4-1). ��between_yes��=1�� �ݺ� ����ġ�� ������ counting-process format���� ��ȯ */
/* �� ��first��, ��last�� tbl */
/* Get records who have "between_yes" = 1*/
proc sql;
create table yes_tbl as select * from temp3
where INDI_ID in (select distinct INDI_ID from temp3 where between_yes=1);
quit;

/* Plus the obs cancer history for lung_cancer outcome */
DATA yes_1;
SET yes_tbl;
IF between_yes=1;
run;

DATA first last;
SET yes_1;
ECNY_DT2 = ECNY_DT;
OUT_DT2 = fdx1;
DROP ECNY_DT OUT_DT;
RENAME ECNY_DT2 = ECNY_DT OUT_DT2 = OUT_DT;
Output first;
ECNY_DT2 = fdx1;
OUT_DT2 = OUT_DT;
DROP ECNY_DT OUT_DT;
RENAME ECNY_DT2 = ECNY_DT OUT_DT2 = OUT_DT;
Output last;
run;

/* 4-2) ��between_yes��=1�� ����ġ �����ϰ� 4-1)���� ������ tbl�� ���� tbl�� rbind */
/*  unique key ��NO2�� ���� �̿� */
proc sql;
create table except_yes as select * from yes_tbl
where NO2 not in (select NO2 from yes_1);
quit;

DATA yes_all;
SET except_yes first last;
DROP between_yes;
run;

proc sort data = yes_all;
BY INDI_ID ECNY_DT OUT_DT;
run;

/* 5. ���� �� �̷��� �����ϳ� ��� �ݺ� ����ġ�� ��between_yes�� = 0�� ��ü (��, (�����, �����) ���̿� �� ���� ���ڰ� �������� �ʴ� case) ��� �������� */
/* (�����ϴ� case) */
/* - ��� ���ڿ� �� ���� ���ڰ� ������ ��� */
/* - ��OUT_DT���� �� ���� ���ڰ� ������ ��� */
/* - last follow up date(2018.12.31)�� �� ���� ���ڰ� ������ ��� */
/* �� �ش� case�� ���� ���� ��ȯ�� �ʿ䰡 ���ٰ� �Ǵ���. */

/* Get all records who don't have "between_yes" = 1 */
proc sql;
create table check_ID as select * from temp3
where INDI_ID not in (select distinct INDI_ID from yes_tbl);
quit;

/* 6. ���� �� �̷��� �����ϴ� ��ü ���� ���, counting-process format���� ��ȯ�� tbl ��� rbind + time-varying ��lung_cancer_history�� covariate ���� ������ */
/* time-varying ��lung_cancer_history�� covariate�� ù �� ���� ���� ���� ��� 1�� ���� ����. */

/* MERGE data who have cancer history for lung cancer outcome */
DATA mine.final_history_yes;
SET yes_all check_ID(DROP = between_yes);
IF ECNY_DT >= fdx1 then lung_cancer_history=1;
ELSE lung_cancer_history=0;
run;

/* 7. ���� �� �̷��� �ƿ� ���� ��ü���� �ڷ�� 6)���� ������ ��final_history_yes�� tbl rbind */
/* ���� �� �̷��� ���� ��ü�鿡 ���ؼ��� ��lung_cancer_history�� ���� ���� ��� 0�� �ǵ��� ���� */

/* MERGE all data */
proc sql;
create table final_history_not as select * from mine.final_lung_plus_no2
where INDI_ID not in (select distinct INDI_ID from mine.final_history_yes);
quit;

DATA final_history_not2;
SET final_history_not;
lung_cancer_history=0;
run;

DATA mine.final_time_varying_cancer;
SET mine.final_history_yes(DROP = fdx1) final_history_not(DROP = start stop);
run;

/* 8. �� ��ȯ�� counting-process format data�� ��(Start, Stop]�� ���� �߰� + unique key ��NO3��  ���� �߰� ���� */
/* - ��(Start, Stop]���� ������ year�� �ǵ��� ���� + ��NO3�� ������ obs number�� ���� */
proc sort data = mine.final_time_varying_cancer;
BY INDI_ID ECNY_DT OUT_DT;
run;

/* Add (Start, Stop] and "NO3" variable */
DATA mine.final_lung_time_varying_NO3;
SET mine.final_time_varying_cancer;
BY INDI_ID ECNY_DT OUT_DT;
RETAIN first;
IF first.INDI_ID then first = ECNY_DT;
start = (input(ECNY_DT, yymmdd8.) - input(first, yymmdd8.))/365.25;
stop = (input(OUT_DT, yymmdd8.) - input(first, yymmdd8.))/365.25;
NO3 = _n_;
DROP first;
run;
