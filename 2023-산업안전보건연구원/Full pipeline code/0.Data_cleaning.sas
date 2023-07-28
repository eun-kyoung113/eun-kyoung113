/****************************************************************************************************/
/*                                                                     What TO DO                                                                                   */
/************************************************************************************************** */
/* 1. Data cleaning                                                                                                                                                 */
/* 2  Data quality check                                                                                                                                          */
/* - �̻��� ����ġ�� �����ϴ� ��ü�� �ݺ� ����ġ ��� �����ϴ� ��� ����                                                                */
/* - Ư�� ���� �����ϴ� ������ġ���� ����(���� ���� ����� ��ü�� ���� ������ �� �����Ƿ� ������ġ�� ���� ����� ����) */
/* 3. �߰� �������� ���� (���� ��뺸�� ����� ���� �� �̷��� �ִ� ��ü ����)                                                          */
/* 4. �߰� ��ó��(Data ���캸���� �� �߰ߵ� �̻� case ����)                                                                                   */
/* - ��� ����� �߻��� ��ü ��, ������� ����Ϻ��� ���� ������ ����ġ�� ������. (���� ���� ������ �Ǵܵ�)            */
/*    �� �� ���, ��뺸�� ������� ����Ϸ� ����                                                                                                      */
/* - �� ���� ����(fdx1 ~ fdx6)�� ��� ����(DTH_DATE1 ~ DTH_DATE3) �� �޷¿� �������� ��¥�� ������ ��ü          */ 
/*    �����ܿ��� �߰� ����                                                                                                                                       */
/***************************************************************************************************/

LIBNAME dir 'E:\koshri-23\data';

/* 0. �� ������ �ҷ����� */
DATA cohort;
SET dir.Db3 (DROP = AGE DURATION DREGDATE REAL_AUTH_CODE2 DTH_POS MRR_STATUS DTH_EDU DTH_AGE NATIONALITY_CLASS_F NATIONALITY_F CODE_103 CODE_56 
                                 DTH_JOB NEW_DTH_CODE1 NEW_DTH_CODE2 BIZ_INDUTY8 BIZ_INDUTY9);
run;

proc sql;
	select count(distinct indi_id) as indi_id_count
	from cohort;;
quit;

/* 1.DB �ڵ� ���� �� �м� ��� ���Ե��� �ʴ� �ڷ� ���� */
/* 1-1. ENCY_DT(�����), OUT_DT(�����) ���� �ڵ� ���� ���� */
/* 1-1-1. ECNY_DT�� �����̰ų�  ECNY_DT, OUT_DT�� 8���ڰ� �ƴ� ��� ���� */
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

/* 1-1-2. ECNY_DT - OUT_DT �� 0 �� ��� ���� */
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

/* 1-2. ������� ���� �߸� �Էµ� ������ ���� */
/* 1-2-1. ������� ����(BYEAR, BMONTH, BDAY)�� �ϳ��� ���� �̰�, ���� 1-12 ���̰� �ƴϰ�, ���� 1-31 ���̰� �ƴ� ��� ���� */
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

/* 1-2-2. ��������� ENCY_DT���� �̷������� ��� ���� */
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

/*1-2-3. ��������� �޷»� �������� �ʴ� ��¥�� �Է�(ex. 1994.13.31 / 1999.04.31.)�� ��� ���� */
/* ���������� ���ǵǾ� �ִ� �������� ��¥������ �ٲٸ� �޷»� �������� �ʴ� ��¥�� �������� ǥ�õ��� �̿� */
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

/* 1-3. �ߺ� ���� */
proc sort data = cohort2 noduprecs;
BY INDI_ID ECNY_DT;
run;

/* 1-4. ����� ������ ������ ��� ���� */
/* ����� ���� ���� �� �İ����� ���� �� ������ �����ϰ� ���� */
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

/* 1-5. �� �߻� ���� ���� ���� �ڵ� ���� ���� */
/* �� �߻� ���� ���� ��¥�� �����̹Ƿ� YYYYMMDD �����̾�� ��. ��, ���� ���̰� ��8���̾�� �Ѵ�. */
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

/* 1-6. ��� ���� ���� ���� �ڵ� ���� ���� */
/* ��� ������ ���� �޷»� �������� �ʴ� ���� ������ ��ü ��� ���� */
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

/*Data quality check + ��AGE��(�Ի� �� ����), ��DURATION��(�ټӿ���) ���� ������ (�̻��� ����ġ�� �����ϴ� ��ü�� �ݺ� ����ġ ��� �����ϴ� ��� ����)
: �����͸� ���캸��, �Ի� �� ����, �ټӿ��� ���� ���� Ȥ�� 200 �̻� �� �̻�ġ�� �����Ͽ� ���� �����ϴ� ���� ���ٰ� �Ǵ�. */

/* 2. ������ ��ü �� ���� ������ */
/* 2-1. out_dt ������ ��ü */
/* out_dt�� ���� �̶�� ���� ������ �ٹ��� �ϰ� �ִٴ� ���� ���ϳ�, ���� ���� ������ 2018.12.31.�� ���Ͽ����Ƿ� ��� 2018.12.31.�� �� ��ü */
data cohort2;
set cohort;
if out_dt='' then out_dt='20181231';
run;

/* 2-2. out_dt�� ������ �� ���� ��¥�� ������ ��� ���� */
/* ���������� ���ǵǾ� �ִ� �������� ��¥������ �ٲٸ� �޷»� �������� �ʴ� ��¥�� �������� ǥ�õ��� �̿� */
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

/*2-3. age, duration ���� �����*/
/* AGE : (����� ? �������)/365.25 & DURATION = (����� ? �����)/365.25 ������� ������ ��, �Ҽ��� ��°�ڸ����� �ݿø� */
data cohort2;
set cohort;
AGE = round( ( input(ECNY_DT, yymmdd10.) - input(cats(BYEAR, BMONTH, BDAY), yymmdd10.) )/365.25,  .01);
DURATION = round( ( input(OUT_DT, yymmdd10.) - input(ECNY_DT, yymmdd10.) ) /365.25, .01);
run;

proc sql;
	select count(distinct indi_id) as indi_id_count
	from cohort2;;
quit;

/* Ư�� ���� �����ϴ� ������ġ���� ����(���� ���� ����� ��ü�� ���� ������ �� �����Ƿ� ������ġ�� ���� ����� ����) */

/* 3. �м� ��� ���Ե��� �ʴ� �ڷ� ���� */
/* 3-1. ECNY_DT�� ��20181231�� ������ ������(��) ���� */
/* ������ �ִ� �� DB�� 2018.12.31.������ ��ϵǾ� �����Ƿ� ������ ����ġ�� ���� */
data cohort;
	set cohort2;
	if ECNY_DT < '20181231';
run;

/* out_dt�� 2018.12.31. ������ �������� ���� ��20181231���� ���� */
/* ���� ���� ������ 2018.12.31.�� �����Ͽ����Ƿ�  2019������� ������� ��� 2018.12.31.�� �ٲ�. */
data cohort2;
set cohort;
if out_dt > '20181231' then out_dt='20181231';
run;

proc sql;
	select count(distinct indi_id) as indi_id_count
	from cohort2;
quit;

/* 3-2. ��������� �������� �м� ��� ����(1940 - 1999) */
/* ��BYEAR�� �� ����ġ ���� Histogram ���캻 ���, ��������� 1940-1999�⵵ ������ ��ü���� �ݺ� ����ġ�� ���� �󵵸� ������ Ȯ���� */
data cohort;
	set cohort2;
	where not missing(BYEAR) and input(BYEAR, 4.) BETWEEN 1940 AND 1999;
run;

proc sql;
	select count(distinct indi_id) as indi_id_count
	from cohort;;
quit;

/* 3-3. �ٹ���� �� ��ø�� �ִ� ���� ���� */
/* two-job ������ ��ü�� �ƿ� ���������ν� Data �����ϰ� �ٲ� */
/* ����ø�� �� ��ECNY_DT�� �������� �������� �� ���� �ݺ� ����ġ�� ������� �� �ݺ� ����ġ�� ����Ϻ��� �̷������̴�. */
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

/* �߰� �������� ���� (���� ��뺸�� ����� ���� �� �̷��� �ִ� ��ü ����) */
/* 3-4. �����Ի� �� �Ϲߺ��� ���� */
 proc sort data = cohort2;
by INDI_ID ECNY_DT;
run;

/* ù �� ���� ���� ������ ��fdx1������ �̿��� fdx1�� ������ �ƴ� ��ü ��, ���� ��뺸�� ����ϰ� fdx1 ~ fdx6�� �ּڰ� �� */
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

/*�߰� ��ó��(Data ���캸���� �� �߰ߵ� �̻� case ����) 
/* ��� ����� �߻��� ��ü ��, ������� ����Ϻ��� ���� ������ ����ġ�� ������. (���� ���� ������ �Ǵܵ�) 
  	�� �� ���, ��뺸�� ������� ����Ϸ� ����*/
/* �� ���� ����(fdx1 ~ fdx6)�� ��� ����(DTH_DATE1 ~ DTH_DATE3) �� �޷¿� �������� ��¥�� ������ ��ü �����ܿ��� �߰� ����*/

/* 5. �߰� ��ó�� */
/* 5-1. ������� ����Ϻ��� ���� ������ ��� ������� ���� ����Ϸ� ��ü*/
DATA cohort2;
SET cohort;
DTH_DATE = cats(DTH_DATE1, DTH_DATE2, DTH_DATE3);
IF (not missing(DTH_DATE) and DTH_DATE < OUT_DT) then OUT_DT = DTH_DATE;
DROP DTH_DATE;
RUN;

/* ������ Data ���� */
data dir.preproc1;
	set cohort2;
run;

/* DTH DATE or fdx1-fdx6 �޷»� �������� �ʴ� �� �ִ��� Ȯ�� */
/* ���������� ���ǵǾ� �ִ� �������� ��¥������ �ٲٸ� �޷»� �������� �ʴ� ��¥�� �������� ǥ�õ��� �̿� */
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

/* dir.Preproc1���� check_fdx2���� ����� ��ü ������ ��, ��dir.Preproc1_new�� data �ٽ� ���� */
proc sql;
create table dir.Preproc1_new as select * from dir.Preproc1
where INDI_ID not in (select INDI_ID from check_fdx2);
quit;
