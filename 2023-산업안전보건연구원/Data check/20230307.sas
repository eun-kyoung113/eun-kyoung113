/*----------------------------*/
/****** Data Quality check ******/
/*----------------------------*/
/* After reading "readme_data.txt"*/
/*----------------------------*/


LIBNAME dir 'L:\';

proc sql;
create table check_DT as select INDI_ID, ECNY_DT, OUT_DT, BIZ_NM from dir.Db3
where ECNY_DT=OUT_DT;
quit;

proc sql;
create table check1 as select distinct INDI_ID from check_DT;
quit;

proc sql;
create table check_N as select distinct INDI_ID from dir.Db3
where ECNY_DT >='20190101';
quit;

proc sql;
create table check_ID as select NO from dir.Db3
where INDI_ID='1000000000001';
quit;

proc sql;
create table check_NA_ID as select NO from dir.Db3
where INDI_ID='';
quit;

proc sql;
create table check_JSSFC as select JSSFC_NO, ECNY_DT from dir.Db3
where JSSFC_NO='1';
quit;

proc sql;
create table check_year as select * from check_JSSFC
where ECNY_DT>='20060101';
quit;


DATA dir.Sample;
set dir.Db3 (obs=100);
run;
