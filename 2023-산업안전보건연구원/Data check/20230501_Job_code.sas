/* Job code Summary */

LIBNAME dir 'L:\';

DATA data;
SET dir.Db3 (KEEP = BIZ_INDUTY8 BIZ_INDUTY9 BIZ_INDUTY10 JSSFC_CD JSSFC_NO);
run;

DATA dir.job_data;
SET data;
INDUTY8=substr(BIZ_INDUTY8,1,3);
INDUTY9=substr(BIZ_INDUTY9,1,3);
INDUTY10=substr(BIZ_INDUTY10,1,3);
RUN;

proc sql;
create table check_na as select INDUTY9, INDUTY10 from dir.job_data
where INDUTY8='';
quit;

proc sql;
create table check2 as select * from check_na
where INDUTY9='' and INDUTY10='';
quit;

/* Frequency table print */
proc freq data=dir.job_data;
table INDUTY8;
run;

proc freq data=dir.job_data;
table INDUTY9;
run;

proc freq data=dir.job_data;
table INDUTY10;
run;

proc freq data=dir.job_data;
tables JSSFC_CD*JSSFC_NO;
run;

/*Unique value check */
proc sql;
create table check_unique as select distinct JSSFC_NO from dir.job_data;
quit;

proc sql;
create table check_unique as select distinct JSSFC_CD from dir.job_data;
quit;
