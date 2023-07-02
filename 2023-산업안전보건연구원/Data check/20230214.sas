/*----------------------*/
/*** Data Quality check ***/
/*----------------------*/

LIBNAME dir 'L:\';

proc freq data=dir.Db3;
tables Duration;
run;

proc freq data=dir.Db3;
tables BYEAR BMONTH BDAY;
RUN;

proc freq data=dir.Db3;
tables SEX AGE NO ECNY_DT;
run;

proc sql;
create table check_age as select AGE, ECNY_DT,  OUT_DT from dir.Db3
where AGE<='0';
quit;

proc sql;
create table check_age2 as select AGE, ECNY_DT,  OUT_DT from dir.Db3
where AGE>='100';
quit;
