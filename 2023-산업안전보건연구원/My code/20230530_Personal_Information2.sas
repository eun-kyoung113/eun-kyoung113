/* Create Personal Information table */

LIBNAME dir 'L:\code';

DATA birth;
SET dir.Db3 (KEEP= BYEAR BMONTH BDAY);
RUN;

/* Unique element check */
proc sql;
create table BYEAR as select distinct BYEAR from birth;
quit;

proc sql;
create table BMONTH as select distinct BMONTH from birth;
quit;

proc sql;
create table BDAY as select distinct BDAY from birth;
quit;

/* Data cleaning */
DATA temp;
SET dir.Db3 (KEEP = INDI_ID SEX BYEAR BMONTH BDAY);
IF (INDI_ID ^= '1000000000001') & (BYEAR<='2004') & (BMONTH<='12') & (BDAY<='31') ;
run;

DATA temp2;
SET dir.temp;
BYEAR2 = INPUT(BYEAR,12.);
BMONTH2=INPUT(BMONTH,12.);
BDAY2=INPUT(BDAY,12.);
DROP BYEAR BMONTH BDAY;
RUN;

DATA dir.data;
SET temp2;
RENAME BYEAR2 = BYEAR BMONTH2 = BMONTH BDAY2 = BDAY;
RUN;

/* Summary Byear */
proc freq data=dir.data;
tables BYEAR;
run;

proc univariate data=dir.data;
   var BYEAR;
   histogram BYEAR;
run;
