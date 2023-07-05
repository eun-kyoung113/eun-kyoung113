/* Create Personal Information table */

LIBNAME dir 'L:\code';

DATA data;
SET dir.Db3 (KEEP = INDI_ID BYEAR BMONTH BDAY);
IF INDI_ID ^= '1000000000001';
run;

proc sql;
create table dir.temp1 as select distinct INDI_ID, BYEAR, BMONTH, BDAY
from data;
quit;
