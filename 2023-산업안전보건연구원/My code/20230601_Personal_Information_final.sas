/* Create Personal Information table -- Final */

LIBNAME dir 'L:\';
LIBNAME mine 'C:\Users\stat\Dropbox\eklee\SBY_data';

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

/* Unique row - Personal Information table */
proc sort data=dir.data NODUPKEY;
BY INDI_ID;
RUN;

/* Year CUT */
DATA mine.Personal_Information;
SET dir.Data;
IF BYEAR>=1940 & BYEAR<=1999;
RUN;
