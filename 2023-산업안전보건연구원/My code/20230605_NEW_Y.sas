/* Data check : NEW_NY = "Y" */

LIBNAME dir 'L:\';
LIBNAME mine 'C:\Users\stat\Dropbox\eklee\SBY_data';

DATA check_NY;
SET dir.Db3 (KEEP = INDI_ID ECNY_DT NEW_NY NO);
IF NEW_NY='Y';
RUN;

proc sort data = check_NY;
BY INDI_ID NO;
run;

DATA unique_NY;
SET check_NY;
BY INDI_ID NO;
IF first.INDI_ID;
RUN;
