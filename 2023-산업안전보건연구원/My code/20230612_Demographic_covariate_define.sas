/* Demographic Information Covariate define */

LIBNAME dir 'L:\';
LIBNAME mine 'C:\Users\stat\Dropbox\eklee\SBY_data';
LIBNAME dir1 'D:\';

DATA dir.Num_Demographic_raw;
SET dir.Demographic_raw;
AGE2 = INPUT(AGE, 12.);
DURATION2=INPUT(DURATION, 12.);
DROP AGE DURATION;
RENAME AGE2=AGE DURATION2 = DURATION;
RUN;

/* Extract Exclude_ID */
proc sql;
create table mine.exclude_ID as select distinct a.INDI_ID from dir.Num_Demographic_raw as a  join mine.Personal_Information as b
on (a.INDI_ID = b.INDI_ID) and ((a.DURATION < 0) or (a.AGE <0) or (a.AGE >100))  ;
quit;

/* Except */
proc sql;
create table dir.Num_Demographic_ver2 as select * from dir.Num_Demographic_raw as a
where a.INDI_ID not in (select * from mine.exclude_ID);
quit;

/* Define AGE, DURATION variable */
%macro define_demographic;
	%do year = 1940  %to 1999  %by 5;
			proc sql;
			create table dir1.temp_&year._M as select a.* from dir.Num_Demographic_ver2 as a, mine.Dt_&year._M as b
			where a.INDI_ID = b.INDI_ID;
      		quit;

			proc sort data = dir1.temp_&year._M;
			by INDI_ID AGE;
			run;

			proc datasets library=work kill;
			run;
			quit;

			proc sql;
			create table dir1.temp_&year._F as select a.* from dir.Num_Demographic_ver2 as a, mine.Dt_&year._F as b, mine.Exclude_ID as c
			where a.INDI_ID = b.INDI_ID;
      		quit;

			proc sort data = dir1.temp_&year._F;
			by INDI_ID AGE;
			run;

  	%end;
%mend;

options fullstimer;
options compress=yes;
%define_demographic;

proc sql;
create table temp as select INDI_ID, DURATION from dir1.temp_1940_M
where DURATION<0;
quit;

/* Check DURATION & AGE Histogram */
%macro hist_variable;
%do year = 1940  %to 1999  %by 5;
	proc univariate data=dir1.temp_&year._M;
   	var AGE DURATION;
   	histogram AGE DURATION;
	run;

	proc univariate data=dir1.temp_&year._F;
    var AGE DURATION;
    histogram AGE DURATION;
    run;

%end;
%mend;
%hist_variable;
