/* --------------------------------------------------------------- */
/*-------------------Table 1 / HbA1c  check again ------------------*/
/* --------------------------------------------------------------- */
libname dir1 "C:\Users\stat\OneDrive - 숙명여자대학교\석사연구\당뇨병_망막변성_연구자료\sas code\My_data";
run;

/* Check the number of unique Patient_ID */
proc sql;
create table check as select distinct Patient_ID from dir1.newMyData_final;
quit;

/* Use the data that used in NDE / NIE Analysis */
PROC SORT DATA=dir1.newMyData_final
 OUT=unique
 NODUPKEYS ;
 BY patient_id ;
RUN ;

data unique; 
set unique; 
if (A1c_baseline<0) or (Avg_A1C <0) then  A1CM=1; /* a1cm=1 is a1cm missing at either baseline or average */
else A1CM=0;
run;

proc sort data=unique;
by cat;
run;
proc means data =unique;
var A1c_baseline;
by cat;
run;
proc ttest data=unique;
class cat;
var A1c_baseline;
run;

proc means data =unique;
var Avg_A1c;
by cat;
run;
proc ttest data=unique;
class cat;
var Avg_A1c;
run;
