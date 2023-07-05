libname dir1 "C:\Users\stat\Dropbox\2210-diabetes-DR-AMD\data_2209_first";
run;

data dramd;
set dir1.dramd4;
if category="Dronly" then cat=0;
else if  category="AMDandDR" then cat=1;
if curr_smoker<0 then curr_smoker=9;
if past_smoker<0 then past_smoker=9;
if curr_smoker=1 or past_smoker=1 then ever_smoker=1;
else if curr_smoker=0 and past_smoker=0 then ever_smoker=0;
else ever_smoker=9;

if eye="OD" then side=1;
else if eye="OS" then side=2;
else side=3;
if baselinedr="mild" then bdr=1;
else if baselinedr="mod" then bdr=2;
else if baselinedr="severe" then bdr=3;
else bdr=4;
if bdr=3 then bdr2=2;
else bdr2=bdr;
nside=side;
if patient_id="BOER"  then A1C_baseline=.;

if prog_to_pdr="no" then pdr=0; else pdr=1;
if v2_step_prog="no" then prog=0; else prog=1;
if pdr=1 or prog=1 then bad=1; else bad=0;
cnt+1; /*if first.patient_id then cnt=1*/; 

if amd_grade=0 then AMDG=1;
else AMDG=amd_grade;

A1cdiff=A1c_baseline -Avg_a1C;
if AMDG=4 then AMDG2=3;
else AMDG2=AMDG;
agesq=age*age;
smkcat=curr_smoker*cat;
A1ccat=A1C_baseline*cat;
if patient_id="KALA"  then exccr=1;
else if patient_id="DAMU"  then exccr=1;
else exccr=0;
run;




data dramdexc;
set dramd;
if exccr=0;
run;

proc rank data=dramdexc group=2 out=med;
   var age  A1c_baseline  Avg_a1C   DM_duration ;
   ranks ageb A1CB  A1CA DMD;
run;

proc sort data=med;
  by cnt;
  run;

proc sort data=dramdexc;
by cnt;
run;

data all;
   merge dramdexc med; by cnt;
/*** change quintile categories into scores */
array old {4}  ageb A1CB A1CA  DMD;
array new {4} ageb2 A1CB2  A1CA2  DMD2; 
do i=1 to 4;
	new {i}= old{i}+1;
end;
if A1c_baseline<0 then  A1CB2=9;
if Avg_A1C <0 then  A1CA2=9;
run;

PROC SORT DATA=all
 OUT=unique
 NODUPKEYS ;
 BY patient_id ;
RUN ;

data unique; 
set unique; 
if (A1c_baseline<0) or (Avg_A1C <0) then  A1CM=1;
else A1CM=0;
run;

 proc freq data =unique;
tables a1cm;
run;


proc univariate data=all;
var FU_duration ;
run;

proc freq data =unique;
tables cat;
by a1cm;
run;



proc freq data =unique;
tables cat * a1cm
/chisq;
run;



proc freq data =unique;
tables sex;
by a1cm;
run;



proc freq data =unique;
tables sex * a1cm
/chisq;
run;

 proc freq data =unique;
tables HTN;
by a1cm;
run;

proc freq data =unique;
tables HTN * a1cm
/chisq;
run;


proc freq data =unique;
tables DM_type;
by a1cm;
run;

proc freq data =unique;
tables DM_type * a1cm
/chisq;
run;

data uniques;
set unique;
if curr_smoker<9;
run;
proc freq data =uniques;
tables curr_smoker;
by a1cm;
run;
proc freq data =uniques;
tables curr_smoker * a1cm
/chisq;
run;
proc freq data =uniques;
tables past_smoker;
by a1cm;
run;


proc freq data =uniques;
tables past_smoker * a1cm
/chisq;
run;


proc sort data=unique;
by a1cm;
run;

proc univariate data=unique;
var age ;
by a1cm;
run;
proc means data =unique;
var age;
by a1cm;
run;

proc ttest data=unique;
class a1cm;
var age;

run;

proc means data =unique;
var DM_duration;
by a1cm;
run;

proc ttest data=unique;
class a1cm;
var DM_duration;
run;

/*table1*/
proc sort data=unique;
by cat;
run;
proc univariate data=unique;
var age ;
by cat;
run;
proc means data =unique;
var age;
by cat;
run;

proc ttest data=unique;
class cat;
var age;
run;

proc means data =unique;
var DM_duration;
by cat;
run;

proc ttest data=unique;
class cat;
var DM_duration;
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
 proc freq data =unique;
tables bad;
by cat;
run;

proc freq data =unique;
tables sex;
by cat;
run;



proc freq data =unique;
tables sex * cat
/chisq;
run;

 proc freq data =unique;
tables HTN;
by cat;
run;

proc freq data =unique;
tables HTN * cat
/chisq;
run;


proc freq data =unique;
tables DM_type;
by cat;
run;

proc freq data =unique;
tables DM_type * cat
/chisq;
run;

data uniques;
set unique;
if curr_smoker<9;
run;
proc freq data =unique;
tables curr_smoker;
by cat;
run;

proc freq data =unique;
tables A1CA;
by cat;
run;

proc freq data =unique;
tables A1CB;
by cat;
run;

proc freq data =all;
tables cat;
run;

proc freq data =uniques;
tables curr_smoker;
by cat;
run;

proc freq data =uniques;
tables curr_smoker * cat
/chisq;
run;

proc freq data =unique;
tables bdr;
by cat;
run;
proc freq data =all;
tables bdr* cat
/chisq norow nocol;
run;


proc freq data =all;
tables  amdg2*bdr
/chisq;
run;

proc freq data =all;
tables  amdg*bdr
/chisq ;
run;


proc freq data =uniques;
tables past_smoker;
by cat;
run;


proc freq data =uniques;
tables past_smoker * cat
/chisq;
run;


proc freq data =uniques;
tables ever_smoker;
by cat;
run;


proc univariate data=unique;
var age A1c_baseline  Avg_a1C  FU_duration;
run;



 proc freq data =unique;
tables antiVEGF;
by cat;
run;

 proc freq data =unique;
tables amdg;
by cat;
run;



proc freq data =unique;
tables side;
by cat;
run;



proc freq data =unique;
tables prog;
by cat;
run;


proc freq data =unique;
tables pdr;
by cat;
run;


proc univariate;
var A1c_base2;
run;

proc freq data=unique;
tables cat;
run;

proc freq data=unique;
table curr_smoker*past_smoker curr_smoker*ever_smoker;
run;
