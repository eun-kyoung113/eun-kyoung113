libname dir1 "E:\Dropbox\Documents\Research\2210-diabetes-DR-AMD\data_2209_first";
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
/*if A1c_baseline>0 then A1CB=1; else A1CB=0;
if avg_A1c>0 then A1CA=1; else A1CA=0;
if A1cb=0 and a1ca=1 then A1c_base2=avg_a1c; else A1c_base2=A1c_baseline*/;
if patient_id="KALA"  then exccr=1;
else if patient_id="DAMU"  then exccr=1;
else exccr=0;

run;
/*proc univariate data=dramd;
var dm_duration;
run;*/

/*
proc freq data=dramd;
tables exccr;
run;*/

data dramdexc;
set dramd;
if exccr=0;
run;


proc rank data=dramdexc group=2 out=med;
   var age  A1c_baseline  Avg_a1C   DM_duration ;
   ranks ageb A1CB  A1CA DMD;
run;
  /*    
proc freq data=med;
tables  ageb A1CB  A1CA DMD ;
run;*/

   
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

proc freq data=all;
tables A1CB2;
run;
      
data allprog;
set all;
if bdr<3;
run;
/*
proc freq data=all;
tables  ageb A1CB  A1CA DMD ageb2 A1CB2  A1CA2 DMD2;
run;*/

/* PS weight  for composite and PDR*/
 /* exposure pred null */




proc logistic data=all descending;
 class patient_id cat bdr
DM_type 
HTN
Sex
past_smoker /descending;
model cat(event='1')=;
output out=all predicted=pna;
run;




/* exposure pred denominator */

proc logistic data=all descending;
 class patient_id cat bdr
DM_type 
HTN
Sex
past_smoker
curr_smoker
/descending;
model cat(event='1')=
age
agesq
sex
bdr
DM_type
DM_duration 
/*A1C_baseline*/
/*AMD_grade*/
HTN
past_smoker
curr_smoker

;;

output out=all predicted=pda;
run;



/*proc sort data=mydata;
by cnt;
run;

proc sort data=mydata2;
by cnt;
run;

data newmydata;
    merge  mydata(IN=fmyd)  mydata2(IN=fmyda2) ;
    by cnt;
	fmyd1=fmyd;
	fmyd2=fmyda2;
run;*/

data all;
set all;
wa=pna/pda;
run;

proc univariate data=all;
var wa pna pda;
run;

data all2;
set all;
if avg_A1C>0;
if  A1C_baseline>0;
run;

proc logistic data=all2 descending;
 class patient_id cat bdr
DM_type 
HTN
Sex
past_smoker /descending;
model cat(event='1')=;;

output out=all2  predicted=pna1;
run;

proc logistic data=all2 descending;
 class patient_id cat bdr
DM_type 
HTN
Sex
past_smoker
ever_smoker
curr_smoker

/descending;
model cat(event='1')=
age
agesq
sex
bdr
DM_type
DM_duration 
A1c_baseline 
HTN
past_smoker
curr_smoker

;

output out=all2  predicted=pda1;
run;


/* exposure pred denominator 
proc logistic data=dramdgee2 descending;
 class patient_id cat bdr
DM_type 
HTN
Sex
past_smoker
curr_smoker/descending;
model cat(event='1')=

cat | age
cat | sex 
cat | bdr 
cat | DM_type 
cat | DM_duration 
cat | A1C_baseline
cat | HTN 
cat | past_smoker 
cat | curr_smoker


age | sex 
age | bdr 
age | DM_type 
age | DM_duration 
age | A1C_baseline
age | HTN 
age | past_smoker 
age | curr_smoker


sex | bdr 
sex | DM_type 
sex | DM_duration 
sex | A1C_baseline
sex | HTN 
sex | past_smoker 
sex | curr_smoker

 
bdr | DM_type 
bdr | DM_duration 
bdr | A1C_baseline
bdr | HTN 
bdr | past_smoker 
bdr | curr_smoker


DM_type  | DM_duration 
DM_type  | A1C_baseline
DM_type  | HTN 
DM_type  | past_smoker 
DM_type  | curr_smoker



DM_duration   | A1C_baseline
DM_duration   | HTN 
DM_duration   | past_smoker 
DM_duration   | curr_smoker


A1C_baseline   | HTN 
A1C_baseline   | past_smoker 
A1C_baseline   | curr_smoker

HTN   | past_smoker 
HTN   | curr_smoker

 past_smoker   | curr_smoker


;

output out=dramdgee2 predicted=pda1;
run;
*/
/*proc genmod data=dramdgee2 descending;
 class patient_id cat bdr
DM_type 
HTN
Sex
past_smoker
curr_smoker/descending;
model cat(event='1')=

cat | age
cat | sex 
cat | bdr 
cat | DM_type 
cat | DM_duration 
cat | A1C_baseline
cat | HTN 
cat | past_smoker 
cat | curr_smoker


age | sex 
age | bdr 
age | DM_type 
age | DM_duration 
age | A1C_baseline
age | HTN 
age | past_smoker 
age | curr_smoker


sex | bdr 
sex | DM_type 
sex | DM_duration 
sex | A1C_baseline
sex | HTN 
sex | past_smoker 
sex | curr_smoker

 
bdr | DM_type 
bdr | DM_duration 
bdr | A1C_baseline
bdr | HTN 
bdr | past_smoker 
bdr | curr_smoker


DM_type  | DM_duration 
DM_type  | A1C_baseline
DM_type  | HTN 
DM_type  | past_smoker 
DM_type  | curr_smoker



DM_duration   | A1C_baseline
DM_duration   | HTN 
DM_duration   | past_smoker 
DM_duration   | curr_smoker


A1C_baseline   | HTN 
A1C_baseline   | past_smoker 
A1C_baseline   | curr_smoker

HTN   | past_smoker 
HTN   | curr_smoker

 past_smoker   | curr_smoker


/ error=bin link=logit type3;;
repeated subject=patient_id;
output out=dramdgee2 predicted=pda1;
run;*/

proc glm data=all2;
class cat
sex
bdr
DM_type
/*Avg_a1c */
/*AMD_grade*/
HTN
past_smoker
curr_smoker

patient_id;
model Avg_a1c=cat  age
agesq


sex
bdr
DM_type
DM_duration 
A1c_baseline 
HTN
past_smoker
curr_smoker

; output out=all2 student=rden; run; quit;


proc glm data=all2;
class cat  ;

model Avg_a1c=cat  ; output out=all2 student=rnum; run; quit;
*Obtain probabilities from normal p.d.f.; 

data all2; set all2;
if cat=1 then wa1=pna1/pda1; else  wa1=(1-pna1)/(1-pda1);
pnum = exp(-.5*(rnum**2))/2.506; 
pden = exp(-.5*(rden**2))/2.506; 
wm1=pnum/pden;
w=wa1*wm1;

if w>0;
run;

proc univariate data=all2;
var  w;
run;

/* PS weight for prog*/

 /* exposure pred null */




proc logistic data=allprog descending;
 class patient_id cat bdr
DM_type 
HTN
Sex
past_smoker /descending;
model cat(event='1')=;
output out=allprog predicted=pna;
run;




/* exposure pred denominator */

proc logistic data=allprog descending;
 class patient_id cat bdr
DM_type 
HTN
Sex
past_smoker
curr_smoker
/descending;
model cat(event='1')=
age
agesq
sex
bdr
DM_type
DM_duration 
/*A1C_baseline*/
/*AMD_grade*/
HTN
past_smoker
curr_smoker

;;

output out=allprog predicted=pda;
run;



/*proc sort data=mydata;
by cnt;
run;

proc sort data=mydata2;
by cnt;
run;

data newmydata;
    merge  mydata(IN=fmyd)  mydata2(IN=fmyda2) ;
    by cnt;
	fmyd1=fmyd;
	fmyd2=fmyda2;
run;*/

data allprog;
set allprog;
wa=pna/pda;
run;

proc univariate data=allprog;
var wa pna pda;
run;

data allprog2;
set allprog;
if avg_A1C>0;
if  A1C_baseline>0;
run;

proc logistic data=allprog2 descending;
 class patient_id cat bdr
DM_type 
HTN
Sex
past_smoker /descending;
model cat(event='1')=;;

output out=allprog2  predicted=pna1;
run;

proc logistic data=allprog2 descending;
 class patient_id cat bdr
DM_type 
HTN
Sex
past_smoker
ever_smoker
curr_smoker

/descending;
model cat(event='1')=
age
agesq
sex
bdr
DM_type
DM_duration 
A1c_baseline 
HTN
past_smoker
curr_smoker

;

output out=allprog2  predicted=pda1;
run;


/* exposure pred denominator 
proc logistic data=dramdgee2 descending;
 class patient_id cat bdr
DM_type 
HTN
Sex
past_smoker
curr_smoker/descending;
model cat(event='1')=

cat | age
cat | sex 
cat | bdr 
cat | DM_type 
cat | DM_duration 
cat | A1C_baseline
cat | HTN 
cat | past_smoker 
cat | curr_smoker


age | sex 
age | bdr 
age | DM_type 
age | DM_duration 
age | A1C_baseline
age | HTN 
age | past_smoker 
age | curr_smoker


sex | bdr 
sex | DM_type 
sex | DM_duration 
sex | A1C_baseline
sex | HTN 
sex | past_smoker 
sex | curr_smoker

 
bdr | DM_type 
bdr | DM_duration 
bdr | A1C_baseline
bdr | HTN 
bdr | past_smoker 
bdr | curr_smoker


DM_type  | DM_duration 
DM_type  | A1C_baseline
DM_type  | HTN 
DM_type  | past_smoker 
DM_type  | curr_smoker



DM_duration   | A1C_baseline
DM_duration   | HTN 
DM_duration   | past_smoker 
DM_duration   | curr_smoker


A1C_baseline   | HTN 
A1C_baseline   | past_smoker 
A1C_baseline   | curr_smoker

HTN   | past_smoker 
HTN   | curr_smoker

 past_smoker   | curr_smoker


;

output out=dramdgee2 predicted=pda1;
run;
*/
/*proc genmod data=dramdgee2 descending;
 class patient_id cat bdr
DM_type 
HTN
Sex
past_smoker
curr_smoker/descending;
model cat(event='1')=

cat | age
cat | sex 
cat | bdr 
cat | DM_type 
cat | DM_duration 
cat | A1C_baseline
cat | HTN 
cat | past_smoker 
cat | curr_smoker


age | sex 
age | bdr 
age | DM_type 
age | DM_duration 
age | A1C_baseline
age | HTN 
age | past_smoker 
age | curr_smoker


sex | bdr 
sex | DM_type 
sex | DM_duration 
sex | A1C_baseline
sex | HTN 
sex | past_smoker 
sex | curr_smoker

 
bdr | DM_type 
bdr | DM_duration 
bdr | A1C_baseline
bdr | HTN 
bdr | past_smoker 
bdr | curr_smoker


DM_type  | DM_duration 
DM_type  | A1C_baseline
DM_type  | HTN 
DM_type  | past_smoker 
DM_type  | curr_smoker



DM_duration   | A1C_baseline
DM_duration   | HTN 
DM_duration   | past_smoker 
DM_duration   | curr_smoker


A1C_baseline   | HTN 
A1C_baseline   | past_smoker 
A1C_baseline   | curr_smoker

HTN   | past_smoker 
HTN   | curr_smoker

 past_smoker   | curr_smoker


/ error=bin link=logit type3;;
repeated subject=patient_id;
output out=dramdgee2 predicted=pda1;
run;*/

proc glm data=allprog2;
class cat
sex
bdr
DM_type
/*Avg_a1c */
/*AMD_grade*/
HTN
past_smoker
curr_smoker

patient_id;
model Avg_a1c=cat  age
agesq


sex
bdr
DM_type
DM_duration 
A1c_baseline 
HTN
past_smoker
curr_smoker

; output out=allprog2 student=rden; run; quit;


proc glm data=allprog2;
class cat  ;

model Avg_a1c=cat  ; output out=allprog2 student=rnum; run; quit;
*Obtain probabilities from normal p.d.f.; 

data allprog2; set allprog2;
if cat=1 then wa1=pna1/pda1; else  wa1=(1-pna1)/(1-pda1);
pnum = exp(-.5*(rnum**2))/2.506; 
pden = exp(-.5*(rden**2))/2.506; 
wm1=pnum/pden;
w=wa1*wm1;

if w>0;
run;

proc univariate data=allprog2;
var  w;
run;
