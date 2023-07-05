DATA newmydata; SET all2;
catStar = 0; Mtemp =.;
IF cat=catStar then Mtemp = avg_A1C;
OUTPUT; 
catStar = 1; Mtemp =.; 
IF cat=catStar then Mtemp = avg_A1C; 
OUTPUT;
RUN;

DATA newMyData; SET newMyData; 
Atemp = cat; 
RUN;

PROC GENMOD DATA=newmydata; 
CLASS sex   Atemp bdr
DM_type HTN
past_smoker
curr_smoker;
MODEL Mtemp = Atemp age sex
agesq
bdr
DM_type
DM_duration 
A1c_baseline 
HTN
past_smoker
curr_smoker
/ERROR=N; 
output OUT=newmydata 
P = predM;
RUN;

DATA newMyData; 
SET newMyData; 
weightDIR = PDF('normal', avg_A1C, predM, 1.2083);
RUN;



DATA newMyData; SET newMyData; 
Atemp = catStar; 
RUN;

PROC GENMOD DATA=newmydata; 
CLASS sex   Atemp bdr
DM_type HTN
past_smoker
curr_smoker;
MODEL Mtemp = Atemp age sex
agesq
bdr
DM_type
DM_duration 
A1c_baseline 
HTN
past_smoker
curr_smoker
/ERROR=N; 
output OUT=newmydata 
P = predMStar;
RUN;

DATA newMyData; SET newMyData; 
weightINDIR = PDF('normal', avg_A1C, predMStar, 1.2083); RUN;

DATA newMyData; SET newMyData; w2 = weightINDIR/weightDIR;
ww=wa1*w2;
RUN;

proc univariate data=newmydata;
var ww;
run;
/*
proc phreg data=newMyData covsandwich;
  class patient_id cat(ref='0') 
catstar(ref='0') bad
 /descending;
 model FU_duration*bad(0) =  cat catstar /rl;
random patient_id /dist=gamma;
 ID cnt;
 weight ww;
run;*/

/*composite total effect*/

proc phreg data=all2 covsandwich;
  class patient_id cat(ref='0') 
 bad
 /descending;
 model FU_duration*bad(0) =  cat /rl;

 ID patient_id;
 weight wa1;
run;

/*pdr total effect*/
proc phreg data=all2 covsandwich;
  class patient_id cat(ref='0') 
 pdr
 /descending;
 model FU_duration*pdr(0) =  cat  /rl;

 ID patient_id;
 weight wa1;
run;


/*composite nde nie*/
proc phreg data=newMyData covsandwich;
  class patient_id cat(ref='0') 
catstar(ref='0') bad
 /descending;
 model FU_duration*bad(0) =  cat catstar /rl;

 ID patient_id;
 weight ww;
run;

proc phreg data=newMyData covsandwich;
  class patient_id cat(ref='0') 
 bad
 /descending;
 model FU_duration*bad(0) =  cat  /rl;

 ID patient_id;
 weight ww;
run;

proc phreg data=newMyData covsandwich;
  class patient_id cat(ref='0') 
 bad
 /descending;
 model FU_duration*bad(0) =  cat  AVG_A1C/rl;

 ID patient_id;
 weight ww;
run;

proc phreg data=newMyData covsandwich;
  class patient_id cat(ref='0') 
 bad
 /descending;
 model FU_duration*bad(0) =  cat  AVG_A1C cat*AVG_A1C /rl;

 ID patient_id;
 weight ww;
run;

/*pdr nde nie*/
proc phreg data=newMyData covsandwich;
  class patient_id cat(ref='0') 
catstar(ref='0') pdr
 /descending;
 model FU_duration*pdr(0) =  cat catstar /rl;

 ID patient_id;
 weight ww;
run;



/*PROC GENMOD DATA=newMyData DESCENDING; 
CLASS sex child Atemp id;
MODEL event = cat catStar age sex child / ERROR=B; WEIGHT w; REPEATED SUBJECT = id / TYPE=IND; RUN;*/

   /*ods trace on;
   ods listing close;
proc phreg data=newMyData covsandwich;
  class patient_id cat(ref='0') 
catstar(ref='0') pdr
 /descending;
 model FU_duration*pdr(0) =  cat catstar /rl;

 ID patient_id;
 weight ww;

run;
 ods trace off;)*/

  ods trace on;
ods output ParameterEstimates=HRCI;
 proc phreg data=newMyData covsandwich;
  class patient_id cat(ref='0') 
catstar(ref='0') pdr
 /descending;
 model FU_duration*pdr(0) =  cat catstar /rl ;

 ID patient_id;
 weight ww;

run;
 ods trace off;


proc contents data=hrci;
run;

data n0;
 set HRCI;
 if parameter =  "cat" then 
 call symput('HR', HazardRatio);
  if parameter =  "catStar" then 
 call symput('HRs', HazardRatio);
run;
/*

%let rep = 5;
proc surveyselect data=newMyData  out=bootsample
     seed = 1347 method = urs
	 samprate = 1 outhits rep = &rep;
run;
ods listing close;

ods output  ParameterEstimates=HRCI2 (where = (parameter =  "cat" ));
ods output  ParameterEstimates=HRCIs2 (where = (parameter =  "catStar" ));
 proc phreg data=bootsample covsandwich;
  class patient_id cat(ref='0') 
catstar(ref='0') pdr
 /descending;
 model FU_duration*pdr(0) =  cat catstar /rl ;

 ID patient_id;
 weight ww;
  by replicate;
run;
quit;
* converting character type to numeric type;
data HRCI3;
  set HRCI2;
  HR2 = HazardRatio + 0;
run;
data HRCIs3;
  set HRCIs2;
  HRs2 = HazardRatio + 0;
run;
* creating confidence inteval, normal distribution theory method;
* using the t-distribution;
%let alphalev = .05;
ods listing;
proc sql;
  select  &HR as hr2,
          mean(hr2) - &HR as bias, 
		  std(hr2) as std_err,
          &hr - tinv(1-&alphalev/2, &rep-1)*std(hr2) as lb,
          &hr + tinv(1-&alphalev/2, &rep-1)*std(hr2) as hb
  from HRCI3;
quit;

proc sql;
  select  &HRs as hrs2,
          mean(hrs2) - &HRs as bias, 
		  std(hrs2) as std_err,
          &hrs - tinv(1-&alphalev/2, &rep-1)*std(hrs2) as lb,
          &hrs + tinv(1-&alphalev/2, &rep-1)*std(hrs2) as hb
  from HRCIs3;
quit;

/*
proc sql;
  select  &HR as hr2,
          mean(hr2) - &HR as bias, 
		  std(hr2) as std_err,
          &hr - tinv(1-&alphalev/2, &rep-1)*std(hr2) as lb,
          &hr + tinv(1-&alphalev/2, &rep-1)*std(hr2) as hb
  from HRCI3;
quit;

ods output FitStatistics = t0;
proc reg data = newMyData;
  model FU_duration =  cat catstar;
run;
quit;

*store the estimated r-square;
data _null_;
 set t0;
 if label2 =  "R-Square" then 
 call symput('r2bar', cvalue2);
run;

*store the estimated r-square;
data _null_;
 set t0;
 if label2 =  "R-Square" then 
 call symput('r2bar', cvalue2);
run;

%let rep = 500;
proc surveyselect data=newMyData  out=bootsample
     seed = 1347 method = urs
	 samprate = 1 outhits rep = &rep;
run;
ods listing close;

ods output  FitStatistics = t (where = (label2 =  "R-Square"));
proc reg data = bootsample;
  by replicate;
  model FU_duration =  cat catstar;
run;
quit;
* converting character type to numeric type;
data t1;
  set t;
  r2 = cvalue2 + 0;
run;

* creating confidence interval, normal distribution theory method;
* using the t-distribution;
%let alphalev = .05;
ods listing;
proc sql;
  select  &r2bar as r2,
          mean(r2) - &r2bar as bias, 
		  std(r2) as std_err,
          &r2bar - tinv(1-&alphalev/2, &rep-1)*std(r2) as lb,
          &r2bar + tinv(1-&alphalev/2, &rep-1)*std(r2) as hb
  from t1;
quit; 
