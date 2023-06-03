DATA newmydata; SET allprog2;
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



/*progression nde nie*/
proc phreg data=newMyData covsandwich;
  class patient_id cat(ref='0') 
catstar(ref='0') prog
 /descending;
 model FU_duration*prog(0) =  cat catstar /rl;

 ID patient_id;
 weight ww;
run;
/*progression total effect*/
proc phreg data=allprog2 covsandwich;
  class patient_id cat(ref='0') 
 prog
 /descending;
 model FU_duration*prog(0) =  cat  /rl;

 ID patient_id;
 weight wa1;
run;

/*PROC GENMOD DATA=newMyData DESCENDING; 
CLASS sex child Atemp id;
MODEL event = cat catStar age sex child / ERROR=B; WEIGHT w; REPEATED SUBJECT = id / TYPE=IND; RUN;
