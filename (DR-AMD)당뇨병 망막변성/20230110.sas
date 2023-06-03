/***** 230110 ******/
/**** Sensitivity analysis ****/
/**** Run the PHREG Procedure ****/
/**** Use frailty term ****/

libname dir1 "C:\Users\stat\OneDrive - 숙명여자대학교\석사연구\당뇨병_망막변성_연구자료\sas code\My_data";
run;

DATA PHREG_data2;
SET dir1.PHREG_data2;
ID_Eye = CATX("_", Patient_ID, Eye_num);
ID_Repl = CATX("_", Patient_ID, Replication);
RUN;

DATA allprog2;
SET dir1.allprog2;
ID_Eye = CATX("_", Patient_ID, Eye_num);
ID_Repl = CATX("_", Patient_ID, Replication);
RUN;

/******* Frailty1********/
/*composite total effect*/
proc phreg data=PHREG_data2;
  class patient_id cat(ref='0') 
 bad Eye ID_Eye
 /descending;
 model (start, stop)*bad(0) =  cat /rl;
 Random ID_Eye;
 weight wa1;
run;

/*progression total effect*/
proc phreg data=allprog2;
  class patient_id cat(ref='0') 
 prog Eye ID_Eye
 /descending;
 model (start, stop)*prog(0) =  cat  /rl;
 Random ID_Eye;
 weight wa1;
run;

/*pdr total effect*/
proc phreg data=PHREG_data2;
  class patient_id cat(ref='0') 
 pdr Eye ID_Eye
 /descending;
 model (start, stop)*pdr(0) =  cat  /rl;
 Random ID_Eye;
 weight wa1;
run;

/******* Model 1-2 ********/
/*composite total effect*/
proc phreg data=PHREG_data2 ;
  class patient_id cat(ref='0') 
 bad ID_Repl
 /descending;
 model (start, stop)*bad(0) =  cat /rl;
 Random ID_Repl;
 weight wa1;
run;

/*progression total effect*/
proc phreg data=allprog2 ;
  class patient_id cat(ref='0') 
 prog ID_Repl
 /descending;
 model (start, stop)*prog(0) =  cat  /rl;
Random ID_Repl;
 weight wa1;
run;

/*pdr total effect*/
proc phreg data=PHREG_data2 ;
  class patient_id cat(ref='0') 
 pdr ID_Repl
 /descending;
 model (start, stop)*pdr(0) =  cat  /rl;
 Random ID_Repl;
 weight wa1;
run;
