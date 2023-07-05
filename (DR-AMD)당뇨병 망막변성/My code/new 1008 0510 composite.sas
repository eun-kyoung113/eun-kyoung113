

data all;
set all;
LFUD = log(FU_duration);
run;


/*crude*/
/* table 3 crude composite outcome 최종버전에 사용됨 */


proc phreg data=all covs(aggregate) ;
  class patient_id cat(ref='0')  bad
 /descending;
 model FU_duration*bad(0) =  cat/rl;
 ID patient_id;
run;




/*age sex*/

proc phreg data=all covs(aggregate) ;
  class patient_id cat(ref='0')  bad sex
 /descending;
 model FU_duration*bad(0) =  cat age sex/rl;
 ID patient_id;
run;



/*age, sex, baseline DR,  HTN, current smoker adjusted model*/
/* table 3 unweighted multivariable coxph composite outcome 최종버전에 사용됨 */

proc phreg data=all covs(aggregate) ;
  class patient_id cat(ref='0')   bad
sex
bdr
HTN
curr_smoker
/descending;
  model FU_duration*bad(0) =  cat
age
sex
bdr
HTN
curr_smoker/rl;
 ID patient_id;
run;



/*full model*/

proc phreg data=all covs(aggregate) ;
  class patient_id cat(ref='0')  bdr bad
DM_type 
HTN
Sex
antiVEGF
past_smoker
curr_smoker/descending;
  model FU_duration*bad(0) =  
age
agesq
sex
cat

bdr
DM_type
DM_duration 
/*A1c_baseline /
/*Avg_a1c */
/*AMD_grade*/
HTN
/*antiVEGF*/
past_smoker
curr_smoker/rl;
ID patient_id;
   /*by baselinedr;*/
  
run;




data all2;
set all2;
ass=FU_duration*cat;
run;


