/************************************/
/**** Comorbidities Define ****/
/*** 첫 번째 포함조건 만족하는 obj들의 진료DB ***/
DATA T60_T20_screening;
SET '/userdata12/room115/data_out/data_store/T60_T20_screening.sas7bdat';
KEEP PERSON_ID RECU_FR_DT MAIN_SICK SUB_SICK;
RUN;

DATA total_study_pop;
SET '/userdata12/room115/data_out/data_store/total_study_pop.sas7bdat';
RUN;

PROC SQL;
create table T20_study_pop as select * from total_study_pop join T60_T20_screening
on total_study_pop.PERSON_ID = T60_T20_screening.PERSON_ID
and total_study_pop.HME_DT >= T60_T20_screening.RECU_FR_DT;
quit;

proc sql;
create table temp as select PERSON_ID, RECU_FR_DT, MAIN_SICK, SUB_SICK,
case when (MAIN_SICK like "J45%" or MAIN_SICK like "J46%") 
or (SUB_SICK like "J45%" or SUB_SICK like "J46%")then 1 else 0 end as Asthma,

case when ((MAIN_SICK like "I26%" or MAIN_SICK like "I63%" or MAIN_SICK like "I74%" or MAIN_SICK like "I82%") 
or (MAIN_SICK in ("I801", "I802", "I803", "I808", "I809"))) or 
((SUB_SICK like "I26%" or SUB_SICK like "I63%" or SUB_SICK like "I74%" or SUB_SICK like "I82%") 
or (SUB_SICK in ("I801", "I802", "I803", "I808", "I809"))) then 1 else 0 end as Thromboembolism,

case when MAIN_SICK like "K20%" or MAIN_SICK like "K21%" or  MAIN_SICK like "K22%"
or MAIN_SICK like "K23%" or MAIN_SICK like "K24%" or MAIN_SICK like "K25%" or MAIN_SICK like "K26%"
or MAIN_SICK like "K27%" or MAIN_SICK like "K28%" or MAIN_SICK like "K29%" or MAIN_SICK like "K30%"
or MAIN_SICK like "K31%" or MAIN_SICK like "K50%" or MAIN_SICK like "K51%" or MAIN_SICK like "K52%"
or MAIN_SICK like "K53%" or MAIN_SICK like "K54%" or  MAIN_SICK like "K55%"
or MAIN_SICK like "K56%" or MAIN_SICK like "K57%" or MAIN_SICK like "K58%" or MAIN_SICK like "K59%"
or MAIN_SICK like "K60%" or MAIN_SICK like "K61%" or MAIN_SICK like "K62%" or MAIN_SICK like "K63%"
or MAIN_SICK like "K64%" or MAIN_SICK like "K90%" or MAIN_SICK like "K91%" or MAIN_SICK like "K92%"
or MAIN_SICK like "K93%" 
or SUB_SICK like "K20%" or SUB_SICK like "K21%" or  SUB_SICK like "K22%"
or SUB_SICK like "K23%" or SUB_SICK like "K24%" or SUB_SICK like "K25%" or SUB_SICK like "K26%"
or SUB_SICK like "K27%" or SUB_SICK like "K28%" or SUB_SICK like "K29%" or SUB_SICK like "K30%"
or SUB_SICK like "K31%" or SUB_SICK like "K50%" or SUB_SICK like "K51%" or SUB_SICK like "K52%"
or SUB_SICK like "K53%" or SUB_SICK like "K54%" or SUB_SICK like "K55%"
or SUB_SICK like "K56%" or SUB_SICK like "K57%" or SUB_SICK like "K58%" or SUB_SICK like "K59%"
or SUB_SICK like "K60%" or SUB_SICK like "K61%" or SUB_SICK like "K62%" or SUB_SICK like "K63%"
or SUB_SICK like "K64%" or SUB_SICK like "K90%" or SUB_SICK like "K91%" or SUB_SICK like "K92%"
or SUB_SICK like "K93%" 
then 1 else 0 end as GI_disorders, 

case when ((MAIN_SICK like "F%") and (MAIN_SICK not like "F01%") and (MAIN_SICK not like "F00%")
and (MAIN_SICK not like "F03%")) or
((SUB_SICK like "F%") and (SUB_SICK not like "F01%") and (SUB_SICK not like "F00%")
and (SUB_SICK not like "F03%")) then 1 else 0 end as Psychiatric_disorders,

case when MAIN_SICK like "I10%" or MAIN_SICK like "I11%" or MAIN_SICK like "I12%"
or MAIN_SICK like "I13%"  or MAIN_SICK like "I14%" or MAIN_SICK like "I15%" 
or SUB_SICK like "I10%" or SUB_SICK like "I11%" or SUB_SICK like "I12%"
or SUB_SICK like "I13%"  or SUB_SICK like "I14%" or SUB_SICK like "I15%" 
then 1 else 0 end as Hypertension,

case when MAIN_SICK like "J40%" or MAIN_SICK like "J41%" or MAIN_SICK like "J42%"
or MAIN_SICK like "J43%"  or MAIN_SICK like "J44%" or MAIN_SICK like "J47%" 
or SUB_SICK like "J40%" or SUB_SICK like "J41%" or SUB_SICK like "J42%"
or SUB_SICK like "J43%"  or SUB_SICK like "J44%" or SUB_SICK like "J47%" 
then 1 else 0 end as COPD,

case when MAIN_SICK like "K70%" or MAIN_SICK like "K71%" or MAIN_SICK like "K72%"
or MAIN_SICK like "K73%"  or MAIN_SICK like "K74%" or MAIN_SICK like "K75%" 
or MAIN_SICK like "K76%" or MAIN_SICK like "K77%"
or SUB_SICK like "K70%" or SUB_SICK like "K71%" or SUB_SICK like "K72%"
or SUB_SICK like "K73%"  or SUB_SICK like "K74%" or SUB_SICK like "K75%" 
or SUB_SICK like "K76%" or SUB_SICK like "K77%"
then 1 else 0 end as CLD,

case when MAIN_SICK like "J12%" or MAIN_SICK like "J13%" or MAIN_SICK like "J14%"
or MAIN_SICK like "J15%"  or MAIN_SICK like "J16%" or MAIN_SICK like "J17%" 
or MAIN_SICK like "J18%" 
or SUB_SICK like "J12%" or SUB_SICK like "J13%" or SUB_SICK like "J14%"
or SUB_SICK like "J15%"  or SUB_SICK like "J16%" or SUB_SICK like "J17%" 
or SUB_SICK like "J18%" then 1 else 0 end as Pneumonia,

case when MAIN_SICK like "I20%" or MAIN_SICK like "I21%" or MAIN_SICK like "I22%"
or MAIN_SICK like "I23%"  or MAIN_SICK like "I24%" or MAIN_SICK like "I25%" 
or SUB_SICK like "I20%" or SUB_SICK like "I21%" or SUB_SICK like "I22%"
or SUB_SICK like "I23%"  or SUB_SICK like "I24%" or SUB_SICK like "I25%" 
then 1 else 0 end as CAD,

case when MAIN_SICK like "I70%" or MAIN_SICK like "I71%" or MAIN_SICK like "I72%"
or MAIN_SICK like "I73%"  or MAIN_SICK like "I74%" 
or MAIN_SICK like "I77%" or MAIN_SICK like "I79%"
or SUB_SICK like "I70%" or SUB_SICK like "I71%" or SUB_SICK like "I72%"
or SUB_SICK like "I73%"  or SUB_SICK like "I74%" or SUB_SICK like "I77%" 
or SUB_SICK like "I79%" 
then 1 else 0 end as PVD,

case when ((MAIN_SICK like "F00%" or MAIN_SICK like "F01%" or MAIN_SICK like "F03%" or MAIN_SICK like "G30%") 
or (MAIN_SICK in ("G3100", "G3182"))) or 
((SUB_SICK like "F00%" or SUB_SICK like "F01%" or SUB_SICK like "F03%" or SUB_SICK like "G30%") 
or (SUB_SICK in ("G3100", "G3182"))) then 1 else 0 end as Dementia,

case when (MAIN_SICK like "I48%") 
or (SUB_SICK like "I48%")then 1 else 0 end as Atrial_fibrillation,

case when (MAIN_SICK like "E78%") 
or (SUB_SICK like "E78%")then 1 else 0 end as Hyperlipidemia,

case when (MAIN_SICK like "N18%") 
or (SUB_SICK like "N18%")then 1 else 0 end as CKD
from T20_study_pop;
quit; 

/* 변수값 정리 */
proc sql;
create table result as select PERSON_ID, SUM(Hypertension) as Hypertension_T20, 
SUM(Asthma) as Asthma_result, SUM(COPD) as COPD_result, 
SUM(Atrial_fibrillation) as Atrial_fibrillation_result, SUM(Thromboembolism) as Thromboembolism_result,
SUM(CLD) as CLD_result, SUM(CKD) as CKD_result,
SUM(CAD) as CAD_result, SUM(PVD) as PVD_result,
SUM(Dementia) as Dementia_result, SUM(GI_Disorders) as GI_Disorders_result,
SUM(Hyperlipidemia) as Hyperlipidemia_result, SUM(Pneumonia) as Pneumonia_result,
SUM(Psychiatric_disorders) as Psychiatric_disorders_result
from temp
group by PERSON_ID;
QUIT;

DATA T20_comorbidities;
SET result;
IF Asthma_result>=1 then Asthma_result=1; 
else Asthma_result=0;
IF COPD_result>=1 then COPD_result=1; 
else COPD_result=0;
IF Atrial_fibrillation_result>=1 then Atrial_fibrillation_result=1; 
else Atrial_fibrillation_result=0;
IF Thromboembolism_result>=1 then Thromboembolism_result=1; 
else Thromboembolism_result=0;
IF CLD_result>=1 then CLD_result=1; 
else CLD_result=0;
IF CAD_result>=1 then CAD_result=1; 
else CAD_result=0;
IF CKD_result>=1 then CKD_result=1; 
else CKD_result=0;
IF PVD_result>=1 then PVD_result=1; 
else PVD_result=0;
IF Dementia_result>=1 then Dementia_result=1; 
else Dementia_result=0;
IF GI_Disorders_result>=1 then GI_Disorders_result=1; 
else GI_Disorders_result=0;
IF Hyperlipidemia_result>=1 then Hyperlipidemia_result=1; 
else Hyperlipidemia_result=0;
IF Pneumonia_result>=1 then Pneumonia_result=1; 
else Pneumonia_result=0;
IF Psychiatric_disorders_result>=1 then Psychiatric_disorders_result=1; 
else Psychiatric_disorders_result=0;
run;

/* 검진DB - 2002년 ~ 2008년 DB 불러오기 */
DATA screening_08;
SET '/userdata12/room115/data_out/data_store/screening_08.sas7bdat';
KEEP PERSON_ID HME_DT BP_HIGH BP_LWST; 
RUN;

/* 검진DB - 2009년 ~ 2015년 DB 불러오기 */
DATA screening_09_15;
SET '/userdata12/room115/data_out/data_store/screening_09_15.sas7bdat';
KEEP PERSON_ID HME_DT BP_HIGH BP_LWST;
IF PERSON_ID NOT = '';
RUN;

/* 검진DB - 2016년 ~ 2017년 DB 불러오기 */
DATA screening_16_17;
SET '/userdata12/room115/data_source/1619/nhis_heals_gj_1617.sas7bdat';
KEEP PERSON_ID HME_DT BP_HIGH BP_LWST;
IF PERSON_ID NOT = '';
RUN;

/* 생애전환기검진DB - 2007년 ~ 2008년 DB 불러오기 */
DATA ex_screening_08;
SET '/userdata12/room115/data_out/data_store/ex_screening_08.sas7bdat';
IF PERSON_ID NOT = '';
KEEP PERSON_ID HME_DT BP_HIGH BP_LWST;
run;

/* 생애전환기검진DB - 2009년 ~ 2015년 DB 불러오기 */
DATA ex_screening_09_15;
SET '/userdata12/room115/data_out/data_store/ex_screening_09_15.sas7bdat';
IF PERSON_ID NOT = '';
KEEP PERSON_ID HME_DT BP_HIGH BP_LWST;
run;

/* 생애전환기검진DB - 2016년 ~ 2017년 DB 불러오기 */
DATA ex_screening_16_17;
SET '/userdata12/room115/data_source/1619/nhis_heals_lj_1617.sas7bdat';
IF PERSON_ID NOT = '';
KEEP PERSON_ID HME_DT BP_HIGH BP_LWST;
RUN;

/* 건강검진DB merge */
DATA total_screening;
SET screening_08 screening_09_15 screening_16_17 ex_screening_08 ex_screening_09_15 ex_screening_16_17;
run;

proc sql;
create table screening_comorbidities as select * from total_screening join total_study_pop
on total_screening.PERSON_ID = total_study_pop.PERSON_ID
and total_screening.HME_DT <= total_study_pop.HME_DT;
quit;

DATA temp2;
SET screening_comorbidities;
IF BP_HIGH>=130 or BP_LWST>=80 then Hypertension_screening=1;
ELSE Hypertension_screening=0;
run;

proc sql;
create table result2 as select PERSON_ID, SUM(Hypertension_screening) as Hypertension_screening2
from temp2
group by PERSON_ID;
quit;

proc sort data=T20_comorbidities;
by PERSON_ID;
run;

proc sort data=result2;
by PERSON_ID;
run;

DATA comorbidities_total;
MERGE T20_comorbidities result2;
by PERSON_ID;
run;

DATA Comorbidities;
SET comorbidities_total;
IF Hypertension_T20>=1 or Hypertension_screening2>=1 then Hypertension_result=1;
ELSE Hypertension_result=0;
DROP Hypertension_T20 Hypertension_screening2;
run;


/******************************/
/**** Medication ATC Code Define ****/

/***** ATC Code excel file import *****/
DATA ATC;
SET '/userdata12/room115/data_out/data_store/ATCcode_ver2.sas7bdat';
RENAME 주성분코드=main_code;
RENAME 제품코드=item_code;
RENAME ATC코드=ATC_code;
RUN;

/*** 첫 번째 포함조건 만족하는 obj들의 진료DB ***/
DATA T60_T20_screening;
SET '/userdata12/room115/data_out/data_store/T60_T20_screening.sas7bdat';
KEEP PERSON_ID RECU_FR_DT DIV_CD GNL_NM_CD;
RUN;

DATA total_study_pop;
SET '/userdata12/room115/data_out/data_store/total_study_pop.sas7bdat';
RUN;

PROC SQL;
create table T60_study_pop as select * from total_study_pop join T60_T20_screening
on total_study_pop.PERSON_ID = T60_T20_screening.PERSON_ID
and total_study_pop.HME_DT >= T60_T20_screening.RECU_FR_DT;
quit;
proc sql;
create table med_uniq_ID as select distinct PERSON_ID from T60_study_pop;
quit;

DATA T60_study_pop_2009;
SET T60_study_pop;
IF RECU_FR_DT<'20100101';
DROP HME_DT;
RUN;

DATA T60_study_pop_2010;
SET T60_study_pop;
IF (RECU_FR_DT>='20100101') and (RECU_FR_DT<'20160101');
DROP HME_DT;
RUN;

DATA T60_study_pop_2016;
SET T60_study_pop;
IF RECU_FR_DT>='20160101';
RUN;

proc sql;
create table unique_ATC as select distinct main_code, item_code, ATC_code from ATC;
quit;

proc sql;
create table T60_study_2009_ATC as select * from T60_study_pop_2009 
inner join unique_ATC
on T60_study_pop_2009.GNL_NM_CD = unique_ATC.main_code
ORDER BY T60_study_pop_2009.PERSON_ID;
quit;

proc sql;
create table T60_study_2010_ATC as select * from T60_study_pop_2010 
inner join unique_ATC
on T60_study_pop_2010.GNL_NM_CD = unique_ATC.main_code
and T60_study_pop_2010.DIV_CD = unique_ATC.item_code
ORDER BY T60_study_pop_2010.PERSON_ID;
quit;

proc sql;
create table T60_study_2016_ATC as select * from T60_study_pop_2016
inner join unique_ATC
on T60_study_pop_2016.DIV_CD = unique_ATC.main_code
order by T60_study_pop_2016.PERSON_ID;
quit;

DATA T60_ATC;
SET T60_study_2009_ATC T60_study_2010_ATC T60_study_2016_ATC;
RUN;

proc sql;
create table temp1 as select *,
case when (ATC_code like "A10A%") then 1 else 0 end as insulin,

case when ATC_code like "A10B%" then 1 else 0 end as oral_diabetes,

case when (ATC_code like "C10%") and (ATC_code not like "C10AA%") and (ATC_code not like "C10BA%")
and (ATC_code not like "C10BX%") and (ATC_code not like "A10BH51") and (ATC_code not like "A10BH52")
then 1 else 0 end as Non_statin,

case when (ATC_code like "C10AA%") or (ATC_code like "C10BA%")
or (ATC_code like "C10BX%") or (ATC_code like "A10BH51") 
or (ATC_code like "A10BH52") then 1 else 0 end as statin,

case when (ATC_code like "C09A%") or (ATC_code like "C09B%")
or (ATC_code like "C09C%") or (ATC_code like "C09D%") 
Then 1 else 0 end as Hypertension_renin,

case when (ATC_code like "C07%") or (ATC_code like "C08%")
or (ATC_code like "C03%") or (ATC_code like "C01D%") 
or (ATC_code like "C02D%") or (ATC_code like "C04%")
or (ATC_code like "C02A%") 
or (ATC_code like "C02B%") or (ATC_code like "C02C%") 
then 1 else 0 end as Hypertension_except_RAS,

case when (ATC_code like "N06D%") or (ATC_code like "C01B%")
or (ATC_code like "N05B%") or (ATC_code like "N05A%") 
or (ATC_code like "N06A%")
then 1 else 0 end as CNS,

case when (ATC_code like "M01A%") or (ATC_code like "N02BE01")
or (ATC_code like "N02BE05") or (ATC_code like "N02BE51") 
or (ATC_code like "N02BE71")
then 1 else 0 end as Non_opioid,

case when (ATC_code like "B01AA%") or (ATC_code like "B01AB%")
or (ATC_code like "B01AE%") or (ATC_code like "B01AF%") 
or (ATC_code like "B01AX%")
then 1 else 0 end as Anticoagulants,

case when (ATC_code like "A01AD05") or (ATC_code like "B01AC%")
or (ATC_code like "C07FX02") or (ATC_code like "C07FX03") 
or (ATC_code like "C07FX04") or (ATC_code like "C10BX%")
or (ATC_code like "M01BA03") 
or (ATC_code like "N02AJ02") or (ATC_code like "N02AJ07") 
or (ATC_code like "N02AJ18") or (ATC_code like "N02BA01")
or (ATC_code like "N02BA51") or (ATC_code like "N02BA71")
then 1 else 0 end as Antiplatelets,

case when (ATC_code like "L04A%") then 1 else 0 end as immunosuppressant
from T60_ATC;
quit; 

proc sql;
create table result_ATC as select PERSON_ID,
SUM(insulin) AS insulin_result, SUM(oral_diabetes) as oral_diabetes_result,
SUM(Non_statin) as Non_statin_result, SUM(statin) as statin_result,
SUM(Hypertension_renin) as Hypertension_renin_result,
SUM(Hypertension_except_RAS) as Hypertension_except_RAS_result,
SUM(CNS) as CNS_result, SUM(Non_opioid) AS Non_opioid_result,
SUM(Anticoagulants) as Anticoagulants_result, SUM(Antiplatelets) as Antiplatelets_result,
SUM(immunosuppressant) as immunosuppressant_result
from temp1
group by PERSON_ID;
quit;

DATA Med;
SET result_ATC;
IF insulin_result>=1 then insulin_result=1;
ELSE insulin_result=0;

IF oral_diabetes_result>=1 then oral_diabetes_result=1;
ELSE oral_diabetes_result=0;

IF Non_statin_result>=1 then Non_statin_result=1;
ELSE Non_statin_result=0;

IF statin_result>=1 then statin_result=1;
ELSE statin_result=0;

IF Hypertension_renin_result >=1 then Hypertension_renin_result=1;
ELSE Hypertension_renin_result=0;

IF Hypertension_except_RAS_result>=1 then Hypertension_except_RAS_result=1;
ELSE Hypertension_except_RAS_result=0;

IF CNS_result>=1 then CNS_result=1;
ELSE CNS_result=0;

IF Non_opioid_result>=1 then Non_opioid_result=1;
ELSE Non_opioid_result=0;

IF Anticoagulants_result>=1 then Anticoagulants_result=1;
ELSE Anticoagulants_result=0;

IF Antiplatelets_result>=1 then Antiplatelets_result=1;
ELSE Antiplatelets_result=0;

IF immunosuppressant_result>=1 then immunosuppressant_result=1;
ELSE immunosuppressant_result=0;

DROP RECU_FR_DT GNL_NM_CD DIV_CD main_code item_code ATC_code;
RUN;

DATA Medication_covariate;
MERGE Med total_study_pop;
BY PERSON_ID;
IF missing(insulin_result) then do;
	insulin_result=0;
	oral_diabetes_result=0;
	Non_statin_result=0;
	statin_result=0;
	Hypertension_renin_result=0;
	Hypertension_except_RAS_result=0;
	CNS_result=0;
	Non_opioid_result=0;
	Anticoagulants_result=0;
	Antiplatelets_result=0;
	immunosuppressant_result=0;
end;
DROP HME_DT;
RUN;
