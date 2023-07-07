/***** Outcome Define *****/
libname dir1 "/userdata12/room111/data_out/data_store";
run;

/*** Outcome : Death ***/
DATA total_study_pop;
SET '/userdata12/room111/data_out/data_store/total_study_pop2.sas7bdat';
RUN;

DATA jk_total;
SET '/userdata12/room111/data_out/data_store/jk_total.sas7bdat';
KEEP PERSON_ID STND_Y DTH_MDY DTH_CODE1 DTH_CODE2;
RUN;

DATA total_study_pop_ver2;
SET total_study_pop;
year=SUBSTR(HME_DT,1,4);
RUN;

proc sql;
create table death_check as select * from total_study_pop_ver2 join jk_total
on total_study_pop_ver2.PERSON_ID = jk_total.PERSON_ID
and total_study_pop_ver2.year <= jk_total.STND_Y;
quit;

DATA death;
SET death_check;
IF DTH_MDY ^= '' then Death=1;
ELSE Death=0;
DROP year;
RUN;

PROC SORT DATA=death;
by PERSON_ID STND_Y;
RUN;

DATA death_check2;
SET death;
BY PERSON_ID STND_Y;
IF last.PERSON_ID;
RUN;

DATA death_check3;
SET death_check2;
IF Death=1 and HME_DT >= DTH_MDY then delete;
run;

proc sql;
create table check_outcome as select PERSON_ID, DTH_MDY, DTH_CODE1, DTH_CODE2, Death,
case when ((DTH_CODE1 like "I%") or (DTH_CODE1 like "G45%") or (DTH_CODE1 like "G46%") or 
(DTH_CODE2 like "I%") or (DTH_CODE2 like "G45%") or (DTH_CODE2 like "G46%")) 
then 1 else 0 end as Cardio_Death,
case when ((DTH_CODE1 like "C%") and (DTH_CODE1 not like "C_%" OR DTH_CODE1 not like "C98%" or DTH_CODE1 not like "C99%")) or
((DTH_CODE2 like "C%") and (DTH_CODE2 not like "C_%" or DTH_CODE2 not like "C98%" or DTH_CODE2 not like "C99%")) 
then 1 else 0 end as Cancer_Death 
from death_check3;
quit;

DATA dir1.Death_detail;
SET check_outcome;
IF Death=1 and Cardio_death=0 and Cancer_Death=0 then Other_Death=1;
ELSE Other_Death=0;
KEEP PERSON_ID Death Cardio_Death Cancer_Death Other_Death;
RUN;

/* Data Merge */
DATA total_study_pop_Duration;
SET dir1.total_study_pop_duration;
RENAME Duration = Cohort_entry_Duration;
DROP HME_DT DISEASE_ONSET;
RUN;

proc sort data=dir1.bmi_exposure;
by person_id;
run;

proc sort data=dir1.EXERCI_METS_COV;
by person_id;
run;

DATA Analysis_data;
MERGE total_study_pop_Duration dir1.Outcome_all dir1.Death_detail dir1.BMI_Exposure dir1.Demographic_cov dir1.Biomarker_cov dir1.family_history_covariate
           dir1.Case_History_covariate dir1.Smoking_cov dir1.Drink_habit_cov dir1.EXERCI_FREQ_COV dir1.EXERCI_METS_COV dir1.Comorbidities dir1.Medication_covariate;
BY PERSON_ID;
DROP HME_DT;
IF EXERCI_HABIT=0 and METS_minutes=. then METS_minutes=0;
/*IF SMK_STAT_TYPE_RSPS_CD='' OR SMK_STAT_TYPE_RSPS_CD='.' THEN do;
	SMK_STAT_TYPE_RSPS_CD='1';
	CUR_SMK_TERM=0;
	CUR_DSQTY=0;
	CUR_PACK_YEAR=0;
	PAST_SMK_TERM=0;
	PAST_DSQTY=0;
	PAST_PACK_YEAR=0;
END;*/
IF Death=. then DELETE;
RUN;

DATA dir1.Mydata_ver2;
SET Analysis_data;
IF Outcome_category="Last_follow" then Outcome=0;
ELSE Outcome=1;
RUN;

proc sort data=dir1.Mydata_ver2;
BY Category;
RUN;

proc freq data=dir1.Mydata_ver2;
tables Cardio_Death * Category;
RUN;

proc freq data=dir1.Mydata_ver2;
tables Cancer_Death * Category;
RUN;

proc freq data=dir1.Mydata_ver2;
tables Other_Death * Category;
RUN;

/* Unadjusted */
/** Cardiovascular related death **/
proc phreg data=dir1.Mydata_ver2;
class Category(ref='Moderate');
model FU_Duration_Death*Cardio_Death(0) = Category;
RUN;

/** Cancer related Death**/
proc phreg data=dir1.Mydata_ver2;
class Category(ref='Moderate');
model FU_Duration_Death*Cancer_Death(0) = Category;
RUN;

/** Other_Death **/
proc phreg data=dir1.Mydata_ver2;
class Category(ref='Moderate');
model FU_Duration_Death*Other_Death(0) = Category;
RUN;

/* Adjusted */
/* Cardiovascular Related Death */
proc phreg data=dir1.Mydata_ver2;
class ctrb_pt_type_cd(ref='0') /*olig_prote_cd(ref='1')*/ FMLY_Heart_disease(ref='0') FMLY_Diabetes(ref='0') 
FMLY_Cancer(ref='0') MI_case(REF='0') Heart_failure_case(ref ='0') Cancer_case(ref='0')
Hypertension_case(ref='0') TIA_Stroke_case(REF='0') Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0') SMK_STAT_TYPE_RSPS_CD(ref='1');

model FU_Duration_death*Cardio_Death(0) = Cohort_entry_Duration Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes;

RUN;

/* Cancer Related Death */
proc phreg data=dir1.Mydata_ver2;
class ctrb_pt_type_cd(ref='0') /*olig_prote_cd(ref='1')*/ FMLY_Heart_disease(ref='0') FMLY_Diabetes(ref='0') 
FMLY_Cancer(ref='0') MI_case(REF='0') Heart_failure_case(ref ='0') Cancer_case(ref='0')
Hypertension_case(ref='0') TIA_Stroke_case(REF='0') Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0') SMK_STAT_TYPE_RSPS_CD(ref='1');

model FU_Duration_death*Cancer_Death(0) = Cohort_entry_Duration Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes;

RUN;

/* Other Cause Death */
proc phreg data=dir1.Mydata_ver2;
class ctrb_pt_type_cd(ref='0') /*olig_prote_cd(ref='1')*/ FMLY_Heart_disease(ref='0') FMLY_Diabetes(ref='0') 
FMLY_Cancer(ref='0') MI_case(REF='0') Heart_failure_case(ref ='0') Cancer_case(ref='0')
Hypertension_case(ref='0') TIA_Stroke_case(REF='0') Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0') SMK_STAT_TYPE_RSPS_CD(ref='1');

model FU_Duration_death*Other_Death(0) = Cohort_entry_Duration Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes;

RUN;

/* Sub group Analysis */
DATA data;
SET dir1.Mydata_ver2;
IF AGE<=65 THEN AGE_65=1;
ELSE AGE_65=0;
IF SMK_STAT_TYPE_RSPS_CD='.' or SMK_STAT_TYPE_RSPS_CD='' then SMK_STAT_TYPE_RSPS_CD=0;
RUN;

/* Cancer Related Death*/
PROC SORT DATA=data;
BY Category;
run;

proc freq data=data;
tables AGE_65 * Cancer_Death * Category;
RUN;

proc freq data=data;
tables SEX * Cancer_Death * Category;
RUN;

proc freq data=data;
tables Hyperlipidemia_result * Cancer_Death * Category;
RUN;

proc freq data=data;
tables Hypertension_result * Cancer_Death * Category;
RUN;

proc freq data=data;
tables SMK_STAT_TYPE_RSPS_CD * Cancer_Death * Category;
RUN;

/*AGE_65*/
proc phreg data=data covs(aggregate);
class ctrb_pt_type_cd(ref='0') /*olig_prote_cd(ref='1')*/ FMLY_Heart_disease(ref='0') FMLY_Diabetes(ref='0') 
FMLY_Cancer(ref='0') MI_case(REF='0') Heart_failure_case(ref ='0') Cancer_case(ref='0')
Hypertension_case(ref='0') TIA_Stroke_case(REF='0')Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0') SMK_STAT_TYPE_RSPS_CD(ref='1') AGE_65(ref='0')
/*BMI_Category(ref='2')*/;

model FU_Duration_Death*Cancer_Death(0) = Cohort_entry_Duration /*BMI_Category*/ AGE_65 Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes AGE_65|Category;

HAZARDRATIO  Category/diff=ref at (AGE_65=all);
*HAZARDRATIO AGE_65*Category / cl=both; 
/*RANDOM PERSON_ID;*/
		
contrast 'Under vs Moderate at AGE <=65' Category 0 0 1 AGE_65*Category 0 0 1/ estimate=exp;
contrast 'Over vs Moderate at AGE <=65' Category 0 1 AGE_65*Category 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at AGE <=65' Category 1 AGE_65*Category 1 / estimate=exp;
contrast 'Under vs Moderate at AGE >65' Category 0 0 1 AGE_65*Category 0 / estimate=exp;
contrast 'Over vs Moderate at AGE >65' Category 0 1 AGE_65*Category 0 / estimate=exp;
contrast 'Obesity vs Moderate at AGE >65' Category 1 AGE_65*Category 0/ estimate=exp;
*contrast "AGE_65 at Category=Overweighted" BMI_Category 2 AGE_65 1 BMI_Category*AGE_65 3 / estimate=exp;
*contrast "AGE_65 at Category=Obesity" BMI_Category 2 AGE_65 1 BMI_Category*AGE_65 4 / estimate=exp;

RUN;

/*SEX*/
proc phreg data=data covs(aggregate);
class ctrb_pt_type_cd(ref='0') /*olig_prote_cd(ref='1')*/ FMLY_Heart_disease(ref='0') FMLY_Diabetes(ref='0') 
FMLY_Cancer(ref='0') MI_case(REF='0') Heart_failure_case(ref ='0') Cancer_case(ref='0')
Hypertension_case(ref='0') TIA_Stroke_case(REF='0')Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0') SMK_STAT_TYPE_RSPS_CD(ref='1') AGE_65(ref='0')
/*BMI_Category(ref='2')*/;

model FU_Duration_Death*Cancer_Death(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes SEX * Category;

HAZARDRATIO  SEX/diff=ref at (Category=all);
HAZARDRATIO  Category/diff=ref at (SEX=all);

contrast 'Under vs Moderate at SEX=1' Category 0 0 1 SEX*Category 0/ estimate=exp;
contrast 'Over vs Moderate at SEX=1' Category 0 1 SEX*Category 0 / estimate=exp;
contrast 'Obesity vs Moderate at SEX=1' Category 1 SEX*Category 0 / estimate=exp;
contrast 'Under vs Moderate at SEX=2' Category 0 0 1 SEX*Category 0 0 1 / estimate=exp;
contrast 'Over vs Moderate at SEX=2' Category 0 1 SEX*Category 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at SEX=2' Category 1 SEX*Category 1/ estimate=exp;
*HAZARDRATIO AGE_65*Category / cl=both; 
/*RANDOM PERSON_ID;
		
*contrast "AGE_65 at Category=Underweighted" BMI_Category 2 AGE_65 1 BMI_Category*AGE_65 1 / estimate=exp;
*contrast "AGE_65 at Category=Overweighted" BMI_Category 2 AGE_65 1 BMI_Category*AGE_65 3 / estimate=exp;
*contrast "AGE_65 at Category=Obesity" BMI_Category 2 AGE_65 1 BMI_Category*AGE_65 4 / estimate=exp;*/

RUN;

/*Hyperlipidemia*/
proc phreg data=data covs(aggregate);
class ctrb_pt_type_cd(ref='0') /*olig_prote_cd(ref='1')*/ FMLY_Heart_disease(ref='0') FMLY_Diabetes(ref='0') 
FMLY_Cancer(ref='0') MI_case(REF='0') Heart_failure_case(ref ='0') Cancer_case(ref='0')
Hypertension_case(ref='0') TIA_Stroke_case(REF='0')Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0') SMK_STAT_TYPE_RSPS_CD(ref='1') 
/*BMI_Category(ref='2')*/;

model FU_Duration_Death*Cancer_Death(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes Hyperlipidemia_result * Category;

HAZARDRATIO  Category/diff=ref at (Hyperlipidemia_result=all);
HAZARDRATIO  Hyperlipidemia_result /diff=ref at (Category=all);
contrast 'Under vs Moderate at Hyperlipidemia=0' Category 0 0 1 Hyperlipidemia_result*Category 0/ estimate=exp;
contrast 'Over vs Moderate at Hyperlipidemia=0' Category 0 1 Hyperlipidemia_result*Category 0 / estimate=exp;
contrast 'Obesity vs Moderate at Hyperlipidemia=0' Category 1 Hyperlipidemia_result*Category 0 / estimate=exp;
contrast 'Under vs Moderate at Hyperlipidemia=1' Category 0 0 1 Hyperlipidemia_result*Category 0 0 1 / estimate=exp;
contrast 'Over vs Moderate at Hyperlipidemia=1' Category 0 1 Hyperlipidemia_result*Category 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at Hyperlipidemia=1' Category 1 Hyperlipidemia_result*Category 1/ estimate=exp;
RUN;

/*Hypertension*/
proc phreg data=data covs(aggregate);
class ctrb_pt_type_cd(ref='0') /*olig_prote_cd(ref='1')*/ FMLY_Heart_disease(ref='0') FMLY_Diabetes(ref='0') 
FMLY_Cancer(ref='0') MI_case(REF='0') Heart_failure_case(ref ='0') Cancer_case(ref='0')
Hypertension_case(ref='0') TIA_Stroke_case(REF='0')Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0') SMK_STAT_TYPE_RSPS_CD(ref='1') 
/*BMI_Category(ref='2')*/;

model FU_Duration_Death*Cancer_Death(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes Hypertension_result * Category;


HAZARDRATIO  Hypertension_result /diff=ref at (Category=all);
HAZARDRATIO  Category/diff=ref at (Hypertension_result=all);

contrast 'Under vs Moderate at Hypertension=0' Category 0 0 1 Hypertension_result*Category 0/ estimate=exp;
contrast 'Over vs Moderate at Hypertension=0' Category 0 1 Hypertension_result*Category 0 / estimate=exp;
contrast 'Obesity vs Moderate at Hypertension=0' Category 1 Hypertension_result*Category 0 / estimate=exp;
contrast 'Under vs Moderate at Hypertension=1' Category 0 0 1 Hypertension_result*Category 0 0 1 / estimate=exp;
contrast 'Over vs Moderate at Hypertension=1' Category 0 1 Hypertension_result*Category 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at Hypertension=1' Category 1 Hypertension_result*Category 1/ estimate=exp;

RUN;

/*Smoking status*/
proc phreg data=data covs(aggregate);
class ctrb_pt_type_cd(ref='0') /*olig_prote_cd(ref='1')*/ FMLY_Heart_disease(ref='0') FMLY_Diabetes(ref='0') 
FMLY_Cancer(ref='0') MI_case(REF='0') Heart_failure_case(ref ='0') Cancer_case(ref='0')
Hypertension_case(ref='0') TIA_Stroke_case(REF='0') Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0') SMK_STAT_TYPE_RSPS_CD(ref='1') 
/*BMI_Category(ref='2')*/;

model FU_Duration_Death*Cancer_Death(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes SMK_STAT_TYPE_RSPS_CD|Category;

HAZARDRATIO  Category/diff=ref at (SMK_STAT_TYPE_RSPS_CD=all);

contrast 'Under vs Moderate at Non-Response' Category 0 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 0 0 0 0 1/ estimate=exp;
contrast 'Over vs Moderate at Non-Response' Category 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at Non-Response' Category 1 SMK_STAT_TYPE_RSPS_CD*Category 1 / estimate=exp;
contrast 'Under vs Moderate at Never' Category 0 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0/ estimate=exp;
contrast 'Over vs Moderate at Never' Category 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 / estimate=exp;
contrast 'Obesity vs Moderate at Never' Category 1 SMK_STAT_TYPE_RSPS_CD*Category 0 / estimate=exp;
contrast 'Under vs Moderate at Past' Category 0 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 0 0 0 0 0 1 / estimate=exp;
contrast 'Over vs Moderate at Past' Category 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 0 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at Past' Category 1 SMK_STAT_TYPE_RSPS_CD*Category 0 1/ estimate=exp;
contrast 'Under vs Moderate at Current' Category 0 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 0 0 0 0 0 0 1 / estimate=exp;
contrast 'Over vs Moderate at Current' Category 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 0 0 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at Current' Category 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 1/ estimate=exp;

RUN;

/**********************/
/* Cardiovascular Related Death*/
PROC SORT DATA=data;
BY Category;
run;

proc freq data=data;
tables AGE_65 * Cardio_Death * Category;
RUN;

proc freq data=data;
tables SEX * Cardio_Death * Category;
RUN;

proc freq data=data;
tables Hyperlipidemia_result * Cardio_Death * Category;
RUN;

proc freq data=data;
tables Hypertension_result * Cardio_Death * Category;
RUN;

proc freq data=data;
tables SMK_STAT_TYPE_RSPS_CD * Cardio_Death * Category;
RUN;

/*AGE_65*/
proc phreg data=data covs(aggregate);
class ctrb_pt_type_cd(ref='0') /*olig_prote_cd(ref='1')*/ FMLY_Heart_disease(ref='0') FMLY_Diabetes(ref='0') 
FMLY_Cancer(ref='0') MI_case(REF='0') Heart_failure_case(ref ='0') Cancer_case(ref='0')
Hypertension_case(ref='0') TIA_Stroke_case(REF='0')Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0') SMK_STAT_TYPE_RSPS_CD(ref='1') AGE_65(ref='0')
/*BMI_Category(ref='2')*/;

model FU_Duration_Death*Cardio_Death(0) = Cohort_entry_Duration /*BMI_Category*/ AGE_65 Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes AGE_65|Category;

HAZARDRATIO  Category/diff=ref at (AGE_65=all);
*HAZARDRATIO AGE_65*Category / cl=both; 
/*RANDOM PERSON_ID;*/
		
contrast 'Under vs Moderate at AGE <=65' Category 0 0 1 AGE_65*Category 0 0 1/ estimate=exp;
contrast 'Over vs Moderate at AGE <=65' Category 0 1 AGE_65*Category 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at AGE <=65' Category 1 AGE_65*Category 1 / estimate=exp;
contrast 'Under vs Moderate at AGE >65' Category 0 0 1 AGE_65*Category 0 / estimate=exp;
contrast 'Over vs Moderate at AGE >65' Category 0 1 AGE_65*Category 0 / estimate=exp;
contrast 'Obesity vs Moderate at AGE >65' Category 1 AGE_65*Category 0/ estimate=exp;
*contrast "AGE_65 at Category=Overweighted" BMI_Category 2 AGE_65 1 BMI_Category*AGE_65 3 / estimate=exp;
*contrast "AGE_65 at Category=Obesity" BMI_Category 2 AGE_65 1 BMI_Category*AGE_65 4 / estimate=exp;

RUN;

/*SEX*/
proc phreg data=data covs(aggregate);
class ctrb_pt_type_cd(ref='0') /*olig_prote_cd(ref='1')*/ FMLY_Heart_disease(ref='0') FMLY_Diabetes(ref='0') 
FMLY_Cancer(ref='0') MI_case(REF='0') Heart_failure_case(ref ='0') Cancer_case(ref='0')
Hypertension_case(ref='0') TIA_Stroke_case(REF='0')Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0') SMK_STAT_TYPE_RSPS_CD(ref='1') AGE_65(ref='0')
/*BMI_Category(ref='2')*/;

model FU_Duration_Death*Cardio_Death(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes SEX * Category;

HAZARDRATIO  SEX/diff=ref at (Category=all);
HAZARDRATIO  Category/diff=ref at (SEX=all);

contrast 'Under vs Moderate at SEX=1' Category 0 0 1 SEX*Category 0/ estimate=exp;
contrast 'Over vs Moderate at SEX=1' Category 0 1 SEX*Category 0 / estimate=exp;
contrast 'Obesity vs Moderate at SEX=1' Category 1 SEX*Category 0 / estimate=exp;
contrast 'Under vs Moderate at SEX=2' Category 0 0 1 SEX*Category 0 0 1 / estimate=exp;
contrast 'Over vs Moderate at SEX=2' Category 0 1 SEX*Category 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at SEX=2' Category 1 SEX*Category 1/ estimate=exp;
*HAZARDRATIO AGE_65*Category / cl=both; 
/*RANDOM PERSON_ID;
		
*contrast "AGE_65 at Category=Underweighted" BMI_Category 2 AGE_65 1 BMI_Category*AGE_65 1 / estimate=exp;
*contrast "AGE_65 at Category=Overweighted" BMI_Category 2 AGE_65 1 BMI_Category*AGE_65 3 / estimate=exp;
*contrast "AGE_65 at Category=Obesity" BMI_Category 2 AGE_65 1 BMI_Category*AGE_65 4 / estimate=exp;*/

RUN;

/*Hyperlipidemia*/
proc phreg data=data covs(aggregate);
class ctrb_pt_type_cd(ref='0') /*olig_prote_cd(ref='1')*/ FMLY_Heart_disease(ref='0') FMLY_Diabetes(ref='0') 
FMLY_Cancer(ref='0') MI_case(REF='0') Heart_failure_case(ref ='0') Cancer_case(ref='0')
Hypertension_case(ref='0') TIA_Stroke_case(REF='0')Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0') SMK_STAT_TYPE_RSPS_CD(ref='1') 
/*BMI_Category(ref='2')*/;

model FU_Duration_Death*Cardio_Death(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes Hyperlipidemia_result * Category;

HAZARDRATIO  Category/diff=ref at (Hyperlipidemia_result=all);
HAZARDRATIO  Hyperlipidemia_result /diff=ref at (Category=all);
contrast 'Under vs Moderate at Hyperlipidemia=0' Category 0 0 1 Hyperlipidemia_result*Category 0/ estimate=exp;
contrast 'Over vs Moderate at Hyperlipidemia=0' Category 0 1 Hyperlipidemia_result*Category 0 / estimate=exp;
contrast 'Obesity vs Moderate at Hyperlipidemia=0' Category 1 Hyperlipidemia_result*Category 0 / estimate=exp;
contrast 'Under vs Moderate at Hyperlipidemia=1' Category 0 0 1 Hyperlipidemia_result*Category 0 0 1 / estimate=exp;
contrast 'Over vs Moderate at Hyperlipidemia=1' Category 0 1 Hyperlipidemia_result*Category 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at Hyperlipidemia=1' Category 1 Hyperlipidemia_result*Category 1/ estimate=exp;
RUN;

/*Hypertension*/
proc phreg data=data covs(aggregate);
class ctrb_pt_type_cd(ref='0') /*olig_prote_cd(ref='1')*/ FMLY_Heart_disease(ref='0') FMLY_Diabetes(ref='0') 
FMLY_Cancer(ref='0') MI_case(REF='0') Heart_failure_case(ref ='0') Cancer_case(ref='0')
Hypertension_case(ref='0') TIA_Stroke_case(REF='0')Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0') SMK_STAT_TYPE_RSPS_CD(ref='1') 
/*BMI_Category(ref='2')*/;

model FU_Duration_Death*Cardio_Death(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes Hypertension_result * Category;


HAZARDRATIO  Hypertension_result /diff=ref at (Category=all);
HAZARDRATIO  Category/diff=ref at (Hypertension_result=all);

contrast 'Under vs Moderate at Hypertension=0' Category 0 0 1 Hypertension_result*Category 0/ estimate=exp;
contrast 'Over vs Moderate at Hypertension=0' Category 0 1 Hypertension_result*Category 0 / estimate=exp;
contrast 'Obesity vs Moderate at Hypertension=0' Category 1 Hypertension_result*Category 0 / estimate=exp;
contrast 'Under vs Moderate at Hypertension=1' Category 0 0 1 Hypertension_result*Category 0 0 1 / estimate=exp;
contrast 'Over vs Moderate at Hypertension=1' Category 0 1 Hypertension_result*Category 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at Hypertension=1' Category 1 Hypertension_result*Category 1/ estimate=exp;

RUN;

/*Smoking status*/
proc phreg data=data covs(aggregate);
class ctrb_pt_type_cd(ref='0') /*olig_prote_cd(ref='1')*/ FMLY_Heart_disease(ref='0') FMLY_Diabetes(ref='0') 
FMLY_Cancer(ref='0') MI_case(REF='0') Heart_failure_case(ref ='0') Cancer_case(ref='0')
Hypertension_case(ref='0') TIA_Stroke_case(REF='0') Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0') SMK_STAT_TYPE_RSPS_CD(ref='1') 
/*BMI_Category(ref='2')*/;

model FU_Duration_Death*Cardio_Death(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes SMK_STAT_TYPE_RSPS_CD|Category;

HAZARDRATIO  Category/diff=ref at (SMK_STAT_TYPE_RSPS_CD=all);

contrast 'Under vs Moderate at Non-Response' Category 0 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 0 0 0 0 1/ estimate=exp;
contrast 'Over vs Moderate at Non-Response' Category 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at Non-Response' Category 1 SMK_STAT_TYPE_RSPS_CD*Category 1 / estimate=exp;
contrast 'Under vs Moderate at Never' Category 0 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0/ estimate=exp;
contrast 'Over vs Moderate at Never' Category 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 / estimate=exp;
contrast 'Obesity vs Moderate at Never' Category 1 SMK_STAT_TYPE_RSPS_CD*Category 0 / estimate=exp;
contrast 'Under vs Moderate at Past' Category 0 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 0 0 0 0 0 1 / estimate=exp;
contrast 'Over vs Moderate at Past' Category 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 0 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at Past' Category 1 SMK_STAT_TYPE_RSPS_CD*Category 0 1/ estimate=exp;
contrast 'Under vs Moderate at Current' Category 0 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 0 0 0 0 0 0 1 / estimate=exp;
contrast 'Over vs Moderate at Current' Category 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 0 0 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at Current' Category 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 1/ estimate=exp;

RUN;


/**********************/
/* Other Death*/
DATA data;
SET dir1.Mydata_ver2;
IF AGE<=65 THEN AGE_65=1;
ELSE AGE_65=0;
IF SMK_STAT_TYPE_RSPS_CD='.' or SMK_STAT_TYPE_RSPS_CD='' then SMK_STAT_TYPE_RSPS_CD=0;
RUN;

PROC SORT DATA=data;
BY Category;
run;

proc freq data=data;
tables AGE_65 * Other_Death * Category;
RUN;

proc freq data=data;
tables SEX * Other_Death * Category;
RUN;

proc freq data=data;
tables Hyperlipidemia_result * Other_Death * Category;
RUN;

proc freq data=data;
tables Hypertension_result * Other_Death * Category;
RUN;

proc freq data=data;
tables SMK_STAT_TYPE_RSPS_CD * Other_Death * Category;
RUN;

/*AGE_65*/
proc phreg data=data covs(aggregate);
class ctrb_pt_type_cd(ref='0') /*olig_prote_cd(ref='1')*/ FMLY_Heart_disease(ref='0') FMLY_Diabetes(ref='0') 
FMLY_Cancer(ref='0') MI_case(REF='0') Heart_failure_case(ref ='0') Cancer_case(ref='0')
Hypertension_case(ref='0') TIA_Stroke_case(REF='0')Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0') SMK_STAT_TYPE_RSPS_CD(ref='1') AGE_65(ref='0')
/*BMI_Category(ref='2')*/;

model FU_Duration_Death*Other_Death(0) = Cohort_entry_Duration /*BMI_Category*/ AGE_65 Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes AGE_65|Category;

HAZARDRATIO  Category/diff=ref at (AGE_65=all);
*HAZARDRATIO AGE_65*Category / cl=both; 
/*RANDOM PERSON_ID;*/
		
contrast 'Under vs Moderate at AGE <=65' Category 0 0 1 AGE_65*Category 0 0 1/ estimate=exp;
contrast 'Over vs Moderate at AGE <=65' Category 0 1 AGE_65*Category 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at AGE <=65' Category 1 AGE_65*Category 1 / estimate=exp;
contrast 'Under vs Moderate at AGE >65' Category 0 0 1 AGE_65*Category 0 / estimate=exp;
contrast 'Over vs Moderate at AGE >65' Category 0 1 AGE_65*Category 0 / estimate=exp;
contrast 'Obesity vs Moderate at AGE >65' Category 1 AGE_65*Category 0/ estimate=exp;
*contrast "AGE_65 at Category=Overweighted" BMI_Category 2 AGE_65 1 BMI_Category*AGE_65 3 / estimate=exp;
*contrast "AGE_65 at Category=Obesity" BMI_Category 2 AGE_65 1 BMI_Category*AGE_65 4 / estimate=exp;

RUN;

/*SEX*/
proc phreg data=data covs(aggregate);
class ctrb_pt_type_cd(ref='0') /*olig_prote_cd(ref='1')*/ FMLY_Heart_disease(ref='0') FMLY_Diabetes(ref='0') 
FMLY_Cancer(ref='0') MI_case(REF='0') Heart_failure_case(ref ='0') Cancer_case(ref='0')
Hypertension_case(ref='0') TIA_Stroke_case(REF='0')Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0') SMK_STAT_TYPE_RSPS_CD(ref='1') AGE_65(ref='0')
/*BMI_Category(ref='2')*/;

model FU_Duration_Death*Other_Death(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes SEX * Category;

HAZARDRATIO  SEX/diff=ref at (Category=all);
HAZARDRATIO  Category/diff=ref at (SEX=all);

contrast 'Under vs Moderate at SEX=1' Category 0 0 1 SEX*Category 0/ estimate=exp;
contrast 'Over vs Moderate at SEX=1' Category 0 1 SEX*Category 0 / estimate=exp;
contrast 'Obesity vs Moderate at SEX=1' Category 1 SEX*Category 0 / estimate=exp;
contrast 'Under vs Moderate at SEX=2' Category 0 0 1 SEX*Category 0 0 1 / estimate=exp;
contrast 'Over vs Moderate at SEX=2' Category 0 1 SEX*Category 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at SEX=2' Category 1 SEX*Category 1/ estimate=exp;
*HAZARDRATIO AGE_65*Category / cl=both; 
/*RANDOM PERSON_ID;
		
*contrast "AGE_65 at Category=Underweighted" BMI_Category 2 AGE_65 1 BMI_Category*AGE_65 1 / estimate=exp;
*contrast "AGE_65 at Category=Overweighted" BMI_Category 2 AGE_65 1 BMI_Category*AGE_65 3 / estimate=exp;
*contrast "AGE_65 at Category=Obesity" BMI_Category 2 AGE_65 1 BMI_Category*AGE_65 4 / estimate=exp;*/

RUN;

/*Hyperlipidemia*/
proc phreg data=data covs(aggregate);
class ctrb_pt_type_cd(ref='0') /*olig_prote_cd(ref='1')*/ FMLY_Heart_disease(ref='0') FMLY_Diabetes(ref='0') 
FMLY_Cancer(ref='0') MI_case(REF='0') Heart_failure_case(ref ='0') Cancer_case(ref='0')
Hypertension_case(ref='0') TIA_Stroke_case(REF='0')Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0') SMK_STAT_TYPE_RSPS_CD(ref='1') 
/*BMI_Category(ref='2')*/;

model FU_Duration_Death*Other_Death(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes Hyperlipidemia_result * Category;

HAZARDRATIO  Category/diff=ref at (Hyperlipidemia_result=all);
HAZARDRATIO  Hyperlipidemia_result /diff=ref at (Category=all);
contrast 'Under vs Moderate at Hyperlipidemia=0' Category 0 0 1 Hyperlipidemia_result*Category 0/ estimate=exp;
contrast 'Over vs Moderate at Hyperlipidemia=0' Category 0 1 Hyperlipidemia_result*Category 0 / estimate=exp;
contrast 'Obesity vs Moderate at Hyperlipidemia=0' Category 1 Hyperlipidemia_result*Category 0 / estimate=exp;
contrast 'Under vs Moderate at Hyperlipidemia=1' Category 0 0 1 Hyperlipidemia_result*Category 0 0 1 / estimate=exp;
contrast 'Over vs Moderate at Hyperlipidemia=1' Category 0 1 Hyperlipidemia_result*Category 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at Hyperlipidemia=1' Category 1 Hyperlipidemia_result*Category 1/ estimate=exp;
RUN;

/*Hypertension*/
proc phreg data=data covs(aggregate);
class ctrb_pt_type_cd(ref='0') /*olig_prote_cd(ref='1')*/ FMLY_Heart_disease(ref='0') FMLY_Diabetes(ref='0') 
FMLY_Cancer(ref='0') MI_case(REF='0') Heart_failure_case(ref ='0') Cancer_case(ref='0')
Hypertension_case(ref='0') TIA_Stroke_case(REF='0')Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0') SMK_STAT_TYPE_RSPS_CD(ref='1') 
/*BMI_Category(ref='2')*/;

model FU_Duration_Death*Other_Death(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes Hypertension_result * Category;


HAZARDRATIO  Hypertension_result /diff=ref at (Category=all);
HAZARDRATIO  Category/diff=ref at (Hypertension_result=all);

contrast 'Under vs Moderate at Hypertension=0' Category 0 0 1 Hypertension_result*Category 0/ estimate=exp;
contrast 'Over vs Moderate at Hypertension=0' Category 0 1 Hypertension_result*Category 0 / estimate=exp;
contrast 'Obesity vs Moderate at Hypertension=0' Category 1 Hypertension_result*Category 0 / estimate=exp;
contrast 'Under vs Moderate at Hypertension=1' Category 0 0 1 Hypertension_result*Category 0 0 1 / estimate=exp;
contrast 'Over vs Moderate at Hypertension=1' Category 0 1 Hypertension_result*Category 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at Hypertension=1' Category 1 Hypertension_result*Category 1/ estimate=exp;

RUN;

/*Smoking status*/
proc phreg data=data covs(aggregate);
class ctrb_pt_type_cd(ref='0') /*olig_prote_cd(ref='1')*/ FMLY_Heart_disease(ref='0') FMLY_Diabetes(ref='0') 
FMLY_Cancer(ref='0') MI_case(REF='0') Heart_failure_case(ref ='0') Cancer_case(ref='0')
Hypertension_case(ref='0') TIA_Stroke_case(REF='0') Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0') SMK_STAT_TYPE_RSPS_CD(ref='1') 
/*BMI_Category(ref='2')*/;

model FU_Duration_Death*Other_Death(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes SMK_STAT_TYPE_RSPS_CD|Category;

HAZARDRATIO  Category/diff=ref at (SMK_STAT_TYPE_RSPS_CD=all);

contrast 'Under vs Moderate at Non-Response' Category 0 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 0 0 0 0 1/ estimate=exp;
contrast 'Over vs Moderate at Non-Response' Category 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at Non-Response' Category 1 SMK_STAT_TYPE_RSPS_CD*Category 1 / estimate=exp;
contrast 'Under vs Moderate at Never' Category 0 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0/ estimate=exp;
contrast 'Over vs Moderate at Never' Category 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 / estimate=exp;
contrast 'Obesity vs Moderate at Never' Category 1 SMK_STAT_TYPE_RSPS_CD*Category 0 / estimate=exp;
contrast 'Under vs Moderate at Past' Category 0 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 0 0 0 0 0 1 / estimate=exp;
contrast 'Over vs Moderate at Past' Category 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 0 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at Past' Category 1 SMK_STAT_TYPE_RSPS_CD*Category 0 1/ estimate=exp;
contrast 'Under vs Moderate at Current' Category 0 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 0 0 0 0 0 0 1 / estimate=exp;
contrast 'Over vs Moderate at Current' Category 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 0 0 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at Current' Category 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 1/ estimate=exp;

RUN;

/********************/
/* Smoking status */
/* 2 Category : Never VS Ever */
DATA data2;
SET dir1.Mydata_ver2;
IF SMK_STAT_TYPE_RSPS_CD ^='0';
IF SMK_STAT_TYPE_RSPS_CD='1' THEN Smoking='Never';
ELSE Smoking='Ever';
RUN;

proc sort data=data2;
BY Category;

proc freq data=data2;
tables Smoking * Cancer_Death * Category;
RUN;

proc freq data=data2;
tables Smoking * Cardio_Death * Category;
RUN;

proc freq data=data2;
tables Smoking * Other_Death * Category;
RUN;

/*Cancer Related Death*/
proc phreg data=data2 covs(aggregate);
class ctrb_pt_type_cd(ref='0') /*olig_prote_cd(ref='1')*/ FMLY_Heart_disease(ref='0') FMLY_Diabetes(ref='0') 
FMLY_Cancer(ref='0') MI_case(REF='0') Heart_failure_case(ref ='0') Cancer_case(ref='0')
Hypertension_case(ref='0') TIA_Stroke_case(REF='0') Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0') /*SMK_STAT_TYPE_RSPS_CD(ref='1')*/ Smoking(ref='Never') 
/*BMI_Category(ref='2')*/;

model FU_Duration_Death*Cancer_Death(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result /*SMK_STAT_TYPE_RSPS_CD*/
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP Smoking /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes Smoking|Category;

HAZARDRATIO  Category/diff=ref at (Smoking=all);

contrast 'Under vs Moderate at Never' Category 0 0 1 Smoking*Category 0/ estimate=exp;
contrast 'Over vs Moderate at Never' Category 0 1 Smoking*Category 0 / estimate=exp;
contrast 'Obesity vs Moderate at Never' Category 1 Smoking*Category 0 / estimate=exp;
contrast 'Under vs Moderate at Ever' Category 0 0 1 Smoking*Category 0 0 1 / estimate=exp;
contrast 'Over vs Moderate at Ever' Category 0 1 Smoking*Category 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at Ever' Category 1 Smoking*Category 1/ estimate=exp;
RUN;

/*Cardiovascular Related Death*/
proc phreg data=data2 covs(aggregate);
class ctrb_pt_type_cd(ref='0') /*olig_prote_cd(ref='1')*/ FMLY_Heart_disease(ref='0') FMLY_Diabetes(ref='0') 
FMLY_Cancer(ref='0') MI_case(REF='0') Heart_failure_case(ref ='0') Cancer_case(ref='0')
Hypertension_case(ref='0') TIA_Stroke_case(REF='0') Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0') /*SMK_STAT_TYPE_RSPS_CD(ref='1')*/ Smoking(ref='Never') 
/*BMI_Category(ref='2')*/;

model FU_Duration_Death*Cardio_Death(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result /*SMK_STAT_TYPE_RSPS_CD*/
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP Smoking /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes Smoking|Category;

HAZARDRATIO  Category/diff=ref at (Smoking=all);

contrast 'Under vs Moderate at Never' Category 0 0 1 Smoking*Category 0/ estimate=exp;
contrast 'Over vs Moderate at Never' Category 0 1 Smoking*Category 0 / estimate=exp;
contrast 'Obesity vs Moderate at Never' Category 1 Smoking*Category 0 / estimate=exp;
contrast 'Under vs Moderate at Ever' Category 0 0 1 Smoking*Category 0 0 1 / estimate=exp;
contrast 'Over vs Moderate at Ever' Category 0 1 Smoking*Category 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at Ever' Category 1 Smoking*Category 1/ estimate=exp;
RUN;

/*Other Death*/
proc phreg data=data2 covs(aggregate);
class ctrb_pt_type_cd(ref='0') /*olig_prote_cd(ref='1')*/ FMLY_Heart_disease(ref='0') FMLY_Diabetes(ref='0') 
FMLY_Cancer(ref='0') MI_case(REF='0') Heart_failure_case(ref ='0') Cancer_case(ref='0')
Hypertension_case(ref='0') TIA_Stroke_case(REF='0') Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0') /*SMK_STAT_TYPE_RSPS_CD(ref='1')*/ Smoking(ref='Never') 
/*BMI_Category(ref='2')*/;

model FU_Duration_Death*Other_Death(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result /*SMK_STAT_TYPE_RSPS_CD*/
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP Smoking /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes Smoking|Category;

HAZARDRATIO  Category/diff=ref at (Smoking=all);
contrast 'Under vs Moderate at Never' Category 0 0 1 Smoking*Category 0/ estimate=exp;
contrast 'Over vs Moderate at Never' Category 0 1 Smoking*Category 0 / estimate=exp;
contrast 'Obesity vs Moderate at Never' Category 1 Smoking*Category 0 / estimate=exp;
contrast 'Under vs Moderate at Ever' Category 0 0 1 Smoking*Category 0 0 1 / estimate=exp;
contrast 'Over vs Moderate at Ever' Category 0 1 Smoking*Category 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at Ever' Category 1 Smoking*Category 1/ estimate=exp;
RUN;

/* 2 Category : Current VS Not Current */
DATA data3;
SET dir1.Mydata_ver2;
IF SMK_STAT_TYPE_RSPS_CD ^='0';
IF SMK_STAT_TYPE_RSPS_CD='3' THEN Smoking='Current';
ELSE Smoking='Not_Current';
RUN;

proc sort data=data3;
BY Category;
RUN;

proc freq data=data3;
tables Smoking * Cancer_Death * Category;
RUN;

proc freq data=data3;
tables Smoking * Cardio_Death * Category;
RUN;

proc freq data=data3;
tables Smoking * Other_Death * Category;
RUN;

/* Cancer Related Death */
proc phreg data=data3 covs(aggregate);
class ctrb_pt_type_cd(ref='0') /*olig_prote_cd(ref='1')*/ FMLY_Heart_disease(ref='0') FMLY_Diabetes(ref='0') 
FMLY_Cancer(ref='0') MI_case(REF='0') Heart_failure_case(ref ='0') Cancer_case(ref='0')
Hypertension_case(ref='0') TIA_Stroke_case(REF='0') Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0') /*SMK_STAT_TYPE_RSPS_CD(ref='1')*/ Smoking(ref='Current') 
/*BMI_Category(ref='2')*/;

model FU_Duration_Death*Cancer_Death(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result /*SMK_STAT_TYPE_RSPS_CD*/
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP Smoking /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes Smoking|Category;

HAZARDRATIO  Category/diff=ref at (Smoking=all);

contrast 'Under vs Moderate at Current' Category 0 0 1 Smoking*Category 0/ estimate=exp;
contrast 'Over vs Moderate at Current' Category 0 1 Smoking*Category 0 / estimate=exp;
contrast 'Obesity vs Moderate at Current' Category 1 Smoking*Category 0 / estimate=exp;
contrast 'Under vs Moderate at Not Current' Category 0 0 1 Smoking*Category 0 0 1 / estimate=exp;
contrast 'Over vs Moderate at Not Current' Category 0 1 Smoking*Category 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at Not Current' Category 1 Smoking*Category 1/ estimate=exp;

RUN;


/* Cardiovascular Related Death */
proc phreg data=data3 covs(aggregate);
class ctrb_pt_type_cd(ref='0') /*olig_prote_cd(ref='1')*/ FMLY_Heart_disease(ref='0') FMLY_Diabetes(ref='0') 
FMLY_Cancer(ref='0') MI_case(REF='0') Heart_failure_case(ref ='0') Cancer_case(ref='0')
Hypertension_case(ref='0') TIA_Stroke_case(REF='0') Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0') /*SMK_STAT_TYPE_RSPS_CD(ref='1')*/ Smoking(ref='Current') 
/*BMI_Category(ref='2')*/;

model FU_Duration_Death*Cardio_Death(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result /*SMK_STAT_TYPE_RSPS_CD*/
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP Smoking /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes Smoking|Category;

HAZARDRATIO  Category/diff=ref at (Smoking=all);

contrast 'Under vs Moderate at Current' Category 0 0 1 Smoking*Category 0/ estimate=exp;
contrast 'Over vs Moderate at Current' Category 0 1 Smoking*Category 0 / estimate=exp;
contrast 'Obesity vs Moderate at Current' Category 1 Smoking*Category 0 / estimate=exp;
contrast 'Under vs Moderate at Not Current' Category 0 0 1 Smoking*Category 0 0 1 / estimate=exp;
contrast 'Over vs Moderate at Not Current' Category 0 1 Smoking*Category 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at Not Current' Category 1 Smoking*Category 1/ estimate=exp;

RUN;


/* Other Death */
proc phreg data=data3 covs(aggregate);
class ctrb_pt_type_cd(ref='0') /*olig_prote_cd(ref='1')*/ FMLY_Heart_disease(ref='0') FMLY_Diabetes(ref='0') 
FMLY_Cancer(ref='0') MI_case(REF='0') Heart_failure_case(ref ='0') Cancer_case(ref='0')
Hypertension_case(ref='0') TIA_Stroke_case(REF='0') Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0') /*SMK_STAT_TYPE_RSPS_CD(ref='1')*/ Smoking(ref='Current') 
/*BMI_Category(ref='2')*/;

model FU_Duration_Death*Other_Death(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result /*SMK_STAT_TYPE_RSPS_CD*/
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP Smoking /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes Smoking|Category;

HAZARDRATIO  Category/diff=ref at (Smoking=all);

contrast 'Under vs Moderate at Current' Category 0 0 1 Smoking*Category 0/ estimate=exp;
contrast 'Over vs Moderate at Current' Category 0 1 Smoking*Category 0 / estimate=exp;
contrast 'Obesity vs Moderate at Current' Category 1 Smoking*Category 0 / estimate=exp;
contrast 'Under vs Moderate at Not Current' Category 0 0 1 Smoking*Category 0 0 1 / estimate=exp;
contrast 'Over vs Moderate at Not Current' Category 0 1 Smoking*Category 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at Not Current' Category 1 Smoking*Category 1/ estimate=exp;

RUN;
