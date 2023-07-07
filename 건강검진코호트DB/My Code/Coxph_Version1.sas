/* Coxph fitting Version 1*/
/* For Current Smoker */
libname dir1 "/userdata12/room111/data_out/data_store";

DATA data2;
SET dir1.Mydata;
IF SMK_STAT_TYPE_RSPS_CD='3';
RUN;

proc phreg data=data2;
class ctrb_pt_type_cd(ref='0') olig_prote_cd(ref='1') FMLY_Heart_disease(ref='0') FMLY_Diabetes(ref='0') 
FMLY_Cancer(ref='0') Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0');

model Outcome_Duration*Outcome(0) = Cohort_entry_Duration Category AGE SEX ctrb_pt_type_cd olig_prote_cd FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP CUR_SMK_TERM DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes;
        *PAST_SMK_TERM PAST_DSQTY PAST_PACK_YEAR 

*RANDOM PERSON_ID;
		
RUN;

/* Numeric BMI */
proc phreg data=data2;

*EFFECT BMI_Exposure = SPLINE(BMI / NATURALCUBIC knotmethod = LIST(18.5, 25, 30));

class ctrb_pt_type_cd(ref='0') olig_prote_cd(ref='1') FMLY_Heart_disease(ref='0') FMLY_Diabetes(ref='0') 
FMLY_Cancer(ref='0') Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') /*Category(ref='Underweighted')*/ GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0');

model Outcome_Duration*Outcome(0) = Cohort_entry_Duration BMI AGE SEX ctrb_pt_type_cd olig_prote_cd FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP CUR_SMK_TERM CUR_DSQTY CUR_PACK_YEAR DRNK_FREQUENCY_HABIT METS_minutes;
        *PAST_SMK_TERM PAST_DSQTY PAST_PACK_YEAR 

HAZARDRATIO BMI;

*RANDOM PERSON_ID;
		
RUN;

/* Numeric BMI */
proc phreg data=data2;
*EFFECT BMI_Exposure = SPLINE(BMI / NATURALCUBIC knotmethod = LIST(18.5, 25, 30));
class SEX(ref='1') ;
model Outcome_Duration*Outcome(0) = BMI AGE AGE*AGE SEX Cohort_entry_Duration;
RUN;

/* Categorical BMI */
proc phreg data=data2;
*EFFECT BMI_Exposure = SPLINE(BMI / NATURALCUBIC knotmethod = LIST(18.5, 25, 30));
class SEX(ref='1') Category(ref='Moderate');
model Outcome_Duration*Outcome(0) = Category AGE AGE*AGE SEX Cohort_entry_Duration;
RUN;

/*******************/
/* For Past Smoker */
DATA data3;
SET dir1.Mydata;
IF SMK_STAT_TYPE_RSPS_CD='2';
RUN;

proc phreg data=data3;
class ctrb_pt_type_cd(ref='0') olig_prote_cd(ref='1') FMLY_Heart_disease(ref='0') FMLY_Diabetes(ref='0') 
FMLY_Cancer(ref='0') Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Underweighted') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0');

model Outcome_Duration*Outcome(0) = Cohort_entry_Duration Category AGE SEX ctrb_pt_type_cd olig_prote_cd FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP CUR_SMK_TERM CUR_DSQTY CUR_PACK_YEAR DRNK_FREQUENCY_HABIT METS_minutes;
        *PAST_SMK_TERM PAST_DSQTY PAST_PACK_YEAR 

*RANDOM PERSON_ID;
		
RUN;

/* Numeric BMI */
proc phreg data=data3;

*EFFECT BMI_Exposure = SPLINE(BMI / NATURALCUBIC knotmethod = LIST(18.5, 25, 30));

class ctrb_pt_type_cd(ref='0') olig_prote_cd(ref='1') FMLY_Heart_disease(ref='0') FMLY_Diabetes(ref='0') 
FMLY_Cancer(ref='0') Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') /*Category(ref='Underweighted')*/ GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0');

model Outcome_Duration*Outcome(0) = Cohort_entry_Duration BMI AGE SEX ctrb_pt_type_cd olig_prote_cd FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP CUR_SMK_TERM CUR_DSQTY CUR_PACK_YEAR DRNK_FREQUENCY_HABIT METS_minutes;
        *PAST_SMK_TERM PAST_DSQTY PAST_PACK_YEAR 

HAZARDRATIO BMI;

*RANDOM PERSON_ID;
		
RUN;

/* Numeric BMI */
proc phreg data=data3;
*EFFECT BMI_Exposure = SPLINE(BMI / NATURALCUBIC knotmethod = LIST(18.5, 25, 30));
class SEX(ref='1') ;
model Outcome_Duration*Outcome(0) = BMI AGE AGE*AGE SEX Cohort_entry_Duration;
RUN;

/* Categorical BMI */
proc phreg data=data3;
*EFFECT BMI_Exposure = SPLINE(BMI / NATURALCUBIC knotmethod = LIST(18.5, 25, 30));
class SEX(ref='1') Category(ref='Underweighted');
model Outcome_Duration*Outcome(0) = Category AGE AGE*AGE SEX Cohort_entry_Duration;
RUN;


/*******************/
/* For Never Smoker */
DATA data4;
SET dir1.Mydata;
IF SMK_STAT_TYPE_RSPS_CD='1';
RUN;

proc phreg data=data4;
class ctrb_pt_type_cd(ref='0') olig_prote_cd(ref='1') FMLY_Heart_disease(ref='0') FMLY_Diabetes(ref='0') 
FMLY_Cancer(ref='0') Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0');

model Outcome_Duration*Outcome(0) = Cohort_entry_Duration Category AGE SEX ctrb_pt_type_cd olig_prote_cd FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP CUR_SMK_TERM DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes;
        *PAST_SMK_TERM PAST_DSQTY PAST_PACK_YEAR 

*RANDOM PERSON_ID;
		
RUN;

/* Numeric BMI */
proc phreg data=data4;

*EFFECT BMI_Exposure = SPLINE(BMI / NATURALCUBIC knotmethod = LIST(18.5, 25, 30));

class ctrb_pt_type_cd(ref='0') olig_prote_cd(ref='1') FMLY_Heart_disease(ref='0') FMLY_Diabetes(ref='0') 
FMLY_Cancer(ref='0') Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') /*Category(ref='Underweighted')*/ GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0');

model Outcome_Duration*Outcome(0) = Cohort_entry_Duration BMI AGE SEX ctrb_pt_type_cd olig_prote_cd FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP CUR_SMK_TERM CUR_DSQTY CUR_PACK_YEAR DRNK_FREQUENCY_HABIT METS_minutes;
        *PAST_SMK_TERM PAST_DSQTY PAST_PACK_YEAR 

HAZARDRATIO BMI;

*RANDOM PERSON_ID;
		
RUN;

/* Numeric BMI */
proc phreg data=data4;
*EFFECT BMI_Exposure = SPLINE(BMI / NATURALCUBIC knotmethod = LIST(18.5, 25, 30));
class SEX(ref='1') ;
model Outcome_Duration*Outcome(0) = BMI AGE AGE*AGE SEX Cohort_entry_Duration;
RUN;

/* Categorical BMI */
proc phreg data=data4;
*EFFECT BMI_Exposure = SPLINE(BMI / NATURALCUBIC knotmethod = LIST(18.5, 25, 30));
class SEX(ref='1') Category(ref='Moderate');
model Outcome_Duration*Outcome(0) = Category AGE AGE*AGE SEX Cohort_entry_Duration;
RUN;
