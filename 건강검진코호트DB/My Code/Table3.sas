libname dir1 "/userdata12/room111/data_out/data_store";
run;

DATA data;
SET dir1.Mydata;
IF AGE<=65 THEN AGE_65=1;
ELSE AGE_65=0;
RUN;

/* Composite Outcome*/
PROC SORT DATA=data;
BY Category;
run;

proc freq data=data;
tables AGE_65 * Outcome * Category;
RUN;

proc freq data=data;
tables SEX * Outcome * Category;
RUN;

proc freq data=data;
tables Hyperlipidemia_result * Outcome * Category;
RUN;

proc freq data=data;
tables Hypertension_result * Outcome * Category;
RUN;

proc freq data=data;
tables SMK_STAT_TYPE_RSPS_CD * Outcome * Category;
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

model Outcome_Duration*Outcome(0) = Cohort_entry_Duration /*BMI_Category*/ AGE_65 Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes AGE_65|Category;

HAZARDRATIO  AGE_65/diff=ref at (Category=all);
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

model Outcome_Duration*Outcome(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
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

model Outcome_Duration*Outcome(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
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

model Outcome_Duration*Outcome(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
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
Hypertension_case(ref='0') TIA_Stroke_case(REF='0')Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0') SMK_STAT_TYPE_RSPS_CD(ref='1') 
/*BMI_Category(ref='2')*/;

model Outcome_Duration*Outcome(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes SMK_STAT_TYPE_RSPS_CD|Category;

HAZARDRATIO  Category/diff=ref at (SMK_STAT_TYPE_RSPS_CD=all);

contrast 'Under vs Moderate at Never' Category 0 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0/ estimate=exp;
contrast 'Over vs Moderate at Never' Category 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 / estimate=exp;
contrast 'Obesity vs Moderate at Never' Category 1 SMK_STAT_TYPE_RSPS_CD*Category 0 / estimate=exp;
contrast 'Under vs Moderate at Past' Category 0 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 0 0 1 / estimate=exp;
contrast 'Over vs Moderate at Past' Category 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at Past' Category 1 SMK_STAT_TYPE_RSPS_CD*Category 1/ estimate=exp;
contrast 'Under vs Moderate at Current' Category 0 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 0 0 0 1 / estimate=exp;
contrast 'Over vs Moderate at Current' Category 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at Current' Category 1 SMK_STAT_TYPE_RSPS_CD*Category 0 1/ estimate=exp;

RUN;



/***************/
/* Death*/
PROC SORT DATA=data;
BY Category;
run;

proc freq data=data;
tables AGE_65 * Death * Category;
RUN;

proc freq data=data;
tables SEX * Death * Category;
RUN;

proc freq data=data;
tables Hyperlipidemia_result * Death * Category;
RUN;

proc freq data=data;
tables Hypertension_result * Death * Category;
RUN;

proc freq data=data;
tables SMK_STAT_TYPE_RSPS_CD * Death * Category;
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

model FU_Duration_Death*Death(0) = Cohort_entry_Duration /*BMI_Category*/ AGE_65 Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes AGE_65|Category;

HAZARDRATIO  AGE_65/diff=ref at (Category=all);
HAZARDRATIO  Category/diff=ref at (AGE_65=all);
*HAZARDRATIO AGE_65*Category / cl=both; 
/*RANDOM PERSON_ID;*/
		
contrast 'Under vs Moderate at AGE <=65(1)' Category 0 0 1 AGE_65*Category 0 0 1/ estimate=exp;
contrast 'Over vs Moderate at AGE <=65(1)' Category 0 1 AGE_65*Category 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at AGE <=65(1)' Category 1 AGE_65*Category 1 / estimate=exp;
contrast 'Under vs Moderate at AGE >65(0)' Category 0 0 1 AGE_65*Category 0 / estimate=exp;
contrast 'Over vs Moderate at AGE >65(0)' Category 0 1 AGE_65*Category 0 / estimate=exp;
contrast 'Obesity vs Moderate at AGE >65(0)' Category 1 AGE_65*Category 0/ estimate=exp;
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
statin_result(ref='0')  Thromboembolism_result(ref='0') SMK_STAT_TYPE_RSPS_CD(ref='1') 
/*BMI_Category(ref='2')*/;

model FU_Duration_Death*Death(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes SEX|Category;

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
Hypertension_case(ref='0') TIA_Stroke_case(REF='0') Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0') SMK_STAT_TYPE_RSPS_CD(ref='1') 
/*BMI_Category(ref='2')*/;

model FU_Duration_Death*Death(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes Hyperlipidemia_result|Category;

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

model FU_Duration_Death*Death(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes Hypertension_result|Category;


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
Hypertension_case(ref='0') TIA_Stroke_case(REF='0')Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0') SMK_STAT_TYPE_RSPS_CD(ref='1') 
/*BMI_Category(ref='2')*/;

model FU_Duration_Death*Death(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes SMK_STAT_TYPE_RSPS_CD|Category;

HAZARDRATIO  Category/diff=ref at (SMK_STAT_TYPE_RSPS_CD=all);

contrast 'Under vs Moderate at Never' Category 0 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0/ estimate=exp;
contrast 'Over vs Moderate at Never' Category 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 / estimate=exp;
contrast 'Obesity vs Moderate at Never' Category 1 SMK_STAT_TYPE_RSPS_CD*Category 0 / estimate=exp;
contrast 'Under vs Moderate at Past' Category 0 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 0 0 1 / estimate=exp;
contrast 'Over vs Moderate at Past' Category 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at Past' Category 1 SMK_STAT_TYPE_RSPS_CD*Category 1/ estimate=exp;
contrast 'Under vs Moderate at Current' Category 0 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 0 0 0 1 / estimate=exp;
contrast 'Over vs Moderate at Current' Category 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at Current' Category 1 SMK_STAT_TYPE_RSPS_CD*Category 0 1/ estimate=exp;

RUN;


/***************/
/***************/
/***************/
/* MI*/
PROC SORT DATA=data;
BY Category;
run;

proc freq data=data;
tables AGE_65 * MI * Category;
RUN;

proc freq data=data;
tables SEX * MI * Category;
RUN;

proc freq data=data;
tables Hyperlipidemia_result * MI * Category;
RUN;

proc freq data=data;
tables Hypertension_result * MI * Category;
RUN;

proc freq data=data;
tables SMK_STAT_TYPE_RSPS_CD * MI * Category;
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

model MI_Duration*MI(0) = Cohort_entry_Duration /*BMI_Category*/ AGE_65 Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes AGE_65|Category;

HAZARDRATIO  AGE_65/diff=ref at (Category=all);
HAZARDRATIO  Category/diff=ref at (AGE_65=all);
*HAZARDRATIO AGE_65*Category / cl=both; 
/*RANDOM PERSON_ID;*/
		
contrast 'Under vs Moderate at AGE <=65(1)' Category 0 0 1 AGE_65*Category 0 0 1/ estimate=exp;
contrast 'Over vs Moderate at AGE <=65(1)' Category 0 1 AGE_65*Category 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at AGE <=65(1)' Category 1 AGE_65*Category 1 / estimate=exp;
contrast 'Under vs Moderate at AGE >65(0)' Category 0 0 1 AGE_65*Category 0 / estimate=exp;
contrast 'Over vs Moderate at AGE >65(0)' Category 0 1 AGE_65*Category 0 / estimate=exp;
contrast 'Obesity vs Moderate at AGE >65(0)' Category 1 AGE_65*Category 0/ estimate=exp;
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
statin_result(ref='0')  Thromboembolism_result(ref='0') SMK_STAT_TYPE_RSPS_CD(ref='1') 
/*BMI_Category(ref='2')*/;

model MI_Duration*MI(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes SEX|Category;

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
Hypertension_case(ref='0') TIA_Stroke_case(REF='0') Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0') SMK_STAT_TYPE_RSPS_CD(ref='1') 
/*BMI_Category(ref='2')*/;

model MI_Duration*MI(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes Hyperlipidemia_result|Category;

HAZARDRATIO  Hyperlipidemia_result /diff=ref at (Category=all);
HAZARDRATIO  Category/diff=ref at (Hyperlipidemia_result=all);
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

model MI_Duration*MI(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes Hypertension_result|Category;


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
Hypertension_case(ref='0') TIA_Stroke_case(REF='0')Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0') SMK_STAT_TYPE_RSPS_CD(ref='1') 
/*BMI_Category(ref='2')*/;

model MI_Duration*MI(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes SMK_STAT_TYPE_RSPS_CD|Category;

HAZARDRATIO  Category/diff=ref at (SMK_STAT_TYPE_RSPS_CD=all);

contrast 'Under vs Moderate at Never' Category 0 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0/ estimate=exp;
contrast 'Over vs Moderate at Never' Category 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 / estimate=exp;
contrast 'Obesity vs Moderate at Never' Category 1 SMK_STAT_TYPE_RSPS_CD*Category 0 / estimate=exp;
contrast 'Under vs Moderate at Past' Category 0 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 0 0 1 / estimate=exp;
contrast 'Over vs Moderate at Past' Category 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at Past' Category 1 SMK_STAT_TYPE_RSPS_CD*Category 1/ estimate=exp;
contrast 'Under vs Moderate at Current' Category 0 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 0 0 0 1 / estimate=exp;
contrast 'Over vs Moderate at Current' Category 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at Current' Category 1 SMK_STAT_TYPE_RSPS_CD*Category 0 1/ estimate=exp;

RUN;


/***************/
/***************/
/***************/
/* Heart failure*/
PROC SORT DATA=data;
BY Category;
run;

proc freq data=data;
tables AGE_65 * Heart_failure * Category;
RUN;

proc freq data=data;
tables SEX * Heart_failure * Category;
RUN;

proc freq data=data;
tables Hyperlipidemia_result * Heart_failure * Category;
RUN;

proc freq data=data;
tables Hypertension_result * Heart_failure * Category;
RUN;

proc freq data=data;
tables SMK_STAT_TYPE_RSPS_CD * Heart_failure * Category;
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

model HF_Duration*Heart_failure(0) = Cohort_entry_Duration /*BMI_Category*/ AGE_65 Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes AGE_65|Category;

HAZARDRATIO  AGE_65/diff=ref at (Category=all);
HAZARDRATIO  Category/diff=ref at (AGE_65=all);
*HAZARDRATIO AGE_65*Category / cl=both; 
/*RANDOM PERSON_ID;*/
		
contrast 'Under vs Moderate at AGE <=65(1)' Category 0 0 1 AGE_65*Category 0 0 1/ estimate=exp;
contrast 'Over vs Moderate at AGE <=65(1)' Category 0 1 AGE_65*Category 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at AGE <=65(1)' Category 1 AGE_65*Category 1 / estimate=exp;
contrast 'Under vs Moderate at AGE >65(0)' Category 0 0 1 AGE_65*Category 0 / estimate=exp;
contrast 'Over vs Moderate at AGE >65(0)' Category 0 1 AGE_65*Category 0 / estimate=exp;
contrast 'Obesity vs Moderate at AGE >65(0)' Category 1 AGE_65*Category 0/ estimate=exp;
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
statin_result(ref='0')  Thromboembolism_result(ref='0') SMK_STAT_TYPE_RSPS_CD(ref='1') 
/*BMI_Category(ref='2')*/;

model HF_Duration*Heart_failure(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes SEX|Category;

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
Hypertension_case(ref='0') TIA_Stroke_case(REF='0') Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0') SMK_STAT_TYPE_RSPS_CD(ref='1') 
/*BMI_Category(ref='2')*/;

model HF_Duration*Heart_failure(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes Hyperlipidemia_result|Category;

HAZARDRATIO  Hyperlipidemia_result /diff=ref at (Category=all);
HAZARDRATIO  Category/diff=ref at (Hyperlipidemia_result=all);
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

model HF_Duration*Heart_failure(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes Hypertension_result|Category;


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
Hypertension_case(ref='0') TIA_Stroke_case(REF='0')Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0') SMK_STAT_TYPE_RSPS_CD(ref='1') 
/*BMI_Category(ref='2')*/;

model HF_Duration*Heart_failure(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes SMK_STAT_TYPE_RSPS_CD|Category;

HAZARDRATIO  Category/diff=ref at (SMK_STAT_TYPE_RSPS_CD=all);

contrast 'Under vs Moderate at Never' Category 0 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0/ estimate=exp;
contrast 'Over vs Moderate at Never' Category 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 / estimate=exp;
contrast 'Obesity vs Moderate at Never' Category 1 SMK_STAT_TYPE_RSPS_CD*Category 0 / estimate=exp;
contrast 'Under vs Moderate at Past' Category 0 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 0 0 1 / estimate=exp;
contrast 'Over vs Moderate at Past' Category 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at Past' Category 1 SMK_STAT_TYPE_RSPS_CD*Category 1/ estimate=exp;
contrast 'Under vs Moderate at Current' Category 0 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 0 0 0 1 / estimate=exp;
contrast 'Over vs Moderate at Current' Category 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at Current' Category 1 SMK_STAT_TYPE_RSPS_CD*Category 0 1/ estimate=exp;

RUN;


/*******************/
/*******************/
/*******************/
/* TIA or Stroke*/
PROC SORT DATA=data;
BY Category;
run;

proc freq data=data;
tables AGE_65 * TIA_Stroke * Category;
RUN;

proc freq data=data;
tables SEX * TIA_Stroke * Category;
RUN;

proc freq data=data;
tables Hyperlipidemia_result * TIA_Stroke * Category;
RUN;

proc freq data=data;
tables Hypertension_result * TIA_Stroke * Category;
RUN;

proc freq data=data;
tables SMK_STAT_TYPE_RSPS_CD * TIA_Stroke * Category;
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

model TIA_Duration*TIA_Stroke(0) = Cohort_entry_Duration /*BMI_Category*/ AGE_65 Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes AGE_65|Category;

HAZARDRATIO  AGE_65/diff=ref at (Category=all);
HAZARDRATIO  Category/diff=ref at (AGE_65=all);
*HAZARDRATIO AGE_65*Category / cl=both; 
/*RANDOM PERSON_ID;*/
		
contrast 'Under vs Moderate at AGE <=65(1)' Category 0 0 1 AGE_65*Category 0 0 1/ estimate=exp;
contrast 'Over vs Moderate at AGE <=65(1)' Category 0 1 AGE_65*Category 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at AGE <=65(1)' Category 1 AGE_65*Category 1 / estimate=exp;
contrast 'Under vs Moderate at AGE >65(0)' Category 0 0 1 AGE_65*Category 0 / estimate=exp;
contrast 'Over vs Moderate at AGE >65(0)' Category 0 1 AGE_65*Category 0 / estimate=exp;
contrast 'Obesity vs Moderate at AGE >65(0)' Category 1 AGE_65*Category 0/ estimate=exp;
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
statin_result(ref='0')  Thromboembolism_result(ref='0') SMK_STAT_TYPE_RSPS_CD(ref='1') 
/*BMI_Category(ref='2')*/;

model TIA_Duration*TIA_Stroke(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes SEX|Category;

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
Hypertension_case(ref='0') TIA_Stroke_case(REF='0') Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0') SMK_STAT_TYPE_RSPS_CD(ref='1') 
/*BMI_Category(ref='2')*/;

model TIA_Duration*TIA_Stroke(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes Hyperlipidemia_result|Category;

HAZARDRATIO  Hyperlipidemia_result /diff=ref at (Category=all);
HAZARDRATIO  Category/diff=ref at (Hyperlipidemia_result=all);
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

model TIA_Duration*TIA_Stroke(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes Hypertension_result|Category;


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
Hypertension_case(ref='0') TIA_Stroke_case(REF='0')Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0') SMK_STAT_TYPE_RSPS_CD(ref='1') 
/*BMI_Category(ref='2')*/;

model TIA_Duration*TIA_Stroke(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes SMK_STAT_TYPE_RSPS_CD|Category;

HAZARDRATIO  Category/diff=ref at (SMK_STAT_TYPE_RSPS_CD=all);

contrast 'Under vs Moderate at Never' Category 0 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0/ estimate=exp;
contrast 'Over vs Moderate at Never' Category 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 / estimate=exp;
contrast 'Obesity vs Moderate at Never' Category 1 SMK_STAT_TYPE_RSPS_CD*Category 0 / estimate=exp;
contrast 'Under vs Moderate at Past' Category 0 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 0 0 1 / estimate=exp;
contrast 'Over vs Moderate at Past' Category 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at Past' Category 1 SMK_STAT_TYPE_RSPS_CD*Category 1/ estimate=exp;
contrast 'Under vs Moderate at Current' Category 0 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 0 0 0 1 / estimate=exp;
contrast 'Over vs Moderate at Current' Category 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at Current' Category 1 SMK_STAT_TYPE_RSPS_CD*Category 0 1/ estimate=exp;

RUN;


/*******************/
/*******************/
/*******************/
/* Cancer*/
PROC SORT DATA=data;
BY Category;
run;

proc freq data=data;
tables AGE_65 * Cancer * Category;
RUN;

proc freq data=data;
tables SEX * Cancer * Category;
RUN;

proc freq data=data;
tables Hyperlipidemia_result * Cancer * Category;
RUN;

proc freq data=data;
tables Hypertension_result * Cancer * Category;
RUN;

proc freq data=data;
tables SMK_STAT_TYPE_RSPS_CD * Cancer * Category;
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

model Cancer_Duration*Cancer(0) = Cohort_entry_Duration /*BMI_Category*/ AGE_65 Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes AGE_65|Category;

HAZARDRATIO  AGE_65/diff=ref at (Category=all);
HAZARDRATIO  Category/diff=ref at (AGE_65=all);
*HAZARDRATIO AGE_65*Category / cl=both; 
/*RANDOM PERSON_ID;*/
		
contrast 'Under vs Moderate at AGE <=65(1)' Category 0 0 1 AGE_65*Category 0 0 1/ estimate=exp;
contrast 'Over vs Moderate at AGE <=65(1)' Category 0 1 AGE_65*Category 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at AGE <=65(1)' Category 1 AGE_65*Category 1 / estimate=exp;
contrast 'Under vs Moderate at AGE >65(0)' Category 0 0 1 AGE_65*Category 0 / estimate=exp;
contrast 'Over vs Moderate at AGE >65(0)' Category 0 1 AGE_65*Category 0 / estimate=exp;
contrast 'Obesity vs Moderate at AGE >65(0)' Category 1 AGE_65*Category 0/ estimate=exp;
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
statin_result(ref='0')  Thromboembolism_result(ref='0') SMK_STAT_TYPE_RSPS_CD(ref='1') 
/*BMI_Category(ref='2')*/;

model Cancer_Duration*Cancer(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes SEX|Category;

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
Hypertension_case(ref='0') TIA_Stroke_case(REF='0') Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0') SMK_STAT_TYPE_RSPS_CD(ref='1') 
/*BMI_Category(ref='2')*/;

model Cancer_Duration*Cancer(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes Hyperlipidemia_result|Category;

HAZARDRATIO  Hyperlipidemia_result /diff=ref at (Category=all);
HAZARDRATIO  Category/diff=ref at (Hyperlipidemia_result=all);
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

model Cancer_Duration*Cancer(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes Hypertension_result|Category;


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
Hypertension_case(ref='0') TIA_Stroke_case(REF='0')Anticoagulants_result(ref='0') Antiplatelets_result(ref='0') Asthma_result(ref='0')  
Atrial_fibrillation_result(ref='0') CAD_result(ref='0') CKD_result(ref='0') CLD_result(ref='0') CNS_result(ref='0') 
COPD_result(ref='0') Dementia_result(ref='0') Category(ref='Moderate') GI_Disorders_result(ref='0') 
Hyperlipidemia_result(ref='0') Hypertension_except_RAS_result(ref='0') Hypertension_renin_result(ref='0') 
Hypertension_result(ref='0') immunosuppressant_result(ref='0') insulin_result(ref='0') Non_opioid_result(ref='0')  
Non_statin_result(ref='0')  oral_diabetes_result(ref='0') SEX(ref='1') 
Pneumonia_result(ref='0')  Psychiatric_disorders_result(ref='0')  PVD_result(ref='0')  
statin_result(ref='0')  Thromboembolism_result(ref='0') SMK_STAT_TYPE_RSPS_CD(ref='1') 
/*BMI_Category(ref='2')*/;

model Cancer_Duration*Cancer(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes SMK_STAT_TYPE_RSPS_CD|Category;

HAZARDRATIO  Category/diff=ref at (SMK_STAT_TYPE_RSPS_CD=all);

contrast 'Under vs Moderate at Never' Category 0 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0/ estimate=exp;
contrast 'Over vs Moderate at Never' Category 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 / estimate=exp;
contrast 'Obesity vs Moderate at Never' Category 1 SMK_STAT_TYPE_RSPS_CD*Category 0 / estimate=exp;
contrast 'Under vs Moderate at Past' Category 0 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 0 0 1 / estimate=exp;
contrast 'Over vs Moderate at Past' Category 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at Past' Category 1 SMK_STAT_TYPE_RSPS_CD*Category 1/ estimate=exp;
contrast 'Under vs Moderate at Current' Category 0 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 0 0 0 1 / estimate=exp;
contrast 'Over vs Moderate at Current' Category 0 1 SMK_STAT_TYPE_RSPS_CD*Category 0 0 0 1 / estimate=exp;
contrast 'Obesity vs Moderate at Current' Category 1 SMK_STAT_TYPE_RSPS_CD*Category 0 1/ estimate=exp;

RUN;


/**********************************/
/**** SMK_STAT_TYPE_RSPS_CD ****/
DATA data;
SET dir1.Mydata;
IF MISSING(SMK_STAT_TYPE_RSPS_CD) or SMK_STAT_TYPE_RSPS_CD='.' 
then SMK_STAT_TYPE_RSPS_CD='0';
RUN;

proc sort data=data;
BY Category;

proc freq data=data;
tables SMK_STAT_TYPE_RSPS_CD * Death * Category;
RUN;

/*Smoking status*/
/* 4 Category */
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

model FU_Duration_Death*Death(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
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

/* 2 Category : Never VS Ever */
DATA data2;
SET dir1.Mydata;
IF SMK_STAT_TYPE_RSPS_CD ^='0';
IF SMK_STAT_TYPE_RSPS_CD='1' THEN Smoking='Never';
ELSE Smoking='Ever';
RUN;

proc sort data=data2;
BY Category;

proc freq data=data2;
tables Smoking * Death * Category;
RUN;

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

model FU_Duration_Death*Death(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
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
SET dir1.Mydata;
IF SMK_STAT_TYPE_RSPS_CD ^='0';
IF SMK_STAT_TYPE_RSPS_CD='3' THEN Smoking='Current';
ELSE Smoking='Not_Current';
RUN;

proc sort data=data3;
BY Category;

proc freq data=data3;
tables Smoking * Death * Category;
RUN;

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

model FU_Duration_Death*Death(0) = Cohort_entry_Duration /*BMI_Category*/ Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
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



 
