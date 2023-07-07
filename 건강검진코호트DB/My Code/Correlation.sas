libname dir1 "/userdata12/room111/data_out/data_store";
run;

proc corr data=dir1.Mydata;
var Asthma_result COPD_result Atrial_fibrillation_result Thromboembolism_result CLD_result
CKD_result CAD_result PVD_result Dementia_result GI_Disorders_result Hyperlipidemia_result
Pneumonia_result Psychiatric_disorders_result Hypertension_result insulin_result oral_diabetes_result
Non_statin_result statin_result Hypertension_renin_result Hypertension_except_RAS_result CNS_result
Non_opioid_result Anticoagulants_result Antiplatelets_result immunosuppressant_result;
run;

DATA cor_data;
SET dir1.Mydata;
SEX2=input(SEX,8.);
CTRB_PT_TYPE_CD2=input(CTRB_PT_TYPE_CD,8.);
OLIG_PROTE_CD2=input(OLIG_PROTE_CD,8.);
SMK_STAT_TYPE_RSPS_CD1=input(SMK_STAT_TYPE_RSPS_CD,8.);
RUN;

proc corr data=cor_data;
var Cohort_entry_Duration AGE SEX2 CTRB_PT_TYPE_CD2 OLIG_PROTE_CD2 SMK_STAT_TYPE_RSPS_CD1 FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer 
MI_case Heart_failure_case Cancer_case Hypertension_case TIA_Stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP CUR_SMK_TERM 
        PAST_SMK_TERM DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes;
run;
