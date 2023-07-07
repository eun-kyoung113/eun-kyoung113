/*** Make Table 1 ***/

libname dir1 "/userdata12/room111/data_out/data_store";
run;

proc sort data=dir1.Mydata;
BY Category;
RUN;

proc means data=dir1.Mydata;
VAR Cohort_entry_Duration BMI AGE BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP CUR_SMK_TERM
CUR_DSQTY CUR_PACK_YEAR PAST_SMK_TERM PAST_PACK_YEAR PAST_DSQTY DRNK_FREQUENCY_HABIT
EXERCI_HABIT METS_minutes;
BY Category;
RUN;

proc freq data=dir1.Mydata;
tables Death * Category;
RUN;

proc freq data=dir1.Mydata;
tables MI * Category;
RUN;

proc freq data=dir1.Mydata;
tables Cancer * Category;
RUN;

proc freq data=dir1.Mydata;
tables Heart_failure * Category;
RUN;

proc freq data=dir1.Mydata;
tables TIA_Stroke * Category;
RUN;
/*********/
proc freq data=dir1.Mydata;
tables Outcome_Category * Category;
RUN;

proc freq data=dir1.Mydata;
tables SEX * Category;
RUN;

proc freq data=dir1.Mydata;
tables CTRB_PT_TYPE_CD * Category;
RUN;

proc freq data=dir1.Mydata;
tables SMK_STAT_TYPE_RSPS_CD * Category;
RUN;

proc freq data=dir1.Mydata;
tables FMLY_Heart_disease * Category;
RUN;

proc freq data=dir1.Mydata;
tables FMLY_Diabetes * Category;
RUN;

proc freq data=dir1.Mydata;
tables FMLY_Cancer * Category;
RUN;

proc freq data=dir1.Mydata;
tables MI_case * Category;
RUN;

proc freq data=dir1.Mydata;
tables Heart_failure_case * Category;
RUN;

proc freq data=dir1.Mydata;
tables Cancer_case * Category;
RUN;

proc freq data=dir1.Mydata;
tables Hypertension_case * Category;
RUN;

proc freq data=dir1.Mydata;
tables TIA_Stroke_case * Category;
RUN;

proc freq data=dir1.Mydata;
tables Asthma_result * Category;
RUN;

proc freq data=dir1.Mydata;
tables COPD_result * Category;
RUN;

proc freq data=dir1.Mydata;
tables Atrial_fibrillation_result * Category;
RUN;

proc freq data=dir1.Mydata;
tables Thromboembolism_result * Category;
RUN;

proc freq data=dir1.Mydata;
tables CLD_result * Category;
RUN;

proc freq data=dir1.Mydata;
tables CKD_result * Category;
RUN;

proc freq data=dir1.Mydata;
tables CAD_result * Category;
RUN;

proc freq data=dir1.Mydata;
tables PVD_result * Category;
RUN;

proc freq data=dir1.Mydata;
tables Dementia_result * Category;
RUN;

proc freq data=dir1.Mydata;
tables GI_Disorders_result * Category;
RUN;

proc freq data=dir1.Mydata;
tables Hyperlipidemia_result * Category;
RUN;

proc freq data=dir1.Mydata;
tables Pneumonia_result * Category;
RUN;

proc freq data=dir1.Mydata;
tables Psychiatric_disorders_result * Category;
RUN;

proc freq data=dir1.Mydata;
tables Hypertension_result * Category;
RUN;

proc freq data=dir1.Mydata;
tables insulin_result * Category;
RUN;

proc freq data=dir1.Mydata;
tables oral_diabetes_result * Category;
RUN;

proc freq data=dir1.Mydata;
tables Non_statin_result * Category;
RUN;

proc freq data=dir1.Mydata;
tables statin_result* Category;
RUN;

proc freq data=dir1.Mydata;
tables Hypertension_renin_result * Category;
RUN;

proc freq data=dir1.Mydata;
tables Hypertension_except_RAS_result * Category;
RUN;

proc freq data=dir1.Mydata;
tables CNS_result * Category;
RUN;

proc freq data=dir1.Mydata;
tables Non_opioid_result * Category;
RUN;

proc freq data=dir1.Mydata;
tables Anticoagulants_result * Category;
RUN;

proc freq data=dir1.Mydata;
tables Antiplatelets_result * Category;
RUN;

proc freq data=dir1.Mydata;
tables immunosuppressant_result * Category;
RUN;

proc freq data=dir1.Mydata;
tables Outcome * Category;
RUN;

proc freq data=dir1.Mydata;
table _numeric_;
run;

proc freq data=dir1.Mydata;
table SMK_STAT_TYPE_RSPS_CD;
RUN;
