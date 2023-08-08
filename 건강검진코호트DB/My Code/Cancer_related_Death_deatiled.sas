/***** Cancer Related Death - Deatiled *****/
libname dir1 "/userdata12/room111/data_out/data_store";
run;

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
case when ((DTH_CODE1 like "C00%") or (DTH_CODE1 like "C01%") or (DTH_CODE1 like "C02%") or (DTH_CODE1 like "C03%")
or (DTH_CODE1 like "C04%") or (DTH_CODE1 like "C05%") or (DTH_CODE1 like "C06%") or (DTH_CODE1 like "C07%") or (DTH_CODE1 like "C08%")
or (DTH_CODE1 like "C09%") or (DTH_CODE1 like "C10%") or (DTH_CODE1 like "C11%") or (DTH_CODE1 like "C12%") or (DTH_CODE1 like "C13%")
or (DTH_CODE1 like "C14%") or (DTH_CODE1 like "C30%") or (DTH_CODE1 like "C31%") or (DTH_CODE1 like "C32%") 
or (DTH_CODE1 like "C33%") or (DTH_CODE1 like "C69%")) or 
((DTH_CODE2 like "C00%") or (DTH_CODE2 like "C01%") or (DTH_CODE2 like "C02%") or (DTH_CODE2 like "C03%")
or (DTH_CODE2 like "C04%") or (DTH_CODE2 like "C05%") or (DTH_CODE2 like "C06%") or (DTH_CODE2 like "C07%") or (DTH_CODE2 like "C08%")
or (DTH_CODE2 like "C09%") or (DTH_CODE2 like "C10%") or (DTH_CODE2 like "C11%") or (DTH_CODE2 like "C12%") or (DTH_CODE2 like "C13%")
or (DTH_CODE2 like "C14%") or (DTH_CODE2 like "C30%") or (DTH_CODE2 like "C31%") or (DTH_CODE2 like "C32%") 
or (DTH_CODE2 like "C33%") or (DTH_CODE2 like "C69%")) then 1 else 0 end as Head_neck_Cancer,

case when ((DTH_CODE1 like "C15%") or (DTH_CODE1 like "C16%") or (DTH_CODE1 like "C17%") or (DTH_CODE1 like "C18%")
or (DTH_CODE1 like "C19%") or (DTH_CODE1 like "C20%") or (DTH_CODE1 like "C21%") or (DTH_CODE1 like "C22%") or (DTH_CODE1 like "C23%")
or (DTH_CODE1 like "C24%") or (DTH_CODE1 like "C25%") or (DTH_CODE1 like "C26%")) or
((DTH_CODE2 like "C15%") or (DTH_CODE2 like "C16%") or (DTH_CODE2 like "C17%") or (DTH_CODE2 like "C18%")
or (DTH_CODE2 like "C19%") or (DTH_CODE2 like "C20%") or (DTH_CODE2 like "C21%") or (DTH_CODE2 like "C22%") or (DTH_CODE2 like "C23%")
or (DTH_CODE2 like "C24%") or (DTH_CODE2 like "C25%") or (DTH_CODE2 like "C26%")) then 1 else 0 end as Digestive_Cancer,

case when ((DTH_CODE1 like "C34%") or (DTH_CODE1 like "C35%") or (DTH_CODE1 like "C36%") or (DTH_CODE1 like "C37%")
or (DTH_CODE1 like "C38%") or (DTH_CODE1 like "C39%") or (DTH_CODE1 like "C45%")) or 
((DTH_CODE2 like "C34%") or (DTH_CODE2 like "C35%") or (DTH_CODE2 like "C36%") or (DTH_CODE2 like "C37%")
or (DTH_CODE2 like "C38%") or (DTH_CODE2 like "C39%") or (DTH_CODE2 like "C45%")) then 1 else 0 end as Intrac_Cancer,

case when ((DTH_CODE1 like "C40%") or (DTH_CODE1 like "C41%") or (DTH_CODE1 like "C46%") or (DTH_CODE1 like "C49%")) or 
((DTH_CODE2 like "C40%") or (DTH_CODE2 like "C41%") or (DTH_CODE2 like "C46%") or (DTH_CODE2 like "C49%")) then 1 else 0 end as Bone_Cancer,

case when ((DTH_CODE1 like "C43%") or (DTH_CODE1 like "C44%")) or ((DTH_CODE2 like "C43%") or (DTH_CODE2 like "C44%")) 
then 1 else 0 end as Skin_Cancer,

case when (DTH_CODE1 like "C50%") or (DTH_CODE2 like "C50%") then 1 else 0 end as Breast_Cancer,

case when ((DTH_CODE1 like "C51%") or (DTH_CODE1 like "C52%") or (DTH_CODE1 like "C53%") or (DTH_CODE1 like "C54%")
or (DTH_CODE1 like "C55%") or (DTH_CODE1 like "C56%") or (DTH_CODE1 like "C57%") or (DTH_CODE1 like "C58%")) or
((DTH_CODE2 like "C51%") or (DTH_CODE2 like "C52%") or (DTH_CODE2 like "C53%") or (DTH_CODE2 like "C54%")
or (DTH_CODE2 like "C55%") or (DTH_CODE2 like "C56%") or (DTH_CODE2 like "C57%") or (DTH_CODE2 like "C58%")) then 1 else 0 end as Female_Cancer,

case when ((DTH_CODE1 like "C60%") or (DTH_CODE1 like "C61%") or (DTH_CODE1 like "C62%") or (DTH_CODE1 like "C63%")) or 
((DTH_CODE2 like "C60%") or (DTH_CODE2 like "C61%") or (DTH_CODE2 like "C62%") or (DTH_CODE2 like "C63%")) then 1 else 0 end as Male_Cancer,

case when ((DTH_CODE1 like "C64%") or (DTH_CODE1 like "C65%") or (DTH_CODE1 like "C66%") or (DTH_CODE1 like "C67%") or (DTH_CODE1 like "C68%")) or 
((DTH_CODE2 like "C64%") or (DTH_CODE2 like "C65%") or (DTH_CODE2 like "C66%") or (DTH_CODE2 like "C67%") or (DTH_CODE2 like "C68%")) 
then 1 else 0 end as Urinary_Cancer,

case when ((DTH_CODE1 like "C70%") or (DTH_CODE1 like "C71%") or (DTH_CODE1 like "C72%") or (DTH_CODE1 like "C47%")) or 
((DTH_CODE2 like "C70%") or (DTH_CODE2 like "C71%") or (DTH_CODE2 like "C72%") or (DTH_CODE2 like "C47%")) then 1 else 0 end as Nervous_Cancer,

case when ((DTH_CODE1 like "C73%") or (DTH_CODE1 like "C74%") or (DTH_CODE1 like "C75%")) or 
((DTH_CODE2 like "C73%") or (DTH_CODE2 like "C74%") or (DTH_CODE2 like "C75%")) then 1 else 0 end as Endocrine_Cancer,

case when ((DTH_CODE1 like "C81%") or (DTH_CODE1 like "C82%") or (DTH_CODE1 like "C83%") or (DTH_CODE1 like "C84%")
or (DTH_CODE1 like "C85%") or (DTH_CODE1 like "C86%") or (DTH_CODE1 like "C87%") or (DTH_CODE1 like "C88%") or (DTH_CODE1 like "C89%")
or (DTH_CODE1 like "C90%") or (DTH_CODE1 like "C91%") or (DTH_CODE1 like "C92%") or (DTH_CODE1 like "C93%") or (DTH_CODE1 like "C94%")
or (DTH_CODE1 like "C95%") or (DTH_CODE1 like "C96%")) or 
((DTH_CODE2 like "C81%") or (DTH_CODE2 like "C82%") or (DTH_CODE2 like "C83%") or (DTH_CODE2 like "C84%")
or (DTH_CODE2 like "C85%") or (DTH_CODE2 like "C86%") or (DTH_CODE2 like "C87%") or (DTH_CODE2 like "C88%") or (DTH_CODE2 like "C89%")
or (DTH_CODE2 like "C90%") or (DTH_CODE2 like "C91%") or (DTH_CODE2 like "C92%") or (DTH_CODE2 like "C93%") or (DTH_CODE2 like "C94%")
or (DTH_CODE2 like "C95%") or (DTH_CODE2 like "C96%")) then 1 else 0 end as Hematologic_Cancer,

case when ((DTH_CODE1 like "C76%") or (DTH_CODE1 like "C77%") or (DTH_CODE1 like "C78%") or (DTH_CODE1 like "C79%") 
or (DTH_CODE1 like "C80%") or (DTH_CODE1 like "C97%")) or 
((DTH_CODE2 like "C76%") or (DTH_CODE2 like "C77%") or (DTH_CODE2 like "C78%") or (DTH_CODE2 like "C79%") 
or (DTH_CODE2 like "C80%") or (DTH_CODE2 like "C97%")) 
then 1 else 0 end as Other_Cancer

from death_check3;
quit;

DATA dir1.Cancer_Death_detail;
SET check_outcome;
DROP DTH_MDY DTH_CODE1 DTH_CODE2;
RUN;

/* Data Merge */
proc sql;
create table dir1.Cancer_Death_data as select * from dir1.Mydata as a, dir1.Cancer_Death_detail as b
where a.PERSON_ID = b.PERSON_ID;
quit;

/* Table 2 */
proc sort data=dir1.Cancer_Death_data;
BY Category;
RUN;

proc freq data=dir1.Cancer_Death_data;
tables Head_neck_Cancer * Category;
RUN;

proc freq data=dir1.Cancer_Death_data;
tables Digestive_Cancer * Category;
RUN;

proc freq data=dir1.Cancer_Death_data;
tables Intrac_Cancer * Category;
RUN;

proc freq data=dir1.Cancer_Death_data;
tables Bone_Cancer * Category;
RUN;

proc freq data=dir1.Cancer_Death_data;
tables Skin_Cancer * Category;
RUN;

proc freq data=dir1.Cancer_Death_data;
tables Breast_Cancer * Category;
RUN;

proc freq data=dir1.Cancer_Death_data;
tables Female_Cancer * Category;
RUN;

proc freq data=dir1.Cancer_Death_data;
tables Male_Cancer * Category;
RUN;

proc freq data=dir1.Cancer_Death_data;
tables Urinary_Cancer * Category;
RUN;

proc freq data=dir1.Cancer_Death_data;
tables Nervous_Cancer * Category;
RUN;

proc freq data=dir1.Cancer_Death_data;
tables Endocrine_Cancer * Category;
RUN;

proc freq data=dir1.Cancer_Death_data;
tables Hematologic_Cancer * Category;
RUN;

proc freq data=dir1.Cancer_Death_data;
tables Other_Cancer * Category;
RUN;

/* Unadjusted */
/** Digestive Cancer **/
proc phreg data=dir1.Cancer_Death_data;
class Category(ref='Moderate');
model FU_Duration_Death*Digestive_Cancer(0) = Category;
RUN;

/** Intrac Cacner **/
proc phreg data=dir1.Cancer_Death_data;
class Category(ref='Moderate');
model FU_Duration_Death*Intrac_Cancer(0) = Category;
RUN;

/** Male Cacner **/
proc phreg data=dir1.Cancer_Death_data;
class Category(ref='Moderate');
model FU_Duration_Death*Male_Cancer(0) = Category;
RUN;

/** Urinary Cacner **/
proc phreg data=dir1.Cancer_Death_data;
class Category(ref='Moderate');
model FU_Duration_Death*Urinary_Cancer(0) = Category;
RUN;

/** Hematologic cancer **/
proc phreg data=dir1.Cancer_Death_data;
class Category(ref='Moderate');
model FU_Duration_Death*Hematologic_Cancer(0) = Category;
RUN;


/* Adjusted */
/*Head and neck cancer Death */
proc phreg data=dir1.Cancer_Death_data;
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

model FU_Duration_death*Head_neck_Cancer(0) = Cohort_entry_Duration Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes;

RUN;

/*Digestive cancer Death */
proc phreg data=dir1.Cancer_Death_data;
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

model FU_Duration_death*Digestive_cancer(0) = Cohort_entry_Duration Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes;

RUN;

/* Intrac Cancer Death */
proc phreg data=dir1.Cancer_Death_data;
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

model FU_Duration_death*Intrac_Cancer(0) = Cohort_entry_Duration Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes;

RUN;

/* Bone Cancer Death */
proc phreg data=dir1.Cancer_Death_data;
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

model FU_Duration_death*Bone_Cancer(0) = Cohort_entry_Duration Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes;

RUN;

/* Skin Cancer Death */
proc phreg data=dir1.Cancer_Death_data;
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

model FU_Duration_death*Skin_Cancer(0) = Cohort_entry_Duration Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes;

RUN;

/* Breast Cancer Death */
proc phreg data=dir1.Cancer_Death_data;
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

model FU_Duration_death*Breast_Cancer(0) = Cohort_entry_Duration Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes;

RUN;

/* Female Cancer Death */
proc phreg data=dir1.Cancer_Death_data;
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

model FU_Duration_death*Female_Cancer(0) = Cohort_entry_Duration Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes;

RUN;

/* Male Cancer Death */
proc phreg data=dir1.Cancer_Death_data;
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

model FU_Duration_death*Male_Cancer(0) = Cohort_entry_Duration Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes;

RUN;

/* Urinary Cancer Death */
proc phreg data=dir1.Cancer_Death_data;
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

model FU_Duration_death*Urinary_Cancer(0) = Cohort_entry_Duration Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes;

RUN;

/* Nervous Cancer Death */
proc phreg data=dir1.Cancer_Death_data;
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

model FU_Duration_death*Nervous_Cancer(0) = Cohort_entry_Duration Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes;

RUN;

/* Endocrine Cancer Death */
proc phreg data=dir1.Cancer_Death_data;
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

model FU_Duration_death*Endocrine_Cancer(0) = Cohort_entry_Duration Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes;

RUN;

/* Hematologic cancer Death */
proc phreg data=dir1.Cancer_Death_data;
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

model FU_Duration_death*Hematologic_Cancer(0) = Cohort_entry_Duration Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes;

RUN;

/* Other cancer Death */
proc phreg data=dir1.Cancer_Death_data;
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

model FU_Duration_death*Other_Cancer(0) = Cohort_entry_Duration Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes;

RUN;

