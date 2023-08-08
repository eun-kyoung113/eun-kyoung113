/***** Outcome Define *****/
libname dir1 "/userdata12/room111/data_out/data_store";
run;

DATA total_study_pop;
SET '/userdata12/room111/data_out/data_store/total_study_pop2.sas7bdat';
RUN;

DATA T60_T20_screening;
SET '/userdata12/room111/data_out/data_store/T60_T20_screening.sas7bdat';
KEEP PERSON_ID RECU_FR_DT MAIN_SICK SUB_SICK;
RUN;

proc sql;
create table T60_T20_study as select * from dir1.total_study_pop2 join T60_T20_screening
on total_study_pop2.PERSON_ID = T60_T20_screening.PERSON_ID
and total_study_pop2.HME_DT <= T60_T20_screening.RECU_FR_DT;
quit;

proc sql;
create table dir1.check_cancer as select PERSON_ID, HME_DT, RECU_FR_DT,
case when ((MAIN_SICK like "C00%") or (MAIN_SICK like "C01%") or (MAIN_SICK like "C02%") or (MAIN_SICK like "C03%")
or (MAIN_SICK like "C04%") or (MAIN_SICK like "C05%") or (MAIN_SICK like "C06%") or (MAIN_SICK like "C07%") or (MAIN_SICK like "C08%")
or (MAIN_SICK like "C09%") or (MAIN_SICK like "C10%") or (MAIN_SICK like "C11%") or (MAIN_SICK like "C12%") or (MAIN_SICK like "C13%")
or (MAIN_SICK like "C14%") or (MAIN_SICK like "C30%") or (MAIN_SICK like "C31%") or (MAIN_SICK like "C32%") 
or (MAIN_SICK like "C33%") or (MAIN_SICK like "C69%")) or 
((SUB_SICK like "C00%") or (SUB_SICK like "C01%") or (SUB_SICK like "C02%") or (SUB_SICK like "C03%")
or (SUB_SICK like "C04%") or (SUB_SICK like "C05%") or (SUB_SICK like "C06%") or (SUB_SICK like "C07%") or (SUB_SICK like "C08%")
or (SUB_SICK like "C09%") or (SUB_SICK like "C10%") or (SUB_SICK like "C11%") or (SUB_SICK like "C12%") or (SUB_SICK like "C13%")
or (SUB_SICK like "C14%") or (SUB_SICK like "C30%") or (SUB_SICK like "C31%") or (SUB_SICK like "C32%") 
or (SUB_SICK like "C33%") or (SUB_SICK like "C69%")) then 1 else 0 end as Head_neck_Cancer,

case when ((MAIN_SICK like "C15%") or (MAIN_SICK like "C16%") or (MAIN_SICK like "C17%") or (MAIN_SICK like "C18%")
or (MAIN_SICK like "C19%") or (MAIN_SICK like "C20%") or (MAIN_SICK like "C21%") or (MAIN_SICK like "C22%") or (MAIN_SICK like "C23%")
or (MAIN_SICK like "C24%") or (MAIN_SICK like "C25%") or (MAIN_SICK like "C26%")) or
((SUB_SICK like "C15%") or (SUB_SICK like "C16%") or (SUB_SICK like "C17%") or (SUB_SICK like "C18%")
or (SUB_SICK like "C19%") or (SUB_SICK like "C20%") or (SUB_SICK like "C21%") or (SUB_SICK like "C22%") or (SUB_SICK like "C23%")
or (SUB_SICK like "C24%") or (SUB_SICK like "C25%") or (SUB_SICK like "C26%")) then 1 else 0 end as Digestive_Cancer,

case when ((MAIN_SICK like "C34%") or (MAIN_SICK like "C35%") or (MAIN_SICK like "C36%") or (MAIN_SICK like "C37%")
or (MAIN_SICK like "C38%") or (MAIN_SICK like "C39%") or (MAIN_SICK like "C45%")) or 
((SUB_SICK like "C34%") or (SUB_SICK like "C35%") or (SUB_SICK like "C36%") or (SUB_SICK like "C37%")
or (SUB_SICK like "C38%") or (SUB_SICK like "C39%") or (SUB_SICK like "C45%")) then 1 else 0 end as Intrac_Cancer,

case when ((MAIN_SICK like "C40%") or (MAIN_SICK like "C41%") or (MAIN_SICK like "C46%") or (MAIN_SICK like "C49%")) or 
((SUB_SICK like "C40%") or (SUB_SICK like "C41%") or (SUB_SICK like "C46%") or (SUB_SICK like "C49%")) then 1 else 0 end as Bone_Cancer,

case when ((MAIN_SICK like "C43%") or (MAIN_SICK like "C44%")) or ((SUB_SICK like "C43%") or (SUB_SICK like "C44%")) 
then 1 else 0 end as Skin_Cancer,

case when (MAIN_SICK like "C50%") or (SUB_SICK like "C50%") then 1 else 0 end as Breast_Cancer,

case when ((MAIN_SICK like "C51%") or (MAIN_SICK like "C52%") or (MAIN_SICK like "C53%") or (MAIN_SICK like "C54%")
or (MAIN_SICK like "C55%") or (MAIN_SICK like "C56%") or (MAIN_SICK like "C57%") or (MAIN_SICK like "C58%")) or
((SUB_SICK like "C51%") or (SUB_SICK like "C52%") or (SUB_SICK like "C53%") or (SUB_SICK like "C54%")
or (SUB_SICK like "C55%") or (SUB_SICK like "C56%") or (SUB_SICK like "C57%") or (SUB_SICK like "C58%")) then 1 else 0 end as Female_Cancer,

case when ((MAIN_SICK like "C60%") or (MAIN_SICK like "C61%") or (MAIN_SICK like "C62%") or (MAIN_SICK like "C63%")) or 
((SUB_SICK like "C60%") or (SUB_SICK like "C61%") or (SUB_SICK like "C62%") or (SUB_SICK like "C63%")) then 1 else 0 end as Male_Cancer,

case when ((MAIN_SICK like "C64%") or (MAIN_SICK like "C65%") or (MAIN_SICK like "C66%") or (MAIN_SICK like "C67%") or (MAIN_SICK like "C68%")) or 
((SUB_SICK like "C64%") or (SUB_SICK like "C65%") or (SUB_SICK like "C66%") or (SUB_SICK like "C67%") or (SUB_SICK like "C68%")) 
then 1 else 0 end as Urinary_Cancer,

case when ((MAIN_SICK like "C70%") or (MAIN_SICK like "C71%") or (MAIN_SICK like "C72%") or (MAIN_SICK like "C47%")) or 
((SUB_SICK like "C70%") or (SUB_SICK like "C71%") or (SUB_SICK like "C72%") or (SUB_SICK like "C47%")) then 1 else 0 end as Nervous_Cancer,

case when ((MAIN_SICK like "C73%") or (MAIN_SICK like "C74%") or (MAIN_SICK like "C75%")) or 
((SUB_SICK like "C73%") or (SUB_SICK like "C74%") or (SUB_SICK like "C75%")) then 1 else 0 end as Endocrine_Cancer,

case when ((MAIN_SICK like "C81%") or (MAIN_SICK like "C82%") or (MAIN_SICK like "C83%") or (MAIN_SICK like "C84%")
or (MAIN_SICK like "C85%") or (MAIN_SICK like "C86%") or (MAIN_SICK like "C87%") or (MAIN_SICK like "C88%") or (MAIN_SICK like "C89%")
or (MAIN_SICK like "C90%") or (MAIN_SICK like "C91%") or (MAIN_SICK like "C92%") or (MAIN_SICK like "C93%") or (MAIN_SICK like "C94%")
or (MAIN_SICK like "C95%") or (MAIN_SICK like "C96%")) or 
((SUB_SICK like "C81%") or (SUB_SICK like "C82%") or (SUB_SICK like "C83%") or (SUB_SICK like "C84%")
or (SUB_SICK like "C85%") or (SUB_SICK like "C86%") or (SUB_SICK like "C87%") or (SUB_SICK like "C88%") or (SUB_SICK like "C89%")
or (SUB_SICK like "C90%") or (SUB_SICK like "C91%") or (SUB_SICK like "C92%") or (SUB_SICK like "C93%") or (SUB_SICK like "C94%")
or (SUB_SICK like "C95%") or (SUB_SICK like "C96%")) then 1 else 0 end as Hematologic_Cancer,

case when ((MAIN_SICK like "C76%") or (MAIN_SICK like "C77%") or (MAIN_SICK like "C78%") or (MAIN_SICK like "C79%") 
or (MAIN_SICK like "C80%") or (MAIN_SICK like "C97%")) or 
((SUB_SICK like "C76%") or (SUB_SICK like "C77%") or (SUB_SICK like "C78%") or (SUB_SICK like "C79%") 
or (SUB_SICK like "C80%") or (SUB_SICK like "C97%")) 
then 1 else 0 end as Other_Cancer

from T60_T20_study;
quit;

proc sort data=dir1.check_cancer;
by PERSON_ID RECU_FR_DT;
RUN;

proc sort data=dir1.total_study_pop2;
by PERSON_ID;
RUN;

/* Cancer related Outcome table macro */
%macro define_Outcome_tbl;
	%let value = Head_neck Digestive Intrac Bone Skin Breast Female Male Urinary Nervous Endocrine
					  Hematologic Other;
	%local i next_value;
	%let i=1;
		%do %while (%scan(&value, &i) ne);
		%let next_value = %scan(&value, &i);
			DATA &next_value._outcome;
			SET dir1.check_cancer;
			BY PERSON_ID RECU_FR_DT;
			IF &next_value._Cancer=1;
			RUN;

			DATA &next_value._result;
			SET &next_value._outcome;
			BY PERSON_ID RECU_FR_DT;
			IF first.PERSON_ID;
			HME_DT_new=input(HME_DT,YYMMDD10.);
			RECU_FR_DT_new=input(RECU_FR_DT,YYMMDD10.);
			&next_value._Duration=(RECU_FR_DT_new-HME_DT_new)/365.25;
			KEEP PERSON_ID HME_DT &next_value._Cancer &next_value._Duration;
			RUN;

			DATA &next_value._all;
			MERGE dir1.total_study_pop2 &next_value._result;
			BY PERSON_ID;
			IF missing(&next_value._Cancer) then do;
   		 		&next_value._Cancer=0;
				HME_DT_new=input(HME_DT,YYMMDD10.);
				RECU_FR_DT_new=input('20191231',YYMMDD10.);
				&next_value._Duration=(RECU_FR_DT_new-HME_DT_new)/365.25;
		  	end;
			DROP RECU_FR_DT HME_DT_new RECU_FR_DT_new;
			RUN;
			%let i = %eval(&i +1);
		%end;
	%mend;

	%define_Outcome_tbl;


DATA dir1.Outcome_Incidence_Cancer;
MERGE Head_neck_all Digestive_all Intrac_all Bone_all Skin_all Breast_all Female_all 
		   Male_all Urinary_all Nervous_all Endocrine_all Hematologic_all Other_all;
BY PERSON_ID;
RUN;

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

DATA dir1.Cancer_Analysis_data;
MERGE total_study_pop_Duration dir1.Outcome_Incidence_Cancer dir1.BMI_Exposure dir1.Demographic_cov 
		   dir1.Biomarker_cov dir1.family_history_covariate dir1.Case_History_covariate dir1.Smoking_cov dir1.Drink_habit_cov 
		   dir1.EXERCI_FREQ_COV dir1.EXERCI_METS_COV dir1.Comorbidities dir1.Medication_covariate;
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
RUN;


/* Table 2 */
proc sort data=dir1.Cancer_Analysis_data;
BY Category;
RUN;

proc freq data=dir1.Cancer_Analysis_data;
tables Head_neck_Cancer * Category;
RUN;

proc freq data=dir1.Cancer_Analysis_data;
tables Digestive_Cancer * Category;
RUN;

proc freq data=dir1.Cancer_Analysis_data;
tables Intrac_Cancer * Category;
RUN;

proc freq data=dir1.Cancer_Analysis_data;
tables Bone_Cancer * Category;
RUN;

proc freq data=dir1.Cancer_Analysis_data;
tables Skin_Cancer * Category;
RUN;

proc freq data=dir1.Cancer_Analysis_data;
tables Breast_Cancer * Category;
RUN;

proc freq data=dir1.Cancer_Analysis_data;
tables Female_Cancer * Category;
RUN;

proc freq data=dir1.Cancer_Analysis_data;
tables Male_Cancer * Category;
RUN;

proc freq data=dir1.Cancer_Analysis_data;
tables Urinary_Cancer * Category;
RUN;

proc freq data=dir1.Cancer_Analysis_data;
tables Nervous_Cancer * Category;
RUN;

proc freq data=dir1.Cancer_Analysis_data;
tables Endocrine_Cancer * Category;
RUN;

proc freq data=dir1.Cancer_Analysis_data;
tables Hematologic_Cancer * Category;
RUN;

proc freq data=dir1.Cancer_Analysis_data;
tables Other_Cancer * Category;
RUN;

/* Unadjusted */
/** Digestive Cancer **/
proc phreg data=dir1.Cancer_Analysis_data;
class Category(ref='Moderate');
model Digestive_Duration*Digestive_Cancer(0) = Category;
RUN;

/** Intrac Cacner **/
proc phreg data=dir1.Cancer_Analysis_data;
class Category(ref='Moderate');
model Intrac_Duration*Intrac_Cancer(0) = Category;
RUN;

/** Endocrine Cacner **/
proc phreg data=dir1.Cancer_Analysis_data;
class Category(ref='Moderate');
model Endocrine_Duration*Endocrine_Cancer(0) = Category;
RUN;

/** Other Cacner **/
proc phreg data=dir1.Cancer_Analysis_data;
class Category(ref='Moderate');
model Other_Duration*Other_Cancer(0) = Category;
RUN;

/** Head and neck Cancer **/
proc phreg data=dir1.Cancer_Analysis_data;
class Category(ref='Moderate');
model Head_neck_Duration*Head_neck_Cancer(0) = Category;
RUN;

/** Skin Cancer **/
proc phreg data=dir1.Cancer_Analysis_data;
class Category(ref='Moderate');
model Skin_Duration*Skin_Cancer(0) = Category;
RUN;

/** Urinary Cancer **/
proc phreg data=dir1.Cancer_Analysis_data;
class Category(ref='Moderate');
model Urinary_Duration*Urinary_Cancer(0) = Category;
RUN;

/** Hematologic Cancer **/
proc phreg data=dir1.Cancer_Analysis_data;
class Category(ref='Moderate');
model Hematologic_Duration*Hematologic_Cancer(0) = Category;
RUN;

/* Adjusted */
/*Digestive cancer */
proc phreg data=dir1.Cancer_Analysis_data;
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

model Digestive_Duration*Digestive_cancer(0) = Cohort_entry_Duration Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes;

RUN;

/* Intrac Cancer */
proc phreg data=dir1.Cancer_Analysis_data;
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

model Intrac_Duration*Intrac_Cancer(0) = Cohort_entry_Duration Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes;

RUN;


/* Endocrine_Cancer  */
proc phreg data=dir1.Cancer_Analysis_data;
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

model Endocrine_Duration*Endocrine_Cancer(0) = Cohort_entry_Duration Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes;

RUN;


/* Other_Cancer  */
proc phreg data=dir1.Cancer_Analysis_data;
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

model Other_Duration*Other_Cancer(0) = Cohort_entry_Duration Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes;

RUN;

/* Head_neck_Cancer  */
proc phreg data=dir1.Cancer_Analysis_data;
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

model Head_neck_Duration*Head_neck_Cancer(0) = Cohort_entry_Duration Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes;

RUN;

/* Skin_Cancer  */
proc phreg data=dir1.Cancer_Analysis_data;
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

model Skin_Duration*Skin_Cancer(0) = Cohort_entry_Duration Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes;

RUN;

/* Urinary_Cancer  */
proc phreg data=dir1.Cancer_Analysis_data;
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

model Urinary_Duration*Urinary_Cancer(0) = Cohort_entry_Duration Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes;

RUN;

/* Hematologic_Cancer  */
proc phreg data=dir1.Cancer_Analysis_data;
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

model Hematologic_Duration*Hematologic_Cancer(0) = Cohort_entry_Duration Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes;

RUN;

/*Bone cancer */
proc phreg data=dir1.Cancer_Analysis_data;
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

model Bone_Duration*Bone_cancer(0) = Cohort_entry_Duration Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes;

RUN;

/*Nervous cancer */
proc phreg data=dir1.Cancer_Analysis_data;
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

model Nervous_Duration*Nervous_cancer(0) = Cohort_entry_Duration Category AGE SEX ctrb_pt_type_cd /*olig_prote_cd*/ FMLY_Heart_disease FMLY_Diabetes FMLY_Cancer
        MI_case Heart_failure_case Cancer_case Hypertension_case TIA_stroke_case Anticoagulants_result Antiplatelets_result  Asthma_result  Atrial_fibrillation_result CAD_result CKD_result CLD_result CNS_result 
		COPD_result Dementia_result GI_Disorders_result Hyperlipidemia_result  Hypertension_except_RAS_result
		Hypertension_renin_result Hypertension_result immunosuppressant_result  insulin_result Non_opioid_result Non_statin_result oral_diabetes_result 
        Pneumonia_result Psychiatric_disorders_result PVD_result statin_result Thromboembolism_result SMK_STAT_TYPE_RSPS_CD
        BLDS TOT_CHOLE SGOT_AST SGPT_ALT GAMMA_GTP /*CUR_DSQTY CUR_PACK_YEAR
        PAST_DSQTY PAST_PACK_YEAR*/ DRNK_FREQUENCY_HABIT EXERCI_HABIT METS_minutes;

RUN;
