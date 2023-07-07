/*SAS table 저장 경로 지정 */
libname dir1 "/userdata12/room115/data_out/data_store";
run;

/* 검진DB - 2002년 ~ 2015년 DB 불러오기 */
DATA screening1;
SET '/userdata12/room115/data_source/nhis_heals_gj.sas7bdat';
RUN;

/* 건강검진DB split */
/* screening1에서 2002년 ~ 2008년까지만 가져오기*/
DATA dir1.screening_08;
SET screening1;
IF (HCHK_YEAR<=2008) and (PERSON_ID NOT = '');
RUN;

/* screening1에서 2008년 ~ 2015년까지만 가져오기*/
DATA dir1.screening_09_15;
SET screening1;
IF HME_DT>='20090101';
RUN;

/* 생애전환기검진DB split */
DATA ex_screening_07_15;
SET '/userdata12/room115/data_source/nhis_heals_lj.sas7bdat';
IF PERSON_ID NOT = '';
run;

DATA dir1.ex_screening_08;
SET ex_screening_07_15;
IF HCHK_YEAR<=2008;
RUN;

DATA dir1.ex_screening_09_15;
SET ex_screening_07_15;
IF HCHK_YEAR>=2009;
RUN;

/********************************************/
/* 자격DB 가져오기 + 합치기*/
DATA jk;
SET '/userdata12/room115/data_source/nhis_heals_jk.sas7bdat';
RUN;

DATA jk_1619;
SET '/userdata12/room115/data_source/1619/nhis_heals_jk_1619.sas7bdat';
CTRB_PT_TYPE_CD2 = PUT(CTRB_PT_TYPE_CD, 2.);
DROP CTRB_PT_TYPE_CD;
RENAME CTRB_PT_TYPE_CD2 = CTRB_PT_TYPE_CD;
RUN;

DATA dir1.jk_total;
SET jk jk_1619;
RUN;

/************ Covariate 정의 **************/
DATA total_study_pop;
SET dir1.total_study_pop;
RUN;

DATA study_pop;
SET total_study_pop;
YEAR = SUBSTR(HME_DT,1,4);
RUN;
/* Demographic covariate define */
DATA jk_total;
SET dir1.jk_total;
RUN;

proc sql;
create table jk_cov as select * from study_pop, jk_total
where study_pop.PERSON_ID=jk_total.PERSON_ID 
and study_pop.YEAR>=jk_total.STND_Y;
quit;

PROC SORT DATA=jk_cov;
BY PERSON_ID STND_Y;
run;

DATA dir1.Demographic_cov;
SET jk_cov;
BY PERSON_ID STND_Y;
IF last.PERSON_ID;
KEEP PERSON_ID SEX AGE CTRB_PT_TYPE_CD;
RUN;

/***** Family history Define ******/
/* 검진DB - 2002년 ~ 2008년 DB 불러오기 */
DATA screening_08;
SET dir1.screening_08;
KEEP PERSON_ID HME_DT FMLY_HDISE_PATIEN_YN FMLY_DIABML_PATIEN_YN FMLY_CANCER_PATIEN_YN;
RUN;

/* 검진DB - 2009년 ~ 2015년 DB 불러오기 */
DATA screening_09_15;
SET dir1.screening_09_15;
KEEP PERSON_ID HME_DT FMLY_HDISE_PATIEN_YN FMLY_DIABML_PATIEN_YN;
IF PERSON_ID NOT = '';
RUN;

/* 검진DB - 2016년 ~ 2017년 DB 불러오기 */
DATA screening_16_17;
SET '/userdata12/room115/data_source/1619/nhis_heals_gj_1617.sas7bdat';
KEEP PERSON_ID HME_DT FMLY_HDISE_PATIEN_YN FMLY_DIABML_PATIEN_YN;
IF PERSON_ID NOT = '';
RUN;

/* 검진DB - 2018년 ~ 2019년 DB 불러오기 */
DATA screening_19;
SET '/userdata12/room115/data_source/1619/nhis_heals_gj_1819.sas7bdat';
IF PERSON_ID NOT = '';
KEEP PERSON_ID HME_DT FMLY_HDISE_PATIEN_YN FMLY_DIABML_PATIEN_YN;
RUN;

/* 생애전환기검진DB - 2007년 ~ 2008년 DB 불러오기 */
DATA ex_screening_08;
SET dir1.ex_screening_08;
IF PERSON_ID NOT = '';
KEEP PERSON_ID HME_DT FMLY_HDISE_PATIEN_YN FMLY_DIABML_PATIEN_YN FMLY_CANCER_PATIEN_YN;
run;

/* 생애전환기검진DB - 2009년 ~ 2015년 DB 불러오기 */
DATA ex_screening_09_15;
SET dir1.ex_screening_09_15;
IF PERSON_ID NOT = '';
KEEP PERSON_ID HME_DT FMLY_HDISE_PATIEN_YN FMLY_DIABML_PATIEN_YN;
RUN;

/* 생애전환기검진DB - 2016년 ~ 2017년 DB 불러오기 */
DATA ex_screening_16_17;
SET '/userdata12/room115/data_source/1619/nhis_heals_lj_1617.sas7bdat';
IF PERSON_ID NOT = '';
KEEP PERSON_ID HME_DT FMLY_HDISE_PATIEN_YN FMLY_DIABML_PATIEN_YN;
RUN;

/**** 검진DB 연도별로 합치기 --> 이력 변수 생성****/
/* ~ 2008년 */
DATA total_screening_08;
SET screening_08 ex_screening_08;
RUN;

proc sql;
create table fmly_hist_check_08 as select *,
case 
	when FMLY_HDISE_PATIEN_YN="2" THEN 1 
	when FMLY_HDISE_PATIEN_YN="" THEN . 
	ELSE 0 
end as Heart_disease,
case 
	when FMLY_DIABML_PATIEN_YN="2" THEN 1
	when FMLY_DIABML_PATIEN_YN='' THEN . 
	ELSE 0 
end as Diabetes,
case 
	when FMLY_CANCER_PATIEN_YN="2" THEN 1
	when FMLY_CANCER_PATIEN_YN='' THEN . 
	ELSE 0 
end as Cancer
from total_screening_08, total_study_pop
where total_screening_08.PERSON_ID = total_study_pop.PERSON_ID;
quit;

/* 09년도 ~ 15년도 */
DATA total_screening_09_15;
SET screening_09_15 ex_screening_08;
IF PERSON_ID NOT="";
FMLY_CANCER_PATIEN_YN='';
Cancer = . ;
RUN;

proc sql;
create table fmly_hist_check_09_15 as select *,
case 
	when FMLY_HDISE_PATIEN_YN="1" THEN 1 
	when FMLY_HDISE_PATIEN_YN="" THEN . 
	ELSE 0 
end as Heart_disease,
case 
	when FMLY_DIABML_PATIEN_YN="1" THEN 1
	when FMLY_DIABML_PATIEN_YN='' THEN . 
	ELSE 0 
end as Diabetes
from total_screening_09_15, total_study_pop
where total_screening_09_15.PERSON_ID = total_study_pop.PERSON_ID;
quit;

/* 16년도 ~ 19년도 */
DATA total_screening_16_19;
SET screening_16_17 screening_19 ex_screening_16_17;
IF PERSON_ID NOT="";
FMLY_CANCER_PATIEN_YN='';
Cancer = .;
FMLY_HDISE_PATIEN_YN2=PUT(FMLY_HDISE_PATIEN_YN,1.);
FMLY_DIABML_PATIEN_YN2=PUT(FMLY_DIABML_PATIEN_YN,1.);
DROP FMLY_HDISE_PATIEN_YN FMLY_DIABML_PATIEN_YN;
RUN;

DATA total_screening_19;
SET total_screening_16_19;
RENAME FMLY_HDISE_PATIEN_YN2=FMLY_HDISE_PATIEN_YN FMLY_DIABML_PATIEN_YN2=FMLY_DIABML_PATIEN_YN;
RUN;

proc sql;
create table fmly_hist_check_19 as select *,
case 
	when FMLY_HDISE_PATIEN_YN="1" THEN 1 
	when FMLY_HDISE_PATIEN_YN="." THEN . 
	ELSE 0 
end as Heart_disease,
case 
	when FMLY_DIABML_PATIEN_YN="1" THEN 1
	when FMLY_DIABML_PATIEN_YN='.' THEN . 
	ELSE 0 
end as Diabetes
from total_screening_19, total_study_pop
where total_screening_19.PERSON_ID = total_study_pop.PERSON_ID;
quit;

/* 가족력 결측 비율 확인 
DATA check_na;
SET fmly_hist_check_08 fmly_hist_check_09_15 fmly_hist_check_19;
RUN;

PROC FREQ DATA=check_na;
TABLES FMLY_HDISE_PATIEN_YN /MISSING;
RUN;

PROC FREQ DATA=check_na;
TABLES FMLY_DIABML_PATIEN_YN /MISSING;
RUN;

PROC FREQ DATA=check_na;
TABLES FMLY_CANCER_PATIEN_YN /MISSING;
RUN;*/

/* 이력 변수 정리*/
DATA total_screening;
SET fmly_hist_check_08 fmly_hist_check_09_15 fmly_hist_check_19;
KEEP PERSON_ID HME_DT Heart_disease Diabetes Cancer;
RUN;

proc sort data=total_screening;
BY PERSON_ID;
RUN;

proc sql;
create table fmly_check_total as select PERSON_ID, 
SUM(Heart_disease) as FMLY_Heart_disease, SUM(Diabetes) as FMLY_Diabetes,
SUM(Cancer) as FMLY_Cancer
from total_screening
group by PERSON_ID;
QUIT;

DATA dir1.Family_history_covariate;
SET fmly_check_total;
IF FMLY_Heart_disease>=1 THEN FMLY_Heart_disease=1;
ELSE FMLY_Heart_disease=0;
IF FMLY_Diabetes>=1 THEN FMLY_Diabetes=1;
ELSE FMLY_Diabetes=0;
IF FMLY_Cancer>=1 THEN FMLY_Cancer=1;
ELSE FMLY_Cancer=0;
RUN;

/*********************************/
/*** Biomarker covariate define ***/
/* 검진DB - 2002년 ~ 2008년 DB 불러오기 */
DATA screening_08;
SET dir1.screening_08;
KEEP PERSON_ID HME_DT TOT_CHOLE OLIG_PROTE_CD BLDS SGOT_AST SGPT_ALT GAMMA_GTP;
RUN;

/* 검진DB - 2009년 ~ 2015년 DB 불러오기 */
DATA screening_09_15;
SET dir1.screening_09_15;
KEEP PERSON_ID HME_DT TOT_CHOLE OLIG_PROTE_CD BLDS SGOT_AST SGPT_ALT GAMMA_GTP;
IF PERSON_ID NOT = '';
RUN;

/* 검진DB - 2016년 ~ 2017년 DB 불러오기 */
DATA screening_16_17;
SET '/userdata12/room115/data_source/1619/nhis_heals_gj_1617.sas7bdat';
KEEP PERSON_ID HME_DT TOT_CHOLE OLIG_PROTE_CD BLDS SGOT_AST SGPT_ALT GAMMA_GTP;
IF PERSON_ID NOT = '';
RUN;

/* 생애전환기검진DB - 2007년 ~ 2008년 DB 불러오기 */
DATA ex_screening_08;
SET dir1.ex_screening_08;
IF PERSON_ID NOT = '';
KEEP PERSON_ID HME_DT TOT_CHOLE OLIG_PROTE_CD BLDS SGOT_AST SGPT_ALT GAMMA_GTP;
run;

/* 생애전환기검진DB - 2009년 ~ 2015년 DB 불러오기 */
DATA ex_screening_09_15;
SET dir1.ex_screening_09_15;
IF PERSON_ID NOT = '';
KEEP PERSON_ID HME_DT TOT_CHOLE OLIG_PROTE_CD BLDS SGOT_AST SGPT_ALT GAMMA_GTP;
RUN;

/* 생애전환기검진DB - 2016년 ~ 2017년 DB 불러오기 */
DATA ex_screening_16_17;
SET '/userdata12/room115/data_source/1619/nhis_heals_lj_1617.sas7bdat';
IF PERSON_ID NOT = '';
KEEP PERSON_ID HME_DT TOT_CHOLE OLIG_PROTE_CD BLDS SGOT_AST SGPT_ALT GAMMA_GTP;
RUN;

/** 변수 속성 맞는 TBL끼리 binding **/
DATA total_screening_15;
SET screening_08 screening_09_15 ex_screening_08 ex_screening_09_15;
RUN;

DATA total_screening_17;
SET screening_16_17 ex_screening_16_17;
RUN;

/** Study population과 joint **/
proc sql;
create table study_pop_biomarker_15 as select * 
from total_study_pop, total_screening_15
where (total_study_pop.PERSON_ID = total_screening_15.PERSON_ID)
AND (total_study_pop.HME_DT >= total_screening_15.HME_DT);
quit;

proc sql;
create table study_pop_biomarker_17 as select * 
from total_study_pop, total_screening_17
where (total_study_pop.PERSON_ID = total_screening_17.PERSON_ID)
AND (total_study_pop.HME_DT >= total_screening_17.HME_DT);
quit;

/* "OLIG_PROTE_CD" 변수명 type 통일시키기 */
DATA study_pop_biomarker_17;
SET study_pop_biomarker_17;
OLIG_PROTE_CD2 = PUT(OLIG_PROTE_CD,1.);
DROP OLIG_PROTE_CD;
RENAME OLIG_PROTE_CD2=OLIG_PROTE_CD;
RUN;

/* biomarker table rbinding 후 정렬 -> 최신 기록 가져오기 */
DATA study_pop_biomarker;
SET study_pop_biomarker_15 study_pop_biomarker_17;
RUN;

PROC SORT DATA=study_pop_biomarker;
BY PERSON_ID HME_DT;
RUN;

DATA dir1.Biomarker_cov;
SET study_pop_biomarker;
BY PERSON_ID HME_DT;
IF last.PERSON_ID;
DROP HME_DT;
RUN;

/* Biomarker missing rate check */
PROC FREQ DATA=Biomarker_cov;
TABLES TOT_CHOLE BLDS OLIG_PROTE_CD SGOT_AST SGPT_ALT GAMMA_GTP /MISSING;
RUN;


/*************************************/
/****** 음주습관 *******/
/* 검진DB - 2002년 ~ 2008년 DB 불러오기 */
DATA screening_08;
SET '/userdata12/room115/data_out/data_store/screening_08.sas7bdat';
KEEP PERSON_ID HME_DT DRNK_HABIT_RSPS_CD; 
RUN;

/* 검진DB - 2009년 ~ 2015년 DB 불러오기 */
DATA screening_09_15;
SET '/userdata12/room115/data_out/data_store/screening_09_15.sas7bdat';
KEEP PERSON_ID HME_DT DRNK_HABIT_RSPS_CD;
IF PERSON_ID NOT = '';
RUN;

/* 검진DB - 2016년 ~ 2017년 DB 불러오기 */
DATA screening_16_17;
SET '/userdata12/room115/data_source/1619/nhis_heals_gj_1617.sas7bdat';
KEEP PERSON_ID HME_DT DRNK_HABIT_RSPS_CD;
IF PERSON_ID NOT = '';
RUN;

/* 생애전환기검진DB - 2007년 ~ 2015년 DB 불러오기 */
DATA ex_screening_15;
SET '/userdata12/room115/data_source/nhis_heals_lj.sas7bdat';
IF PERSON_ID NOT = '';
KEEP PERSON_ID HME_DT DRNK_HABIT_RSPS_CD;
run;

/* 생애전환기검진DB - 2016년 ~ 2017년 DB 불러오기 */
DATA ex_screening_16_17;
SET '/userdata12/room115/data_source/1619/nhis_heals_lj_1617.sas7bdat';
IF PERSON_ID NOT = '';
KEEP PERSON_ID HME_DT DRNK_HABIT_RSPS_CD;
RUN;

/* 음주습관 변수값 재정의 */
DATA DRNK_check1;
SET screening_08;
IF DRNK_HABIT_RSPS_CD='1' THEN DRNK_FREQUENCY_HABIT=0;
ELSE IF DRNK_HABIT_RSPS_CD='2' THEN DRNK_FREQUENCY_HABIT=0.082;
ELSE IF DRNK_HABIT_RSPS_CD='3' THEN DRNK_FREQUENCY_HABIT=0.214;
ELSE IF DRNK_HABIT_RSPS_CD='4' THEN DRNK_FREQUENCY_HABIT=0.5;
ELSE IF DRNK_HABIT_RSPS_CD='5' THEN DRNK_FREQUENCY_HABIT=1;
ELSE DRNK_FREQUENCY_HABIT=.;
DROP DRNK_HABIT_RSPS_CD;
RUN;

DATA DRNK_check2;
SET screening_09_15;
IF DRNK_HABIT_RSPS_CD='' THEN DRNK_FREQUENCY_HABIT=.;
ELSE DRNK_FREQUENCY_HABIT=DRNK_HABIT_RSPS_CD/7;
DROP DRNK_HABIT_RSPS_CD;
RUN;

DATA DRNK_check3;
SET screening_16_17;
IF DRNK_HABIT_RSPS_CD=. THEN DRNK_FREQUENCY_HABIT=.;
ELSE DRNK_FREQUENCY_HABIT=DRNK_HABIT_RSPS_CD/7;
DROP DRNK_HABIT_RSPS_CD;
RUN;

DATA DRNK_check4;
SET ex_screening_15;
IF DRNK_HABIT_RSPS_CD='' THEN DRNK_FREQUENCY_HABIT=.;
ELSE DRNK_FREQUENCY_HABIT=DRNK_HABIT_RSPS_CD/7;
DROP DRNK_HABIT_RSPS_CD;
RUN;

DATA DRNK_check5;
SET ex_screening_16_17;
IF DRNK_HABIT_RSPS_CD=. THEN DRNK_FREQUENCY_HABIT=.;
ELSE DRNK_FREQUENCY_HABIT=DRNK_HABIT_RSPS_CD/7;
DROP DRNK_HABIT_RSPS_CD;
RUN;

DATA DRNK_HABIT_check_01;
SET DRNK_check1 DRNK_check2 DRNK_check3 DRNK_check4 DRNK_check5;
RUN;

PROC sql;
create table DRNK_HABIT_check_02 as select * from DRNK_HABIT_check_01, total_study_pop
where DRNK_HABIT_check_01.PERSON_ID = total_study_pop.PERSON_ID
AND DRNK_HABIT_check_01.HME_DT <= total_study_pop.HME_DT;
quit;

PROC sort data=DRNK_HABIT_check_02;
BY PERSON_ID HME_DT;
RUN;

DATA dir1.DRINK_HABIT_COV;
SET DRNK_HABIT_check_02;
BY PERSON_ID HME_DT;
IF last.PERSON_ID;
DROP HME_DT;
RUN;

/***************************/
/*** 운동빈도 covariate define ***/
/* 검진DB - 2002년 ~ 2008년 DB 불러오기 */
DATA screening_08;
SET '/userdata12/room115/data_out/data_store/screening_08.sas7bdat';
KEEP PERSON_ID HME_DT EXERCI_FREQ_RSPS_CD; 
RUN;

/* 검진DB - 2009년 ~ 2015년 DB 불러오기 */
DATA screening_09_15;
SET '/userdata12/room115/data_out/data_store/screening_09_15.sas7bdat';
KEEP PERSON_ID HME_DT MOV20_WEK_FREQ_ID MOV30_WEK_FREQ_ID WLK30_WEK_FREQ_ID;
IF PERSON_ID NOT = '';
RUN;

/* 검진DB - 2016년 ~ 2017년 DB 불러오기 */
DATA screening_16_17;
SET '/userdata12/room115/data_source/1619/nhis_heals_gj_1617.sas7bdat';
KEEP PERSON_ID HME_DT MOV20_WEK_FREQ_ID MOV30_WEK_FREQ_ID WLK30_WEK_FREQ_ID;
IF PERSON_ID NOT = '';
RUN;

/* 생애전환기검진DB - 2007년 ~ 2008년 DB 불러오기 */
DATA ex_screening_08;
SET '/userdata12/room115/data_out/data_store/ex_screening_08.sas7bdat';
IF PERSON_ID NOT = '';
KEEP PERSON_ID HME_DT MOV20_WEK_FREQ_ID MOV30_WEK_FREQ_ID WLK30_WEK_FREQ_ID;
run;

/* 생애전환기검진DB - 2009년 ~ 2015년 DB 불러오기 */
DATA ex_screening_09_15;
SET '/userdata12/room115/data_out/data_store/ex_screening_09_15.sas7bdat';
IF PERSON_ID NOT = '';
KEEP PERSON_ID HME_DT MOV20_WEK_FREQ_ID MOV30_WEK_FREQ_ID WLK30_WEK_FREQ_ID;
run;

/* 생애전환기검진DB - 2016년 ~ 2017년 DB 불러오기 */
DATA ex_screening_16_17;
SET '/userdata12/room115/data_source/1619/nhis_heals_lj_1617.sas7bdat';
IF PERSON_ID NOT = '';
KEEP PERSON_ID HME_DT MOV20_WEK_FREQ_ID MOV30_WEK_FREQ_ID WLK30_WEK_FREQ_ID;
RUN;

DATA screening_08_1;
SET screening_08;
EXERCI_HABIT=EXERCI_FREQ_RSPS_CD/7;
DROP EXERCI_FREQ_RSPS_CD;
RUN;

DATA ex_screening_08_1;
SET ex_screening_08;
EXERCI_HABIT = (SUM(MOV20_WEK_FREQ_ID,MOV30_WEK_FREQ_ID,WLK30_WEK_FREQ_ID) / 3) /7;
DROP MOV20_WEK_FREQ_ID MOV30_WEK_FREQ_ID WLK30_WEK_FREQ_ID;
RUN;

DATA total_screening_09_15;
SET screening_09_15 ex_screening_09_15;
RUN;

DATA total_screening_09_15_1;
SET total_screening_09_15;
EXERCI_HABIT = (SUM(MOV20_WEK_FREQ_ID,MOV30_WEK_FREQ_ID,WLK30_WEK_FREQ_ID) / 3) /7;
DROP MOV20_WEK_FREQ_ID MOV30_WEK_FREQ_ID WLK30_WEK_FREQ_ID;
RUN;

DATA total_screening_16_17;
SET screening_16_17 ex_screening_16_17;
RUN;

DATA total_screening_16_17_1;
SET total_screening_16_17;
EXERCI_HABIT = (SUM(MOV20_WEK_FREQ_ID,MOV30_WEK_FREQ_ID,WLK30_WEK_FREQ_ID) / 3) /7;
DROP MOV20_WEK_FREQ_ID MOV30_WEK_FREQ_ID WLK30_WEK_FREQ_ID;
RUN;

DATA total_screening_EXERCI;
SET screening_08_1 ex_screening_08_1 total_screening_09_15_1 total_screening_16_17_1;
RUN;

proc sql;
create table EXERCI_FREQ_cov as select * from total_screening_EXERCI, total_study_pop
WHERE total_screening_EXERCI.PERSON_ID = total_study_pop.PERSON_ID
AND total_screening_EXERCI.HME_DT <= total_study_pop.HME_DT;
quit;

proc sort data=EXERCI_FREQ_cov;
BY PERSON_ID HME_DT;
RUN;

DATA dir1.EXERCI_FREQ_cov;
SET EXERCI_FREQ_cov;
BY PERSON_ID HME_DT;
IF last.PERSON_ID;
RUN;


/****** 운동지속시간 covariate define ******/
DATA EXERCI_HABIT_08;
SET EXERCI_FREQ_cov;
IF (HME_DT<='20081231');
RUN; 

DATA EXERCI_HABIT_09_15;
SET EXERCI_FREQ_cov;
IF (HME_DT>='20090101' AND HME_DT<='20151231');
RUN;

DATA EXERCI_HABIT_16_17;
SET EXERCI_FREQ_cov;
IF (HME_DT>='20160101');
RUN; 

proc sql;
create table EXERCI_METS_09_15 as select * from EXERCI_HABIT_09_15, total_screening_09_15
WHERE EXERCI_HABIT_09_15.PERSON_ID = total_screening_09_15.PERSON_ID
AND EXERCI_HABIT_09_15.HME_DT = total_screening_09_15.HME_DT;
quit;

DATA EXERCI_METS_09_15;
SET EXERCI_METS_09_15;
METS_minutes = SUM(MOV20_WEK_FREQ_ID*6*20,MOV30_WEK_FREQ_ID*3*30);
KEEP PERSON_ID METS_minutes;
RUN; 

proc sql;
create table EXERCI_METS_16_17 as select * from EXERCI_HABIT_16_17, total_screening_16_17
WHERE (EXERCI_HABIT_16_17.PERSON_ID = total_screening_16_17.PERSON_ID)
AND (EXERCI_HABIT_16_17.HME_DT = total_screening_16_17.HME_DT);
quit;

DATA EXERCI_METS_16_17;
SET EXERCI_METS_16_17;
METS_minutes = SUM(MOV20_WEK_FREQ_ID*6*20,MOV30_WEK_FREQ_ID*3*30);
KEEP PERSON_ID METS_minutes;
RUN; 

proc sort data=EXERCI_METS_16_17 out=EXERCI_METS_16_17  nodupkey;
by PERSON_ID METS_minutes;
RUN;

proc sql;
create table EXERCI_METS_08_1 as select * from EXERCI_HABIT_08, ex_screening_08
WHERE EXERCI_HABIT_08.PERSON_ID = ex_screening_08.PERSON_ID
AND EXERCI_HABIT_08.HME_DT = ex_screening_08.HME_DT;
quit;

DATA EXERCI_METS_08_1;
SET EXERCI_METS_08_1;
METS_minutes = SUM(MOV20_WEK_FREQ_ID*6*20,MOV30_WEK_FREQ_ID*3*30);
KEEP PERSON_ID METS_minutes;
RUN; 

proc sql;
create table EXERCI_METS_not as select PERSON_ID
from EXERCI_HABIT_08
except select PERSON_ID from EXERCI_METS_08_1;
quit;

DATA total_screening_16_17;
SET total_screening_16_17;
MOV20_WEK_FREQ_ID2=PUT(MOV20_WEK_FREQ_ID,1.);
MOV30_WEK_FREQ_ID2=PUT(MOV30_WEK_FREQ_ID,1.);
WLK30_WEK_FREQ_ID2=PUT(WLK30_WEK_FREQ_ID,1.);
DROP MOV20_WEK_FREQ_ID MOV30_WEK_FREQ_ID WLK30_WEK_FREQ_ID;
RUN;

DATA total_screening_16_17;
SET total_screening_16_17;
RENAME MOV20_WEK_FREQ_ID2 = MOV20_WEK_FREQ_ID;
RENAME MOV30_WEK_FREQ_ID2 = MOV30_WEK_FREQ_ID;
RENAME WLK30_WEK_FREQ_ID2 = WLK30_WEK_FREQ_ID;
RUN;

DATA total_screening_09_17;
SET total_screening_09_15 total_screening_16_17;
RUN;

proc sql;
create table EXERCI_METS_08_2 as select *  from EXERCI_METS_not, total_screening_09_17
WHERE EXERCI_METS_not.PERSON_ID = total_screening_09_17.PERSON_ID;
quit;

proc sort data=EXERCI_METS_08_2;
BY PERSON_ID HME_DT;
RUN;

DATA EXERCI_METS_08_2;
SET EXERCI_METS_08_2;
BY PERSON_ID HME_DT;
IF first.PERSON_ID;
METS_minutes = SUM(MOV20_WEK_FREQ_ID*6*20,MOV30_WEK_FREQ_ID*3*30);
KEEP PERSON_ID METS_minutes;
RUN; 

DATA EXERCI_METS_08_bind;
SET EXERCI_METS_08_1 EXERCI_METS_08_2;
RUN;

proc sql;
create table EXERCI_METS_08 as select * from EXERCI_HABIT_08
FULL JOIN EXERCI_METS_08_bind on EXERCI_HABIT_08.PERSON_ID = EXERCI_METS_08_bind.PERSON_ID
ORDER BY EXERCI_HABIT_08.PERSON_ID;
quit;

DATA EXERCI_METS_08;
SET EXERCI_METS_08;
DROP EXERCI_HABIT HME_DT;
RUN;

DATA dir1.EXERCI_METS_cov;
SET EXERCI_METS_08 EXERCI_METS_09_15 EXERCI_METS_16_17;
RUN;


/*********************************/
/**** 흡연관련 covariate define ****/
DATA screening_08;
SET '/userdata12/room115/data_out/data_store/screening_08.sas7bdat';
KEEP PERSON_ID HME_DT SMK_STAT_TYPE_RSPS_CD SMK_TERM_RSPS_CD DSQTY_RSPS_CD; 
RUN;

/* 검진DB - 2009년 ~ 2015년 DB 불러오기 */
DATA screening_09_15;
SET '/userdata12/room115/data_out/data_store/screening_09_15.sas7bdat';
KEEP PERSON_ID HME_DT SMK_STAT_TYPE_RSPS_CD PAST_SMK_TERM_RSPS_CD PAST_DSQTY_RSPS_CD CUR_SMK_TERM_RSPS_CD CUR_DSQTY_RSPS_CD;
IF PERSON_ID NOT = '';
RUN;

/* 검진DB - 2016년 ~ 2017년 DB 불러오기 */
DATA screening_16_17;
SET '/userdata12/room115/data_source/1619/nhis_heals_gj_1617.sas7bdat';
KEEP PERSON_ID HME_DT SMK_STAT_TYPE_RSPS_CD PAST_SMK_TERM_RSPS_CD PAST_DSQTY_RSPS_CD CUR_SMK_TERM_RSPS_CD CUR_DSQTY_RSPS_CD;
IF PERSON_ID NOT = '';
RUN;

/* 생애전환기검진DB - 2007년 ~ 2008년 DB 불러오기 */
DATA ex_screening_08;
SET '/userdata12/room115/data_out/data_store/ex_screening_08.sas7bdat';
IF PERSON_ID NOT = '';
KEEP PERSON_ID HME_DT SMK_STAT_TYPE_RSPS_CD PAST_SMK_TERM_RSPS_CD PAST_DSQTY_RSPS_CD CUR_SMK_TERM_RSPS_CD CUR_DSQTY_RSPS_CD;
run;

/* 생애전환기검진DB - 2009년 ~ 2015년 DB 불러오기 */
DATA ex_screening_09_15;
SET '/userdata12/room115/data_out/data_store/ex_screening_09_15.sas7bdat';
IF PERSON_ID NOT = '';
KEEP PERSON_ID HME_DT SMK_STAT_TYPE_RSPS_CD PAST_SMK_TERM_RSPS_CD PAST_DSQTY_RSPS_CD CUR_SMK_TERM_RSPS_CD CUR_DSQTY_RSPS_CD;
run;

/* 생애전환기검진DB - 2016년 ~ 2017년 DB 불러오기 */
DATA ex_screening_16_17;
SET '/userdata12/room115/data_source/1619/nhis_heals_lj_1617.sas7bdat';
IF PERSON_ID NOT = '';
KEEP PERSON_ID HME_DT SMK_STAT_TYPE_RSPS_CD PAST_SMK_TERM_RSPS_CD PAST_DSQTY_RSPS_CD CUR_SMK_TERM_RSPS_CD CUR_DSQTY_RSPS_CD;
RUN;

/***** ~08년도 *****/
/*** 검진DB ***/
/*** 과거 흡연자 대상 ***/
DATA screening_08_PAST;
SET screening_08;
IF SMK_STAT_TYPE_RSPS_CD='2';
IF (SMK_TERM_RSPS_CD='1') THEN PAST_SMK_TERM=2.5;
ELSE IF (SMK_TERM_RSPS_CD='2') THEN PAST_SMK_TERM=7;
ELSE IF (SMK_TERM_RSPS_CD='3') THEN PAST_SMK_TERM=14.5;
ELSE IF (SMK_TERM_RSPS_CD='4') THEN PAST_SMK_TERM=24.5;
ELSE PAST_SMK_TERM=30;
CUR_SMK_TERM=99;
PAST_DSQTY=.;
PAST_PACK_YEAR=.;
CUR_DSQTY=99;
CUR_PACK_YEAR=99;
DROP SMK_TERM_RSPS_CD DSQTY_RSPS_CD;
RUN;

/*** 현재 흡연자 대상 ***/
DATA screening_08_CUR;
SET screening_08;
IF SMK_STAT_TYPE_RSPS_CD='3';
IF (SMK_TERM_RSPS_CD='1') THEN CUR_SMK_TERM=2.5;
ELSE IF (SMK_TERM_RSPS_CD='2') THEN CUR_SMK_TERM=7;
ELSE IF (SMK_TERM_RSPS_CD='3') THEN CUR_SMK_TERM=14.5;
ELSE IF (SMK_TERM_RSPS_CD='4') THEN CUR_SMK_TERM=24.5;
ELSE CUR_SMK_TERM=30;

IF DSQTY_RSPS_CD='1' THEN CUR_DSQTY=0.0625;
ELSE IF DSQTY_RSPS_CD='2' THEN CUR_DSQTY=0.529;
ELSE IF DSQTY_RSPS_CD='3' THEN CUR_DSQTY=2.183;
ELSE CUR_DSQTY=4;
CUR_PACK_YEAR=CUR_SMK_TERM*CUR_DSQTY;

PAST_SMK_TERM=99;
PAST_DSQTY=99;
PAST_PACK_YEAR=99;
DROP SMK_TERM_RSPS_CD DSQTY_RSPS_CD;
RUN;

DATA screening_08_NEVER;
SET screening_08;
IF SMK_STAT_TYPE_RSPS_CD='1';
CUR_SMK_TERM=0;
CUR_DSQTY=0;
CUR_PACK_YEAR=0;
PAST_SMK_TERM=0;
PAST_DSQTY=0;
PAST_PACK_YEAR=0;
DROP SMK_TERM_RSPS_CD DSQTY_RSPS_CD;
RUN;

DATA screening_08_SMK;
SET screening_08_NEVER screening_08_PAST screening_08_CUR;
RUN;

DATA ex_screening_08_2;
SET ex_screening_08;
RENAME PAST_SMK_TERM_RSPS_CD = PAST_SMK_TERM;
RENAME CUR_SMK_TERM_RSPS_CD = CUR_SMK_TERM;
RENAME PAST_DSQTY_RSPS_CD = PAST_DSQTY;
RENAME CUR_DSQTY_RSPS_CD = CUR_DSQTY;
RUN;

DATA ex_screening_08_SMK;
SET ex_screening_08_2;
PAST_DSQTY = PAST_DSQTY/20;
CUR_DSQTY = CUR_DSQTY/20;
PAST_PACK_YEAR=PAST_SMK_TERM*PAST_DSQTY;
CUR_PACK_YEAR=CUR_SMK_TERM*CUR_DSQTY;

IF SMK_STAT_TYPE_RSPS_CD='1' THEN DO;
	PAST_SMK_TERM=0;
	CUR_SMK_TERM=0;
	PAST_DSQTY=0;
	CUR_DSQTY=0;
	PAST_PACK_YEAR=0;
	CUR_PACK_YEAR=0;
END;

IF SMK_STAT_TYPE_RSPS_CD='2' THEN DO;
	CUR_SMK_TERM=99;
	CUR_DSQTY=99;
	CUR_PACK_YEAR=99;
END;

IF SMK_STAT_TYPE_RSPS_CD='3' THEN DO;
	PAST_SMK_TERM=99;
	PAST_DSQTY=99;
	PAST_PACK_YEAR=99;
END;
RUN;

/*** 09~15년도 ***/
DATA total_screening_09_15;
SET screening_09_15 ex_screening_09_15;
RUN;

DATA total_screening_09_15_2;
SET total_screening_09_15;
RENAME PAST_SMK_TERM_RSPS_CD = PAST_SMK_TERM;
RENAME CUR_SMK_TERM_RSPS_CD = CUR_SMK_TERM;
RENAME PAST_DSQTY_RSPS_CD = PAST_DSQTY;
RENAME CUR_DSQTY_RSPS_CD = CUR_DSQTY;
RUN;

DATA total_screening_09_15_SMK;
SET total_screening_09_15_2;
PAST_DSQTY = PAST_DSQTY/20;
CUR_DSQTY = CUR_DSQTY/20;
PAST_PACK_YEAR=PAST_SMK_TERM*PAST_DSQTY;
CUR_PACK_YEAR=CUR_SMK_TERM*CUR_DSQTY;

IF SMK_STAT_TYPE_RSPS_CD='1' THEN DO;
	PAST_SMK_TERM=0;
	CUR_SMK_TERM=0;
	PAST_DSQTY=0;
	CUR_DSQTY=0;
	PAST_PACK_YEAR=0;
	CUR_PACK_YEAR=0;
END;

IF SMK_STAT_TYPE_RSPS_CD='2' THEN DO;
	CUR_SMK_TERM=99;
	CUR_DSQTY=99;
	CUR_PACK_YEAR=99;
END;

IF SMK_STAT_TYPE_RSPS_CD='3' THEN DO;
	PAST_SMK_TERM=99;
	PAST_DSQTY=99;
	PAST_PACK_YEAR=99;
END;
RUN;

/*** 16~17년도 ***/
DATA total_screening_16_17;
SET screening_16_17 ex_screening_16_17;
SMK_STAT_TYPE_RSPS_CD2=PUT(SMK_STAT_TYPE_RSPS_CD,1.);
DROP SMK_STAT_TYPE_RSPS_CD;
RUN;

DATA total_screening_16_17_2;
SET total_screening_16_17;
RENAME SMK_STAT_TYPE_RSPS_CD2=SMK_STAT_TYPE_RSPS_CD;
RENAME PAST_SMK_TERM_RSPS_CD = PAST_SMK_TERM;
RENAME CUR_SMK_TERM_RSPS_CD = CUR_SMK_TERM;
RENAME PAST_DSQTY_RSPS_CD = PAST_DSQTY;
RENAME CUR_DSQTY_RSPS_CD = CUR_DSQTY;
RUN;

DATA total_screening_16_17_SMK;
SET total_screening_16_17_2;
PAST_DSQTY = PAST_DSQTY/20;
CUR_DSQTY = CUR_DSQTY/20;
PAST_PACK_YEAR=PAST_SMK_TERM*PAST_DSQTY;
CUR_PACK_YEAR=CUR_SMK_TERM*CUR_DSQTY;

IF SMK_STAT_TYPE_RSPS_CD='1' THEN DO;
	PAST_SMK_TERM=0;
	CUR_SMK_TERM=0;
	PAST_DSQTY=0;
	CUR_DSQTY=0;
	PAST_PACK_YEAR=0;
	CUR_PACK_YEAR=0;
END;

IF SMK_STAT_TYPE_RSPS_CD='2' THEN DO;
	CUR_SMK_TERM=99;
	CUR_DSQTY=99;
	CUR_PACK_YEAR=99;
END;

IF SMK_STAT_TYPE_RSPS_CD='3' THEN DO;
	PAST_SMK_TERM=99;
	PAST_DSQTY=99;
	PAST_PACK_YEAR=99;
END;
RUN;

DATA total_SMK;
SET screening_08_SMK ex_screening_08_SMK total_screening_09_15_SMK total_screening_16_17_SMK;
RUN;

proc sql;
create table SMK_check as select * from total_study_pop, total_SMK
WHERE total_study_pop.PERSON_ID = total_SMK.PERSON_ID
AND total_study_pop.HME_DT >= total_SMK.HME_DT; 
quit;

PROC SORT DATA= SMK_check;
BY PERSON_ID HME_DT;
RUN;

DATA dir1.SMOKING_COV;
SET SMK_check;
BY PERSON_ID HME_DT;
IF last.PERSON_ID;
RUN; 

/*********************************/
/***** SODA covariate define *****/
/****** 구강검진DB 확인 ******/
DATA total_study_pop;
SET '/userdata12/room115/data_out/data_store/total_study_pop.sas7bdat';
RUN;

/****** 일반건강검진 02~13년 ******/
DATA screening_go_02_13;
SET '/userdata12/room115/data_source/nhis_heals_go.sas7bdat';
KEEP HCHK_YEAR PERSON_ID HME_DT OQ_Q04;
IF PERSON_ID NOT = '';
RUN;

/*** 일반구강검진 02~08년도만 SPLIT ***/
DATA screening_go_02_08;
SET screening_go_02_13;
IF (HCHK_YEAR>=2002) & (HCHK_YEAR<=2008);
IF OQ_Q04='' THEN OQ_Q04='9';
DROP HCHK_YEAR;
RUN;

/*** 구강 생애전환기검진(07~13년도) ***/
DATA ex_screening_go_07_13;
SET '/userdata12/room115/data_source/nhis_heals_lo.sas7bdat';
KEEP HCHK_YEAR PERSON_ID HME_DT OQ_QL05;
IF PERSON_ID NOT = '';
RUN;

/*** 생애구강검진 07~08년도만 SPLIT ***/
DATA ex_screening_go_07_08;
SET ex_screening_go_07_13;
IF (HCHK_YEAR>=2007) & (HCHK_YEAR<=2008);
IF OQ_QL05='' THEN OQ_QL05='9';
DROP HCHK_YEAR;
RUN;

/****** 구강검진DB 일반 + 생애(14~19년도) ******/
DATA screening_go_14_19;
SET '/userdata12/room115/data_source/1619/nhis_heals_go_1419.sas7bdat';
KEEP PERSON_ID HME_DT OQ_SODA_FRQ;
IF PERSON_ID NOT = '';
RUN;

DATA screening_go_14_19_2;
SET screening_go_14_19;
OQ_SODA_FRQ_2=PUT(OQ_SODA_FRQ,1.);
IF OQ_SODA_FRQ_2='' THEN OQ_SODA_FRQ_2=9;
DROP OQ_SODA_FRQ;
RUN;

DATA total_screening_go_14_19;
SET screening_go_14_19_2;
RENAME OQ_SODA_FRQ_2 = OQ_SODA_FRQ;
RUN ;

/*** SODA FRQ Covariate define ***/
DATA ex_screening_go_07_08_SODA;
SET ex_screening_go_07_08;
IF OQ_QL05='1' THEN SODA_FRQ=0;
ELSE IF OQ_QL05='2' THEN SODA_FRQ=1/6.5;
ELSE IF OQ_QL05='3' THEN SODA_FRQ=1/4.5;
ELSE IF OQ_QL05='4' THEN SODA_FRQ=1/2.5;
ELSE IF OQ_QL05='5' THEN SODA_FRQ=1;
ELSE IF OQ_QL05='6' THEN SODA_FRQ=2;
ELSE IF OQ_QL05='7' THEN SODA_FRQ=3;
ELSE SODA_FRQ=9;
DROP OQ_QL05;
RUN;

DATA screening_go_14_19_SODA;
SET total_screening_go_14_19;
IF OQ_SODA_FRQ='1' THEN SODA_FRQ=0;
ELSE IF OQ_SODA_FRQ='2' THEN SODA_FRQ=1;
ELSE IF OQ_SODA_FRQ='3' THEN SODA_FRQ=2.5;
ELSE IF OQ_SODA_FRQ='4' THEN SODA_FRQ=4;
ELSE SODA_FRQ=9;
DROP OQ_SODA_FRQ;
RUN;

DATA screening_go_07_19_SODA;
SET ex_screening_go_07_08_SODA screening_go_14_19_SODA;
RUN;

proc sql;
create table SODA_07_19 as select * from screening_go_07_19_SODA join total_study_pop
on (screening_go_07_19_SODA.PERSON_ID = total_study_pop.PERSON_ID)
and (screening_go_07_19_SODA.HME_DT <= total_study_pop.HME_DT);
quit;

/*** SODA FRQ Variable NA Imputation ***/
proc sort data=SODA_07_19;
by PERSON_ID HME_DT;
run;

DATA SODA_FRQ_NA;
SET SODA_07_19;
BY PERSON_ID HME_DT;
IF (SODA_FRQ=9) THEN DELETE;	 
/* After delete, there is no NA */
run;

DATA SODA_FRQ_Covariate_07_19;
SET SODA_FRQ_NA;
BY PERSON_ID HME_DT;
IF last.PERSON_ID;
RUN;


/*** SODA variable quantile check ***/
proc univariate data=SODA_FRQ_Covariate_07_19;
var soda_frq;
run;

/**** Imputation value check ****/
DATA SODA_FRQ_25;
SET SODA_FRQ_Covariate_07_19;
IF (SODA_FRQ<=0.153846) & (SODA_FRQ>=0);
RUN;

proc univariate data=SODA_FRQ_25;
var SODA_FRQ;
run;
/* 평균값이 0 */

DATA SODA_FRQ_75;
SET SODA_FRQ_Covariate_07_19;
IF (SODA_FRQ>=0.153846);
RUN;

proc univariate data=SODA_FRQ_75;
var SODA_FRQ;
run;
/* 평균값이 1.08 */

/*** 02 ~ 08년도 구강검진 기록 이용 ***/
DATA screening_go_02_08_SODA;
SET screening_go_02_08;
IF OQ_Q04='1' THEN SODA_FRQ=1.08;
ELSE IF (OQ_Q04='2') |  (OQ_Q04='3') THEN SODA_FRQ=0;
ELSE SODA_FRQ=9;
DROP OQ_Q04;
RUN;

proc sql;
create table SODA_FRQ_not as select PERSON_ID
from total_study_pop
except select PERSON_ID from SODA_FRQ_Covariate_07_19;
quit;

proc sql;
create table SODA_FRQ_not_DT as select  * from SODA_FRQ_not join total_study_pop
on SODA_FRQ_not.PERSON_ID = total_study_pop.PERSON_ID;
run;

PROC SQL;
create table SODA_02_08 as select * 
from screening_go_02_08_SODA join SODA_FRQ_not_DT 
on (screening_go_02_08_SODA.PERSON_ID = SODA_FRQ_not_DT.PERSON_ID)
and (screening_go_02_08_SODA.HME_DT <= SODA_FRQ_not_DT.HME_DT);
quit; 

DATA SODA_02_08_NA;
SET SODA_02_08;
IF SODA_FRQ=9 THEN DELETE;	 
run;

proc sort data=SODA_02_08_NA;
by PERSON_ID HME_DT;
run;

DATA SODA_02_08_rm_NA;
SET SODA_02_08_NA;
BY PERSON_ID HME_DT;
IF last.PERSON_ID;
RUN;

/* Record rbinding */
DATA SODA_FRQ_record;
SET SODA_FRQ_Covariate_07_19 SODA_02_08_rm_NA;
DROP HME_DT;
RUN;

/* total study pop & SODA_FRQ_record full join */
proc sql;
create table dir1.SODA_FRQ_Covariate as select total_study_pop.PERSON_ID, SODA_FRQ_record.SODA_FRQ
from total_study_pop FULL JOIN SODA_FRQ_record
on total_study_pop.PERSON_ID = SODA_FRQ_record.PERSON_ID;
quit; 


/*********************************/
/***** Snack covariate define *****/
/****** 구강검진DB 확인 ******/
DATA total_study_pop;
SET '/userdata12/room115/data_out/data_store/total_study_pop.sas7bdat';
RUN;

/****** 일반건강검진 02~13년 ******/
DATA screening_go_02_13;
SET '/userdata12/room115/data_source/nhis_heals_go.sas7bdat';
KEEP HCHK_YEAR PERSON_ID HME_DT OQ_Q06;
IF PERSON_ID NOT = '';
RUN;

/*** 일반구강검진 02~08년도만 SPLIT ***/
DATA screening_go_02_08;
SET screening_go_02_13;
IF (HCHK_YEAR>=2002) & (HCHK_YEAR<=2008);
IF OQ_Q06='' THEN OQ_Q06='9';
DROP HCHK_YEAR;
RUN;

/*** 구강 생애전환기검진(07~13년도) ***/
DATA ex_screening_go_07_13;
SET '/userdata12/room115/data_source/nhis_heals_lo.sas7bdat';
KEEP HCHK_YEAR PERSON_ID HME_DT OQ_QL06;
IF PERSON_ID NOT = '';
RUN;

/*** 생애구강검진 07~08년도만 SPLIT ***/
DATA ex_screening_go_07_08;
SET ex_screening_go_07_13;
IF (HCHK_YEAR>=2007) & (HCHK_YEAR<=2008);
IF OQ_QL06='' THEN OQ_QL06='9';
DROP HCHK_YEAR;
RUN;

/****** 구강검진DB 일반 + 생애(14~19년도) ******/
DATA screening_go_14_19;
SET '/userdata12/room115/data_source/1619/nhis_heals_go_1419.sas7bdat';
KEEP PERSON_ID HME_DT OQ_SNCK_FRQ;
IF PERSON_ID NOT = '';
RUN;

DATA screening_go_14_19_2;
SET screening_go_14_19;
OQ_SNCK_FRQ_2=PUT(OQ_SNCK_FRQ,1.);
IF OQ_SNCK_FRQ_2='' THEN OQ_SNCK_FRQ_2='9';
DROP OQ_SNCK_FRQ;
RUN;

DATA total_screening_go_14_19;
SET screening_go_14_19_2;
RENAME OQ_SNCK_FRQ_2 = OQ_SNCK_FRQ;
RUN ;

/*** SNCK FRQ Covariate define ***/
DATA ex_screening_go_07_08_SNCK;
SET ex_screening_go_07_08;
IF OQ_QL06='1' THEN SNCK_FRQ=0;
ELSE IF OQ_QL06='2' THEN SNCK_FRQ=1/6.5;
ELSE IF OQ_QL06='3' THEN SNCK_FRQ=1/4.5;
ELSE IF OQ_QL06='4' THEN SNCK_FRQ=1/2.5;
ELSE IF OQ_QL06='5' THEN SNCK_FRQ=1;
ELSE IF OQ_QL06='6' THEN SNCK_FRQ=2;
ELSE IF OQ_QL06='7' THEN SNCK_FRQ=3;
ELSE SNCK_FRQ=9;
DROP OQ_QL06;
RUN;

DATA screening_go_14_19_SNCK;
SET total_screening_go_14_19;
IF OQ_SNCK_FRQ='1' THEN SNCK_FRQ=0;
ELSE IF OQ_SNCK_FRQ='2' THEN SNCK_FRQ=1;
ELSE IF OQ_SNCK_FRQ='3' THEN SNCK_FRQ=2.5;
ELSE IF OQ_SNCK_FRQ='4' THEN SNCK_FRQ=4;
ELSE SNCK_FRQ=9;
DROP OQ_SNCK_FRQ;
RUN;

DATA screening_go_07_19_SNCK;
SET ex_screening_go_07_08_SNCK screening_go_14_19_SNCK;
RUN;

proc sql;
create table SNCK_07_19 as select * from screening_go_07_19_SNCK join total_study_pop
on (screening_go_07_19_SNCK.PERSON_ID = total_study_pop.PERSON_ID)
and (screening_go_07_19_SNCK.HME_DT <= total_study_pop.HME_DT);
quit;

/*** SODA FRQ Variable NA Imputation ***/
proc sort data=SNCK_07_19;
by PERSON_ID HME_DT;
run;

DATA SNCK_FRQ_NA;
SET SNCK_07_19;
BY PERSON_ID HME_DT;
IF (SNCK_FRQ=9) THEN DELETE;	 
/* After delete, there is no NA */
run;

DATA SNCK_FRQ_Covariate_07_19;
SET SNCK_FRQ_NA;
BY PERSON_ID HME_DT;
IF last.PERSON_ID;
RUN;


/*** SNCK variable quantile check ***/
proc univariate data=SNCK_FRQ_Covariate_07_19;
var SNCK_FRQ;
run;

/**** Imputation value check ****/
DATA SNCK_FRQ_25;
SET SNCK_FRQ_Covariate_07_19;
IF (SNCK_FRQ<=1) & (SNCK_FRQ>=0);
RUN;

proc univariate data=SNCK_FRQ_25;
var SNCK_FRQ;
run;
/* 평균값이 0.255 */

DATA SNCK_FRQ_75;
SET SNCK_FRQ_Covariate_07_19;
IF (SNCK_FRQ>=1);
RUN;

proc univariate data=SNCK_FRQ_75;
var SNCK_FRQ;
run;
/* 평균값이 1.403 */

/*** 02 ~ 08년도 구강검진 기록 이용 ***/
DATA screening_go_02_08_SNCK;
SET screening_go_02_08;
IF OQ_Q06='1' THEN SNCK_FRQ=1.403;
ELSE IF (OQ_Q06='2') THEN SNCK_FRQ=0;
ELSE IF (OQ_Q06='3') THEN SNCK_FRQ=0.255;
ELSE SNCK_FRQ=9;
DROP OQ_Q06;
RUN;

proc sql;
create table SNCK_FRQ_not as select PERSON_ID
from total_study_pop
except select PERSON_ID from SNCK_FRQ_Covariate_07_19;
quit;

proc sql;
create table SNCK_FRQ_not_DT as select  * from SNCK_FRQ_not join total_study_pop
on SNCK_FRQ_not.PERSON_ID = total_study_pop.PERSON_ID;
run;

PROC SQL;
create table SNCK_02_08 as select * 
from screening_go_02_08_SNCK join SNCK_FRQ_not_DT 
on (screening_go_02_08_SNCK.PERSON_ID = SNCK_FRQ_not_DT.PERSON_ID)
and (screening_go_02_08_SNCK.HME_DT <= SNCK_FRQ_not_DT.HME_DT);
quit; 

DATA SNCK_02_08_NA;
SET SNCK_02_08;
IF SNCK_FRQ=9 THEN DELETE;	 
run;

proc sort data=SNCK_02_08_NA;
by PERSON_ID HME_DT;
run;

DATA SNCK_02_08_rm_NA;
SET SNCK_02_08_NA;
BY PERSON_ID HME_DT;
IF last.PERSON_ID;
RUN;

/* Record rbinding */
DATA SNCK_FRQ_record;
SET SNCK_FRQ_Covariate_07_19 SNCK_02_08_rm_NA;
DROP HME_DT;
RUN;

/* total study pop & SNCK_FRQ_record full join */
proc sql;
create table dir1.SNCK_FRQ_Covariate as select total_study_pop.PERSON_ID, SNCK_FRQ_record.SNCK_FRQ
from total_study_pop FULL JOIN SNCK_FRQ_record
on total_study_pop.PERSON_ID = SNCK_FRQ_record.PERSON_ID;
quit;


/**********************************/
/** FRUIT, VEGETABLE FRQ Define **/
DATA total_study_pop;
SET '/userdata12/room115/data_out/data_store/total_study_pop.sas7bdat';
RUN;

/*** 구강 생애전환기검진(07~13년도) ***/
DATA ex_screening_go_07_13;
SET '/userdata12/room115/data_source/nhis_heals_lo.sas7bdat';
KEEP HCHK_YEAR PERSON_ID HME_DT OQ_QL04;
IF PERSON_ID NOT = '';
RUN;

/*** 생애구강검진 07~08년도만 SPLIT ***/
DATA ex_screening_go_07_08;
SET ex_screening_go_07_13;
IF (HCHK_YEAR>=2007) & (HCHK_YEAR<=2008);
IF OQ_QL04='' THEN OQ_QL04='9';
DROP HCHK_YEAR;
RUN;

/*** FRUIT VEGETABLE FRQ Covariate define ***/
DATA ex_screening_go_07_08_FRUIT_VEG;
SET ex_screening_go_07_08;
IF OQ_QL04='1' THEN FRUIT_VEG_FRQ=0;
ELSE IF OQ_QL04='2' THEN FRUIT_VEG_FRQ=1/6.5;
ELSE IF OQ_QL04='3' THEN FRUIT_VEG_FRQ=1/4.5;
ELSE IF OQ_QL04='4' THEN FRUIT_VEG_FRQ=1/2.5;
ELSE IF OQ_QL04='5' THEN FRUIT_VEG_FRQ=1;
ELSE IF OQ_QL04='6' THEN FRUIT_VEG_FRQ=2;
ELSE IF OQ_QL04='7' THEN FRUIT_VEG_FRQ=3;
ELSE FRUIT_VEG_FRQ=9;
DROP OQ_QL04;
RUN;

proc sql;
create table FRUIT_VEG_07_08 as select * from ex_screening_go_07_08_FRUIT_VEG 
join total_study_pop 
on ex_screening_go_07_08_FRUIT_VEG.PERSON_ID = total_study_pop.PERSON_ID
and ex_screening_go_07_08_FRUIT_VEG.HME_DT <= total_study_pop.HME_DT;
quit;

proc sort data=FRUIT_VEG_07_08;
by PERSON_ID HME_DT;
run;

DATA FRUIT_VEG_FRQ;
SET FRUIT_VEG_07_08;
BY PERSON_ID HME_DT;
IF FRUIT_VEG_FRQ = 9 THEN DELETE;
IF last.PERSON_ID;
run;

/* total study pop & FRUIT_VEG_FRQ_record full join */
proc sql;
create table dir1.FRUIT_VEG_FRQ_Covariate as select total_study_pop.PERSON_ID, FRUIT_VEG_FRQ.FRUIT_VEG_FRQ
from total_study_pop FULL JOIN FRUIT_VEG_FRQ
on total_study_pop.PERSON_ID = FRUIT_VEG_FRQ.PERSON_ID;
quit;


/************************************/
/**** 개인과거병력 Covariate Define ****/
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
case when MAIN_SICK like "I21%" or SUB_SICK like "I21%" then 1 else 0 end as MI,
case when ((MAIN_SICK like "I50%") or (MAIN_SICK in ("I110", "I130", "I132"))) or 
(SUB_SICK like "I50%") or (SUB_SICK in ("I110", "I130", "I132")) then 1 else 0 end as Heart_failure,
case when MAIN_SICK like "G460%" or MAIN_SICK like "G461%" or  MAIN_SICK like "G462%"
or MAIN_SICK like "G463%" or MAIN_SICK like "G464%" or MAIN_SICK like "G465%" or MAIN_SICK like "G466%"
or MAIN_SICK like "G467%" or MAIN_SICK like "G468%" or MAIN_SICK like "I60%" or MAIN_SICK like "I61%"
or MAIN_SICK like "I62%" or MAIN_SICK like "I63%" or MAIN_SICK like "I64%" or MAIN_SICK like "G45%"
or SUB_SICK like "G460%" or SUB_SICK like "G461%" or  SUB_SICK like "G462%"
or SUB_SICK like "G463%" or SUB_SICK like "G464%" or SUB_SICK like "G465%" or SUB_SICK like "G466%"
or SUB_SICK like "G467%" or SUB_SICK like "G468%" or SUB_SICK like "I60%" or SUB_SICK like "I61%"
or SUB_SICK like "I62%" or SUB_SICK like "I63%" or SUB_SICK like "I64%" or SUB_SICK like "G45%" 
then 1 else 0 end as TIA_Stroke_1, 
case when ((MAIN_SICK like "C%") and (MAIN_SICK not like "C98%") and (MAIN_SICK not like "C99%")) or
 ((SUB_SICK like "C%") and (SUB_SICK not like "C98%") and (SUB_SICK not like "C99%")) then 1 else 0 end as Cancer,
case when MAIN_SICK like "I10%" or MAIN_SICK like "I11%" or MAIN_SICK like "I12%"
or MAIN_SICK like "I13%"  or MAIN_SICK like "I14%" or MAIN_SICK like "I15%" 
or SUB_SICK like "I10%" or SUB_SICK like "I11%" or SUB_SICK like "I12%"
or SUB_SICK like "I13%"  or SUB_SICK like "I14%" or SUB_SICK like "I15%" 
then 1 else 0 end as Hypertension
from T20_study_pop;
quit; 

/* Cancer variable 정리 */
DATA temp2;
SET temp;
IF MAIN_SICK = "C_" OR SUB_SICK = "C_" then Cancer=0;
RUN;

/* 변수값 정리 */
proc sql;
create table result as select PERSON_ID, SUM(MI) as MI_result, SUM(Heart_failure) as Heart_failure_1,
SUM(TIA_Stroke_1) as TIA_Stroke_result, SUM(Cancer) as Cancer_result, SUM(Hypertension) as Hypertension_result
from temp2
group by PERSON_ID;
QUIT;

DATA T20_case_history_covariate;
SET result;
IF MI_result>=1 then MI_result=1; 
else MI_result=0;
IF Heart_failure_1>=1 then Heart_failure_1=1; 
else Heart_failure_1=0;
IF TIA_Stroke_result>=1 then TIA_Stroke_result=1; 
else TIA_Stroke_result=0;
IF Cancer_result>=1 then Cancer_result=1; 
else Cancer_result=0;
IF Hypertension_result>=1 then Hypertension_result=1; 
else Hypertension_result=0;
run;

/* 검진DB - 2002년 ~ 2008년 DB 불러오기 */
DATA screening_08;
SET '/userdata12/room115/data_out/data_store/screening_08.sas7bdat';
KEEP PERSON_ID HME_DT HCHK_PMH_CD1 HCHK_PMH_CD2 HCHK_PMH_CD3; 
RUN;

/* 검진DB - 2009년 ~ 2015년 DB 불러오기 */
DATA screening_09_15;
SET '/userdata12/room115/data_out/data_store/screening_09_15.sas7bdat';
KEEP PERSON_ID HME_DT HCHK_APOP_PMH_YN HCHK_HPRTS_PMH_YN;
IF PERSON_ID NOT = '';
RUN;

/* 검진DB - 2016년 ~ 2017년 DB 불러오기 */
DATA screening_16_17;
SET '/userdata12/room115/data_source/1619/nhis_heals_gj_1617.sas7bdat';
KEEP PERSON_ID HME_DT HCHK_APOP_PMH_YN HCHK_HPRTS_PMH_YN;
IF PERSON_ID NOT = '';
RUN;

/* 생애전환기검진DB - 2007년 ~ 2008년 DB 불러오기 */
DATA ex_screening_08;
SET '/userdata12/room115/data_out/data_store/ex_screening_08.sas7bdat';
IF PERSON_ID NOT = '';
KEEP PERSON_ID HME_DT HCHK_APOP_PMH_YN HCHK_HPRTS_PMH_YN;
run;

/* 생애전환기검진DB - 2009년 ~ 2015년 DB 불러오기 */
DATA ex_screening_09_15;
SET '/userdata12/room115/data_out/data_store/ex_screening_09_15.sas7bdat';
IF PERSON_ID NOT = '';
KEEP PERSON_ID HME_DT HCHK_APOP_PMH_YN HCHK_HPRTS_PMH_YN;
run;

/* 생애전환기검진DB - 2016년 ~ 2017년 DB 불러오기 */
DATA ex_screening_16_17;
SET '/userdata12/room115/data_source/1619/nhis_heals_lj_1617.sas7bdat';
IF PERSON_ID NOT = '';
KEEP PERSON_ID HME_DT HCHK_APOP_PMH_YN HCHK_HPRTS_PMH_YN;
RUN;

/* 건강검진DB merge */
DATA total_screening_09_15;
SET screening_09_15 ex_screening_09_15;
run;

DATA total_screening_16_17;
SET screening_16_17 ex_screening_16_17;
run;

DATA screening_history_08;
SET screening_08;
IF HCHK_PMH_CD1='4' or HCHK_PMH_CD2='4' or HCHK_PMH_CD3='4' then Hypertension_2=1;
ELSE Hypertension_2=0;
IF HCHK_PMH_CD1='6' or HCHK_PMH_CD2='6' or HCHK_PMH_CD3='6' then TIA_Stroke_2=1;
ELSE TIA_Stroke_2=0;
DROP HCHK_PMH_CD1 HCHK_PMH_CD2 HCHK_PMH_CD3;
run;

DATA ex_screening_history_08;
SET ex_screening_08;
IF HCHK_APOP_PMH_YN="2" then TIA_Stroke_2=1;
ELSE TIA_Stroke_2=0;
IF HCHK_HPRTS_PMH_YN="2" then Hypertension_2=1;
ELSE Hypertension_2=0;
DROP HCHK_APOP_PMH_YN HCHK_HPRTS_PMH_YN;
run;

DATA screening_history_09_15;
SET total_screening_09_15;
IF HCHK_APOP_PMH_YN="1" then TIA_Stroke_2=1;
ELSE TIA_Stroke_2=0;
IF HCHK_HPRTS_PMH_YN="1" then Hypertension_2=1;
ELSE Hypertension_2=0;
DROP HCHK_APOP_PMH_YN HCHK_HPRTS_PMH_YN;
run;

DATA screening_history_16_17;
SET total_screening_16_17;
IF HCHK_APOP_PMH_YN=1 then TIA_Stroke_2=1;
ELSE TIA_Stroke_2=0;
IF HCHK_HPRTS_PMH_YN=1 then Hypertension_2=1;
ELSE Hypertension_2=0;
DROP HCHK_APOP_PMH_YN HCHK_HPRTS_PMH_YN;
run;

DATA screening_history_all;
SET screening_history_08 ex_screening_history_08 screening_history_09_15 screening_history_16_17;
run;

proc sql;
create table screening_case_history as select * from screening_history_all join total_study_pop
on screening_history_all.PERSON_ID = total_study_pop.PERSON_ID
and screening_history_all.HME_DT <= total_study_pop.HME_DT;
quit;

proc sql;
create table result2 as select PERSON_ID, SUM(Hypertension_2) as Hypertension_result2, 
SUM(TIA_Stroke_2) as TIA_Stroke_result2
from screening_case_history
group by PERSON_ID;
quit;

proc sort data=T20_case_history_covariate;
by PERSON_ID;
run;

proc sort data=result2;
by PERSON_ID;
run;

DATA case_history_total;
MERGE T20_case_history_covariate result2;
by PERSON_ID;
run;

DATA dir1.case_history_covariate;
SET case_history_total;
IF Hypertension_result=1 or Hypertension_result2>=1 then Hypertension_case=1;
ELSE Hypertension_case=0;
IF TIA_Stroke_result=1 or TIA_Stroke_result2>=1 then TIA_Stroke_case=1;
ELSE TIA_Stroke_case=0;
RENAME MI_result=MI_case Cancer_result=Cancer_case Heart_failure_1=Heart_failure_case;
DROP Hypertension_result Hypertension_result2 TIA_Stroke_result TIA_Stroke_result2;
run;
