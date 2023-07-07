/* ----------------------------------------
SAS Enterprise Guide에서 가져온 코드
DATE: 2023년 1월 10일 화요일     TIME: 오후 2:45:44
PROJECT: 프로젝트code
PROJECT PATH: C:\Users\ROOM115_ilojeyou98\프로젝트code.egp
---------------------------------------- */

/* ---------------------------------- */
/* MACRO: enterpriseguide             */
/* PURPOSE: define a macro variable   */
/*   that contains the file system    */
/*   path of the WORK library on the  */
/*   server.  Note that different     */
/*   logic is needed depending on the */
/*   server type.                     */
/* ---------------------------------- */
%macro enterpriseguide;
%global sasworklocation;
%local tempdsn unique_dsn path;

%if &sysscp=OS %then %do; /* MVS Server */
	%if %sysfunc(getoption(filesystem))=MVS %then %do;
        /* By default, physical file name will be considered a classic MVS data set. */
	    /* Construct dsn that will be unique for each concurrent session under a particular account: */
		filename egtemp '&egtemp' disp=(new,delete); /* create a temporary data set */
 		%let tempdsn=%sysfunc(pathname(egtemp)); /* get dsn */
		filename egtemp clear; /* get rid of data set - we only wanted its name */
		%let unique_dsn=".EGTEMP.%substr(&tempdsn, 1, 16).PDSE"; 
		filename egtmpdir &unique_dsn
			disp=(new,delete,delete) space=(cyl,(5,5,50))
			dsorg=po dsntype=library recfm=vb
			lrecl=8000 blksize=8004 ;
		options fileext=ignore ;
	%end; 
 	%else %do; 
        /* 
		By default, physical file name will be considered an HFS 
		(hierarchical file system) file. 
		*/
		%if "%sysfunc(getoption(filetempdir))"="" %then %do;
			filename egtmpdir '/tmp';
		%end;
		%else %do;
			filename egtmpdir "%sysfunc(getoption(filetempdir))";
		%end;
	%end; 
	%let path=%sysfunc(pathname(egtmpdir));
    %let sasworklocation=%sysfunc(quote(&path));  
%end; /* MVS Server */
%else %do;
	%let sasworklocation = "%sysfunc(getoption(work))/";
%end;
%if &sysscp=VMS_AXP %then %do; /* Alpha VMS server */
	%let sasworklocation = "%sysfunc(getoption(work))";                         
%end;
%if &sysscp=CMS %then %do; 
	%let path = %sysfunc(getoption(work));                         
	%let sasworklocation = "%substr(&path, %index(&path,%str( )))";
%end;
%mend enterpriseguide;

%enterpriseguide


/* Conditionally delete set of tables or views, if they exists          */
/* If the member does not exist, then no action is performed   */
%macro _eg_conditional_dropds /parmbuff;
	
   	%local num;
   	%local stepneeded;
   	%local stepstarted;
   	%local dsname;
	%local name;

   	%let num=1;
	/* flags to determine whether a PROC SQL step is needed */
	/* or even started yet                                  */
	%let stepneeded=0;
	%let stepstarted=0;
   	%let dsname= %qscan(&syspbuff,&num,',()');
	%do %while(&dsname ne);	
		%let name = %sysfunc(left(&dsname));
		%if %qsysfunc(exist(&name)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;

			%end;
				drop table &name;
		%end;

		%if %sysfunc(exist(&name,view)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;
			%end;
				drop view &name;
		%end;
		%let num=%eval(&num+1);
      	%let dsname=%qscan(&syspbuff,&num,',()');
	%end;
	%if &stepstarted %then %do;
		quit;
	%end;
%mend _eg_conditional_dropds;


/* save the current settings of XPIXELS and YPIXELS */
/* so that they can be restored later               */
%macro _sas_pushchartsize(new_xsize, new_ysize);
	%global _savedxpixels _savedypixels;
	options nonotes;
	proc sql noprint;
	select setting into :_savedxpixels
	from sashelp.vgopt
	where optname eq "XPIXELS";
	select setting into :_savedypixels
	from sashelp.vgopt
	where optname eq "YPIXELS";
	quit;
	options notes;
	GOPTIONS XPIXELS=&new_xsize YPIXELS=&new_ysize;
%mend _sas_pushchartsize;

/* restore the previous values for XPIXELS and YPIXELS */
%macro _sas_popchartsize;
	%if %symexist(_savedxpixels) %then %do;
		GOPTIONS XPIXELS=&_savedxpixels YPIXELS=&_savedypixels;
		%symdel _savedxpixels / nowarn;
		%symdel _savedypixels / nowarn;
	%end;
%mend _sas_popchartsize;


ODS PROCTITLE;
OPTIONS DEV=PNG;
GOPTIONS XPIXELS=0 YPIXELS=0;
FILENAME EGSRX TEMP;
ODS tagsets.sasreport13(ID=EGSRX) FILE=EGSRX
    STYLE=HtmlBlue
    STYLESHEET=(URL="file:///C:/Program%20Files/SASHome/SASEnterpriseGuide/7.1/Styles/HtmlBlue.css")
    NOGTITLE
    NOGFOOTNOTE
    GPATH=&sasworklocation
    ENCODING=UTF8
    options(rolap="on")
;

/*   노드 시작: Study_pop_define   */
%LET _CLIENTTASKLABEL='Study_pop_define';
%LET _CLIENTPROCESSFLOWNAME='프로세스 플로우';
%LET _CLIENTPROJECTPATH='C:\Users\ROOM115_ilojeyou98\프로젝트code.egp';
%LET _CLIENTPROJECTNAME='프로젝트code.egp';
%LET _SASPROGRAMFILE=;

GOPTIONS ACCESSIBLE;

/*SAS table 저장 경로 지정 */
libname dir1 "/userdata12/room115/data_out/data_store";
run;

/* 검진DB - 2002년 ~ 2015년 DB 불러오기 */
DATA screening1;
SET '/userdata12/room115/data_source/nhis_heals_gj.sas7bdat';
KEEP HCHK_YEAR PERSON_ID HME_DT BMI WAIST BLDS HCHK_PMH_CD1 HCHK_PMH_CD2 HCHK_PMH_CD3 HCHK_DIABML_PMH_YN;
run;

/* unique한 PERSON_ID 가져오기 */
proc sql;
create table screening1_uniq as select distinct PERSON_ID from screening1;
quit;

/* 검진DB - 2016년 ~ 2017년 DB 불러오기 */
DATA screening2;
SET '/userdata12/room115/data_source/1619/nhis_heals_gj_1617.sas7bdat';
KEEP HCHK_YEAR PERSON_ID HME_DT BMI WAIST BLDS HCHK_DIABML_PMH_YN;
RUN;

/* unique한 PERSON_ID 가져오기 */
proc sql;
create table screening2_uniq as select distinct PERSON_ID from screening2;
quit;

/* 검진DB - 2018년 ~ 2019년 DB 불러오기 */
DATA screening_19;
SET '/userdata12/room115/data_source/1619/nhis_heals_gj_1819.sas7bdat';
IF PERSON_ID NOT = '';
KEEP HCHK_YEAR PERSON_ID HME_DT BMI WAIST BLDS HCHK_DIABML_PMH_YN;
RUN;
/* unique한 PERSON_ID 가져오기 */
proc sql;
create table screening_19_uniq as select distinct PERSON_ID from screening_19;
quit;

/****************************************************************************************/
/* screening1에서 2002년 ~ 2008년까지만 가져오기*/
DATA screening_08;
SET screening1;
IF (HCHK_YEAR<=2008) and (PERSON_ID NOT = '');
RUN;

/* screening1에서 2008년 ~ 2015년까지만 가져오기*/
DATA screening_09_15;
SET screening1;
IF HCHK_YEAR>2008;
HCHK_DIABML_PMH_YN2 = INPUT(HCHK_DIABML_PMH_YN, 3.);
DROP HCHK_PMH_CD1 HCHK_PMH_CD2 HCHK_PMH_CD3 HCHK_DIABML_PMH_YN;
RUN;

/* RENAME*/
DATA screening_09_15_2;
SET screening_09_15;
RENAME HCHK_DIABML_PMH_YN2 = HCHK_DIABML_PMH_YN;
RUN;

/* 두 데이터 합치기*/
DATA screening_17;
SET screening_09_15_2 screening2;
IF PERSON_ID NOT = '';
RUN;

/* 2002년 ~ 2008년 기록 : BMI or WAIST or BLDS가 적어도 하나 이상 있는 사람의 PERSON_ID 추출 */
proc sql;
create table include1_08 as select distinct PERSON_ID from screening_08
where not BMI in (.) or not WAIST in (.) or not BLDS in (.);
quit;
/* 2009년 ~ 2017년 기록 : BMI or WAIST or BLDS가 적어도 하나 이상 있는 사람의 PERSON_ID 추출 */
proc sql;
create table include1_17 as select distinct PERSON_ID from screening_17
where not BMI in (.) or not WAIST in (.) or not BLDS in (.);
quit;
/* 2018년 ~ 2019년 기록 : BMI or WAIST or BLDS가 적어도 하나 이상 있는 사람의 PERSON_ID 추출 */
proc sql;
create table include1_19 as select distinct PERSON_ID from screening_19
where not BMI in (.) or not WAIST in (.) or not BLDS in (.);
quit;

/********************************************************************************************/
/* 생애전환기검진DB - 2007년 ~ 2015년 DB 불러오기 */
DATA ex_screening1;
SET '/userdata12/room115/data_source/nhis_heals_lj.sas7bdat';
IF PERSON_ID NOT = '';
KEEP HCHK_YEAR PERSON_ID HME_DT BMI WAIST BLDS HCHK_DIABML_PMH_YN;
run;
DATA ex_screening_15;
SET ex_screening1;
HCHK_DIABML_PMH_YN2 = INPUT(HCHK_DIABML_PMH_YN, 8.);
DROP HCHK_DIABML_PMH_YN;
RENAME HCHK_DIABML_PMH_YN2 = HCHK_DIABML_PMH_YN;
run;
/* unique한 PERSON_ID 가져오기 */
proc sql;
create table ex_screening1_uniq as select distinct PERSON_ID from ex_screening1;
quit;

/* 생애전환기검진DB - 2016년 ~ 2017년 DB 불러오기 */
DATA ex_screening2;
SET '/userdata12/room115/data_source/1619/nhis_heals_lj_1617.sas7bdat';
IF PERSON_ID NOT = '';
KEEP HCHK_YEAR PERSON_ID HME_DT BMI WAIST BLDS HCHK_DIABML_PMH_YN;
RUN;
/* unique한 PERSON_ID 가져오기 */
proc sql;
create table ex_screening2_uniq as select distinct PERSON_ID from ex_screening2;
quit;

/* 2007년 ~ 2015년 기록 : BMI or WAIST or BLDS가 적어도 하나 이상 있는 사람의 PERSON_ID 추출 */
proc sql;
create table include2_15 as select distinct PERSON_ID from ex_screening1
where not BMI in (.) or not WAIST in (.) or not BLDS in (.);
quit;
/* 2016년 ~ 2017년 기록 : BMI or WAIST or BLDS가 적어도 하나 이상 있는 사람의 PERSON_ID 추출 */
proc sql;
create table include2_17 as select distinct PERSON_ID from ex_screening2
where not BMI in (.) or not WAIST in (.) or not BLDS in (.);
quit;

/*PERSON_ID tbl 합치기*/
DATA include_sum;
SET include1_08 include1_17 include1_19 include2_15 include2_17;
RUN;
/*Unique PERSON_ID 가져오기*/
proc sql;
create table include1_total as select distinct PERSON_ID from include_sum;
quit;


/***************************************************************************************************/
/* 진료DB ~2015년 기록 가져오기*/
DATA T20_15;
SET '/userdata12/room115/data_source/nhis_heals_gy20_t1.sas7bdat';
KEEP PERSON_ID KEY_SEQ RECU_FR_DT MAIN_SICK SUB_SICK;
RUN;

/* 진료DB ~2019년 기록 가져오기*/
DATA T20_19;
SET '/userdata12/room115/data_source/1619/nhis_heals_gy20_1619_t1.sas7bdat';
KEEP PERSON_ID KEY_SEQ RECU_FR_DT MAIN_SICK SUB_SICK;
RUN;
/* 진료DB ~2019년 기록 KEY_SEQ 문자형으로 변환*/
DATA T20_19_2;
SET T20_19;
KEY_SEQ2 = PUT(KEY_SEQ,24.);
DROP KEY_SEQ;
RENAME KEY_SEQ2=KEY_SEQ;
RUN;

/*진료DB 합치기*/
DATA T20;
SET T20_15 T20_19_2;
RUN;

/*진료기록이 적어도 한 건 이상 있는 개체 가져오기*/
proc sql;
create table T20_include as select PERSON_ID, KEY_SEQ, RECU_FR_DT from T20
where MAIN_SICK NOT='' or SUB_SICK NOT = '';
quit;

/*진료DB와 검진DB 연결*/
proc sql;
create table T20_screening as select * from T20, include1_total
where T20.PERSON_ID=include1_total.PERSON_ID;
quit;
proc sql;
create table check1 as select distinct PERSON_ID from T20_screening;
quit;

/***************************************************************************************************/
/* 처방DB ~2015년 기록 가져오기*/
DATA T60_15;
SET '/userdata12/room115/data_source/nhis_heals_gy60_t1.sas7bdat';
KEEP KEY_SEQ RECU_FR_DT DIV_CD GNL_NM_CD;
RUN;

/* 처방DB ~2019년 기록 가져오기*/
DATA T60_19;
SET '/userdata12/room115/data_source/1619/nhis_heals_gy60_1619_t1.sas7bdat';
KEEP KEY_SEQ RECU_FR_DT DIV_CD;
RUN;

/* 처방DB ~2019년 기록 KEY_SEQ 문자형으로 변환*/
DATA T60_19_2;
SET T60_19;
KEY_SEQ2 = PUT(KEY_SEQ,24.);
GNL_NM_CD = '';
DROP KEY_SEQ;
RENAME KEY_SEQ2=KEY_SEQ;
RUN;

/*처방DB 합치기*/
DATA T60;
SET T60_15 T60_19_2;
RUN;

/*처방기록이 적어도 한 건 이상 있는 개체 가져오기*/
proc sql;
create table T60_include as select * from T60
where DIV_CD NOT='' ;
quit;

/*처방DB와 진료DB&검진DB 연결*/
proc sql;
create table dir1.T60_T20_screening as select * from T60_include, T20_screening
where T60_include.KEY_SEQ=T20_screening.KEY_SEQ;
quit;

proc sql;
create table dir1.check2 as select distinct PERSON_ID from T60_T20_screening;
quit;

/* 검진DB(~2008년)에서 당뇨병 이력 합치기*/
DATA screening_08_2;
SET screening_08;
IF (HCHK_PMH_CD1 in (7)) OR (HCHK_PMH_CD2 in (7)) OR (HCHK_PMH_CD2 in (7)) 
THEN HCHK_DIABML_PMH_YN = 1;
ELSE HCHK_DIABML_PMH_YN = 0;
DROP HCHK_PMH_CD1 HCHK_PMH_CD2 HCHK_PMH_CD3;
HCHK_DIABML_PMH_YN2 = INPUT(HCHK_DIABML_PMH_YN, 8.);
DROP HCHK_DIABML_PMH_YN;
RENAME HCHK_DIABML_PMH_YN2 = HCHK_DIABML_PMH_YN;
RUN;

/*검진DB 모두 합치기*/
DATA screening_total;
SET screening_08_2 screening_17 screening_19 ex_screening_15 ex_screening2;
RUN;

/*검진DB에 두 번째 포함조건 적용*/
PROC SQL;
create table screening_check as select * from screening_total, check2
where screening_total.PERSON_ID = check2.PERSON_ID and 
(BLDS>=126 OR HCHK_DIABML_PMH_YN=1);
quit;

/* 이후 진료&처방DB와 연결해주기 위해 KEY_SEQ 변수 생성*/
DATA screening_include_total;
SET screening_check;
KEY_SEQ=put("",12.);
KEEP PERSON_ID HME_DT KEY_SEQ;
RENAME HME_DT = RECU_FR_DT;
RUN;

/*진료&처방DB에 대해 두 번째 포함조건 적용*/
proc sql;
create table tbl_check as select * from T60_T20_screening, check2
where T60_T20_screening.PERSON_ID = check2.PERSON_ID and
(MAIN_SICK in ('E11', 'E12', 'E13', 'E14')  OR SUB_SICK in ('E11', 'E12', 'E13', 'E14') OR
DIV_CD in ("A108") OR GNL_NM_CD in ("A108"));
quit;

/* 필요한 변수만 가져오기*/
DATA hospital_include_total;
SET tbl_check;
KEEP PERSON_ID KEY_SEQ RECU_FR_DT;
RUN;

/*검진DB include tbl과 진료&처방DB include tbl 합치기*/
DATA pop_include;
SET screening_include_total hospital_include_total;
RUN;

PROC SORT DATA=pop_include;
BY PERSON_ID RECU_FR_DT;
RUN;

/* Disease onset 구하기*/
DATA Disease_onset_date;
SET pop_include;
BY PERSON_ID RECU_FR_DT;
IF first.PERSON_ID;
RENAME RECU_FR_DT = DISEASE_ONSET;
RUN;

/**************************************************************/
/* Cohort entry date */
/* 포함조건에 들어가는 사람들의 unique PERSON_ID 가지고 오기*/
proc sql;
create table include_ID as select distinct PERSON_ID from Disease_onset_Date;
quit;

/*각 개체의 최초 처방일자 가지고 오기*/
proc sql;
create table max_hospital as select * from include_ID, hospital_include_total
where hospital_include_total.PERSON_ID = include_ID.PERSON_ID;
quit;

PROC SORT DATA=max_hospital;
BY PERSON_ID RECU_FR_DT;
RUN;

DATA first_RECU;
SET max_hospital;
BY PERSON_ID RECU_FR_DT;
IF first.PERSON_ID;
RENAME RECU_FR_DT=Date;
RUN;

/* max(Disease onset date, 최초처방일자) 계산 */
DATA Disease_onset;
SET Disease_onset_date;
RENAME Disease_onset = Date;
RUN;

DATA max_disease_RECU;
SET Disease_onset first_RECU;
RUN;

PROC SORT DATA=max_disease_RECU;
BY PERSON_ID Date;
RUN;

DATA cohort1;
SET max_disease_RECU;
BY PERSON_ID Date;
if last.PERSON_ID;
RUN;

/* Disease onset date 이후 가장 가까운 검진일자 추출 */
proc sql;
create table min_HME_DT as select * from screening_total, Disease_onset_date
where (screening_total.PERSON_ID = Disease_onset_date.PERSON_ID)
and (screening_total.BLDS not in (.) and screening_total.BLDS not in (.))
and (Disease_onset_date.Disease_onset <= screening_total.HME_DT);
quit;

DATA cohort2;
SET min_HME_DT;
KEEP PERSON_ID HME_DT;
RUN;

/* Cohort entry date 구하기 */
proc sql;
create table cohort_date as select * from cohort1, cohort2
where (cohort1.PERSON_ID = cohort2.PERSON_ID) 
and (cohort1.Date <= cohort2.HME_DT);
quit;

DATA cohort_entry;
SET cohort_date;
DROP Date;
RUN;

PROC SORT DATA=cohort_entry;
BY PERSON_ID HME_DT;
RUN;

DATA cohort_entry_date;
SET cohort_entry;
BY PERSON_ID HME_DT;
IF first.PERSON_ID;
IF HME_DT<='20161231';
RUN;

/************************************/
/* 모집단 제외 기준 적용 */
proc sql;
create table before_record as select * from T60_T20_screening, cohort_entry_date
where (T60_T20_screening.PERSON_ID = cohort_entry_date.PERSON_ID) AND
(T60_T20_screening.RECU_FR_DT <= cohort_entry_date.HME_DT);
quit;

DATA before_entry_record;
SET before_record;
KEEP PERSON_ID HME_DT RECU_FR_DT MAIN_SICK SUB_SICK;
RUN;

PROC sql;
create table disease_check_before_cohort as select * ,
case when MAIN_SICK in ("E10", "O24", "C00", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09",
"C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20",
"C21", "C22", "C23", "C24", "C25", "C26", "C27", "C28", "C29", "C30",
"C31", "C32", "C33", "C34", "C35", "C36", "C37", "C38", "C39", "C40",
"C41", "C42", "C43", "C45", "C46", "C47", "C48", "C49", "C50",
"C51", "C52", "C53", "C54", "C55", "C56", "C57", "C58", "C59", "C60",
"C61", "C62", "C63", "C64", "C65", "C66", "C67", "C68", "C69", "C70",
"C71", "C72", "C73", "C74", "C75", "C76", "C77", "C78", "C79", "C80",
"C81", "C82", "C83", "C84", "C85", "C86", "C87", "C88", "C89", "C90",
"C91", "C92", "C93", "C94", "C95", "C96", "C97", "I21", "I110", "I150", "I130",
"I132", "I60", "I64", "G460", "G461", "G462", "G463", "G464", "G465", "G466", "G467", "G468", 
"G45") OR SUB_SICK in ("E10", "O24", "C00", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09",
"C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20",
"C21", "C22", "C23", "C24", "C25", "C26", "C27", "C28", "C29", "C30",
"C31", "C32", "C33", "C34", "C35", "C36", "C37", "C38", "C39", "C40",
"C41", "C42", "C43", "C45", "C46", "C47", "C48", "C49", "C50",
"C51", "C52", "C53", "C54", "C55", "C56", "C57", "C58", "C59", "C60",
"C61", "C62", "C63", "C64", "C65", "C66", "C67", "C68", "C69", "C70",
"C71", "C72", "C73", "C74", "C75", "C76", "C77", "C78", "C79", "C80",
"C81", "C82", "C83", "C84", "C85", "C86", "C87", "C88", "C89", "C90",
"C91", "C92", "C93", "C94", "C95", "C96", "C97", "I21", "I110", "I150", "I130",
"I132", "I60", "I64", "G460", "G461", "G462", "G463", "G464", "G465", "G466", "G467", "G468", 
"G45") then 1 
else 0 end as Exclusion from before_entry_record;
quit;

proc sql;
create table result as select distinct PERSON_ID, HME_DT, 
SUM(Exclusion) as exclude from disease_check_before_cohort
group by PERSON_ID;
quit;

DATA dir1.total_study_pop;
SET result;
IF exclude=0;
KEEP PERSON_ID HME_DT;
OUTPUT total_study_pop;
RUN;





GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
