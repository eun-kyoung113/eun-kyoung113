/* ----------------------------------------
SAS Enterprise Guide���� ������ �ڵ�
DATE: 2023�� 1�� 16�� ������     TIME: ���� 1:28:13
PROJECT: ������Ʈcode
PROJECT PATH: C:\Users\ROOM115_ilojeyou98\������Ʈcode.egp
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

/*   ��� ����: Study_pop_define_ver1   */
%LET _CLIENTTASKLABEL='Study_pop_define_ver1';
%LET _CLIENTPROCESSFLOWNAME='���μ��� �÷ο�';
%LET _CLIENTPROJECTPATH='C:\Users\ROOM115_ilojeyou98\������Ʈcode.egp';
%LET _CLIENTPROJECTNAME='������Ʈcode.egp';
%LET _SASPROGRAMFILE=;

GOPTIONS ACCESSIBLE;
/* ����DB - 2002�� ~ 2015�� DB �ҷ����� */
DATA screening1;
SET '/userdata12/room115/data_source/nhis_heals_gj.sas7bdat';
KEEP HCHK_YEAR PERSON_ID HME_DT BMI WAIST BLDS HCHK_PMH_CD1 HCHK_PMH_CD2 HCHK_PMH_CD3 HCHK_DIABML_PMH_YN;
run;
/* unique�� PERSON_ID �������� */
proc sql;
create table screening1_uniq as select distinct PERSON_ID from screening1;
quit;

/* ����DB - 2016�� ~ 2017�� DB �ҷ����� */
DATA screening2;
SET '/userdata12/room115/data_source/1619/nhis_heals_gj_1617.sas7bdat';
KEEP HCHK_YEAR PERSON_ID HME_DT BMI WAIST BLDS HCHK_DIABML_PMH_YN;
RUN;
/* unique�� PERSON_ID �������� */
proc sql;
create table screening2_uniq as select distinct PERSON_ID from screening2;
quit;

/* ����DB - 2018�� ~ 2019�� DB �ҷ����� */
DATA screening_19;
SET '/userdata12/room115/data_source/1619/nhis_heals_gj_1819.sas7bdat';
IF PERSON_ID NOT = '';
KEEP HCHK_YEAR PERSON_ID HME_DT BMI WAIST BLDS HCHK_DIABML_PMH_YN;
RUN;
/* unique�� PERSON_ID �������� */
proc sql;
create table screening_19_uniq as select distinct PERSON_ID from screening_19;
quit;
/****************************************************************************************/
/* screening1���� 2002�� ~ 2008������� ��������*/
DATA screening_08;
SET screening1;
IF (HCHK_YEAR<=2008) and (PERSON_ID NOT = '');
RUN;

/* screening1���� 2008�� ~ 2015������� ��������*/
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
/* �� ������ ��ġ��*/
DATA screening_17;
SET screening_09_15_2 screening2;
IF PERSON_ID NOT = '';
RUN;

/* 2002�� ~ 2008�� ��� : BMI or WAIST or BLDS�� ��� �ϳ� �̻� �ִ� ����� PERSON_ID ���� */
proc sql;
create table include1_08 as select distinct PERSON_ID from screening_08
where not BMI in (.) or not WAIST in (.) or not BLDS in (.); 
quit;
/* 2009�� ~ 2017�� ��� : BMI or WAIST or BLDS�� ��� �ϳ� �̻� �ִ� ����� PERSON_ID ���� */
proc sql;
create table include1_17 as select distinct PERSON_ID from screening_17
where not BMI in (.) or not WAIST in (.) or not BLDS in (.);
quit;
/* 2018�� ~ 2019�� ��� : BMI or WAIST or BLDS�� ��� �ϳ� �̻� �ִ� ����� PERSON_ID ���� */
proc sql;
create table include1_19 as select distinct PERSON_ID from screening_19
where not BMI in (.) or not WAIST in (.) or not BLDS in (.);
quit;

/********************************************************************************************/
/* ������ȯ�����DB - 2007�� ~ 2015�� DB �ҷ����� */
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
/* unique�� PERSON_ID �������� */
proc sql;
create table ex_screening1_uniq as select distinct PERSON_ID from ex_screening1;
quit;

/* ������ȯ�����DB - 2016�� ~ 2017�� DB �ҷ����� */
DATA ex_screening2;
SET '/userdata12/room115/data_source/1619/nhis_heals_lj_1617.sas7bdat';
IF PERSON_ID NOT = '';
KEEP HCHK_YEAR PERSON_ID HME_DT BMI WAIST BLDS HCHK_DIABML_PMH_YN;
RUN;
/* unique�� PERSON_ID �������� */
proc sql;
create table ex_screening2_uniq as select distinct PERSON_ID from ex_screening2;
quit;

/* 2007�� ~ 2015�� ��� : BMI or WAIST or BLDS�� ��� �ϳ� �̻� �ִ� ����� PERSON_ID ���� */
proc sql;
create table include2_15 as select distinct PERSON_ID from ex_screening1
where not BMI in (.) or not WAIST in (.) or not BLDS in (.);
quit;
/* 2016�� ~ 2017�� ��� : BMI or WAIST or BLDS�� ��� �ϳ� �̻� �ִ� ����� PERSON_ID ���� */
proc sql;
create table include2_17 as select distinct PERSON_ID from ex_screening2
where not BMI in (.) or not WAIST in (.) or not BLDS in (.);
quit;

/*PERSON_ID tbl ��ġ��*/
DATA include_sum;
SET include1_08 include1_17 include1_19 include2_15 include2_17;
RUN;
/*Unique PERSON_ID ��������*/
proc sql;
create table include1_total as select distinct PERSON_ID from include_sum;
quit;


/***************************************************************************************************/
/* ����DB ~2015�� ��� ��������*/
DATA T20_15;
format KEY_SEQ $13.;
SET '/userdata12/room115/data_source/nhis_heals_gy20_t1.sas7bdat';
KEEP PERSON_ID KEY_SEQ RECU_FR_DT MAIN_SICK SUB_SICK;
RUN;

/* ����DB ~2019�� ��� ��������*/
DATA T20_19;
SET '/userdata12/room115/data_source/1619/nhis_heals_gy20_1619_t1.sas7bdat';
KEEP PERSON_ID KEY_SEQ RECU_FR_DT MAIN_SICK SUB_SICK;
RUN;

/* ����DB ~2019�� ��� KEY_SEQ ���������� ��ȯ*/
DATA T20_19_2;
SET T20_19;
KEY_SEQ2 = PUT(KEY_SEQ,13.);
DROP KEY_SEQ;
RENAME KEY_SEQ2=KEY_SEQ;
RUN;

/*����DB ��ġ��*/
DATA T20;
SET T20_15 T20_19_2;
RUN;

/*�������� ��� �� �� �̻� �ִ� ��ü ��������*/
proc sql;
create table T20_include as select * from T20
where MAIN_SICK NOT='' or SUB_SICK NOT = '';
quit;

/***************************************************************************************************/
/* ó��DB ~2015�� ��� ��������*/
DATA T60_15;
format KEY_SEQ $13.;
SET '/userdata12/room115/data_source/nhis_heals_gy60_t1.sas7bdat';
KEEP KEY_SEQ RECU_FR_DT DIV_CD GNL_NM_CD;
RUN;

/* ó��DB ~2019�� ��� ��������*/
DATA T60_19;
SET '/userdata12/room115/data_source/1619/nhis_heals_gy60_1619_t1.sas7bdat';
KEEP KEY_SEQ RECU_FR_DT DIV_CD;
RUN;

/* ó��DB ~2019�� ��� KEY_SEQ ���������� ��ȯ*/
DATA T60_19_2;
SET T60_19;
KEY_SEQ2 = PUT(KEY_SEQ,13.);
GNL_NM_CD = '';
DROP KEY_SEQ;
RENAME KEY_SEQ2=KEY_SEQ;
RUN;

/*ó��DB ��ġ��*/
DATA T60;
SET T60_15 T60_19_2;
RUN;
/*ó������ ��� �� �� �̻� �ִ� ��ü ��������*/
proc sql;
create table T60_include as select * from T60
where DIV_CD NOT='' ;
quit;

LIBNAME dir1 "/userdata12/room115/data_out/data_store";
run;

/*ó��DB�� ����DB ����*/
proc sql;
create table T60_T20_include as select * from T60_include join T20_include 
on T60_include.KEY_SEQ=T20_include.KEY_SEQ
order by T20_include.PERSON_ID;
quit;

proc sql;
create table dir1.T60_T20_screening as select * from T60_T20_include join include1_total
on T60_T20_include.PERSON_ID = include1_total.PERSON_ID;
quit;

proc sql;
create table check2 as select distinct PERSON_ID from T60_T20_screening;
quit;

/* ����DB(~2008��)���� �索�� �̷� ��ġ��*/
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

/*����DB ��� ��ġ��*/
DATA screening_total;
SET screening_08_2 screening_17 screening_19 ex_screening_15 ex_screening2;
RUN;

/*����DB�� �� ��° �������� ����*/
PROC SQL;
create table screening_check as select * from screening_total, check2
where screening_total.PERSON_ID = check2.PERSON_ID and 
(BLDS>=126 OR HCHK_DIABML_PMH_YN=1);
quit;

/* ���� ����&ó��DB�� �������ֱ� ���� KEY_SEQ ���� ����*/
DATA screening_include_total;
SET screening_check;
KEY_SEQ=put("",13.);
KEEP PERSON_ID HME_DT KEY_SEQ;
RENAME HME_DT = RECU_FR_DT;
RUN;

/*����&ó��DB�� ���� �� ��° �������� ����*/
proc sql;
create table tbl_check as select * from dir1.T60_T20_screening, dir1.check2
where T60_T20_screening.PERSON_ID = check2.PERSON_ID and
((MAIN_SICK like "E11%") or  (MAIN_SICK like "E12%") or  (MAIN_SICK like "E13%") 
or  (MAIN_SICK like "E14%") or (SUB_SICK like "E11%") or  (SUB_SICK like "E12%") 
or (SUB_SICK like "E13%") or  (SUB_SICK like "E14%") OR
DIV_CD like "A108%" OR GNL_NM_CD like "A108%");
quit;

/* �ʿ��� ������ ��������*/
DATA hospital_include_total;
SET tbl_check;
KEEP PERSON_ID KEY_SEQ RECU_FR_DT;
RUN;

/*����DB include tbl�� ����&ó��DB include tbl ��ġ��*/
DATA pop_include;
SET screening_include_total hospital_include_total;
RUN;

PROC SORT DATA=pop_include;
BY PERSON_ID RECU_FR_DT;
RUN;

/* Disease onset ���ϱ�*/
DATA Disease_onset_date;
SET pop_include;
BY PERSON_ID RECU_FR_DT;
IF first.PERSON_ID;
RENAME RECU_FR_DT = DISEASE_ONSET;
RUN;

/**************************************************************/
/* Cohort entry date */
/* �������ǿ� ���� ������� unique PERSON_ID ������ ����*/
proc sql;
create table include_ID as select distinct PERSON_ID from Disease_onset_Date;
quit;

/*�� ��ü�� ���� ó������ ������ ����*/
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

/* max(Disease onset date, ����ó������) ��� */
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

/* Disease onset date ���� ���� ����� �������� ���� */
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

/* Cohort entry date ���ϱ� */
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
/* ������ ���� ���� ���� */
proc sql;
create table before_record as select * from dir1.T60_T20_screening, cohort_entry_date
where (T60_T20_screening.PERSON_ID = cohort_entry_date.PERSON_ID) AND
(T60_T20_screening.RECU_FR_DT <= cohort_entry_date.HME_DT);
quit;

DATA before_entry_record;
SET before_record;
KEEP PERSON_ID HME_DT RECU_FR_DT MAIN_SICK SUB_SICK;
RUN;

PROC sql;
create table disease_check_before_cohort as select * ,
case when MAIN_SICK like "I21%" or SUB_SICK like "I21%" or
((MAIN_SICK like "I50%") or (MAIN_SICK in ("I110", "I130", "I132"))) or 
(SUB_SICK like "I50%") or (SUB_SICK in ("I110", "I130", "I132")) or
MAIN_SICK like "G460%" or MAIN_SICK like "G461%" or  MAIN_SICK like "G462%"
or MAIN_SICK like "G463%" or MAIN_SICK like "G464%" or MAIN_SICK like "G465%" or MAIN_SICK like "G466%"
or MAIN_SICK like "G467%" or MAIN_SICK like "G468%" or MAIN_SICK like "I60%" or MAIN_SICK like "I61%"
or MAIN_SICK like "I62%" or MAIN_SICK like "I63%" or MAIN_SICK like "I64%" or MAIN_SICK like "G45%"
or SUB_SICK like "G460%" or SUB_SICK like "G461%" or  SUB_SICK like "G462%"
or SUB_SICK like "G463%" or SUB_SICK like "G464%" or SUB_SICK like "G465%" or SUB_SICK like "G466%"
or SUB_SICK like "G467%" or SUB_SICK like "G468%" or SUB_SICK like "I60%" or SUB_SICK like "I61%"
or SUB_SICK like "I62%" or SUB_SICK like "I63%" or SUB_SICK like "I64%" or SUB_SICK like "G45%" 
or ((MAIN_SICK like "C%") and (MAIN_SICK not like "C_%" OR MAIN_SICK not like "C98%" or MAIN_SICK not like "C99%")) or
((SUB_SICK like "C%") and (SUB_SICK not like "C_%" or SUB_SICK not like "C98%" or SUB_SICK not like "C99%")) 
then 1 else 0 end as Exclusion from before_entry_record;
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
