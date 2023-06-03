/* ----------------------------------------
SAS Enterprise Guide에서 가져온 코드
DATE: 2023년 1월 10일 화요일     TIME: 오후 2:45:31
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

/*   노드 시작: Study_population_split   */
%LET _CLIENTTASKLABEL='Study_population_split';
%LET _CLIENTPROCESSFLOWNAME='프로세스 플로우';
%LET _CLIENTPROJECTPATH='C:\Users\ROOM115_ilojeyou98\프로젝트code.egp';
%LET _CLIENTPROJECTNAME='프로젝트code.egp';
%LET _SASPROGRAMFILE=;

GOPTIONS ACCESSIBLE;

/**** Study population 외 Other study group Define ****/

/*SAS table 저장 경로 지정 */
libname dir1 "/userdata12/room115/data_out/data_store";
run;

/*** 첫 번째 포함조건 만족하는 unique PERSON_ID tbl ***/
DATA check2;
SET dir1.check2;
RUN;

/*** 첫 번째 포함조건 만족하는 obj들의 진료DB ***/
DATA T60_T20_screening;
SET dir1.T60_T20_screening;
RUN;

/*** check2 tbl에서 total_study population obs 제외***/
DATA total_study_pop;
SET dir1.total_study_pop;
RUN;

proc sql;
create table check3 as select PERSON_ID
from check2
except select PERSON_ID from total_study_pop;
quit;

/* 검진DB -  ~ 2008년 DB 불러오기 */
DATA screening_08;
SET '/userdata12/room115/data_out/data_store/screening_08.sas7bdat';
KEEP PERSON_ID HME_DT BMI BLDS; 
RUN;

/* 검진DB - 2009년 ~ 2015년 DB 불러오기 */
DATA screening_09_15;
SET '/userdata12/room115/data_out/data_store/screening_09_15.sas7bdat';
KEEP PERSON_ID HME_DT BMI BLDS;
IF PERSON_ID NOT = '';
RUN;

/* 검진DB - 2016년 ~ 2017년 DB 불러오기 */
DATA screening_16_17;
SET '/userdata12/room115/data_source/1619/nhis_heals_gj_1617.sas7bdat';
KEEP PERSON_ID HME_DT BMI BLDS;
IF PERSON_ID NOT = '';
RUN;

/* 생애전환기검진DB - 2007년 ~ 2008년 DB 불러오기 */
DATA ex_screening_08;
SET '/userdata12/room115/data_out/data_store/ex_screening_08.sas7bdat';
IF PERSON_ID NOT = '';
KEEP PERSON_ID HME_DT BMI BLDS;
run;

/* 생애전환기검진DB - 2009년 ~ 2015년 DB 불러오기 */
DATA ex_screening_09_15;
SET '/userdata12/room115/data_out/data_store/ex_screening_09_15.sas7bdat';
IF PERSON_ID NOT = '';
KEEP PERSON_ID HME_DT BMI BLDS;
run;

/* 생애전환기검진DB - 2016년 ~ 2017년 DB 불러오기 */
DATA ex_screening_16_17;
SET '/userdata12/room115/data_source/1619/nhis_heals_lj_1617.sas7bdat';
IF PERSON_ID NOT = '';
KEEP PERSON_ID HME_DT BMI BLDS;
RUN;

/* 검진 Data Merge */
DATA total_screening_08_15;
SET screening_08 screening_09_15 ex_screening_08 ex_screening_09_15;
RUN;

DATA total_screening_15;
SET total_screening_08_15;
length BLDS BMI 8;
RUN;

DATA screening_total_pop;
SET total_screening_15 screening_16_17 ex_screening_16_17;
RUN;

proc sql;
create table screening_include1 as select * from check3 join screening_total_pop
on check3.PERSON_ID = screening_total_pop.PERSON_ID;
quit;

/*** 당뇨병 위험 group ***/
DATA risk;
SET screening_include1;
IF (BLDS >=100) & (BLDS<=125);
RUN;

proc sort data=risk;
by PERSON_ID HME_DT;
run;

DATA risk_group;
SET risk;
BY PERSON_ID HME_DT;
IF first.PERSON_ID;
KEEP PERSON_ID HME_DT;
RUN;

proc sql;
create table risk_screening as select * from risk_group join screening_include1
on (screening_include1.BMI not = .) and (risk_group.PERSON_ID=screening_include1.PERSON_ID) and
(risk_group.HME_DT <= screening_include1.HME_DT);
quit;

proc sort data=risk_screening;
by PERSON_ID HME_DT;
run;

DATA risk_group_ver2;
SET risk_screening;
BY PERSON_ID HME_DT;
IF first.PERSON_ID;
KEEP PERSON_ID HME_DT;
RUN;

/*** 당뇨병 위험 group 제외조건 적용 ***/
proc sql;
create table T60_T20_risk as select * from risk_group_ver2 join T60_T20_screening
on (risk_group_ver2.HME_DT >= T60_T20_screening.RECU_FR_DT)
and (risk_group_ver2.PERSON_ID = T60_T20_screening.PERSON_ID);
quit;

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
else 0 end as Exclusion from T60_T20_risk;
quit;

proc sql;
create table result as select distinct PERSON_ID, HME_DT, 
SUM(Exclusion) as exclude from disease_check_before_cohort
group by PERSON_ID;
quit;

DATA result2;
SET result;
IF exclude=0;
KEEP PERSON_ID HME_DT;
RUN;

proc sort data=result2;
by PERSON_ID HME_DT;
run;

DATA dir1.total_risk_diabetes_group;
SET result2;
BY PERSON_ID HME_DT;
IF first.PERSON_ID;
DROP exclude;
RUN;


/***********************/
/*** check3 tbl에서 risk_diabetes_group obs 제외***/
proc sql;
create table check4 as select PERSON_ID
from check3
except select PERSON_ID from risk_group;
quit;

/* 검진DB -  ~ 2008년 DB 불러오기 */
DATA screening_08;
SET dir1.screening_08;
KEEP PERSON_ID HME_DT BMI BLDS; 
RUN;

/* 검진DB - 2009년 ~ 2015년 DB 불러오기 */
DATA screening_09_15;
SET dir1.screening_09_15;
KEEP PERSON_ID HME_DT BMI BLDS;
IF PERSON_ID NOT = '';
RUN;

/* 검진DB - 2016년 ~ 2017년 DB 불러오기 */
DATA screening_16_17;
SET '/userdata12/room115/data_source/1619/nhis_heals_gj_1617.sas7bdat';
KEEP PERSON_ID HME_DT BMI BLDS;
IF PERSON_ID NOT = '';
RUN;

/* 생애전환기검진DB - 2007년 ~ 2008년 DB 불러오기 */
DATA ex_screening_08;
SET dir1.ex_screening_08;
IF PERSON_ID NOT = '';
KEEP PERSON_ID HME_DT BMI BLDS;
run;

/* 생애전환기검진DB - 2009년 ~ 2015년 DB 불러오기 */
DATA ex_screening_09_15;
SET dir1.ex_screening_09_15;
IF PERSON_ID NOT = '';
KEEP PERSON_ID HME_DT BMI BLDS;
run;

/* 생애전환기검진DB - 2016년 ~ 2017년 DB 불러오기 */
DATA ex_screening_16_17;
SET '/userdata12/room115/data_source/1619/nhis_heals_lj_1617.sas7bdat';
IF PERSON_ID NOT = '';
KEEP PERSON_ID HME_DT BMI BLDS;
RUN;

/* 검진 Data Merge */
DATA total_screening_08_15;
SET screening_08 screening_09_15 ex_screening_08 ex_screening_09_15;
RUN;

DATA total_screening_15;
SET total_screening_08_15;
length BLDS BMI 8;
RUN;

DATA screening_total_pop;
SET total_screening_15 screening_16_17 ex_screening_16_17;
RUN;

proc sql;
create table screening_include_ow as select * from check4 join screening_total_pop
on (check4.PERSON_ID = screening_total_pop.PERSON_ID) and
(screening_total_pop.BMI not = .) ;
quit;

proc sort data=screening_include_ow;
by PERSON_ID HME_DT;
run;

DATA not_risk_group;
SET screening_include_ow;
BY PERSON_ID HME_DT;
IF first.PERSON_ID;
KEEP PERSON_ID HME_DT;
RUN;

/*** Otherwise group 제외조건 적용 ***/
proc sql;
create table T60_T20_ow as select * from not_risk_group join T60_T20_screening
on (not_risk_group.HME_DT >= T60_T20_screening.RECU_FR_DT)
and (not_risk_group.PERSON_ID = T60_T20_screening.PERSON_ID);
quit;

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
else 0 end as Exclusion from T60_T20_ow;
quit;

proc sql;
create table result as select distinct PERSON_ID, HME_DT, 
SUM(Exclusion) as exclude from disease_check_before_cohort
group by PERSON_ID;
quit;

DATA result2;
SET result;
IF exclude=0;
KEEP PERSON_ID HME_DT;
RUN;

proc sort data=result2;
by PERSON_ID HME_DT;
run;

DATA dir1.total_ow_group;
SET result2;
BY PERSON_ID HME_DT;
IF first.PERSON_ID;
DROP exclude;
RUN;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
