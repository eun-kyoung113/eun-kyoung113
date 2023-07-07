/* ----------------------------------------
SAS Enterprise Guide에서 가져온 코드
DATE: 2023년 1월 17일 화요일     TIME: 오후 3:00:23
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

/*   노드 시작: Outcome_define   */
%LET _CLIENTTASKLABEL='Outcome_define';
%LET _CLIENTPROCESSFLOWNAME='프로세스 플로우';
%LET _CLIENTPROJECTPATH='C:\Users\ROOM115_ilojeyou98\프로젝트code.egp';
%LET _CLIENTPROJECTNAME='프로젝트code.egp';
%LET _SASPROGRAMFILE=;

GOPTIONS ACCESSIBLE;
/***** Outcome Define *****/
/*** Primary Outcome : Death ***/

DATA total_study_pop;
SET '/userdata12/room115/data_out/data_store/total_study_pop.sas7bdat';
RUN;

DATA jk_total;
SET '/userdata12/room115/data_out/data_store/jk_total.sas7bdat';
KEEP PERSON_ID STND_Y DTH_MDY;
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
RUN;

PROC SORT DATA=death;
by PERSON_ID STND_Y;
RUN;

DATA death_check2;
SET death;
BY PERSON_ID STND_Y;
IF last.PERSON_ID;
RUN;

proc sql;
create table death_ver2 as select * from total_study_pop_ver2 
LEFT JOIN death_check2 on total_study_pop_ver2.PERSON_ID = death_check2.PERSON_ID;
quit;

DATA Outcome_death;
SET death_ver2;
IF Death =1 then do;
	HME_DT_new=input(HME_DT, YYMMDD10.);
	DTH_MDY_new=input(DTH_MDY,YYMMDD10.);
	FU_Duration_death = (DTH_MDY_new-HME_DT_new)/365.25;
end;

IF Death=. then Death=0;

KEEP PERSON_ID Death FU_Duration_death;
RUN;



/**********************************/
/***** Secondary Outcome *****/
DATA T60_T20_screening;
SET '/userdata12/room115/data_out/data_store/T60_T20_screening.sas7bdat';
KEEP PERSON_ID RECU_FR_DT MAIN_SICK SUB_SICK;
RUN;

proc sql;
create table T60_T20_study as select * from total_study_pop join T60_T20_screening
on total_study_pop.PERSON_ID = T60_T20_screening.PERSON_ID
and total_study_pop.HME_DT <= T60_T20_screening.RECU_FR_DT;
quit;

proc sql;
create table check_outcome as select PERSON_ID, HME_DT, RECU_FR_DT,
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
or SUB_SICK like "I62%" or SUB_SICK like "I63%" or SUB_SICK like "I64%" or SUB_SICK like "G45%" then 1 else 0 end as TIA_Stroke,
case when ((MAIN_SICK like "C%") and (MAIN_SICK not like "C_%" OR MAIN_SICK not like "C98%" or MAIN_SICK not like "C99%")) or
((SUB_SICK like "C%") and (SUB_SICK not like "C_%" or SUB_SICK not like "C98%" or SUB_SICK not like "C99%")) 
then 1 else 0 end as Cancer 
from T60_T20_study;
quit;

DATA check_outcome2;
SET check_outcome;
HME_DT_new=input(HME_DT,YYMMDD10.);
RECU_FR_DT_new=input(RECU_FR_DT,YYMMDD10.);

IF MI=1 then MI_FU_Duration=(RECU_FR_DT_new-HME_DT_new)/365.25;
IF Heart_failure=1 then Heart_failure_FU_Duration=(RECU_FR_DT_new-HME_DT_new)/365.25;
IF TIA_Stroke=1 then TIA_Stroke_FU_Duration=(RECU_FR_DT_new-HME_DT_new)/365.25;
IF Cancer=1 then Cancer_FU_Duration=(RECU_FR_DT_new-HME_DT_new)/365.25;

RUN;

proc sort data=check_outcome2;
by PERSON_ID RECU_FR_DT;
RUN;

DATA MI_Outcome;
SET check_outcome2;
BY PERSON_ID RECU_FR_DT;
IF MI=1;
IF first.PERSON_ID;
KEEP PERSON_ID HME_DT MI MI_FU_Duration;
RUN;

DATA Cancer_Outcome;
SET check_outcome2;
BY PERSON_ID RECU_FR_DT;
IF Cancer=1;
IF first.PERSON_ID;
KEEP PERSON_ID Cancer Cancer_FU_Duration HME_DT;
RUN;

DATA Heart_failure_Outcome;
SET check_outcome2;
BY PERSON_ID RECU_FR_DT;
IF Heart_failure=1;
IF first.PERSON_ID;
KEEP PERSON_ID HME_DT Heart_failure Heart_failure_FU_Duration;
RUN;

DATA TIA_Stroke_Outcome;
SET check_outcome2;
BY PERSON_ID RECU_FR_DT;
IF TIA_Stroke=1;
IF first.PERSON_ID;
KEEP PERSON_ID HME_DT TIA_Stroke TIA_Stroke_FU_Duration;
RUN;

DATA Outcome_Secondary;
MERGE total_study_pop MI_Outcome Cancer_Outcome Heart_failure_Outcome TIA_Stroke_Outcome;
BY PERSON_ID;
IF MI=. then MI=0;
IF Heart_failure=. then Heart_failure=0;
IF Cancer=. then Cancer=0;
IF TIA_Stroke=. then TIA_Stroke=0;
KEEP PERSON_ID MI MI_FU_Duration Cancer Cancer_FU_Duration Heart_failure Heart_failure_FU_Duration TIA_Stroke TIA_Stroke_FU_Duration;
RUN;




GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROCESSFLOWNAME=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
