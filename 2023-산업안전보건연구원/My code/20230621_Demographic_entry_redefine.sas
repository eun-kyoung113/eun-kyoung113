/* "entry" covariate + BIRTH_Date + SEX + Cancer History Define */

LIBNAME dir 'E:\koshri-23\data';
LIBNAME mine 'E:\koshri-23\eklee\data';

/* Get my raw data - keep needed variable */
DATA mine.my_raw;
SET dir.New_raw;
KEEP INDI_ID NO SEX ECNY_DT OUT_DT AGE DURATION fdx1 icd10_1 fdx2 icd10_2 fdx3 icd10_3 fdx4 icd10_4 fdx5 icd10_5 fdx6 icd10_6
		 DTH_DATE1-DTH_DATE3 BYEAR BMONTH BDAY;
RUN;

/* Get BIRTH_DATE + SEX +  first.ECNY_DT */
proc sort data = mine.my_raw;
BY INDI_ID ECNY_DT;
run;

DATA temp;
SET mine.my_raw(DROP = NO AGE DURATION);
BY INDI_ID ECNY_DT;
IF first.INDI_ID;
BYEAR2= input(BYEAR, 12.);
BMONTH2 = input(BMONTH, 12.);
BDAY2 = input(BDAY, 12.);
DTH_DATE = cats(DTH_DATE1, DTH_DATE2, DTH_DATE3);
DROP BYEAR BMONTH BDAY;
RENAME BYEAR2=BYEAR BMONTH2=BMONTH BDAY2=BDAY;
run;

/* Define "entry", "Death" variable and Get SEX, BYEAR */
DATA mine.Demographic_Info;
SET temp;
first_year = substr(ECNY_DT, 1, 4);
IF first_year < '1999' then Entry = 0;
ELSE IF first_year < '2004' then Entry = 1;
ELSE IF first_year < '2009' then Entry = 2;
ELSE IF first_year < '2014' then Entry =3;
ELSE Entry = 4;
IF not missing(DTH_DATE) then Death=1;
ELSE Death=0;
KEEP INDI_ID BYEAR SEX Entry DTH_DATE Death;
RUN; 

/* Outcome + Cancer History */
DATA mine.Cancer;
SET temp;
/* lung cancer */
IF substr(icd10_1,1,3) = "C34" then do;
	lung_cancer=1;
	lung_cancer_date=fdx1;
end;
ELSE IF substr(icd10_2,1,3) = "C34" then do;
	lung_cancer=1;
	lung_cancer_date=fdx2;
end; 
ELSE IF substr(icd10_3,1,3) = "C34" then do;
	lung_cancer=1;
	lung_cancer_date=fdx3;
end; 
ELSE IF substr(icd10_4,1,3) = "C34" then do;
	lung_cancer=1;
	lung_cancer_date=fdx4;
end; 
ELSE IF substr(icd10_5,1,3) = "C34" then do;
	lung_cancer=1;
	lung_cancer_date=fdx5;
end; 
ELSE IF substr(icd10_6,1,3) = "C34" then do;
	lung_cancer=1;
	lung_cancer_date=fdx6;
end; 
ELSE lung_cancer=0;

/* leukemia */
IF substr(icd10_1,1,3) in ("C91", "C92", "C93", "C94", "C95") then do;
	leukemia=1;
	leukemia_date=fdx1;
end;
ELSE IF substr(icd10_2,1,3) in ("C91", "C92", "C93", "C94", "C95") then do;
	leukemia=1;
	leukemia_date=fdx2;
end; 
ELSE IF substr(icd10_3,1,3) in ("C91", "C92", "C93", "C94", "C95") then do;
	leukemia=1;
	leukemia_date=fdx3;
end; 
ELSE IF substr(icd10_4,1,3) in ("C91", "C92", "C93", "C94", "C95") then do;
	leukemia=1;
	leukemia_date=fdx4;
end; 
ELSE IF substr(icd10_5,1,3) in ("C91", "C92", "C93", "C94", "C95") then do;
	leukemia=1;
	leukemia_date=fdx5;
end; 
ELSE IF substr(icd10_6,1,3) in ("C91", "C92", "C93", "C94", "C95") then do;
	leukemia=1;
	leukemia_date=fdx6;
end; 
ELSE leukemia=0;

/* Cancer History */
IF not missing(fdx1) and lung_cancer=0 and leukemia=0 then Cancer_History=1;
ELSE Cancer_History=0;
KEEP INDI_ID lung_cancer lung_cancer_date leukemia leukemia_date Cancer_History;
RUN;

/* MERGE mine.Demographic_Info and mine.Cancer data */
proc sql;
create table mine.N1_data as select * from mine.Demographic_Info as a join mine.Cancer as b
on a.INDI_ID = b.INDI_ID;
quit;

/* Check death ratio & Lung cancer ratio & Leukemia ratio */
proc freq data=mine.N1_data;
tables Death;
run;

proc freq data=mine.N1_data;
tables lung_cancer;
run;

proc freq data=mine.N1_data;
tables leukemia;
run;
