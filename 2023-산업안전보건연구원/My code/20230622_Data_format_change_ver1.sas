LIBNAME dir 'E:\koshri-23\data';
LIBNAME mine 'E:\koshri-23\eklee\data';

DATA raw2;
SET mine.My_raw (KEEP = INDI_ID NO ECNY_DT OUT_DT);
run;

proc sort data = raw2;
BY INDI_ID;
run;

proc sort data = mine.N1_data;
BY INDI_ID;
run;

DATA mine.lung_temp;
MERGE raw2 mine.N1_data;
BY INDI_ID;
IF lung_cancer=1 then do;
	Outcome_Category="lung_cancer";
	Outcome_date = lung_cancer_date;
end;
ELSE IF Death=1 then do;
	Outcome_Category="Death";
	Outcome_date = DTH_DATE;
end;
ELSE DO;
	Outcome_Category="last_follow";
	Outcome_date = '20181231';
end;
IF (Outcome_date> ECNY_DT) and (Outcome_date < OUT_DT) then between_yes=1;
ELSE between_yes=0;
DROP DTH_DATE Death lung_cancer lung_cancer_date leukemia leukemia_date;
run;

/* "lung_cancer" tbl check */
proc sql;
create table check1 as select distinct INDI_ID from mine.lung_temp
where between_yes=1;
quit;

proc sql;
create table check2 as select a.INDI_ID, a.ECNY_DT, a.OUT_DT, a.Outcome_Category, a.Outcome_date, a.between_yes  from mine.lung_temp as a join check1 as b
on a.INDI_ID = b.INDI_ID;
quit;

DATA check3;
SET check2;
IF between_yes=1;
run;

proc sql;
select distinct Outcome_Category from check3;
quit; /* 취득일과 상실일 사이에 Outcome date가 있는 경우의 Outcome Category는 모두 lung_cancer임을 확인 */


