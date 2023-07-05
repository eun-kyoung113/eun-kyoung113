/* Raw Data re-make */

LIBNAME dir 'L:\';
LIBNAME mine 'C:\Users\stat\Dropbox\eklee\SBY_data';

DATA dir.Demographic_raw;
SET dir.Db3 (KEEP = NO INDI_ID ECNY_DT OUT_DT);
where INDI_ID ^= '1000000000001' and ECNY_DT ^= OUT_DT and ECNY_DT<'20190101';
RUN;

DATA dir.Death_raw;
SET dir.Db3(KEEP = NO INDI_ID DTH_AGE DTH_DATE1 DTH_DATE2 DTH_DATE3);
WHERE  INDI_ID ^= '1000000000001';
run;

DATA dir.Cancer_raw;
SET dir.Db3 (KEEP = NO INDI_ID fdx1-fdx6 icd10_1-icd10_6);
WHERE  INDI_ID ^= '1000000000001';
run;

