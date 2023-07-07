/* Data split by YEAR and SEX and fitted time-varying covariate Cox-ph model (lung_cancer_history is time-varying covariate) */

LIBNAME dir 'E:\koshri-23\data';
LIBNAME mine 'E:\koshri-23\eklee\data';

%macro split_data_fitted_Cox(input_dataset, output_prefix);
	%do year = 1940  %to 1984 %by 5;
      		DATA mine.&output_prefix._&year._M;
        	SET &input_dataset(DROP = Outcome_Category Outcome_date);
        	WHERE BYEAR between &year and (&year + 4) and SEX = "³²";
      		run;

			proc phreg data=mine.&output_prefix._&year._M FAST;
			class Event(ref='0') Entry_2004(ref='0') Entry_2009(ref='0') Entry_2014(ref='0') Entry_2018(ref='0') Lung_Cancer_History(ref='0');
			model (start, stop) * Event(0) = Entry_2004 Entry_2009 Entry_2014 Entry_2018 Lung_Cancer_History /rl;
			run;

			DATA mine.&output_prefix._&year._F;
        	SET &input_dataset(DROP = Outcome_Category Outcome_date);
        	WHERE BYEAR between &year and (&year + 4) and SEX = "¿©";
      		run;

			proc phreg data=mine.&output_prefix._&year._F FAST;
			class Event(ref='0') Entry_2004(ref='0') Entry_2009(ref='0') Entry_2014(ref='0') Entry_2018(ref='0') Lung_Cancer_History(ref='0');
			model (start, stop) * Event(0) = Entry_2004 Entry_2009 Entry_2014 Entry_2018 Lung_Cancer_History /rl;
			run;
  	%end;
%mend;

%split_data_fitted_Cox(input_dataset=mine.final_lung_time_varying_NO3, output_prefix=dt);
