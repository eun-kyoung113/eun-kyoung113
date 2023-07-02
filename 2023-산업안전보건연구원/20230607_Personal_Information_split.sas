/* Data split by YEAR and SEX */

LIBNAME mine 'C:\Users\stat\Dropbox\eklee\SBY_data';


%macro split_data_by_5_year_and_gender(input_dataset, output_prefix);
	%do year = 1940  %to 1999  %by 5;
      		DATA mine.&output_prefix._&year._M;
        	SET &input_dataset;
        	WHERE BYEAR between &year and (&year + 4) and SEX = "³²";
      		run;

			DATA mine.&output_prefix._&year._F;
        	SET &input_dataset;
        	WHERE BYEAR between &year and (&year + 4) and SEX = "¿©";
      		run;
  	%end;
%mend;

%split_data_by_5_year_and_gender(input_dataset=mine.Personal_Information, output_prefix=dt);

