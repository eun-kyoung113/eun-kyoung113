select id
, case when size_of_colony<=100 then "LOW"
      when size_of_colony<=1000 then "MEDIUM"
      else "HIGH" end as size
from ECOLI_DATA
order by id