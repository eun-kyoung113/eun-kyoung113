SET @HOUR = -1;

with animal_count as(
    select extract(hour from datetime) as hour, count(*) as count
    from animal_outs
    group by hour),
    
    hour_count as(   
    SELECT (@HOUR := @HOUR +1) AS HOUR
    FROM ANIMAL_OUTS
    WHERE @HOUR < 23)

select hour, 
case when count is null then 0 
     else count end as count
from
    (select a.hour, b.count
    from hour_count as a left join animal_count as b
    on a.hour=b.hour) as c
order by hour