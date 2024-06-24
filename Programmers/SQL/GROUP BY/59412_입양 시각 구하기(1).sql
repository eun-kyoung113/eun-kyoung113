select extract(hour from datetime) as hour, count(*) as cnt
from animal_outs
group by hour
having hour between '9' and '19'
order by hour