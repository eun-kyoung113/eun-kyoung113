select animal_id, name
from
    (SELECT a.animal_id, a.name,
        (b.datetime-a.datetime) as period
    from animal_ins as a inner join animal_outs as b
    on a.animal_id=b.animal_id
    having period>0
    order by period desc) as c
limit 2