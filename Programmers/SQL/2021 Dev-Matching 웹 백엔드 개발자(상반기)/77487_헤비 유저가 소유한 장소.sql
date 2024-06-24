with host as(
    select host_id, count(*) as cnt
    from places
    group by host_id
    having cnt>=2)

SELECT *
from places
where host_id in (select distinct host_id from host)
order by id