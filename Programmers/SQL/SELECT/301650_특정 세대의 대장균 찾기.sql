with first_parent as(
    select distinct id
    from ecoli_data
    where parent_id is null),
    
    second_parent as(
    select distinct id
    from ecoli_data
    where parent_id in (select id from first_parent))

select id
from ecoli_data
where parent_id in (select id from second_parent)
order by id