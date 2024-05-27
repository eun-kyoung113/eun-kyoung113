-- case when 구문 연습
-- type1 or type2 in ('Rock', 'Ground') then new_type='Rock_Ground'
select
  id, 
  case when (type1 in ('Rock','Ground')) then'Rock_ground'
    else type1
  end as new_type1,
  case when type2 in ('Rock','Ground') then 'Rock_Ground'
    else type2
  end as new_type2
from
  `basic.pokemon`;