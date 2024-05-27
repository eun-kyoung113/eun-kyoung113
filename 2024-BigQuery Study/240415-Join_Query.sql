-- JOIN Query 연습
select
  *
from
  `basic.trainer_pokemon` as tp
  left join 
  `basic.trainer` as t
  on
    tp.trainer_id=t.id;

-- join 여러 번 수행
select
  tp.*,
  t.* except(id),
  p.* except(id)
from
  `basic.trainer_pokemon` as tp
  left join 
  `basic.trainer` as t
  on
    tp.trainer_id=t.id
  left join
    `basic.pokemon` as p
  on tp.pokemon_id=p.id;
