# 연습문제 1번
-- 포켓몬 중 type2가 없는 포켓몬의 수를 출력하는 query를 작성해보세요
select
  count(distinct id) as cnt
from
  `basic.pokemon`
where
  type2 is null;


# 연습문제 2번
-- type2가 없는 포켓몬의 type1과 type1의 포켓몬 수를 출력하는 query를 작성해보세요. 
-- 단, type1의 포켓몬의 수가 큰 순으로 정렬해주세요

select
  type1, count(distinct id) as cnt
from
  `basic.pokemon`
where 
  type2 is null
group by 
  type1
order by cnt desc;

# 연습문제 3번
-- type2 상관없이 type1의 포켓몬 수를 알 수 있는 query를 작성해보세요.
select
  type1, count(id) as pokemon_cnt
from
  `basic.pokemon`
group by
  type1;

# 연습문제 4번
-- 전설 여부에 따른 포켓몬 수를 알 수 있는 query를 작성해주세요.
select
  is_legendary, count(distinct id) as pokemon_id
from
  `basic.pokemon`
group by
  is_legendary;

# 연습문제 5번
-- 동명이인이 있는 이름은 무엇일까요?
select
  name, count(*) as name_cnt
from
  `basic.trainer`
group by name
having 
  name_cnt>1;

# 연습문제 6번
-- trainer table에서 iris 트레이너의 정보를 알 수 있는 query를 작성해주세요
select
  *
from
  `basic.trainer`
where
  name='Iris';

# 연습문제 7번
-- trainer table에서 "Iris", "Whitney", "Cynthia" 트레이너의 정보를 알 수 있는 query를 작성해주세요.
select
  *
from
  `basic.trainer`
where
  name in ("Iris", 'Whitney', 'Cynthia');

# 연습문제 8번
-- 전체 pokemon 수는 얼마인가요
select
  count(id) as pokemon_cnt
from
  `basic.pokemon`;

# 연습문제 9번
-- 세대 별로 포켓몬 수가 얼마나 되는지 counting 하는 query 작성해주세요
select
  generation, count(id) as pokemon_cnt
from
  `basic.pokemon`
group by 
  generation;

# 연습문제 10번
-- type2가 존재하는 포켓몬의 수는 얼마인지 출력하는 query 작성해주세요
select
  count(id) as cnt
from
  `basic.pokemon`
where
  type2 is not null;

# 연습문제 11번
-- type2가 있는 포켓몬 중 제일 많은 type1은 무엇인가요?
select
  type1, count(id) as type1_cnt
from
  `basic.pokemon`
where
  type2 is not null
group by type1
order by type1_cnt desc
limit 1;

# 연습문제 12번
-- 단일 타입만 있는 포켓몬 중 많은 type1은 무엇인가요
select
  type1, count(id) as type1_cnt
from
  `basic.pokemon`
where
  (type1 is null) 
    OR 
  (type2 is null)
group by
  type1
order by type1_cnt DESC
limit 1
;

# 연습문제 13번
-- 포켓몬의 이름에 "파"가 들어가는 포켓몬은 어떤 포켓몬들이 있을까요?
select
  kor_name
from
  `basic.pokemon`
where
  kor_name like "파%";

# 연습문제 14번
-- badge가 6개 이상인 트레이너는 몇명인가요?
select
  count(name) as trainer_cnt
from
  `basic.trainer`
where
  badge_count>=6;

# 연습문제 15번
-- trainer가 보유한 포켓몬이 제일 많은 trainer는 누구인가요?
select
  trainer_id, count(pokemon_id) as pokemon_cnt
from
  `basic.trainer_pokemon`
group by trainer_id
order by pokemon_cnt DESC
limit 1;

# 연습문제 16번
-- 포켓몬을 많이 풀어준 trainer는 누구인가요?
select
  trainer_id, count(pokemon_id) as released_cnt
from
  `basic.trainer_pokemon`
where
  status="Released"
group by
  trainer_id
order by released_cnt desc
limit 1;

# 연습문제 17번
-- trainer 별로 풀어준 포켓몬의 비율(풀어준 포켓몬/전체 포켓몬)이 20%가 넘는 트레이너는 누구인가요?
select
  trainer_id, count(pokemon_id) as total_pokemon,
  countif(status="Released") as released_pokemon 
from
  `basic.trainer_pokemon`
group by
  trainer_id
having
  (released_pokemon/total_pokemon)>=0.2;

