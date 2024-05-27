-- pokemon data에서 'type' 기준으로 그룹화한 뒤, 'type 별 포켓몬 수' 집계하기
select
  type1,avg(attack) as mean_attack, count(*) as cnt
from `basic.pokemon`
group by type1;

# 연습문제
-- pokemon data에 있는 포켓몬 수 구하는 query 작성해보세요
select
  count(distinct kor_name) as cnt
from `basic.pokemon`;

# 연습문제
-- pokemon data의 세대 별 포켓몬 수 구하는 query 작성해보세요
select
  generation, count(id) as cnt
from `basic.pokemon`
group by generation;

# 연습문제
-- 포켓몬의 수를 타입별로 집계하고, 포켓몬의 수가 10 이상인 타입만 남기는 query 작성해주세요. 
-- 포켓몬의 수가 많은 순으로 정렬해주세요.
select
  type1, count(*) as cnt
from `basic.pokemon`
group by 
  type1
having 
  cnt>=10
order by 
  cnt desc;