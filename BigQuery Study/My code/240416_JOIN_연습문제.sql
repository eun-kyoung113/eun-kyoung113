-- 연습문제 1번
-- 트레이너가 보유한 포켓몬들은 얼마나 있는지 알 수 있는 쿼리 작성해 주세요
-- 참고 : 보유했다의 정의는 status가 Active, Training인 경우 의미
-- 예상 결과 : (pokemon_name, cnt) 구성
-- sub-query 이용해 먼저 filtering(status 관련) 하는 것을 추천
select
  kor_name,count(*) as cnt
from
  (select
    a.trainer_id,a.pokemon_id,b.kor_name,a.status
  from
    `basic.trainer_pokemon`as a left join `basic.pokemon` as b
    on a.pokemon_id=b.id
    and a.status in ('Active', 'Training'))
group by kor_name
order by cnt DESC;


-- 연습문제 2번
-- 각 트레이너가 가진 포켓몬 중에서 'Grass' 타입의 포켓몬 수를 계산해주세요.(단, 편의 위해 type1 기준으로 계산)
select
  type1,count(*) as cnt
from
  (select
    a.trainer_id,a.pokemon_id,b.type1,a.status
  from
    `basic.trainer_pokemon`as a left join `basic.pokemon` as b
    on a.pokemon_id=b.id
    and a.status in ('Active', 'Training'))
where
  type1='Grass'
group by type1;

-- 재 작성
select
  b.type1, count(*) as cnt
from
  (select
    trainer_id, pokemon_id, status
  from
    `basic.trainer_pokemon`
  where
    status in ("Active",'Training')) as tp
  left join `basic.pokemon` as b
  on tp.pokemon_id=b.id
where
  type1='Grass'
group by type1;


-- 연습문제 3번
-- 트레이너의 고향(hometown)과 포켓몬을 포획한 위치(location)를 비교하여, 자신의 고향에서 포켓몬을 포획한 트레이너의 수를 계산해주세요.
-- 참고 : status 상관 없이 구해주세요
select
  count(distinct trainer_id) as trainer_cnt
from
  `basic.trainer_pokemon` as a left join `basic.trainer`as b
on
  a.trainer_id=b.id
where
  a.location=b.hometown;


-- 연습문제 4번
-- Master 등급인 트레이너들은 어떤 타입의 제일 많이 보유하고 있을까요?
-- 참고 : 보유했다의 정의는 1번 문제의 정의와 동일
select
  c.type1,count(*) as cnt
from
  (select
    a.trainer_id,a.pokemon_id,a.status,b.achievement_level
  from
    `basic.trainer_pokemon` as a left join `basic.trainer` as b
    on 
      a.trainer_id=b.id
  where 
    (a.status in ("Active","Training"))
    and
      (b.achievement_level='Master')) as tp
  left join `basic.pokemon` as c
  on
    tp.pokemon_id=c.id
group by type1;


-- 연습문제 5번
-- Incheon 출신 트레이너들은 1세대, 2세대 포켓몬을 각각 얼마나 보유하고 있나요?
select
  c.generation,count(c.id) as cnt
from
  (select
    a.trainer_id, a.pokemon_id,a.status,b.id,b.hometown 
  from `basic.trainer_pokemon` as a 
  left join 
  `basic.trainer` as b
  on a.trainer_id=b.id
  where
    hometown='Incheon'
  and
    status in ('Active','Training'))as tp 
  left join `basic.pokemon` as c
  on tp.pokemon_id=c.id
where c.generation in (1,2)
group by generation;


-- 연습문제 6번
-- 각 배틀에서 참가한 두 플레이어의 이름과 승자의 이름을 출력해주세요.
select
  b.name as player1_name,
  c.name as player2_name, d.name as winner_name
from
  `basic.battle` as a left join `basic.trainer` as b
  on a.player1_id=b.id
  left join `basic.trainer` as c
  on a.player2_id=c.id
  left join `basic.trainer` as d
  on a.winner_id=d.id;


