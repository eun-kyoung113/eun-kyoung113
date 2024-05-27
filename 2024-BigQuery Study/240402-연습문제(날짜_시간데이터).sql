## 연습문제 -----------------

-- 1. 트레이너가 포켓몬을 포획한 날짜(catch_date)를 기준으로, 2023년 1월에 포획한 포켓몬의 수를 계산해주세요.
-- 수정 전
select
  count(pokemon_id) as pokemon_cnt
from
  (select
    cast(catch_date as string) as str_catch_date,
    trainer_id,
    pokemon_id
  from
    `basic.trainer_pokemon`)
where 
  substr(str_catch_date,1,7)='2023-01'; -- 86마리

-- 수정 후
select
  count(pokemon_id) as pokemon_cnt
from
  (select
    pokemon_id, cast(datetime(catch_datetime,"Asia/Seoul") as string) as catch_datetime2
  from
    `basic.trainer_pokemon`)
where
  substr(catch_datetime2,1,7)='2023-01'; -- 85마리

-- 선생님 답
select
  count(pokemon_id) as pokemon_cnt
from
  `basic.trainer_pokemon`
where
  extract(year from datetime(catch_datetime,"Asia/Seoul"))=2023
and
  extract(month from datetime(catch_datetime,"Asia/Seoul"))=1;


-- 2. 배틀이 일어난 시간(battle_time)을 기준으로, 오전 6시에서 오후 6시 사이에 일어난 배틀의 수를 계산해주세요.
--- battle_datetime과 battle_timestamp 검증
select
  countif(battle_datetime=datetime(battle_timestamp,"Asia/Seoul")) as battle_datetime_kr_same
from
  `basic.battle`;

-- 답
select
  count(id) as battle_cnt
from
  (select
    id,
    extract(hour from battle_datetime) as battle_time
  from
    `basic.battle`)
where
  battle_time between 06 and 18; -- 44건
  -- extract(hour from battle_datetime) between 6 and 18도 가능


-- 3. 각 트레이너별로 그들이 포켓몬을 포획한 첫 날(catch_date)를 찾고, 그 날짜를 'DD/MM/YYYY' 형식으로 출력해주세요.
-- 주의
-- catch_date는 UTC 기준 값이므로 catch_datetime은 한국 기준으로 선 변경해야 한다!!
select
  trainer_id, format_date("%d/%m/%Y",min(date(datetime(catch_datetime,"Asia/Seoul")))) as first_date
from
  `basic.trainer_pokemon`
group by
  trainer_id
order by trainer_id;


-- 4. 배틀이 일어난 날짜(battle_date)를 기준으로, 요일별로 배틀이 얼마나 자주 일어났는지 계산해주세요.
select
  day_battle,count(id) as battle_cnt
from
  (select
      id, extract(dayofweek from battle_date) as day_battle
    from 
      `basic.battle`)
group by day_battle
order by day_battle;

-- 5. 트레이너가 포켓몬을 처음으로 포획한 날짜와 마지막으로 포획한 날짜의 간격이 큰 순으로 정렬하는 쿼리를 작성해주세요.
select
  trainer_id, 
  date_diff(last_date,first_date,day) as diff_date
from
  (select
    trainer_id,min(datetime(catch_datetime,"Asia/Seoul")) as first_date, 
    max(datetime(catch_datetime,"Asia/Seoul")) as last_date
  from
    `basic.trainer_pokemon`
  group by trainer_id)
order by diff_date DESC;
