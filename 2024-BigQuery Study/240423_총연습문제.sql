
-- 총 연습문제

-- 1. 각 트레이너 별로 가진 포켓몬의 평균 레벨을 계산하고, 그 중 평균 레벨이 높은 TOP3 트레이너의 이름과 보유한 포켓몬의 수, 
-- 평균 레벨을 출력해주세요.
SELECT
  b.name,
  avg(a.level) as level_mean,
  count(*) as pokemon_cnt
FROM
  (SELECT
    trainer_id,
    pokemon_id,
    level,
    status
  FROM
    `basic.trainer_pokemon`
  WHERE
    status in ('Active','Training')) as a left join
  (SELECT
    id,
    name
  FROM
    `basic.trainer`) as b
  on a.trainer_id=b.id
GROUP BY
  name
ORDER BY
  level_mean desc
limit 3;


-- 2. 각 포켓몬 type1을 기준으로, 가장 많이 포획된(방출 여부 상관없음) 포켓몬의 type1, 이름, 포획횟수를 출력해주세요.
SELECT
  type1,
  kor_name,
  count(a.id) as cnt
FROM
  (SELECT
    id,
    pokemon_id
  FROM
    `basic.trainer_pokemon`) as a left join
  (SELECT
    id,
    kor_name,
    type1
  FROM
    `basic.pokemon`) as b
  on a.pokemon_id=b.id 
GROUP BY
  type1, kor_name
ORDER BY
  cnt desc
LIMIT 1;


-- 3. 전설의 포켓몬을 보유한 트레이너들은 전설의 포켓몬과 일반 포켓몬을 얼마나 보유하고 있을까요?
-- 트레이너 이름도 같이 출력해주세요.
WITH trainer_pokemon2 as(
  SELECT
    a.id,
    a.trainer_id,
    a.pokemon_id,
    b.id,
    b.is_legendary,
    a.status
  FROM
    `basic.trainer_pokemon` as a left join
    `basic.pokemon` as b
    on a.pokemon_id=b.id
  WHERE
    status in ('Active','Training')
)
SELECT
  b.name,
  countif(cast(a.is_legendary as string)='true') as legendary_cnt,
  --sum(case when is_legendary is true then 1 else 0 end) as legendary_cnt
  countif(cast(a.is_legendary as string)='false') as not_legendary_cnt
FROM
  trainer_pokemon2 as a left join
  `basic.trainer` as b
  on a.trainer_id=b.id
GROUP BY
  name
HAVING
  legendary_cnt!=0;


-- 4. 가장 승리가 많은 트레이너 ID, 트레이너 이름, 승리한 횟수, 보유한 포켓몬의 수, 평균 포켓몬의 레벨을 출력해주세요.
-- 단, 포켓몬의 레벨을 소수점 2째 자리에서 반올림해주세요. (반올림함수 : ROUND)
WITH trainer_pokemon2 AS(
  SELECT
    a.trainer_id,
    b.name as trainer_name,
    count(a.pokemon_id) as pokemon_cnt,
    round(avg(a.level),2) as pokemon_level,
  FROM
    `basic.trainer_pokemon` as a left join
    `basic.trainer` as b
    on a.trainer_id=b.id
  WHERE
    a.status !='Released'
  GROUP BY
    trainer_id,
    name
),
battle_info AS(
  SELECT
    winner_id,
    count(id) as win_battle_cnt
  FROM
    `basic.battle`
  WHERE
    winner_id is not null
  GROUP BY
    winner_id
  ORDER BY
    win_battle_cnt desc  
)
SELECT
  * except(trainer_id)
FROM
  battle_info as a left join
  trainer_pokemon2 as b
   on a.winner_id=b.trainer_id
ORDER BY
  win_battle_cnt desc
LIMIT 1;

-- 5. 트레이너가 잡았던 포켓몬의 총 공격력(attack)과 방어력(defense)의 합을 계산하고, 이 합이 가장 높은 트레이너를 찾으세요.
WITH pokemon_ability AS(
  SELECT
    id,
    (attack+defense) as sum_attack_defense
  FROM
    `basic.pokemon`
)
SELECT
  a.trainer_id,
  b.*
FROM
  `basic.trainer_pokemon` as a left join
  pokemon_ability as b
  on a.pokemon_id=b.id
ORDER BY sum_attack_defense desc
LIMIT 1;


-- 6. 각 포켓몬의 최고 레벨과 최저 레벨을 계산하고, 레벨 차이가 가장 큰 포켓몬의 이름을 출력하세요. 
WITH pokemon_level AS(
  SELECT
    pokemon_id,
    (max(level)-min(level)) as diff_level
  FROM
  `basic.trainer_pokemon`
  GROUP BY
    pokemon_id
)
SELECT
  a.*,
  b.kor_name
FROM
  pokemon_level as a left join
  `basic.pokemon` as b
  on a.pokemon_id=b.id
ORDER BY
  a.diff_level desc
LIMIT 1;

-- 7. 각 트레이너가 가진 포켓몬 중에서 공격력(attack)이 100 이상인 포켓몬과 100 미만인 포켓몬의 수를 각각 계산해주세요. 트레이너의 이름과 두 조건에 해당하는 포켓몬의 수를 출력해 주세요.
WITH trainer_info AS(
  SELECT
    a.trainer_id,
    a.pokemon_id,
    b.name
  FROM
    `basic.trainer_pokemon` as a left join
    `basic.trainer` as b
    on a.trainer_id=b.id
  WHERE
    a.status != 'Released'
)
SELECT
  a.name,
  countif(b.attack>=100) as attack_top_cnt,
  countif(b.attack<100) as attack_low_cnt
FROM
  trainer_info as a left join
  `basic.pokemon` as b
  on a.pokemon_id=b.id
GROUP BY
  a.name;




