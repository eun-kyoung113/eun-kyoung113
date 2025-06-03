-- 연습문제 데이터 생성
CREATE OR REPLACE TABLE advanced.array_exercises as
SELECT movie_id, title, actors, genres
FROM(
  SELECT 
    1 as movie_id,
    'Avengers:Endgame' as title,
    ARRAY<STRUCT<actor STRING, character STRING>>[
      STRUCT('Robert Downey Jr.','Tony Stark'),
      STRUCT('Chris Evans','Steve Rogers')
    ] as actors, 
    ARRAY<STRING>['Action','Adventure','Drama'] as genres
    UNION ALL
    SELECT
      2,
      'Inception',
      ARRAY<STRUCT<actor STRING, character STRING>>[
        STRUCT('Leonardo Dicaprio','Cobb'),
        STRUCT('Joseph Gordon-Levitt','Arthur')
    ], 
    ARRAY<STRING>['Action','Adventure','Sci-Fi']
    UNION ALL
    SELECT
      3,
      'The Dark Knight',
      ARRAY<STRUCT<actor STRING, character STRING>>[
        STRUCT('Christian Bale','Bruce Wayne'),
        STRUCT('Heath Ledger','Joker')
    ], 
    ARRAY<STRING>['Action','Crime','Drama']

);

-- 연습문제 1) array_exercises table에서 title 별로 영화 genres를 UNNEST해서 보여주세요.
# UNNEST(ARRAY_COLUMN) AS 새로운 이름
# SELECT 절에서 새로운 이름으로 사용한다. 기존의 ARRAY_COLUMN은 사용하지 않는다.
SELECT
  a.title, genre
FROM
  advanced.array_exercises as a
CROSS JOIN
  UNNEST(genres) as genre;


-- 연습문제 2) array_exercises table에서 title 별로 배우(actor)와 배역(character)을 UNNEST해서 보여주세요. 배우와 배역은 별도의 column으로 나와야 합니다.
SELECT a.title, person.actor as actor, person.character as character
FROM 
  advanced.array_exercises as a
CROSS JOIN
    UNNEST(actors) as person;


-- 연습문제 3) array_exercises table에서 title 별로 배우(actor), 배역(character), 장르를 한 row에 표시되도록 출력하세요.
# CROSS JOIN 하면 데이터의 중복이 어느 정도 발생, 어쩔 수 없는 이슈이다.
SELECT
  a.title, person.actor as actor, person.character as character, genre
FROM
  advanced.array_exercises as a
CROSS JOIN
  UNNEST(actors) as person
CROSS JOIN
  UNNEST(genres) as genre;


-- 3)번 문제 관련 추가
# 3) 결과를 기준으로, 조건문을 사용하고 싶다.
# "WHERE"를 바로 사용하면 error 발생 : 실행 순서가 FROM -> JOIN -> SELECT
# actors -> UNNEST(actors) : STRUCT<actor, character>
# genres -> UNNEST(genres) : STRING
SELECT
  a.title, person.actor as actor, person.character as character, genre
FROM
  advanced.array_exercises as a
CROSS JOIN
  UNNEST(actors) as person
CROSS JOIN
  UNNEST(genres) as genre
WHERE
  person.actor='Chris Evans' -- 그냥 actor 사용하면 error 발생함
  and
  genre='Action';


-- 연습문제 4) 앱 로그 데이터(app_logs)의 배열을 풀어주세요
SELECT
  a.user_id, a.event_date, a.event_name, a.user_pseudo_id, b.key as key,
  b.value.string_value as string_value, b.value.int_value as int_value
FROM
  advanced.app_log as a
CROSS JOIN
  UNNEST(event_params) as b;

-- 4)번 문제 관련, 추가 사항(일 별, 사용자 수 집계)
WITH base as(
  SELECT
  a.user_id, a.event_date, a.event_name, a.user_pseudo_id, b.key as key,
  b.value.string_value as string_value, b.value.int_value as int_value
FROM
  advanced.app_log as a
CROSS JOIN
  UNNEST(event_params) as b
)

SELECT
  event_date,count(user_id) as cnt
FROM
  base
GROUP BY
  event_date
ORDER BY
  event_date;
