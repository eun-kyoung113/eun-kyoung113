-- 연습문제 1번
-- 사용자 별 쿼리를 실행한 총 횟수를 구하는 쿼리를 작성해주세요. 
-- 단,GROUP BY를 사용해서 집계하는 것이 아닌 query_logs의 데이터의 우측에 새로운 컬럼을 만들어주세요.
SELECT
  *,
  COUNT(*) OVER(PARTITION BY user) as use_query
FROM
  `advanced.query_logs`;


-- # data 확인
SELECT
  *,
  COUNT(*) OVER(PARTITION BY user) as use_query
FROM
  `advanced.query_logs`
order by
  user;


-- 연습문제 2번
-- 주차 별로 팀 내에서 쿼리를 많이 실행한 수를 구한 후, 실행한 수를 활용해 랭킹을 구해주세요.
-- 단,랭킹이 1등인 사람만 결과가 보이도록 해주세요.
-- Q) 주 차를 뽑는 함수..? : extract 활용
-- partiton by column 잘 생각하기!
with add_weeknum as(
  select
    *,
    extract(week from query_date) as week_number
  from
    `advanced.query_logs`),
  count_query as(
  select
    *,
    count(user) over(partition by week_number, team, user order by week_number,     team, user rows between unbounded preceding and current row) as query_total
  from
    add_weeknum)

select 
  user, team, week_number,
  rank() over(partition by week_number, team order by query_total desc) as rank_query
from
  count_query
qualify
  rank_query=1
order by
  week_number;

-- # 간단한 답
-- # 윈도우 함수 대신, GROUP BY를 쓰면 절차 줄일 수 있음!
with query_total_cnt as(
  select
    extract(week from query_date) as week_number,
    team,
    user,
    count(query_date) as query_total
  from
    `advanced.query_logs`
  group by all
)

select
  *,
  rank() over(partition by week_number, team order by query_total desc) as rank_query
from
  query_total_cnt
qualify
  rank_query=1
order by
  week_number;


-- 연습문제 3번
-- (2번 문제에서 사용한 주차별 쿼리 사용)쿼리를 실행한 시점 기준 1주 전에 쿼리 실행 수를 별도의 컬럼으로 확인할 수 있는 쿼리를 작성해주세요.
-- partiton by column 잘 생각하기!
with add_weeknum as(
  select
    *,
    extract(week from query_date) as week_number
  from
    `advanced.query_logs`),
  count_query as(
  select
    *,
    count(user) over(partition by week_number, user order by week_number, team, user rows between unbounded preceding and current row) 
    as query_total
  from
    add_weeknum
  order by
    user, week_number),
  
  week_query as(
    select
      distinct user, team, week_number,
      last_value(query_total) over (partition by user, week_number order by week_number, team, user) as week_total_cnt
    from
      count_query
    order by
      user, week_number)

select
   *,
   lag(week_total_cnt,1) over (partition by user order by week_number) as last_query
from
 week_query
order by
  user, week_number;


-- 연습문제 4번
-- 시간의 흐름에 따라, 일자별로 유저가 실행한 누적 쿼리수를 작성해 주세요.
-- # WITH 구문에서 윈도우 함수 대신, group by를 사용하는 것이 더 간단할 듯
with daily_query as(
  select
    distinct user, team, query_date,
    count(*) over (partition by user, query_date) as query_count
  from
    `advanced.query_logs`
  order by
    user, query_date
)

select
  *,
  sum(query_count) over (partition by user order by user, query_date rows between unbounded preceding and current row) as cumulative_query_count
  -- # order by 구문이 있고, frame 구문은 없을 때 frame의 default 값 : rows between unbounded preceding and current row
from
  daily_query
order by
  user, query_date;


-- 연습문제 5번
-- 다음 데이터는 주문 횟수를 나타낸 데이터입니다. 만약 주문횟수가 없으면 NULL로 기록됩니다.
-- 이런 데이터에서 NULL 값이라고 되어있는 부분을 바로 이전 날짜의 값으로 채워주는 쿼리를 작성해 주세요.
-- Q) 5월 12일은 어떻게 처리? / 이틀 전 주문량으로 대체?
-- # LAST_VALUE & IGNORE NULLS 활용
WITH raw_data AS(
  SELECT DATE'2024-05-01'AS date,15 AS number_of_orders 
  UNION ALL
  SELECT DATE'2024-05-02',13 
  UNION ALL
  SELECT DATE'2024-05-03',NULL 
  UNION ALL
  SELECT DATE'2024-05-04',16
  UNION ALL
  SELECT DATE'2024-05-05',NULL
  UNION ALL
  SELECT DATE'2024-05-06', 18 
  UNION ALL
  SELECT DATE'2024-05-07',20 
  UNION ALL
  SELECT DATE'2024-05-08',NULL
  UNION ALL
  SELECT DATE'2024-05-09',13
  UNION ALL
  SELECT DATE'2024-05-10',14
  UNION ALL
  SELECT DATE'2024-05-11',NULL
  UNION ALL
  SELECT DATE'2024-05-12',NULL
 )
 SELECT
 *,
 case when number_of_orders is null then
  lag(number_of_orders,1) over(order by date)
  else number_of_orders end as number_of_orders_re
 FROM
  raw_data
order by
  date;

-- 답안
WITH raw_data AS(
  SELECT DATE'2024-05-01'AS date,15 AS number_of_orders 
  UNION ALL
  SELECT DATE'2024-05-02',13 
  UNION ALL
  SELECT DATE'2024-05-03',NULL 
  UNION ALL
  SELECT DATE'2024-05-04',16
  UNION ALL
  SELECT DATE'2024-05-05',NULL
  UNION ALL
  SELECT DATE'2024-05-06', 18 
  UNION ALL
  SELECT DATE'2024-05-07',20 
  UNION ALL
  SELECT DATE'2024-05-08',NULL
  UNION ALL
  SELECT DATE'2024-05-09',13
  UNION ALL
  SELECT DATE'2024-05-10',14
  UNION ALL
  SELECT DATE'2024-05-11',NULL
  UNION ALL
  SELECT DATE'2024-05-12',NULL
 )
 
 SELECT
 *,
 LAST_VALUE(number_of_orders IGNORE NULLS) OVER(ORDER BY date) as number_of_orders_re
 FROM
  raw_data
order by
  date;


-- 연습문제 6번
-- 5번 문제에서 NULL을 채운 후, 2일 전 ~ 현재 데이터의 평균을 구하는 쿼리를 작성해주세요(이동평균).
WITH raw_data AS(
  SELECT DATE'2024-05-01'AS date,15 AS number_of_orders 
  UNION ALL
  SELECT DATE'2024-05-02',13 
  UNION ALL
  SELECT DATE'2024-05-03',NULL 
  UNION ALL
  SELECT DATE'2024-05-04',16
  UNION ALL
  SELECT DATE'2024-05-05',NULL
  UNION ALL
  SELECT DATE'2024-05-06', 18 
  UNION ALL
  SELECT DATE'2024-05-07',20 
  UNION ALL
  SELECT DATE'2024-05-08',NULL
  UNION ALL
  SELECT DATE'2024-05-09',13
  UNION ALL
  SELECT DATE'2024-05-10',14
  UNION ALL
  SELECT DATE'2024-05-11',NULL
  UNION ALL
  SELECT DATE'2024-05-12',NULL
 ),

 newdata as(
  SELECT
    *,
    LAST_VALUE(number_of_orders IGNORE NULLS) OVER(ORDER BY date) as number_of_orders_re -- 수정한 부분
  FROM
    raw_data
  order by
    date)

select
  date, number_of_orders_re,
  avg(number_of_orders_re) over (order by date rows between 2 preceding and current row) as moving_avg
from
  newdata
order by
  date;


-- 연습문제 7번
-- app_logs 테이블에서 CustomSession을 만들어 주세요. 이전 이벤트 로그와 20초가 지나면 새로운 Session을 만들어 주세요.
-- Session은 숫자로(1,2,3…) 표시해도 됩니다. 2022-08-18일의 user_pseudo_id(1997494153.8491999091)은 session_id가 4까지 나옵니다.
-- Q) session_start id 어떻게....????????
-- # 처음에는 datetime diff의 누적 합을 생각했음
with event_data as(
  select
    event_date, event_timestamp, datetime(timestamp_micros(event_timestamp),'Asia/Seoul') as event_datetime,
    event_name, user_id, user_pseudo_id
  from
    `advanced.app_log`
  where
    user_pseudo_id='1997494153.8491999091'
  order by user_pseudo_id, event_datetime
),
before_eventtime_add as(
  select
    *,
    lag(event_datetime,1) over (partition by user_pseudo_id order by user_pseudo_id, event_datetime) as before_datetime
  from
    event_data
  order by
    user_pseudo_id, event_datetime
)

select
  *,
  datetime_diff(event_datetime,before_datetime,second) as second_diff
from
  before_eventtime_add
order by
    user_pseudo_id, event_datetime;

-- 답안
-- # event_datetime이랑 before_datetime을 빼서 20초가 넘으면 새로운 세션으로 정의한다.
with event_data as(
  select
    event_date, event_timestamp, datetime(timestamp_micros(event_timestamp),'Asia/Seoul') as event_datetime,
    event_name, user_id, user_pseudo_id
  from
    `advanced.app_log`
  where
    user_pseudo_id='1997494153.8491999091'
  order by user_pseudo_id, event_datetime
),
before_eventtime_add as(
  select
    *,
    lag(event_datetime,1) over (partition by user_pseudo_id order by user_pseudo_id, event_datetime) as before_datetime
  from
    event_data
  order by
    user_pseudo_id, event_datetime
),
diff_data as(
  select
    *,
    datetime_diff(event_datetime,before_datetime,second) as second_diff
  from
    before_eventtime_add
  order by
    user_pseudo_id, event_datetime
)

select
  *,
  sum(start_id) over (partition by user_pseudo_id order by event_datetime) as session_num
from(
  select
    *,
    case when second_diff is null then 1
      when second_diff>=20 then 1
      else 0 end as start_id
  from
    diff_data);

