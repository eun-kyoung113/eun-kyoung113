# N day retention
# core event : app 접속

# 중간 table : user_pseudo_id | first_date | diff_date | event_date
  # 중간 table을 만들기 위한 table
  -- user_pseudo_id | first_date
  -- user_pseudo_id | event_date
  -- 두 tbl을 join

# 결과 table : diff_of_day | user_cnt


-- 첫 번째 방법
with base as (
  select
    distinct -- 그냥 가져오면 중복이 발생할 수 있음
    user_id,
    event_name,
    datetime(timestamp_micros(event_timestamp), 'Asia/Seoul') as event_datetime,
    date(timestamp_micros(event_timestamp), 'Asia/Seoul') as event_date,
    user_pseudo_id

  from
    `advanced.app_log`
  where
    event_date between "2022-08-01" and "2022-11-03"),
    -- 보통 조건 걸어서 일부 데이터 확인하고 마지막에 where 조건 주석 처리하는 방식

first_data as(
  # 첫 접속일 구하기
  select
    distinct -- distinct option 주지 않으면 중복 row 여러 개 발생함을 확인
    user_pseudo_id,
    min(event_date) as first_date
    --first_value(event_date) over(partition by user_pseudo_id order by event_date) as first_date2
  from
    base
  group by user_pseudo_id),
  --order by user_pseudo_id),

retain_base as(
  -- first_data + base
  select
    distinct
    a.user_pseudo_id,
    a.event_date,
    b.first_date,
    date_diff(a.event_date,b.first_date,day) as diff_date
  from
    base as a left join first_data as b
  on a.user_pseudo_id=b.user_pseudo_id)
  --order by user_pseudo_id, first_date, event_date)

-- 해당 데이터를 사용해 리텐션 커브 구할 때, 구글 시트 등에서 별도의 작업을 수행해야 함
-- 쿼리로 다 진행하고 싶다. 모든 row에 diff_date가 0인 cnt를 별도의 column으로 붙이고 싶다.
-- 1) 쿼리문 만들어서 join 하는 방식 : CROSS JOIN 사용
-- 2) 윈도우 함수 사용하는 방법

select
  *,
  round(safe_divide(user_cnt, first_cnt),3) as retention_ratio
from(
  select
    diff_date,
    user_cnt,
    first_value(user_cnt) over(order by diff_date) as first_cnt
  from
    (select
      diff_date,
      count(distinct user_pseudo_id) as user_cnt
    from
      retain_base
    group by
      diff_date))
order by diff_date;



----------------------------------------------------------------
-- 두 번째 방법 : 짧고 간결한 형태
-- first_date를 구하면서 바로 event_date를 구성하는 형태
with base as (
  select
    distinct -- 그냥 가져오면 중복이 발생할 수 있음
    user_id,
    event_name,
    date(timestamp_micros(event_timestamp), 'Asia/Seoul') as event_date,
    user_pseudo_id
  from
    `advanced.app_log`
  where
    event_date between "2022-08-01" and "2022-11-03"),
    -- 보통 조건 걸어서 일부 데이터 확인하고 마지막에 where 조건 주석 처리하는 방식

first_data as(
  # 첫 접속일 구하기
  select
    distinct -- distinct option 주지 않으면 중복 row 여러 개 발생함을 확인
    user_pseudo_id,
    event_date,
    min(event_date) over(partition by user_pseudo_id) 
    as first_date
  from
    base),
  --group by user_pseudo_id),
  --order by user_pseudo_id),

retain_base as(
  select
    *,
    date_diff(event_date, first_date, day) as diff_date
  from
    first_data
)

select
  *,
  round(safe_divide(user_cnt, first_cnt),3) as retention_ratio
from(
  select
    diff_date,
    user_cnt,
    first_value(user_cnt) over(order by diff_date) as first_cnt
  from
    (select
      diff_date,
      count(distinct user_pseudo_id) as user_cnt
    from
      retain_base
    group by
      diff_date))
order by diff_date;


# 두 번째 방법이 더 짧고 간결하나, 첫 번째 방법이 더 세분화적으로 filtering을 할 수 있음

