# Weekly retention
# core event : app 접속
# query의 핵심 : event_date를 event_week로 변경
-- week 함수를 사용하거나 date_trunc(일자, 자를 기준) 함수 사용
-- week 함수를 추천하지는 않음(직관적이진 않음)
-- 주차 별 첫 일자를 직관적으로 알고 싶을 때 date_trunc를 사용하는 것이 나음

-- (깨알 지식) WEEK VS ISO_WEEK
  -- 주 번호를 계산할 때 사용할 수 있는 함수
  -- WEEK : 일요일이 주의 첫 날로 간주 / 1월 1일이 속한 주를 첫 번째 주로 간주
  -- ISO_WEEK : 월요일이 주의 첫 날로 간주 / 국제표준에 따라 정의하며, 
  --            목요일이 속한 주를 기준으로 첫 번째 주 정의
  --            (연도의 첫 목요일이 있는 주부터 1주)

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
    --min(event_date) as first_date
    date_trunc(min(event_date) over(partition by user_pseudo_id),week(monday)) 
      as first_date,
    date_trunc(event_date, week(monday)) as event_week
  from
    base),
  --group by user_pseudo_id),
  --order by user_pseudo_id),

retain_base as(
  select
    *,
    date_diff(event_week, first_date, week) as diff_week
  from
    first_data
)

select
  *,
  round(safe_divide(user_cnt, first_cnt),3) as retention_ratio
from(
  select
    diff_week,
    user_cnt,
    first_value(user_cnt) over(order by diff_week) as first_cnt
  from
    (select
      diff_week,
      count(distinct user_pseudo_id) as user_cnt
    from
      retain_base
    group by
      diff_week))
order by diff_week;
