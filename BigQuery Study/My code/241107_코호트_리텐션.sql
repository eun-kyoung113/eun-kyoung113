-- 코호트 리텐션
-- 저번에 구성했던 weekly retention query 활용
-- first_week(사용자 별 app 첫 접속 주) | weeks_after_first_week | active_user(week 별 앱 접속한 user 수) | cohort_users(first week에 앱 접속한 user 수) |retention_rate

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
    event_date between "2022-08-01" and "2022-12-31"),
    -- 보통 조건 걸어서 일부 데이터 확인하고 마지막에 where 조건 주석 처리하는 방식

first_data as(
  # 첫 접속 주 구하기
  select
    distinct -- distinct option 주지 않으면 중복 row 여러 개 발생함을 확인
    user_pseudo_id,
    event_date,
    date_trunc(min(event_date) over(partition by user_pseudo_id),week(monday)) 
      as first_week,
    date_trunc(event_date, week(monday)) as event_week
  from
    base),

retain_base as(
  select
    *,
    date_diff(event_week, first_week, week) as weeks_after_first_week
  from
    first_data
)

select
  *,
  round(safe_divide(active_users, cohort_users),3) as retention_ratio
from(
  select
    first_week,
    weeks_after_first_week,
    active_users,
    first_value(active_users) over(partition by first_week order by weeks_after_first_week) as cohort_users
  from
    (select
      first_week,
      weeks_after_first_week,
      count(distinct user_pseudo_id) as active_users
    from
      retain_base
    group by
      all))
order by first_week, weeks_after_first_week;

