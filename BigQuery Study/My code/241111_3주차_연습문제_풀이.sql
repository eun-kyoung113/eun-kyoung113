# 3주차 retention 연습문제

-- 연습문제 1번
-- Weekly retention 구하는 쿼리 스스로 작성해보세요.

## 스스로 생각한 쿼리 단계 -------
## 0) core event는 이전처럼, 앱 접속으로 간주
## 1) 필요한 column만 가져오면서, date_trunc 함수 활용해 각 event_date가 속한 주의 첫 번째 일자 생성하기(변수명 : week_date)
## 2) 각 user_id(혹은 user_pseudo_id) 별, 첫 번째 앱 접속 주 계산 -- 윈도우 함수 first_value 활용(변수명 : first_week)
## 3) 첫 번째 접속 주와 이후 접속 주 간 차이 계산 : date_diff 함수 활용(변수명 : week_diff)
## 4) 각 week_diff 별 count(distinct user_pseudo_id) as user_cnt 계산
## 5) 첫 주의 user_cnt를 윈도우 함수 활용해 column으로 붙인 뒤, retention ratio 계산

with base as(
  select
    distinct
    event_date,
    user_pseudo_id,
    date_trunc(event_date,week(monday)) as date_week
  from
    `advanced.app_log`
),

first_week_add as(
  select
    distinct
    *,
    first_value(date_week) over(partition by user_pseudo_id order by date_week) as first_week
  from
    base
  order by user_pseudo_id, event_date
),

date_diff_add as(
  select
    *,
    date_diff(date_week, first_week, week) as week_diff
  from
    first_week_add
)

select
  *,
  round(safe_divide(user_cnt,first_cnt),3) as retention_ratio
from
  (select
    *,
    first_value(user_cnt) over(order by week_diff) as first_cnt
  from
    (select
      week_diff,
      count(distinct user_pseudo_id) as user_cnt
    from
      date_diff_add
    group by all))
order by week_diff;


-- 연습문제 2번
-- Retain user를 New + Current + Resurrected + Dormant user로 나누는 쿼리 작성해보세요.

## 스스로 생각한 user group 분리 방안 -------
## New user : 첫 앱 접속 주와 2023-01-20가 속한 주가 같은 user
## Dormant user : 앱 마지막 접속 주가 2023-01-20일로부터 3개월 이전인 user
## Resurrected user : LAG 함수 활용해, 전 앱 접속일과 현 앱 접속일 간 차이가 4주 이상(date_diff 활용)인 경우가 있는 user
## Current user : 위 3 group에 속하지 않는 user들

## 데이터 확인 : event_date의 최소값, 최대값 확인
-- select
--   min(event_date) as min_date,--2022-08-01
--   max(event_date) as max_date -- 2023-01-20
-- from
--   `advanced.app_log`;

-- ## 데이터 확인 : user_id가 null인 case 확인
-- select
--   *
-- from
--   `advanced.app_log`
-- where
--   user_id is null; -- user_id가 null인 case는 모두 login 하기 전 상태임을 확인(event_name=click_login)

CREATE OR REPLACE table `advanced.user_seperate` as
  with app_date as(
    select
    distinct
      user_pseudo_id,
      min(date_trunc(event_date,week(monday))) as first_view,
      max(date_trunc(event_date,week(monday))) as last_view,
      date_trunc(cast('2023-01-20'as date), week(monday)) as last_record_date
    from
      `advanced.app_log`
    group by user_pseudo_id),

  user_table1 as (
    select
      distinct user_pseudo_id, user_group
    from(
      select
        *,
        date_diff(last_record_date,last_view,month) as last_record,
        case when date_diff(last_record_date,last_view,month)>=3 then 'dormant_user'
          when date_diff(last_record_date,first_view,week)=0 then 'new_user'
          else NULL end as user_group
      from
        app_date)
    where user_group is not null),

  app_view as (
    select
      distinct
        user_pseudo_id,
        date_trunc(event_date,week(monday)) as event_date
    from
      `advanced.app_log`
    where
      user_pseudo_id not in (select distinct user_pseudo_id from user_table1)
  ),

  app_date_diff as(
    select
      *,
      date_diff(event_date,lag_event_date,week) as diff_date,
      case when date_diff(event_date,lag_event_date,week)>=4 then 'resurrected_user'
        else NULL end as user_group
    from
      (select
        *,
        lag(event_date) over(partition by user_pseudo_id order by event_date) as lag_event_date
      from
        app_view
      order by user_pseudo_id, event_date)),

  user_table2 as(
    select
      distinct user_pseudo_id
    from
      app_date_diff
    where user_group is not null
  ),

  user_seperate as(
    select
      distinct
        user_pseudo_id,
        event_date,
        case when user_pseudo_id in (select distinct user_pseudo_id from user_table1 where user_group='new_user') then 'new_user'
         when user_pseudo_id in (select distinct user_pseudo_id from user_table1 where user_group='dormant_user') then 'dormant_user'
         when user_pseudo_id in (select distinct user_pseudo_id from user_table2) then 'resurrected_user'
         else 'current_user' end as user_group
    from
      `advanced.app_log`)

select
  distinct
    user_pseudo_id,
    user_group
from
  user_seperate;


-- 연습문제 3번
-- 주어진 데이터에서 어떤 사람들의 리텐션이 그나마 높을까요?

## 스스로 생각한 쿼리 단계 -------
## core event는 이전과 동일하게 앱 접속, retention의 주기는 1주일이라 생각한다.
## 연습문제 1번 + 2번 코드 조합한다.
## 달라진 부분 : 첫 step에서 where 조건 활용해 user group 별, retention ratio 구함(좀 더 효율적인 방안...?)

with base as(
  select
    distinct
    event_date,
    user_pseudo_id,
    date_trunc(event_date,week(monday)) as date_week
  from
    `advanced.app_log`
  where
    user_pseudo_id in (select user_pseudo_id from `advanced.user_seperate` where user_group='resurrected_user')
),

first_week_add as(
  select
    distinct
    *,
    first_value(date_week) over(partition by user_pseudo_id order by date_week) as first_week
  from
    base
  order by user_pseudo_id, event_date
),

date_diff_add as(
  select
    *,
    date_diff(date_week, first_week, week) as week_diff
  from
    first_week_add
)

select
  *,
  round(safe_divide(user_cnt,first_cnt),3) as retention_ratio
from
  (select
    *,
    first_value(user_cnt) over(order by week_diff) as first_cnt
  from
    (select
      week_diff,
      count(distinct user_pseudo_id) as user_cnt
    from
      date_diff_add
    group by all))
order by week_diff;




-- 연습문제 4번
-- Core event를 "click payment"라 설정하고 Weekly retention 구하는 쿼리 스스로 작성해보세요.

## 스스로 생각한 쿼리 단계 -------
## 1번 문제와 전반적인 단계는 동일하다 판단, 단지 event_name='click payment'인 row만 filtering 선 필요하다 생각

with base as(
  select
    distinct
    event_date,
    user_pseudo_id,
    date_trunc(event_date,week(monday)) as date_week
  from
    `advanced.app_log`
  where
    event_name='click_payment' -- 추가된 부분
),

first_week_add as(
  select
    distinct
    *,
    first_value(date_week) over(partition by user_pseudo_id order by date_week) as first_week
  from
    base
  order by user_pseudo_id, event_date
),

date_diff_add as(
  select
    *,
    date_diff(date_week, first_week, week) as week_diff
  from
    first_week_add
)

select
  *,
  round(safe_divide(user_cnt,first_cnt),5) as retention_ratio
from
  (select
    *,
    first_value(user_cnt) over(order by week_diff) as first_cnt
  from
    (select
      week_diff,
      count(distinct user_pseudo_id) as user_cnt
    from
      date_diff_add
    group by all))
order by week_diff;



