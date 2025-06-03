-- 퍼널 데이터
-- screen_view : welcome, home, food_category, restaurant, cart, click_payment
-- step_number : 추후 정렬을 위한 column
-- 사용할 데이터 : 앱 로그 데이터 -> UNNEST -> PIVOT
-- 데이터 기간 : 2022-08-01 ~ 2022-08-18

-- 데이터 UNNEST
with base_data as(
  SELECT
    event_date, event_timestamp, event_name, user_id,
    user_pseudo_id,platform,
    MAX(IF(event_params.key="firebase_screen",event_params.value.   string_value,NULL)) as firebase_screen,
    MAX(IF(event_params.key="food_id",event_params.value.int_value,NULL)) as food_id,
    MAX(IF(event_params.key="session_id",event_params.value.string_value,NULL)) as session_id
  FROM
    `advanced.app_log`
  CROSS JOIN
    UNNEST(event_params) as event_params
  WHERE
    event_date between '2022-08-01' and '2022-08-18'
  GROUP BY ALL
), filter_event_and_concat_event_and_screen as(
  -- event_name + screen (필요한 이벤트만 가져올 필요가 있음)
  SELECT
    * EXCEPT(event_name, firebase_screen, food_id, event_timestamp),
    CONCAT(event_name,"-", firebase_screen) as event_name_with_screen,
    datetime(timestamp_micros(event_timestamp),"Asia/Seoul") as event_datetime
  FROM
    base_data
  WHERE
    event_name in ("screen_view","click_payment")
)


-- step_number 생성 + count
-- step_number : CASE WHEN 사용
-- 일자 상관없이 퍼널의 유저 수를 집계
-- SELECT
--   event_name_with_screen,
--   case when event_name_with_screen="screen_view-welcome" then 1
--       when event_name_with_screen="screen_view-home" then 2
--       when event_name_with_screen="screen_view-food_category" then 3
--       when event_name_with_screen="screen_view-restaurant" then 4
--       when event_name_with_screen="screen_view-cart" then 5
--       when event_name_with_screen="click_payment-cart" then 6
--   else NULL
--   end as step_number
--   , count(distinct user_pseudo_id) as cnt

-- FROM
--   filter_event_and_concat_event_and_screen
-- GROUP BY ALL
-- HAVING
--   step_number is not null
-- ORDER BY
--   step_number;

-- 일자 별, 퍼널 별 유저 수 집계
SELECT
  event_date, event_name_with_screen,
  case when event_name_with_screen="screen_view-welcome" then 1
      when event_name_with_screen="screen_view-home" then 2
      when event_name_with_screen="screen_view-food_category" then 3
      when event_name_with_screen="screen_view-restaurant" then 4
      when event_name_with_screen="screen_view-cart" then 5
      when event_name_with_screen="click_payment-cart" then 6
  else NULL
  end as step_number
  , count(distinct user_pseudo_id) as cnt
FROM
  filter_event_and_concat_event_and_screen
GROUP BY ALL
HAVING
  step_number is not null
ORDER BY
  event_date, step_number;


-- 데이터 PIVOT 한 형태로 변환(시각화 용이)
-- with 구문으로 만든 데이터들을 저장하는 방법은...?
with base_data as(
  SELECT
    event_date, event_timestamp, event_name, user_id,
    user_pseudo_id,platform,
    MAX(IF(event_params.key="firebase_screen",event_params.value.   string_value,NULL)) as firebase_screen,
    MAX(IF(event_params.key="food_id",event_params.value.int_value,NULL)) as food_id,
    MAX(IF(event_params.key="session_id",event_params.value.string_value,NULL)) as session_id
  FROM
    `advanced.app_log`
  CROSS JOIN
    UNNEST(event_params) as event_params
  WHERE
    event_date between '2022-08-01' and '2022-08-18'
  GROUP BY ALL
), filter_event_and_concat_event_and_screen as(
  -- event_name + screen (필요한 이벤트만 가져올 필요가 있음)
  SELECT
    * EXCEPT(event_name, firebase_screen, food_id, event_timestamp),
    CONCAT(event_name,"-", firebase_screen) as event_name_with_screen,
    datetime(timestamp_micros(event_timestamp),"Asia/Seoul") as event_datetime
  FROM
    base_data
  WHERE
    event_name in ("screen_view","click_payment")
), funnel_data as(
  SELECT
    event_date, event_name_with_screen,
    case when event_name_with_screen="screen_view-welcome" then 1
        when event_name_with_screen="screen_view-home" then 2
        when event_name_with_screen="screen_view-food_category" then 3
        when event_name_with_screen="screen_view-restaurant" then 4
        when event_name_with_screen="screen_view-cart" then 5
        when event_name_with_screen="click_payment-cart" then 6
    else NULL
    end as step_number
    , count(distinct user_pseudo_id) as cnt

  FROM
    filter_event_and_concat_event_and_screen
  GROUP BY ALL
  HAVING
    step_number is not null
  ORDER BY
    event_date, step_number
)

SELECT
  event_date,
  MAX(IF(step_number=1,cnt,NULL)) as `screen_view-welcome`,
  MAX(IF(step_number=2,cnt,NULL)) as `screen_view-home`,
  MAX(IF(step_number=3,cnt,NULL)) as `screen_view-food_category`,
  MAX(IF(step_number=4,cnt,NULL)) as `screen_view-restaurant`,
  MAX(IF(step_number=5,cnt,NULL)) as `screen_view-cart`,
  MAX(IF(step_number=6,cnt,NULL)) as `click_payment-cart`
FROM
  funnel_data
GROUP BY ALL
ORDER BY event_date;

