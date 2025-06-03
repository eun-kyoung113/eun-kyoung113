-- PIVOT 연습문제

-- 연습문제 1) orders 테이블에서 유저(user_id) 별로 주문 금액(amount)의 합게를 PIVOT 해주세요.
-- 날짜(order_date)를 행(Row)으로, user_id를 열로 만들어야 합니다.
SELECT
  order_date,
  MAX(IF(user_id=1,amount,0)) as user_1,
  MAX(IF(user_id=2,amount,0)) as user_2,
  MAX(IF(user_id=3,amount,0)) as user_3
FROM
  advanced.orders
GROUP BY
  order_date
ORDER BY
  order_date;


-- 연습문제 2) orders 테이블에서 날짜(order_date) 별로 유저들의 주문 금액(amount)의 합계를 PIVOT 해주세요.
-- user_id를 행으로, order_date를 열으로 만들어야 합니다.
# column name을 어떻게 지정? -- backtick(`) 사용

SELECT
  user_id,
  SUM(IF(order_date='2023-05-01',amount,0)) as `2023-05-01`,
  SUM(IF(order_date='2023-05-02',amount,0)) as `2023-05-02`,
  SUM(IF(order_date='2023-05-03',amount,0)) as `2023-05-03`,
  SUM(IF(order_date='2023-05-04',amount,0)) as `2023-05-04`,
  SUM(IF(order_date='2023-05-05',amount,0)) as `2023-05-05`
FROM
  advanced.orders
GROUP BY
  user_id
ORDER BY
  user_id;


-- 연습문제 3) orders 테이블에서 사용자(user_id) 별, 날짜(order_date) 별로 주문이 있다면 1, 없다면 0으로 PIVOT 해주세요. user_id를 행으로, order_date를 열로 만들고 주문을 많이 해도 1로 처리합니다.
# column name을 어떻게 지정? (2번 문제와 연관) -- backtick(`) 사용
SELECT
  user_id,
  IF(SUM(IF(order_date='2023-05-01',amount,0))>0,1,0) as `2023-05-01`,
  IF(SUM(IF(order_date='2023-05-02',amount,0))>0,1,0) as `2023-05-02`,
  IF(SUM(IF(order_date='2023-05-03',amount,0))>0,1,0) as `2023-05-03`,
  IF(SUM(IF(order_date='2023-05-04',amount,0))>0,1,0) as `2023-05-04`,
  IF(SUM(IF(order_date='2023-05-05',amount,0))>0,1,0) as `2023-05-05`
FROM
  advanced.orders
GROUP BY
  user_id
ORDER BY
  user_id;


-- 연습문제 3번 다른 풀이
-- 특정 column 대신 "1"을 사용할 수 있다.(유무에 따라서)
-- 만약, 횟수를 구해야 할 경우에는 MAX대신 SUM을 사용하면 된다.
SELECT
  user_id,
  MAX(IF(order_date='2023-05-01',1,0)) as `2023-05-01`,
  MAX(IF(order_date='2023-05-02',1,0)) as `2023-05-02`,
  MAX(IF(order_date='2023-05-03',1,0)) as `2023-05-03`,
  MAX(IF(order_date='2023-05-04',1,0)) as `2023-05-04`,
  MAX(IF(order_date='2023-05-05',1,0)) as `2023-05-05`
FROM
  advanced.orders
GROUP BY
  user_id
ORDER BY
  user_id;


-- 연습문제 4) 앱 로그 데이터 배열 PIVOT 하기
WITH example as (
  SELECT
    a.user_id, a.event_date, a.event_timestamp, a.event_name, a.user_pseudo_id,
    b.key as key, b.value.string_value as string_value,
    b.value.int_value as int_value
  FROM
    advanced.app_log as a
  CROSS JOIN
    UNNEST(event_params) as b
  WHERE
    event_date='2022-08-01'
)

SELECT
  user_id, event_date, event_timestamp, event_name, user_pseudo_id,
  ANY_VALUE(IF(key='firebase_screen',string_value,NULL)) as firebase_screen,
  ANY_VALUE(IF(key='food_id',int_value,NULL)) as food_id,
  ANY_VALUE(IF(key='session_id',string_value,NULL)) as session_id
FROM
  example
GROUP BY ALL
ORDER BY
  event_date, event_name;
