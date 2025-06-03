
-- window 탐색 함수 연습문제 풀이
CREATE OR REPLACE TABLE
 advanced.analytics_function_01 AS(
  SELECT 1004 AS user_id,1 AS visit_month
  UNION ALL
  SELECT 1004, 3 
  UNION ALL
  SELECT 1004, 7 
  UNION ALL
  SELECT 1004, 8 
  UNION ALL
  SELECT 2112, 3 
  UNION ALL
  SELECT 2112, 6 
  UNION ALL
  SELECT 2112, 7 
  UNION ALL
  SELECT 3912, 4
 );

-- 연습문제 1번
-- user들의 다음 접속 월과 다다음 접속 월을 구하는 쿼리를 작성해주세요.
-- My answer
SELECT
  user_id, visit_month,
  LEAD(visit_month,1) OVER (PARTITION BY user_id ORDER BY visit_month) as next_visit_month,
  LEAD(visit_month,2) OVER (PARTITION BY user_id ORDER BY visit_month) as next_next_visit_month
FROM
  `advanced.analytics_function_01`
ORDER BY user_id, visit_month;

-- MEMO --
-- LEAD 함수는 ORDER BY 필수 적용해야 한다.
-- LEAD는 순서를 알아야 동작하는 함수이기 때문이다.


-- 연습문제 2번
-- user들의 다음 접속 월과 다다음 접속 월, 이전 접속 월을 구하는 쿼리를 작성해 주세요.
-- My answer
SELECT
  user_id, visit_month,
  LEAD(visit_month,1) OVER (PARTITION BY user_id ORDER BY visit_month) as next_visit_month,
  LEAD(visit_month,2) OVER (PARTITION BY user_id ORDER BY visit_month) as next_next_visit_month,
  LAG(visit_month,1) OVER (PARTITION BY user_id ORDER BY visit_month) as before_visit_month
FROM
  `advanced.analytics_function_01`
ORDER BY user_id, visit_month;

-- MEMO --
-- LAG 함수를 사용할 때 NULL이 나온다 -> 그 값은 처음이다!
-- LEAD 함수를 사용할 때 NULL이 나온다 -> 그 값은 마지막이다!

-- (연습문제) 3번
-- 유저가 접속했을 때, 다음 접속까지의 간격을 구하시오.
-- My answer
with visit_info as(
  SELECT *,
    LEAD(visit_month,1) OVER (PARTITION BY user_id ORDER BY visit_month) as next_visit_month
  FROM
    `advanced.analytics_function_01`
  ORDER BY user_id, visit_month)

SELECT
  user_id, visit_month, next_visit_month, 
  SAFE_SUBTRACT(next_visit_month,visit_month) as between_visit
FROM
  visit_info;

-- 한 줄에 쓰고 싶으면, LEAD ~~~ - visit_month as diff_month 코드 사용하면 ok
-- 중복된 쿼리를 줄이는 것이 좋으므로 sub-query 사용해서 작성하는 것이 better
-- 쿼리를 덜 수정하는 구조를 만드는 것이 good!
