-- 윈도우 함수 집계 및 frame 연습 문제

-- My answer
SELECT
  *,
  sum(amount) over (order by order_id rows between unbounded preceding and unbounded following) as total_amount,
  -- 전체 합을 구하고 싶을 때, over () 안에 아무것도 작성 안해도 됨
  sum(amount) over (order by order_date, order_id rows between unbounded preceding and current row) as cumulative_sum,
  -- sum(amount) over(order by order_id) 로만 해도 ok
  sum(amount) over (partition by user_id order by user_id, order_date 
                    rows between unbounded preceding and current row) as cumulative_sum_user,
  avg(amount) over (order by order_date, order_id rows between 5 preceding and 1 preceding) as avg_5_last
FROM
  `advanced.orders`
ORDER BY
  order_id;


-- MEMO --
-- OVER() 안에 아무것도 들어가지 않는 경우도 있음

