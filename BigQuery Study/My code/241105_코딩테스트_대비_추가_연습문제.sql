create or replace table advanced.transaction_data AS (
  SELECT 111 AS user_id, 1001 AS item_id, 719200 AS actual_price, '01/08/2024 12:00:00' AS transaction_date UNION ALL
  SELECT 111, 2002, 89000, '01/10/2024 12:00:00' UNION ALL
  SELECT 189, 2002, 89000, '01/12/2024 12:00:00' UNION ALL
  SELECT 156, 3002, 459000, '01/15/2024 12:00:00' UNION ALL
  SELECT 121, 1001, 719200, '01/18/2024 12:00:00' UNION ALL
  SELECT 156, 2001, 90300, '01/25/2024 12:00:00' UNION ALL 
  SELECT 145, 3001, 399000, '01/26/2024 12:00:00' UNION ALL
  SELECT 189, 1002, 607200, '01/28/2024 12:00:00' UNION ALL
  SELECT 111, 3001, 399000, '02/05/2024 12:00:00' UNION ALL
  SELECT 178, 1002, 759000, '02/07/2024 12:00:00' UNION ALL
  SELECT 121, 2002, 62300, '02/08/2024 12:00:00' UNION ALL 
  SELECT 156, 1001, 899000, '02/10/2024 12:00:00' UNION ALL
  SELECT 190, 2001, 90300, '02/11/2024 12:00:00' UNION ALL 
  SELECT 189, 2001, 90300, '02/14/2024 12:00:00' UNION ALL 
  SELECT 111, 1002, 759000, '02/15/2024 12:00:00' UNION ALL
  SELECT 156, 3001, 299250, '02/20/2024 12:00:00' UNION ALL
  SELECT 189, 3002, 344250, '02/25/2024 12:00:00' UNION ALL
  SELECT 111, 2001, 90300, '02/28/2024 12:00:00'  
);
create or replace table advanced.user_info AS (
  SELECT 111 AS user_id, 'Seoul' AS city, 28 AS age, 'Female' AS gender UNION ALL
  SELECT 121, 'Busan', 35, 'Male' UNION ALL
  SELECT 145, 'Incheon', 42, 'Female' UNION ALL
  SELECT 156, 'Seoul', 31, 'Male' UNION ALL
  SELECT 178, 'Daegu', 25, 'Female' UNION ALL
  SELECT 189, 'Seoul', 39, 'Male' UNION ALL
  SELECT 190, 'Busan', 29, 'Female'
);

create or replace table advanced.item_info AS (
  SELECT 1001 AS item_id, 'Electronics' AS category, 'Smartphone' AS item_name, 899000 AS list_price UNION ALL
  SELECT 1002 AS item_id, 'Electronics' AS category, 'Tablet' AS item_name, 759000 AS list_price UNION ALL
  SELECT 2001 AS item_id, 'Fashion' AS category, 'Sneakers' AS item_name, 129000 AS list_price UNION ALL
  SELECT 2002 AS item_id, 'Fashion' AS category, 'Backpack' AS item_name, 89000 AS list_price UNION ALL
  SELECT 3001 AS item_id, 'Home' AS category, 'Coffee Machine' AS item_name, 399000 AS list_price UNION ALL
  SELECT 3002 AS item_id, 'Home' AS category, 'Air Purifier' AS item_name, 459000 AS list_price
);

with item_sale as (
  SELECT
	a.item_id,
  b.category,
  parse_datetime("%M/%d/%Y %H:%M:%S", a.transaction_date) as date_format,
  a.actual_price,
  b.list_price,
  ((b.list_price-a.actual_price)/b.list_price)*100 as sale_ratio
FROM `advanced.transaction_data` as a left join `advanced.item_info` as b
on a.item_id=b.item_id)

,item_sale_weekday_add as(
  select
    *,
    extract(week from date_format) as weekday
  from item_sale
)

select
  category,
  sale_ratio,
  weekday,
  rank() over(partition by category, weekday order by sale_ratio desc) as rank_sale
from
  item_sale_weekday_add
qualify rank_sale=1
order by
  category, weekday;

--------
with item_data as (
  SELECT
	a.item_id,
  b.category,
  parse_datetime("%M/%d/%Y %H:%M:%S", a.transaction_date) as date_format,
  a.actual_price,
FROM `advanced.transaction_data` as a left join `advanced.item_info` as b
on a.item_id=b.item_id),

item_sale_weekday_add as(
  select
    *,
    extract(year from date_format) as year,
    extract(month from date_format) as month
  from item_data
)

select
  category,
  sum(actual_price) as total_price
from
  item_sale_weekday_add
where
  (year=2024)
and (month=1)
group by all
order by total_price desc
limit 1;

-----
with vip_user as(
  select
    user_id,
    sum(actual_price) as total_price
  from
    `advanced.transaction_data`
  group by all
  having total_price>=2000000
),

vip_buy as(
  select
    a.user_id,
    b.category,
    count(a.actual_price) as buy_cnt
  from
    `advanced.transaction_data` as a left join `advanced.item_info` as b
  on a.item_id=b.item_id
  where user_id in (select distinct user_id from vip_user)
  group by all
  order by buy_cnt)

select
  user_id,
  category,
  rank() over(partition by user_id order by buy_cnt desc) as rank_buy
from
  vip_buy
qualify
  rank_buy=1;
  
