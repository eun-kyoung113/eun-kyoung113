select a.year, a.month, b.gender, count(distinct a.user_id) as users
from
(SELECT user_id, product_id
, substr(sales_date,1,4) as year
, substr(sales_date,6,2) as month
, substr(sales_date, 9,2) as day
from online_sale) as a left join
user_info as b
on a.user_id=b.user_id
where b.gender is not null
group by year, month, gender
order by year, month, gender