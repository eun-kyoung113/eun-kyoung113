select product_id, product_name, sum(sales) as total_sales
from
(select a.product_id, a.product_name, b.amount, a.price, (b.amount*a.price) as sales
from food_product as a inner join food_order as b
on a.product_id=b.product_id
where substr(b.produce_date,1,7)='2022-05') as b
group by product_id
order by total_sales desc, product_id