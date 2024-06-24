SELECT b.category, b.max_price, a.product_name
from food_product as a 
right join
(select category, max(price) as max_price 
from food_product
where category in ("과자", "국", "김치", "식용유")
group by category) as b
on a.category=b.category
and a.price=b.max_price
order by price desc