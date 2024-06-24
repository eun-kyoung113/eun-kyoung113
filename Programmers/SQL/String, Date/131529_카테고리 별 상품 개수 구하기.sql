SELECT substr(product_code,1,2) as product_category
, count(product_id) as count
from product
group by product_category
order by product_category