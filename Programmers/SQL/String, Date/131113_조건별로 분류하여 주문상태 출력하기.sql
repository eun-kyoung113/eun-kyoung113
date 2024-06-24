SELECT order_id, product_id, substr(out_date,1,10) as out_date
, case when (date(out_date) < date('2022-05-01')) or (date(out_date) = date('2022-05-01')) then '출고완료'
        when date(out_date) > date('2022-05-01') then '출고대기'
        else '출고미정' end as 출고여부
from food_order
order by order_id