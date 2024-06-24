with all_sale as (
        SELECT sales_date, product_id, user_id, sales_amount
        from online_sale
        union all
        select sales_date, product_id, NULL as user_id, sales_amount
        from
        offline_sale)

select substr(sales_date,1,10) as sales_date, product_id, user_id, sales_amount 
from all_sale
where substr(sales_date, 1, 7)='2022-03'
order by sales_date, product_id, user_id