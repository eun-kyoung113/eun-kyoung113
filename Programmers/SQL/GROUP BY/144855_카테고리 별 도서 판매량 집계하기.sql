select category, sum(sales) as total_sales
from
    (SELECT *,substr(sales_date,1,7) as sales_month
    from book_sales
    having sales_month='2022-01') as a join
    book as b
    on a.book_id=b.book_id
group by category
order by category