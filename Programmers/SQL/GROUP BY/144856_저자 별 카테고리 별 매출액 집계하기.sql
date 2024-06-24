SELECT c.author_id, c.author_name, b.category, sum((a.sales*b.price)) as total_sales
from book_sales as a left join
book as b
on a.book_id=b.book_id
left join author as c
on b.author_id=c.author_id
where substr(sales_date,1,7)='2022-01'
group by c.author_id, b.category
order by c.author_id, b.category desc