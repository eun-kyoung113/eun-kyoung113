select flavor
from
    (select flavor, sum(total_sales) as total_order
    from
        (SELECT a.flavor, (a.total_order+b.total_order) as total_sales
        from first_half as a join july as b
        on a.flavor=b.flavor) as a
    group by flavor
    order by total_order desc) as b
limit 3