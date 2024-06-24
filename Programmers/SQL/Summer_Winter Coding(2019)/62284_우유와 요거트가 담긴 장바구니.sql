select cart_id
from
(select cart_id, sum(cnt+cnt2) as milk_yogurt
 from
    (SELECT id, cart_id, name
        , case when name='Milk' then 1 else 0 end as cnt
        , case when name='Yogurt' then 10 else 0 end as cnt2
    from cart_products
    where name in ("Milk", "Yogurt")) as a
group by cart_id
having (milk_yogurt>=11) and (milk_yogurt%10!=0)) as b
order by cart_id