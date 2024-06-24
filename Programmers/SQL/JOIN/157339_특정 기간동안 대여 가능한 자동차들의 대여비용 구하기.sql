with car_info as(
    SELECT distinct a.car_id, a.car_type, a.daily_fee, max(b.end_date) as end_date_end
    from car_rental_company_car as a left join 
    car_rental_company_rental_history as b
    on a.car_id=b.car_id
    where a.car_type in ("세단", "SUV")
    group by car_id, car_type, daily_fee
    ),
    
    car_fee as(
    select car_type, round(discount_rate/100,2) as discount
    from car_rental_company_discount_plan
    where car_type in ("세단", "SUV")
    and duration_type="30일 이상")

select a.car_id, a.car_type, round(a.daily_fee*(1-b.discount)*30,0) as fee
from car_info as a left join car_fee as b
on a.car_type=b.car_type
where substr(a.end_date_end,1,10)<'2022-11-01'
having fee>=500000 and fee<2000000
order by fee desc, car_type, car_id desc