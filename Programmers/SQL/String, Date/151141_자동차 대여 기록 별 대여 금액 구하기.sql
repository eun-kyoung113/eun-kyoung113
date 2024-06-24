-- 코드를 입력하세요
with car_info as(
    select *
    from car_rental_company_car
    where car_type='트럭'),
    
    car_rental as(
    select a.history_id, b.car_id, b.car_type, b.daily_fee,
           datediff(a.end_date,a.start_date)+1 as diff_date
    from car_rental_company_rental_history as a join car_info as b
    on a.car_id=b.car_id),
    
    car_fee as(
    select *, 
    case when diff_date>=90 then "90일 이상"
         when diff_date>=30 then "30일 이상"
         when diff_date>=7 then "7일 이상"
         else NULL end as duration
    from car_rental),
    
    car_discount as(
    select a.*, b.duration_type
        , case when duration is null then 0
            else round(b.discount_rate/100,2) end as discount
    from car_fee as a left join car_rental_company_discount_plan as b
    on a.car_type=b.car_type
    and a.duration=b.duration_type)

select history_id, round(daily_fee*diff_date*(1-discount),0) as fee
from car_discount
order by fee desc, history_id desc

