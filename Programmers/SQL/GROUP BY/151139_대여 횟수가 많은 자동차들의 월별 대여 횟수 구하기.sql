with car_rental as(
    select car_id, count(*) as cnt 
    from car_rental_company_rental_history
    where substr(start_date, 1, 7) between "2022-08" and "2022-10"
    group by car_id 
    having cnt>=5)

select extract(month from start_date) as month, car_id
, count(*) as records
from car_rental_company_rental_history
where car_id in (select distinct car_id from car_rental)
and substr(start_date, 1, 7) between "2022-08" and "2022-10"
group by car_id, month
having records > 0
order by month, car_id desc