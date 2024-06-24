SELECT distinct a.car_id
from car_rental_company_car as a right join car_rental_company_rental_history as b
on a.car_id=b.car_id
where substr(b.start_date,6,2)='10'
and a.car_type='세단'
order by a.car_id desc