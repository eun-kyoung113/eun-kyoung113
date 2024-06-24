SELECT a.rest_id, a.rest_name, a.food_type, a.favorites, a.address,
round(avg(b.review_score),2) as score
from rest_info as a inner join rest_review as b
on a.rest_id=b.rest_id
where substr(a.address,1,2)="서울"
group by rest_id
order by score desc, favorites desc