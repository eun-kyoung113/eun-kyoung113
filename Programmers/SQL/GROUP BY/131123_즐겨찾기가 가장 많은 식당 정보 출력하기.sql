select a.food_type, a.rest_id, a.rest_name, a.favorites
from rest_info as a join 
(SELECT food_type, max(favorites) as max_favorite
from rest_info
group by food_type) as b
on a.food_type=b.food_type
and a.favorites=b.max_favorite
order by food_type desc