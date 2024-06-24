-- 코드를 입력하세요
with max_review as
    (SELECT member_id, count(review_id) as review_cnt
    from rest_review
    group by member_id),

    max_cnt as
    (select max(review_cnt) as max_cnt
    from max_review)

select a.member_name, b.review_text, substr(b.review_date,1,10) as review_date
from member_profile as a join rest_review as b
on a.member_id=b.member_id
where b.member_id in 
    (select distinct member_id 
    from max_review 
    where review_cnt in (select max_cnt from max_cnt))
order by review_date, review_text