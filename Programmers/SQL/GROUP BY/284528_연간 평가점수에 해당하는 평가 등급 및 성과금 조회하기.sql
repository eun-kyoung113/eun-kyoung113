-- 코드를 작성해주세요
with score as(
    select emp_no, year, avg(score) as avg_score
    from hr_grade
    group by emp_no, year),
    
    grade as(
    select a.emp_no, b.emp_name, b.sal
    ,case when a.avg_score<80 then "C"
     when a.avg_score<90 then "B"
     when a.avg_score<96 then "A"
     else "S" end as grade
    from score as a join hr_employees as b
    on a.emp_no=b.emp_no)

select emp_no, emp_name, grade,
case when grade="C" then (sal*0)
     when grade="B" then (sal*0.1)
     when grade="A" then (sal*0.15)
     else (sal*0.2) end as bonus
from grade
