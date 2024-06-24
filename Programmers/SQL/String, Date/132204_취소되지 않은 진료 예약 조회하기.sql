select a.apnt_no, b.pt_name, b.pt_no, a.mcdp_cd, c.dr_name, a.apnt_ymd
from
(SELECT apnt_ymd, apnt_no, pt_no, mcdp_cd, mddr_id
from appointment
where (APNT_CNCL_YN = "N") and (substr(apnt_ymd,1,10)='2022-04-13')
and (mcdp_cd="CS")) as a left join patient as b
on a.pt_no=b.pt_no
left join doctor as c
on a.mddr_id=c.dr_id
order by apnt_ymd