create table
  advanced.app_log
partition by event_date
as select *
from advanced.app_log_temp