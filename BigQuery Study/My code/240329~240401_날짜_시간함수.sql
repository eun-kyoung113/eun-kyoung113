-- 날짜 및 시간 데이터 함수 이해하기
select
  timestamp_millis(1704176819711) as milli_to_timestamp,
  timestamp_micros(1704176819711000) as micro_to_timestamp,
  datetime(timestamp_micros(1704176819711000)) as micro_to_datetime,
  datetime(timestamp_micros(1704176819711000),"Asia/Seoul") as micro_to_timestamp2;

-- timestampe와 datetime 함수 비교하기
select
  current_timestamp() as current_time,
  datetime(current_timestamp(),"Asia/Seoul") as current_datetime;

-- current_datetime()
select
  current_datetime() as current_datetime,
  current_datetime("Asia/Seoul") as current_datetime2,
  current_date() as current_date,
  current_date("Asia/Seoul") as seoul_date;

-- extract() 함수
select
  extract(date from datetime"2024-01-02 14:00:00") as date,
  extract(year from datetime "2024-01-02 14:00:00") as year,
  extract(month from datetime "2024-01-02 14:00:00") as month,
  extract(day from datetime "2024-01-02 14:00:00") as day,
  extract(hour from datetime "2024-01-02 14:00:00") as hour,
  extract(minute from datetime "2024-01-02 14:00:00") as minute;

-- dayofweek()
select
  extract(dayofweek from datetime "2024-01-02 14:00:00") as day1,
  extract(dayofweek from datetime "2024-01-09 14:00:00") as day2,
  extract(dayofweek from datetime "2024-01-22 14:00:00") as day3,
  extract(dayofweek from datetime "2024-01-30 14:00:00") as day4;

-- datetime_trunc()
select
  datetime "2024-01-02 14:42:23" as original_date,
  datetime_trunc(datetime "2024-01-02 14:42:34", year) as year_trunc,
  datetime_trunc(datetime "2024-01-02 14:42:23", month) as month_trunc,
  datetime_trunc(datetime "2024-01-02 14:42:23", day) as day_trunc,
  datetime_trunc(datetime "2024-01-02 14:42:23", hour) as hour_trunc;

-- parse_datetime() / format_datetime()
select
  parse_datetime("%Y-%M-%d %H:%M:%S", '2024-01-02 12:35:35') as parse_datetime,
  format_datetime("%c",datetime '2024-01-02 12:35:35') as format_datetime;

-- last_day()
select
  last_day(datetime "2024-01-02 13:34:56") as last_day,
  last_day(datetime "2024-01-02 13:34:56",month) as last_month,
  last_day(datetime "2024-01-02 13:34:56", week) as last_week,
  last_day(datetime "2024-01-02 13:34:56", week(sunday)) as last_week_sunday;

-- datetime_diff()
select
  datetime_diff(first_date,second_date, day) as day_diff,
  datetime_diff(first_date, second_date, week) as week_diff,
  datetime_diff(first_date, second_date, month) as month_diff
from
  (select
    datetime "2024-04-02 10:32:23" as first_date,
    datetime "2024-01-03 12:00:02" as second_date);

