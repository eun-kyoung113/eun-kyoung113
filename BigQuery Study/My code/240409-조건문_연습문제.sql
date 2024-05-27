-- [조건문 연습 문제]

-- 1. 포켓몬의 speed가 70 이상이면 '빠름', 그렇지 않으면 '느림'으로 표시하는 
--    새로운 컬럼 'Speed_category'를 만들어주세요.
select
  *,
  case when speed>=70 then '빠름'
  else '느림'
  end as Speed_categoty
from
  `basic.pokemon`
;

-- 2. 포켓몬의 'type1'에 따라 'Water', 'Fire', 'Electric' 타입은 각각 '물', '불', '전기'로, 
--    그 외 타입은 '기타'로 분류하는 새로운 컬럼 'type_korean'을 만들어 주세요.
select
  *,
  case when type1='Water' then '물'
       when type1='Fire' then '불'
       when type1='Electric' then '전기'
  else '기타'
  end as type_korean
from
  `basic.pokemon`;


-- 3. 각 포켓몬의 총점(total)을 기준으로, 300 이하면 'Low', 
--    301에서 500 사이면 'Medium', 501 이상이면 'High'로 분류해주세요.
select
  *,
  case when total<=300 then 'Low'
       when total between 301 and 500 then 'Medium'
       when total>=501 then 'High' -- else 구문 이용해도 ok
       end as total_category
from
  `basic.pokemon`;


-- 4. 각 트레이너의 배지 개수(badge_count)를 기준으로, 5개 이하면 'Beginner', 6개에서 8개 사이면 'Intermediate',
--    그 이상이면 'Advacned'로 분류해주세요.
select
  *,
  case when badge_count<=5 then 'Begineer'
       when badge_count between 6 and 8 then 'Intermediate'
       when badge_count>8 then 'Advanced'
  end as trainer_category
from
  `basic.trainer`;


-- 5. 트레이너가 포켓몬을 포획한 날짜(catch_date)가 '2023-01-01' 이후이면, 'Recent', 그렇지 않으면 'Old'로 분류해주세요.
select
  *,
  case when catch_date2>date('2023-01-01','Asia/Seoul') then 'Recenct'
  else 'Old' end as period_category -- if(date(catch_datetime,"Asia/Seoul")>'2023-01-01','Recent','Old')도 가능
from
  (select
    *,
    date(datetime(catch_datetime,'Asia/Seoul')) as catch_date2
  from
    `basic.trainer_pokemon`);


-- 6. 배틀에서 승자(winner_id)가 player_id1과 같으면 'Player 1 wins', player_id2와 같으면 'Player 2 wins', 
--    그렇지 않으면 'Draw'로 결과가 나오게 해주세요.
select
  *,
  case when winner_id=player1_id then 'Player 1 wins'
       when winner_id=player2_id then 'Player 2 wins'
  else 'Draw'
  end as winner_message
from
  `basic.battle`;

