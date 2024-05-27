
-- WITH, Partition 구문 연습
SELECT
  *  
FROM 
  `eklee-project.basic.battle` 
WHERE 
  battle_datetime BETWEEN DATETIME("2022-04-22") 
  AND DATETIME_ADD("2024-04-22", INTERVAL 1 DAY) LIMIT 1000;


WITH base AS(
  SELECT
    player1_id,
    player2_id,
    winner_id
  FROM
    `basic.battle`
)

SELECT
  *
FROM
  base;