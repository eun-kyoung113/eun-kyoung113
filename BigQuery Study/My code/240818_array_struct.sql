
----- ARRAY, STRUCT 다루기 ------

-- 1) 대괄호 [] 사용하기
SELECT
  [0,1,2,3,4] as some_numbers
UNION ALL
SELECT
  [2,4,6,8,12]
UNION ALL
SELECT
  [5,10];

-- 2) ARRAY<> 사용하기 : ARRAY<자료형>
SELECT
  ARRAY<int64>[0,1,2] as some_numbers;

-- 3) 배열 생성 함수 사용
SELECT
  GENERATE_DATE_ARRAY('2024-01-01', '2024-02-01', INTERVAL 1 WEEK) as 
  output1,
  GENERATE_ARRAY(1,5,2) as output2;

-- 4) ARRAY_AGG 함수 사용
WITH programming_languages as(
  SELECT "python" as programming_language
  UNION ALL
  SELECT "go"
  UNION ALL
  SELECT "scala"
)

SELECT ARRAY_AGG(programming_language) as output
FROM
  programming_languages;


-- ARRAY 데이터 접근하기
WITH array_samples as(
  SELECT [0,1,1,2,3,5] AS some_numbers,
  UNION ALL
  SELECT [2,4,8,6,12]
  UNION ALL
  SELECT [5,10]
)

SELECT
  some_numbers[safe_offset(5)] as first_value
FROM
  array_samples;


-- STRUCT 생성하기

-- 1) 소괄호() 사용하기
SELECT
  (1,2,3) AS struct_test;


-- 2) STRUCT<자료형>(데이터) 사용하기
SELECT
  STRUCT<hi INT64, hello INT64, awesome STRING>(1,2,'HI') as struct_test;

-- 3) STRUCT 값에 접근하기
SELECT
  struct_test.hi,
  struct_test.hello
FROM(
  SELECT
    STRUCT<hi INT64, hello INT64, awesome STRING>(1,2,'HI') as struct_test
);

-- example) 배열과 함께 존재하는 데이터
WITH example_data as(
  SELECT
    'kyle' as name,
    ['Python', 'SQL', 'R', 'Julia', 'GO'] as preferred_language,
    'incheon' as hometown
  UNION ALL
  SELECT
    'max' as name,
    ['Python','SQL','Scala','Java','Kotlin'] as preferred_language,
    'Seoul' as hometown
  UNION ALL
  SELECT
    'yun' as name,
    ['Python','SQL'] as preferred_language,
    'incheon' as hometown
)

SELECT *
FROM
  example_data;


-- UNNEST 사용해 중첩된 데이터 구조 풀기
WITH example_data as(
  SELECT
    'kyle' as name,
    ['Python', 'SQL', 'R', 'Julia', 'GO'] as preferred_language,
    'incheon' as hometown
  UNION ALL
  SELECT
    'max' as name,
    ['Python','SQL','Scala','Java','Kotlin'] as preferred_language,
    'Seoul' as hometown
  UNION ALL
  SELECT
    'yun' as name,
    ['Python','SQL'] as preferred_language,
    'incheon' as hometown
)

SELECT
  a.name, pref_lang, a.hometown
FROM
  example_data as a, 
  UNNEST(preferred_language) as pref_lang;

