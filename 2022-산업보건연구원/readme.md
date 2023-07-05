## 📂 프로젝트 명
#### **근로자 집단 암 발병 예측 모형 시범 개발** 

### 1. 수행기간
2022.04 ~ 2022.11

### 2. 프로젝트 내용 & 역할
해당 프로젝트는 산업안전보건연구원에서 위탁한 용역 과제로, 과제의 목표는 2000년부터 2018년까지 구축되어 있는 대규모의 고용 보험 자료를 이용해 
백혈병과 폐암 발병률을 예측하는 모형 개발이었다. 본인은 연구 보조원으로서 자료 분석 실무를 담당하였다.
* **고용 보험 빅데이터 전처리(관측치 수 = 92,714,004)**
  - R program 이용(“dtplyr”/ “ggplot2” package) <br> 
  - Data quality check(누적 근로자 수 변수에 대해 주로 진행) <br>
  - EDA 과정 진행(데이터 시각화) <br> 
  - 직종 분류 기준(대분류, 중분류, 소분류), 성별로 Data grouping <br>

* **Prediction model 선정**
  - simple linear regression, Poisson regression, Quadratic regression, 본인이 제안한 linear spline model,
  cubic spline model 이용해 rMSE, MAPE 성능 지표 측정 <br>
  ↳ 본인이 제안한 linear spline model이 최종 model로 선정됨.<br>

  - 2019년 ~ 2022년에 대해 백혈병 / 폐암 누적 발병 건수 예측 과정 진행 <br>

* **R code 정리**
  - code pipeline 생성<br>

### 3. 획득 역량
* R programming 능력(빅데이터 전처리 / 데이터 시각화 / Prediction) <br>

### 4. 주요 성과
* 산업안전보건연구원에 최종 보고서 발간
