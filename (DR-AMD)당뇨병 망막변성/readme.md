## 📂 프로젝트 명
#### Progression of Diabetic Retinopathy in Presence of Age-related Macular Degeneration <br>
(연령 관련 황반 변성 동반 시 당뇨 망막병증의 진행)

### 1. 수행기간
2022.10 ~ 2023.02

### 2. 프로젝트 내용 & 역할
이전에 보고된 "황반 변성이 있는 당뇨병 환자들은 당뇨병 망막 변성이 천천히 진행된다.” 가설 검정을 목적으로 이루어진 연구이며,
본인은 연구 보조원으로서 Joslin Diabetes Center에서 chart review(EMR Record) 통해 얻은 의료 데이터 분석 실무를 담당하였다. <br>
* **Counting-process 형태로 데이터 변환**
  - SAS program & SQL procedure 이용<br>
  - “HbA1c”반복 측정 자료와 EMR record를 연결<br>
	- 각 반복 측정치에 대해 "관측 시작 시점", "관측 종료 시점", "관측 지속 기간" 변수 추가 정의<br>

* **Survival Analysis 진행**
  - SAS "PHREG" Procedure를 이용해 time-varying covariate cox-PH model을 적합 / 결과 해석<br>
  - 민감도 분석 추가 진행<br>

### 3. 획득 역량 / 능력
- SAS / SQL 프로그래밍 능력 (Counting-process data format 생성 + time-varying covariate Cox-ph model fitting)
