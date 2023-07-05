# [0] Data import R code --------------------------------------------------
## <What to do>
## 1. Raw data import
## 2. Raw data에서 필요한 부분만 가져오기
## 3. csv file로 재 저장 & re-import
## 4. Data quality check
# -------------------------------------------------------------------------

# install.packages("haven")
# install.packages("tidyverse")


# library import ----------------------------------------------------------
library(data.table)
setDTthreads(24)
library(tidyverse)
library(stringr)
library(bit64)
library(dtplyr) 


# Raw Data import ---------------------------------------------------------
## 1.Leukemia  Raw data import ---------------------------------------------
raw = haven::read_sas("/shared/data/koshri/ref_leukemia_bz_v4.sas7bdat")
dat = as.data.table(raw)

# 사업장 구분코드가 결측이고, 누적질병발생건수와 추적인년합계가 0미만인 관측치 제거 & 변수 순서 지정
dat1 = dat[ (INDDIS_NO != "") & (ENR_BZ > 0) & (FY_BZ > 0) ][ ,
              .(INDDIS_NO, BIZ_ZIP, UP1, UP2, UP3, SEX, CAL, ECNY_AGE, YEAR,
               ENR_BZ, FY_BZ, LEUKEMIA_BZ, INC_BZ)]

# Data 정렬
setorderv(dat1, c("UP1", "UP2", "UP3", "INDDIS_NO", "SEX", "CAL", "ECNY_AGE", "YEAR"))

# ENCY_AGE 변수에서 numeric term만 가져오기
dat1$ECNY_AGE = substr(dat1$ECNY_AGE, 2, 3) 

# csv file로 saving
fwrite(dat1, "/shared/data/koshri/ref_leukemia_bz_v4.csv")

# memory 정리
rm(dat)
rm(dat1)
rm(raw)
gc()


## 2.Lung cancer Raw data import ---------------------------------------------
raw = haven::read_sas("/shared/data/koshri/ref_lung_bz_v4.sas7bdat")
dat = as.data.table(raw)

# 사업장 구분코드가 결측이고, 누적질병발생건수와 추적인년합계가 0미만인 관측치 제거 & 변수 순서 지정
dat2 = dat[ (INDDIS_NO != "") & (ENR_BZ > 0) & (FY_BZ > 0) ][ ,
          .(INDDIS_NO, BIZ_ZIP, UP1, UP2, UP3, SEX, CAL, ECNY_AGE, YEAR,
            ENR_BZ, FY_BZ, LUNG_BZ, INC_BZ)]

# Data 정렬
setorderv(dat2, c("UP1", "UP2", "UP3", "INDDIS_NO", "SEX", "CAL", "ECNY_AGE", "YEAR"))

# ENCY_AGE 변수에서 numeric term만 가져오기
dat2$ECNY_AGE = substr(dat2$ECNY_AGE, 2, 3) 

# csv file로 saving
fwrite(dat2, "/shared/data/koshri/ref_lung_bz_v4.csv")

# memory 정리
rm(dat)
rm(dat2)
rm(raw)
gc()


## 3. csv file import ---------------------------------------------------------
dat1 = fread("/shared/data/koshri/ref_leukemia_bz_v4.csv")
dat1<-lazy_dt(dat1)

dat2 = fread("/shared/data/koshri/ref_lung_bz_v4.csv")
dat2<-lazy_dt(dat2)


## 4. Data check --------------------------------------------------------------
## "LEUKEMIA_BZ", "LUNG_BZ", "FY_BZ" 변수에 대한 monotonicity test
is.monotone = function(x) { 
    x = x[!is.na(x)]
    if (length(x) > 1) return( sum (abs(x[2:length(x)] - x[1:(length(x)-1)])) >= 0 )
    if (length(x) <= 1) return(TRUE) 
  }


res1_2 = dat1 %>% group_by(INDDIS_NO, SEX, CAL, ECNY_AGE) %>% summarize(mon_LEUKEMIA = is.monotone(LEUKEMIA_BZ),
                                                                      mon_FY_BZ = is.monotone(FY_BZ))%>%as_tibble()
sum(res1_2$mon_LEUKEMIA, na.rm=TRUE)/nrow(res1) # "1"이 나오는지 확인
sum(res1_2$mon_FY_BZ, na.rm=TRUE)/nrow(res1_2) # "1"이 나오는지 확인


res2 = dat2 %>% group_by(INDDIS_NO, SEX, CAL, ECNY_AGE) %>% summarize(mon_LUNG = is.monotone(LUNG_BZ),
                                                                      mon_FY_BZ = is.monotone(FY_BZ))%>%as_tibble()

sum(res2$mon_LUNG, na.rm=TRUE)/nrow(res2) # "1"이 나오는지 확인
sum(res2$mon_FY_BZ, na.rm=TRUE)/nrow(res2) # "1"이 나오는지 확인


