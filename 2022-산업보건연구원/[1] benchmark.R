# [1] 업종단위 (level 1 - level 3ab) 계층시계열 생성 R code ------------------------------------
## <What to do>
## 1. data pre-processing
##  - SEX 변수값 변환 & 추세가 불안정한 사업장 제외 & CAL2 변수 새롭게 범주화(2000년 이전 / 이후) --> "CAL2"변수

## 2. 계층 시계열 series 생성
##  - "NAME" 변수 새로 추가 : "X" + "UP1" + "UP2" + "SEX" + "CAL2" (SEX와 CAL2는 "_" 기호로 구분)
##  - LEUKEMIA Series와 LUNG_CANCER Series 병합
# -------------------------------------------------------------------------


## 1. import library ----------------------------------------------------------
library(tidyverse)
library(data.table)
setDTthreads(24)
library(stringr)
library(dtplyr)


## 2. data import ----------------------------------------------------------
dat1 = fread("/shared/data/koshri/ref_leukemia_bz_v4.csv")
dat2 = fread("/shared/data/koshri/ref_lung_bz_v4.csv")



## 3. data pre-processing -----------------------------------------------------
### leukemia data -----------------------------------------------------------
dat1$SEX = ifelse(dat1$SEX=="남", "M", "F")
dat1<-dat1 %>% filter(!(UP1 %in% c("","T", "U"))) %>%
  mutate(CAL2=ifelse( (CAL<=1), "01", "234")) %>% as_tibble()


### lung cancer data -----------------------------------------------------------
dat2$SEX = ifelse(dat2$SEX=="남", "M", "F")
dat2<-dat2 %>% filter(!(UP1 %in% c("","T", "U"))) %>%
  mutate(CAL2=ifelse( (CAL<=1), "01", "234")) %>% as_tibble()



## 4. 계층 시계열 생성 ------------------------------------------------------------

### level1 Data : "YEAR" 기준으로 grouping --------------------------------------
full11 <- dat1 %>% 
  group_by(YEAR) %>%
  summarize(FY_LEUKEMIA = sum(FY_BZ), LEUKEMIA = sum(LEUKEMIA_BZ)) %>% as_tibble()

full21 <- dat2 %>% 
  group_by(YEAR) %>%
  summarize(FY_LUNG = sum(FY_BZ), LUNG = sum(LUNG_BZ)) %>% as_tibble()

full1 = data.frame(LEVEL="L1", NAME="X", UP1=NA, UP2=NA, SEX=NA, CAL2=NA, 
                 YEAR=full11$YEAR, 
                 full11[,c("FY_LEUKEMIA", "LEUKEMIA")], full21[,c("FY_LUNG", "LUNG")])


### level1a Data : "SEX", "YEAR" 기준으로 grouping --------------------------------------
full11a <- dat1 %>% 
  group_by(SEX, YEAR) %>%
  summarize(FY_LEUKEMIA = sum(FY_BZ), LEUKEMIA = sum(LEUKEMIA_BZ)) %>% as_tibble()

full21a <- dat2 %>% 
  group_by(SEX, YEAR) %>%
  summarize(FY_LUNG = sum(FY_BZ), LUNG = sum(LUNG_BZ)) %>% as_tibble()

full1a = data.frame(LEVEL="L1a", NAME=paste0("X", "_", full11a$SEX), 
                    UP1=NA, UP2=NA, SEX=full11a$SEX, CAL2=NA, YEAR=full11a$YEAR, 
                   full11a[,c("FY_LEUKEMIA", "LEUKEMIA")], full21a[,c("FY_LUNG", "LUNG")])


### level1b Data : "CAL2", "YEAR" 기준으로 grouping --------------------------------------
full11b <- dat1 %>% 
  group_by(CAL2, YEAR) %>%
  summarize(FY_LEUKEMIA = sum(FY_BZ), LEUKEMIA = sum(LEUKEMIA_BZ)) %>% as_tibble() 

full21b <- dat2 %>% 
  group_by(CAL2, YEAR) %>%
  summarize(FY_LUNG = sum(FY_BZ), LUNG = sum(LUNG_BZ)) %>% as_tibble()

full1b = data.frame(LEVEL="L1b", NAME=paste0("X", "_", full11b$CAL2), 
                    UP1=NA, UP2=NA, SEX=NA, CAL2=full11b$CAL2, YEAR=full11b$YEAR, 
                    full11b[,c("FY_LEUKEMIA", "LEUKEMIA")], full21b[,c("FY_LUNG", "LUNG")])


### level1ab Data : "SEX", "CAL2", "YEAR" 기준으로 grouping --------------------------------------
full11ab <- dat1 %>% 
  group_by(SEX, CAL2, YEAR) %>%
  summarize(FY_LEUKEMIA = sum(FY_BZ), LEUKEMIA = sum(LEUKEMIA_BZ)) %>% as_tibble()

full21ab <- dat2 %>% 
  group_by(SEX, CAL2, YEAR) %>%
  summarize(FY_LUNG = sum(FY_BZ), LUNG = sum(LUNG_BZ)) %>% as_tibble()

full1ab = data.frame(LEVEL="L1ab", NAME=paste0("X", "_", full11ab$SEX, full11ab$CAL2), 
                     UP1=NA, UP2=NA, SEX=full11ab$SEX, CAL2=full11ab$CAL2, 
                    YEAR=full11ab$YEAR, 
                    full11ab[,c("FY_LEUKEMIA", "LEUKEMIA")], full21ab[,c("FY_LUNG", "LUNG")])



#### -------------------------------------------------------------------------
### level2 Data : "UP1", "YEAR" 기준으로 grouping --------------------------------------
full12 <- dat1 %>% 
  group_by(UP1, YEAR) %>%
  summarize(FY_LEUKEMIA = sum(FY_BZ), LEUKEMIA = sum(LEUKEMIA_BZ)) %>% as_tibble()

full22 <- dat2 %>% 
  group_by(UP1, YEAR) %>%
  summarize(FY_LUNG = sum(FY_BZ), LUNG = sum(LUNG_BZ)) %>% as_tibble()

full2 = data.frame(LEVEL="L2", NAME=paste0("X", full12$UP1), UP1=full12$UP1, 
                 UP2=NA, SEX=NA, CAL2=NA, YEAR=full12$YEAR,
                 full12[,c("FY_LEUKEMIA", "LEUKEMIA")], full22[,c("FY_LUNG", "LUNG")])



### level2a Data : "UP1", "SEX", "YEAR" 기준으로 grouping --------------------------------------
full12a <- dat1 %>% 
  group_by(UP1, SEX, YEAR) %>%
  summarize(FY_LEUKEMIA = sum(FY_BZ), LEUKEMIA = sum(LEUKEMIA_BZ)) %>% as_tibble()

full22a <- dat2 %>% 
  group_by(UP1, SEX, YEAR) %>%
  summarize(FY_LUNG = sum(FY_BZ), LUNG = sum(LUNG_BZ)) %>% as_tibble()

full2a = data.frame(LEVEL="L2a", NAME=paste0("X", full12a$UP1, "_", full12a$SEX), 
                    UP1=full12a$UP1, UP2=NA, SEX=full12a$SEX, CAL2=NA, YEAR=full12a$YEAR, 
                    full12a[,c("FY_LEUKEMIA", "LEUKEMIA")], full22a[,c("FY_LUNG", "LUNG")])



### level2b Data : "UP1", "SEX", "YEAR" 기준으로 grouping --------------------------------------
full12b <- dat1 %>% 
  group_by(UP1, CAL2, YEAR) %>%
  summarize(FY_LEUKEMIA = sum(FY_BZ), LEUKEMIA = sum(LEUKEMIA_BZ)) %>% as_tibble()

full22b <- dat2 %>% 
  group_by(UP1, CAL2, YEAR) %>%
  summarize(FY_LUNG = sum(FY_BZ), LUNG = sum(LUNG_BZ)) %>% as_tibble()

full2b = data.frame(LEVEL="L2b", NAME=paste0("X", full12b$UP1, "_", full12b$CAL2), 
                    UP1=full12b$UP1, UP2=NA, SEX=NA, CAL2=full12b$CAL2, YEAR=full12b$YEAR, 
                    full12b[,c("FY_LEUKEMIA", "LEUKEMIA")], full22b[,c("FY_LUNG", "LUNG")])



### level2ab Data : "UP1", "SEX", "CAL2", "YEAR" 기준으로 grouping --------------------------------------
full12ab <- dat1 %>% 
  group_by(UP1, SEX, CAL2, YEAR) %>%
  summarize(FY_LEUKEMIA = sum(FY_BZ), LEUKEMIA = sum(LEUKEMIA_BZ)) %>% as_tibble()

full22ab <- dat2 %>% 
  group_by(UP1, SEX, CAL2, YEAR) %>%
  summarize(FY_LUNG = sum(FY_BZ), LUNG = sum(LUNG_BZ)) %>% as_tibble()

full2ab = data.frame(LEVEL="L2ab", NAME=paste0("X", full12ab$UP1, "_", full12ab$SEX, full12b$CAL2), 
                    UP1=full12ab$UP1, UP2=NA, SEX=full12ab$SEX, CAL2=full12ab$CAL2, YEAR=full12ab$YEAR, 
                    full12ab[,c("FY_LEUKEMIA", "LEUKEMIA")], full22ab[,c("FY_LUNG", "LUNG")])



#### -------------------------------------------------------------------------
### level3 Data : "UP1","UP2", "YEAR" 기준으로 grouping --------------------------------------
full13 <- dat1 %>% 
  group_by(UP1, UP2, YEAR) %>%
  summarize(FY_LEUKEMIA = sum(FY_BZ), LEUKEMIA = sum(LEUKEMIA_BZ)) %>% as_tibble()

full23 <- dat2 %>% 
  group_by(UP1, UP2, YEAR) %>%
  summarize(FY_LUNG = sum(FY_BZ), LUNG = sum(LUNG_BZ)) %>% as_tibble()

full3 = data.frame(LEVEL="L3", NAME=paste0("X", full13$UP1, full13$UP2), UP1=full13$UP1, 
                   UP2=full13$UP2, SEX=NA, CAL2=NA, YEAR=full13$YEAR,
                   full13[,c("FY_LEUKEMIA", "LEUKEMIA")], full23[,c("FY_LUNG", "LUNG")])



### level3a Data : "UP1", "UP2", "SEX", "YEAR" 기준으로 grouping --------------------------------------
full13a <- dat1 %>% 
  group_by(UP1, UP2, SEX, YEAR) %>%
  summarize(FY_LEUKEMIA = sum(FY_BZ), LEUKEMIA = sum(LEUKEMIA_BZ)) %>% as_tibble()

full23a <- dat2 %>% 
  group_by(UP1, UP2, SEX, YEAR) %>%
  summarize(FY_LUNG = sum(FY_BZ), LUNG = sum(LUNG_BZ)) %>% as_tibble()

full3a = data.frame(LEVEL="L3a", NAME=paste0("X", full13a$UP1, full13a$UP2, "_", full13a$SEX), 
                    UP1=full13a$UP1, UP2=full13a$UP2, SEX=full13a$SEX, CAL2=NA, YEAR=full13a$YEAR, 
                    full13a[,c("FY_LEUKEMIA", "LEUKEMIA")], full23a[,c("FY_LUNG", "LUNG")])



### level3b Data : "UP1", "UP2", "CAL2", "YEAR" 기준으로 grouping --------------------------------------
full13b <- dat1 %>% 
  group_by(UP1, UP2, CAL2, YEAR) %>%
  summarize(FY_LEUKEMIA = sum(FY_BZ), LEUKEMIA = sum(LEUKEMIA_BZ)) %>% as_tibble()

full23b <- dat2 %>% 
  group_by(UP1, UP2, CAL2, YEAR) %>%
  summarize(FY_LUNG = sum(FY_BZ), LUNG = sum(LUNG_BZ)) %>% as_tibble()

full3b = data.frame(LEVEL="L3b", NAME=paste0("X", full13b$UP1, full13b$UP2, "_", full13b$CAL2), 
                    UP1=full13b$UP1, UP2=full13b$UP2, SEX=NA, CAL2=full13b$CAL2, YEAR=full13b$YEAR, 
                    full13b[,c("FY_LEUKEMIA", "LEUKEMIA")], full23b[,c("FY_LUNG", "LUNG")])



### level3ab Data : "UP1", "UP2", "SEX", "CAL2", "YEAR" 기준으로 grouping --------------------------------------
full13ab <- dat1 %>% 
  group_by(UP1, UP2, SEX, CAL2, YEAR) %>%
  summarize(FY_LEUKEMIA = sum(FY_BZ), LEUKEMIA = sum(LEUKEMIA_BZ))

full23ab <- dat2 %>% 
  group_by(UP1, UP2, SEX, CAL2, YEAR) %>%
  summarize(FY_LUNG = sum(FY_BZ), LUNG = sum(LUNG_BZ)) 

full3ab = data.frame(LEVEL="L3ab", NAME=paste0("X", full13ab$UP1, full13ab$UP2, "_", full13ab$SEX, full13ab$CAL2), 
                    UP1=full13ab$UP1, UP2=full13ab$UP2, SEX=full13ab$SEX, CAL2=full13ab$CAL2, YEAR=full13ab$YEAR, 
                    full13ab[,c("FY_LEUKEMIA", "LEUKEMIA")], full23ab[,c("FY_LUNG", "LUNG")])



# -------------------------------------------------------------------------
## 5. Data binding ------------------------------------------------------------
fulls = rbind(full1, full1a, full1b, full1ab, 
              full2, full2a, full2b, full2ab,
              full3, full3a, full3b, full3ab)

fulls$ID = rep(1:length(unique(fulls$NAME)), table(factor(fulls$NAME, levels=unique(fulls$NAME))))
fulls = fulls[ ,c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2",
                  "YEAR", "FY_LEUKEMIA", "LEUKEMIA", "FY_LUNG", "LUNG")]



# -------------------------------------------------------------------------
## 6. 계층 시계열 data store ----------------------------------------------------
write.csv(fulls, "full_v2.csv", row.names=F, col.names=T)




# -------------------------------------------------------------------------
## 7. validation set split - 2015년 이후 --------------------------------------
vals = fulls %>% filter(YEAR>=2015)
write.csv(vals, "validation_v2.csv", row.names=F, col.names=T)



