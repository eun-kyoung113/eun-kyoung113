# [2] 업종단위 (level 1 - level 3ab) 계층시계열 data 이용한 모형적합 R code ----------------------------
## <What to do>
## 1. model fitting
##    - simple linear regression
##    - poisson regression
##    - linear spline function
##    - Quadratic regression

## 2. 적합한 model 기반으로 train set의 적합값 계산 & validation set에 대해 예측값 도출
##  : 고려하는 반응변수
##    - 누적발병건수
##    - 추적인년합계

## 참고사항) model 적합 때 Leukemia, Lung Cancer data 구분해 따로 fitting 진행
# -------------------------------------------------------------------------------------------------------




## 1. library import -------------------------------------------------------
library(data.table)
setDTthreads(24)
library(tidyverse)
library(stringr)
library(bit64)
library(dtplyr)
library(splines)

## 2. csv file import ------------------------------------------------------
data <- read.csv('full_v2.csv')



## 3. model fitting --------------------------------------------------------

### 3-1. leukemia data ------------------------------------------------------
### 3-1-1) LEVEL L1 data ------------------------------------------------------
lv1_leukemia_total<-data[data$LEVEL=="L1",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LEUKEMIA", "LEUKEMIA")]

lv1_leukemia_total$simple.reg<-0
lv1_leukemia_total$poisson.reg<-0
lv1_leukemia_total$linear.spline<-0
lv1_leukemia_total$quadratic.reg<-0

lv1_leukemia_total$simple.reg.FY<-0
lv1_leukemia_total$poisson.reg.FY<-0
lv1_leukemia_total$linear.spline.FY<-0
lv1_leukemia_total$quadratic.reg.FY<-0

### train set, validation set split -----
ind.tr<-which(lv1_leukemia_total$YEAR<=2014)
ind.val<-which(lv1_leukemia_total$YEAR>2014)


### 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
### 각 model 별 train set에 대한 적합값, validation set에 대한 예측값 도출 --> 저장
model.simple.reg<-lm(LEUKEMIA~YEAR, data=lv1_leukemia_total[ind.tr,])
lv1_leukemia_total$simple.reg[ind.tr]<-model.simple.reg$fitted.values
lv1_leukemia_total$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv1_leukemia_total[ind.val,])

model.simple.reg.FY<-lm(FY_LEUKEMIA~YEAR, data=lv1_leukemia_total[ind.tr,])
lv1_leukemia_total$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
lv1_leukemia_total$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv1_leukemia_total[ind.val,])

model.poisson.reg<-glm(LEUKEMIA~YEAR, data=lv1_leukemia_total[ind.tr,],family='poisson')
lv1_leukemia_total$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
lv1_leukemia_total$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv1_leukemia_total[ind.val,]))

model.poisson.reg.FY<-glm(FY_LEUKEMIA~YEAR, data=lv1_leukemia_total[ind.tr,],family='poisson')
lv1_leukemia_total$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
lv1_leukemia_total$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv1_leukemia_total[ind.val,]))


model.quadratic.reg<-lm(LEUKEMIA~poly(YEAR,2), data=lv1_leukemia_total[ind.tr,])
lv1_leukemia_total$quadratic.reg[ind.tr]<-ifelse(model.quadratic.reg$fitted.values<0, 0, model.quadratic.reg$fitted.values)
lv1_leukemia_total$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv1_leukemia_total[ind.val,])

model.quadratic.reg.FY<-lm(FY_LEUKEMIA~poly(YEAR,2), data=lv1_leukemia_total[ind.tr,])
lv1_leukemia_total$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
lv1_leukemia_total$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv1_leukemia_total[ind.val,])


model.linear.spline<-lm(LEUKEMIA~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_leukemia_total[ind.tr,])
lv1_leukemia_total$linear.spline[ind.tr]<-model.linear.spline$fitted.values
lv1_leukemia_total$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv1_leukemia_total[ind.val,])

model.linear.spline.FY<-lm(FY_LEUKEMIA~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_leukemia_total[ind.tr,])
lv1_leukemia_total$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
lv1_leukemia_total$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv1_leukemia_total[ind.val,])



### 3-1-2) LEVEL L1a data --------------------------------------------------
lv1_leukemia_SEX<-data[data$LEVEL=="L1a",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LEUKEMIA", "LEUKEMIA")]

lv1_leukemia_SEX$simple.reg<-0
lv1_leukemia_SEX$poisson.reg<-0
lv1_leukemia_SEX$linear.spline<-0
lv1_leukemia_SEX$quadratic.reg<-0

lv1_leukemia_SEX$simple.reg.FY<-0
lv1_leukemia_SEX$poisson.reg.FY<-0
lv1_leukemia_SEX$linear.spline.FY<-0
lv1_leukemia_SEX$quadratic.reg.FY<-0
SEX_level<-unique(lv1_leukemia_SEX$SEX)

### SEX 마다 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
### 각 model 별 train set에 대한 적합값, validation set에 대한 예측값 도출 --> 저장
for(i in 1:length(SEX_level)){
  ind.tr<-which(lv1_leukemia_SEX$YEAR<=2014 & lv1_leukemia_SEX$SEX==SEX_level[i])
  ind.val<-which(lv1_leukemia_SEX$YEAR>2014 & lv1_leukemia_SEX$SEX==SEX_level[i])
  
  model.simple.reg<-lm(LEUKEMIA~YEAR, data=lv1_leukemia_SEX[ind.tr,])
  lv1_leukemia_SEX$simple.reg[ind.tr]<-ifelse(model.simple.reg$fitted.values<0, 0, model.simple.reg$fitted.values)
  lv1_leukemia_SEX$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv1_leukemia_SEX[ind.val,])
  
  model.simple.reg.FY<-lm(FY_LEUKEMIA~YEAR, data=lv1_leukemia_SEX[ind.tr,])
  lv1_leukemia_SEX$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv1_leukemia_SEX$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv1_leukemia_SEX[ind.val,])
  
  model.poisson.reg<-glm(LEUKEMIA~YEAR, data=lv1_leukemia_SEX[ind.tr,],family='poisson')
  lv1_leukemia_SEX$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv1_leukemia_SEX$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv1_leukemia_SEX[ind.val,]))
  
  model.poisson.reg.FY<-glm(FY_LEUKEMIA~YEAR, data=lv1_leukemia_SEX[ind.tr,],family='poisson')
  lv1_leukemia_SEX$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv1_leukemia_SEX$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv1_leukemia_SEX[ind.val,]))
  
  
  model.quadratic.reg<-lm(LEUKEMIA~poly(YEAR,2), data=lv1_leukemia_SEX[ind.tr,])
  lv1_leukemia_SEX$quadratic.reg[ind.tr]<-ifelse(model.quadratic.reg$fitted.values<0, 0, model.quadratic.reg$fitted.values)
  lv1_leukemia_SEX$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv1_leukemia_SEX[ind.val,])
  
  model.quadratic.reg.FY<-lm(FY_LEUKEMIA~poly(YEAR,2), data=lv1_leukemia_SEX[ind.tr,])
  lv1_leukemia_SEX$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv1_leukemia_SEX$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv1_leukemia_SEX[ind.val,])
  
  model.linear.spline<-lm(LEUKEMIA~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_leukemia_SEX[ind.tr,])
  lv1_leukemia_SEX$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv1_leukemia_SEX$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv1_leukemia_SEX[ind.val,])
  
  model.linear.spline.FY<-lm(FY_LEUKEMIA~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_leukemia_SEX[ind.tr,])
  lv1_leukemia_SEX$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv1_leukemia_SEX$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv1_leukemia_SEX[ind.val,])
  
}


### 3-1-3) LEVEL L1b data --------------------------------------------------
lv1_leukemia_CAL2<-data[data$LEVEL=="L1b",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LEUKEMIA", "LEUKEMIA")]

lv1_leukemia_CAL2$simple.reg<-0
lv1_leukemia_CAL2$poisson.reg<-0
lv1_leukemia_CAL2$linear.spline<-0
lv1_leukemia_CAL2$quadratic.reg<-0

lv1_leukemia_CAL2$simple.reg.FY<-0
lv1_leukemia_CAL2$poisson.reg.FY<-0
lv1_leukemia_CAL2$linear.spline.FY<-0
lv1_leukemia_CAL2$quadratic.reg.FY<-0

CAL2_level<-unique(lv1_leukemia_CAL2$CAL2)

### CAL2(입사시기) 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
### 각 model 별 train set에 대한 적합값, validation set에 대한 예측값 도출 --> 저장
for(i in 1:length(CAL2_level)){
  ind.tr<-which(lv1_leukemia_CAL2$YEAR<=2014 & lv1_leukemia_CAL2$CAL2==CAL2_level[i])
  ind.val<-which(lv1_leukemia_CAL2$YEAR>2014 & lv1_leukemia_CAL2$CAL2==CAL2_level[i])
  
  model.poisson.reg<-glm(LEUKEMIA~YEAR, data=lv1_leukemia_CAL2[ind.tr,],family='poisson')
  lv1_leukemia_CAL2$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv1_leukemia_CAL2$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv1_leukemia_CAL2[ind.val,]))
  
  model.poisson.reg.FY<-glm(FY_LEUKEMIA~YEAR, data=lv1_leukemia_CAL2[ind.tr,],family='poisson')
  lv1_leukemia_CAL2$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv1_leukemia_CAL2$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv1_leukemia_CAL2[ind.val,]))
  
  model.simple.reg<-lm(LEUKEMIA~YEAR, data=lv1_leukemia_CAL2[ind.tr,])
  lv1_leukemia_CAL2$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv1_leukemia_CAL2$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv1_leukemia_CAL2[ind.val,])
  
  model.simple.reg.FY<-lm(FY_LEUKEMIA~YEAR, data=lv1_leukemia_CAL2[ind.tr,])
  lv1_leukemia_CAL2$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv1_leukemia_CAL2$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv1_leukemia_CAL2[ind.val,])
  
  model.quadratic.reg<-lm(LEUKEMIA~poly(YEAR,2), data=lv1_leukemia_CAL2[ind.tr,])
  lv1_leukemia_CAL2$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv1_leukemia_CAL2$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv1_leukemia_CAL2[ind.val,])
  
  model.quadratic.reg.FY<-lm(FY_LEUKEMIA~poly(YEAR,2), data=lv1_leukemia_CAL2[ind.tr,])
  lv1_leukemia_CAL2$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv1_leukemia_CAL2$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv1_leukemia_CAL2[ind.val,])
  
  model.linear.spline<-lm(LEUKEMIA~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_leukemia_CAL2[ind.tr,])
  lv1_leukemia_CAL2$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv1_leukemia_CAL2$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv1_leukemia_CAL2[ind.val,])
  
  model.linear.spline.FY<-lm(FY_LEUKEMIA~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_leukemia_CAL2[ind.tr,])
  lv1_leukemia_CAL2$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv1_leukemia_CAL2$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv1_leukemia_CAL2[ind.val,])
  

}


### 3-1-4) LEVEL L1ab data --------------------------------------------------
lv1_leukemia_SEX_CAL2<-data[data$LEVEL=="L1ab",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LEUKEMIA", "LEUKEMIA")]


SEX_CAL2_level<-unique(lv1_leukemia_SEX_CAL2$NAME)

lv1_leukemia_SEX_CAL2$simple.reg<-0
lv1_leukemia_SEX_CAL2$poisson.reg<-0
lv1_leukemia_SEX_CAL2$linear.spline<-0
lv1_leukemia_SEX_CAL2$quadratic.reg<-0

lv1_leukemia_SEX_CAL2$simple.reg.FY<-0
lv1_leukemia_SEX_CAL2$poisson.reg.FY<-0
lv1_leukemia_SEX_CAL2$linear.spline.FY<-0
lv1_leukemia_SEX_CAL2$quadratic.reg.FY<-0

### 각 SEX, CAL2 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
### 각 model 별 train set에 대한 적합값, validation set에 대한 예측값 도출 --> 저장
for(i in 1:length(SEX_CAL2_level)){
  ind.tr<-which(lv1_leukemia_SEX_CAL2$YEAR<=2014 & lv1_leukemia_SEX_CAL2$NAME==SEX_CAL2_level[i])
  ind.val<-which(lv1_leukemia_SEX_CAL2$YEAR>2014 & lv1_leukemia_SEX_CAL2$NAME==SEX_CAL2_level[i])
  
  model.simple.reg<-lm(LEUKEMIA~YEAR, data=lv1_leukemia_SEX_CAL2[ind.tr,])
  lv1_leukemia_SEX_CAL2$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv1_leukemia_SEX_CAL2$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv1_leukemia_SEX_CAL2[ind.val,])
  
  model.simple.reg.FY<-lm(FY_LEUKEMIA~YEAR, data=lv1_leukemia_SEX_CAL2[ind.tr,])
  lv1_leukemia_SEX_CAL2$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv1_leukemia_SEX_CAL2$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv1_leukemia_SEX_CAL2[ind.val,])
  
  model.poisson.reg<-glm(LEUKEMIA~YEAR, data=lv1_leukemia_SEX_CAL2[ind.tr,],family='poisson')
  lv1_leukemia_SEX_CAL2$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv1_leukemia_SEX_CAL2$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv1_leukemia_SEX_CAL2[ind.val,]))
  
  model.poisson.reg.FY<-glm(FY_LEUKEMIA~YEAR, data=lv1_leukemia_SEX_CAL2[ind.tr,],family='poisson')
  lv1_leukemia_SEX_CAL2$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv1_leukemia_SEX_CAL2$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv1_leukemia_SEX_CAL2[ind.val,]))
  
  model.quadratic.reg<-lm(LEUKEMIA~poly(YEAR,2), data=lv1_leukemia_SEX_CAL2[ind.tr,])
  lv1_leukemia_SEX_CAL2$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv1_leukemia_SEX_CAL2$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv1_leukemia_SEX_CAL2[ind.val,])
  
  model.quadratic.reg.FY<-lm(FY_LEUKEMIA~poly(YEAR,2), data=lv1_leukemia_SEX_CAL2[ind.tr,])
  lv1_leukemia_SEX_CAL2$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv1_leukemia_SEX_CAL2$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv1_leukemia_SEX_CAL2[ind.val,])
  
  model.linear.spline<-lm(LEUKEMIA~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_leukemia_SEX_CAL2[ind.tr,])
  lv1_leukemia_SEX_CAL2$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv1_leukemia_SEX_CAL2$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv1_leukemia_SEX_CAL2[ind.val,])
  
  model.linear.spline.FY<-lm(FY_LEUKEMIA~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_leukemia_SEX_CAL2[ind.tr,])
  lv1_leukemia_SEX_CAL2$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv1_leukemia_SEX_CAL2$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv1_leukemia_SEX_CAL2[ind.val,])
}



### Leukemia - L1으로 시작하는 data binding ---------------------------------------
## train set에 대한 적합값, validation set에 대한 예측값 포함
val_leukemia_lv1<-as.data.frame(rbind(lv1_leukemia_total,lv1_leukemia_SEX,lv1_leukemia_CAL2,
                                     lv1_leukemia_SEX_CAL2))


# --------------------------------------------------------------------------------------------------------
### 3-1-5) LEVEL L2 data ----------------------------------------------------
lv2_leukemia_total<-data[data$LEVEL=="L2",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LEUKEMIA", "LEUKEMIA")]

lv2_leukemia_total$simple.reg<-0
lv2_leukemia_total$poisson.reg<-0
lv2_leukemia_total$linear.spline<-0
lv2_leukemia_total$quadratic.reg<-0

lv2_leukemia_total$simple.reg.FY<-0
lv2_leukemia_total$poisson.reg.FY<-0
lv2_leukemia_total$linear.spline.FY<-0
lv2_leukemia_total$quadratic.reg.FY<-0


### 각 NAME 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
### 각 model 별 train set에 대한 적합값, validation set에 대한 예측값 도출 --> 저장
name_level<-unique(lv2_leukemia_total$NAME)

for(i in 1:length(name_level)){
  ind.tr<-which(lv2_leukemia_total$YEAR<=2014 & lv2_leukemia_total$NAME==name_level[i])
  ind.val<-which(lv2_leukemia_total$YEAR>2014 & lv2_leukemia_total$NAME==name_level[i])
  
  model.simple.reg<-lm(LEUKEMIA~YEAR, data=lv2_leukemia_total[ind.tr,])
  lv2_leukemia_total$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv2_leukemia_total$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv2_leukemia_total[ind.val,])
  
  model.simple.reg.FY<-lm(FY_LEUKEMIA~YEAR, data=lv2_leukemia_total[ind.tr,])
  lv2_leukemia_total$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv2_leukemia_total$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv2_leukemia_total[ind.val,])
  
  model.poisson.reg<-glm(LEUKEMIA~YEAR, data=lv2_leukemia_total[ind.tr,],family='poisson')
  lv2_leukemia_total$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv2_leukemia_total$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv2_leukemia_total[ind.val,]))
  
  model.poisson.reg.FY<-glm(FY_LEUKEMIA~YEAR, data=lv2_leukemia_total[ind.tr,],family='poisson')
  lv2_leukemia_total$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv2_leukemia_total$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv2_leukemia_total[ind.val,]))
  
  model.quadratic.reg<-lm(LEUKEMIA~poly(YEAR,2), data=lv2_leukemia_total[ind.tr,])
  lv2_leukemia_total$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv2_leukemia_total$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv2_leukemia_total[ind.val,])
  
  model.quadratic.reg.FY<-lm(FY_LEUKEMIA~poly(YEAR,2), data=lv2_leukemia_total[ind.tr,])
  lv2_leukemia_total$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv2_leukemia_total$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv2_leukemia_total[ind.val,])
  
  model.linear.spline<-lm(LEUKEMIA~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_leukemia_total[ind.tr,])
  lv2_leukemia_total$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv2_leukemia_total$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv2_leukemia_total[ind.val,])
  
  model.linear.spline.FY<-lm(FY_LEUKEMIA~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_leukemia_total[ind.tr,])
  lv2_leukemia_total$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv2_leukemia_total$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv2_leukemia_total[ind.val,])

}




### 3-1-6) LEVEL L2a Data ---------------------------------------------------
lv2_leukemia_SEX<-data[data$LEVEL=="L2a",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LEUKEMIA", "LEUKEMIA")]

lv2_leukemia_SEX$simple.reg<-0
lv2_leukemia_SEX$poisson.reg<-0
lv2_leukemia_SEX$linear.spline<-0
lv2_leukemia_SEX$quadratic.reg<-0

lv2_leukemia_SEX$simple.reg.FY<-0
lv2_leukemia_SEX$poisson.reg.FY<-0
lv2_leukemia_SEX$linear.spline.FY<-0
lv2_leukemia_SEX$quadratic.reg.FY<-0



### 각 NAME 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
### 각 model 별 train set에 대한 적합값, validation set에 대한 예측값 도출 --> 저장
name_level<-unique(lv2_leukemia_SEX$NAME)

for(i in 1:length(name_level)){
  ind.tr<-which(lv2_leukemia_SEX$YEAR<=2014 & lv2_leukemia_SEX$NAME==name_level[i])
  ind.val<-which(lv2_leukemia_SEX$YEAR>2014 & lv2_leukemia_SEX$NAME==name_level[i])
  
  model.simple.reg<-lm(LEUKEMIA~YEAR, data=lv2_leukemia_SEX[ind.tr,])
  lv2_leukemia_SEX$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv2_leukemia_SEX$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv2_leukemia_SEX[ind.val,])
  
  model.simple.reg.FY<-lm(FY_LEUKEMIA~YEAR, data=lv2_leukemia_SEX[ind.tr,])
  lv2_leukemia_SEX$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv2_leukemia_SEX$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv2_leukemia_SEX[ind.val,])
  
  model.poisson.reg<-glm(LEUKEMIA~YEAR, data=lv2_leukemia_SEX[ind.tr,],family='poisson')
  lv2_leukemia_SEX$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv2_leukemia_SEX$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv2_leukemia_SEX[ind.val,]))
  
  model.poisson.reg.FY<-glm(FY_LEUKEMIA~YEAR, data=lv2_leukemia_SEX[ind.tr,],family='poisson')
  lv2_leukemia_SEX$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv2_leukemia_SEX$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv2_leukemia_SEX[ind.val,]))
  
  model.quadratic.reg<-lm(LEUKEMIA~poly(YEAR,2), data=lv2_leukemia_SEX[ind.tr,])
  lv2_leukemia_SEX$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv2_leukemia_SEX$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv2_leukemia_SEX[ind.val,])
  
  model.quadratic.reg.FY<-lm(FY_LEUKEMIA~poly(YEAR,2), data=lv2_leukemia_SEX[ind.tr,])
  lv2_leukemia_SEX$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv2_leukemia_SEX$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv2_leukemia_SEX[ind.val,])
  
  model.linear.spline<-lm(LEUKEMIA~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_leukemia_SEX[ind.tr,])
  lv2_leukemia_SEX$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv2_leukemia_SEX$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv2_leukemia_SEX[ind.val,])
  
  model.linear.spline.FY<-lm(FY_LEUKEMIA~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_leukemia_SEX[ind.tr,])
  lv2_leukemia_SEX$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv2_leukemia_SEX$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv2_leukemia_SEX[ind.val,])
}





### 3-1-7) LEVEL L2b data ---------------------------------------------------
lv2_leukemia_CAL2<-data[data$LEVEL=="L2b",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LEUKEMIA", "LEUKEMIA")]

lv2_leukemia_CAL2$simple.reg<-0
lv2_leukemia_CAL2$poisson.reg<-0
lv2_leukemia_CAL2$linear.spline<-0
lv2_leukemia_CAL2$quadratic.reg<-0

lv2_leukemia_CAL2$simple.reg.FY<-0
lv2_leukemia_CAL2$poisson.reg.FY<-0
lv2_leukemia_CAL2$linear.spline.FY<-0
lv2_leukemia_CAL2$quadratic.reg.FY<-0


### 각 NAME 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
### 각 model 별 train set에 대한 적합값, validation set에 대한 예측값 도출 --> 저장
name_level<-unique(lv2_leukemia_CAL2$NAME)

for(i in 1:length(name_level)){
  ind.tr<-which(lv2_leukemia_CAL2$YEAR<=2014 & lv2_leukemia_CAL2$NAME==name_level[i])
  ind.val<-which(lv2_leukemia_CAL2$YEAR>2014 & lv2_leukemia_CAL2$NAME==name_level[i])
  
  model.simple.reg<-lm(LEUKEMIA~YEAR, data=lv2_leukemia_CAL2[ind.tr,])
  lv2_leukemia_CAL2$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv2_leukemia_CAL2$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv2_leukemia_CAL2[ind.val,])
  
  model.simple.reg.FY<-lm(FY_LEUKEMIA~YEAR, data=lv2_leukemia_CAL2[ind.tr,])
  lv2_leukemia_CAL2$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv2_leukemia_CAL2$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv2_leukemia_CAL2[ind.val,])
  
  model.poisson.reg<-glm(LEUKEMIA~YEAR, data=lv2_leukemia_CAL2[ind.tr,],family='poisson')
  lv2_leukemia_CAL2$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv2_leukemia_CAL2$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv2_leukemia_CAL2[ind.val,]))
  
  model.poisson.reg.FY<-glm(FY_LEUKEMIA~YEAR, data=lv2_leukemia_CAL2[ind.tr,],family='poisson')
  lv2_leukemia_CAL2$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv2_leukemia_CAL2$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv2_leukemia_CAL2[ind.val,]))
  
  model.quadratic.reg<-lm(LEUKEMIA~poly(YEAR,2), data=lv2_leukemia_CAL2[ind.tr,])
  lv2_leukemia_CAL2$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv2_leukemia_CAL2$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv2_leukemia_CAL2[ind.val,])
  
  model.quadratic.reg.FY<-lm(FY_LEUKEMIA~poly(YEAR,2), data=lv2_leukemia_CAL2[ind.tr,])
  lv2_leukemia_CAL2$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv2_leukemia_CAL2$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv2_leukemia_CAL2[ind.val,])
  
  model.linear.spline<-lm(LEUKEMIA~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_leukemia_CAL2[ind.tr,])
  lv2_leukemia_CAL2$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv2_leukemia_CAL2$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv2_leukemia_CAL2[ind.val,])
  
  model.linear.spline.FY<-lm(FY_LEUKEMIA~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_leukemia_CAL2[ind.tr,])
  lv2_leukemia_CAL2$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv2_leukemia_CAL2$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv2_leukemia_CAL2[ind.val,])

}



### 3-1-8) LEVEL L2ab data --------------------------------------------------
lv2_leukemia_SEX_CAL2<-data[data$LEVEL=="L2ab",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LEUKEMIA", "LEUKEMIA")]

lv2_leukemia_SEX_CAL2$simple.reg<-0
lv2_leukemia_SEX_CAL2$poisson.reg<-0
lv2_leukemia_SEX_CAL2$linear.spline<-0
lv2_leukemia_SEX_CAL2$quadratic.reg<-0

lv2_leukemia_SEX_CAL2$simple.reg.FY<-0
lv2_leukemia_SEX_CAL2$poisson.reg.FY<-0
lv2_leukemia_SEX_CAL2$linear.spline.FY<-0
lv2_leukemia_SEX_CAL2$quadratic.reg.FY<-0

### 각 NAME 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
### 각 model 별 train set에 대한 적합값, validation set에 대한 예측값 도출 --> 저장
name_level<-unique(lv2_leukemia_SEX_CAL2$NAME)

for(i in 1:length(name_level)){
  ind.tr<-which(lv2_leukemia_SEX_CAL2$YEAR<=2014 & lv2_leukemia_SEX_CAL2$NAME==name_level[i])
  ind.val<-which(lv2_leukemia_SEX_CAL2$YEAR>2014 & lv2_leukemia_SEX_CAL2$NAME==name_level[i])
  
  model.simple.reg<-lm(LEUKEMIA~YEAR, data=lv2_leukemia_SEX_CAL2[ind.tr,])
  lv2_leukemia_SEX_CAL2$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv2_leukemia_SEX_CAL2$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv2_leukemia_SEX_CAL2[ind.val,])
  
  model.simple.reg.FY<-lm(FY_LEUKEMIA~YEAR, data=lv2_leukemia_SEX_CAL2[ind.tr,])
  lv2_leukemia_SEX_CAL2$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv2_leukemia_SEX_CAL2$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv2_leukemia_SEX_CAL2[ind.val,])
  
  model.poisson.reg<-glm(LEUKEMIA~YEAR, data=lv2_leukemia_SEX_CAL2[ind.tr,],family='poisson')
  lv2_leukemia_SEX_CAL2$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv2_leukemia_SEX_CAL2$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv2_leukemia_SEX_CAL2[ind.val,]))
  
  model.poisson.reg.FY<-glm(FY_LEUKEMIA~YEAR, data=lv2_leukemia_SEX_CAL2[ind.tr,],family='poisson')
  lv2_leukemia_SEX_CAL2$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv2_leukemia_SEX_CAL2$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv2_leukemia_SEX_CAL2[ind.val,]))
  
  model.quadratic.reg<-lm(LEUKEMIA~poly(YEAR,2), data=lv2_leukemia_SEX_CAL2[ind.tr,])
  lv2_leukemia_SEX_CAL2$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv2_leukemia_SEX_CAL2$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv2_leukemia_SEX_CAL2[ind.val,])
  
  model.quadratic.reg.FY<-lm(FY_LEUKEMIA~poly(YEAR,2), data=lv2_leukemia_SEX_CAL2[ind.tr,])
  lv2_leukemia_SEX_CAL2$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv2_leukemia_SEX_CAL2$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv2_leukemia_SEX_CAL2[ind.val,])
  
  model.linear.spline<-lm(LEUKEMIA~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_leukemia_SEX_CAL2[ind.tr,])
  lv2_leukemia_SEX_CAL2$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv2_leukemia_SEX_CAL2$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv2_leukemia_SEX_CAL2[ind.val,])
  
  model.linear.spline.FY<-lm(FY_LEUKEMIA~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_leukemia_SEX_CAL2[ind.tr,])
  lv2_leukemia_SEX_CAL2$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv2_leukemia_SEX_CAL2$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv2_leukemia_SEX_CAL2[ind.val,])

}

### Leukemia - L2으로 시작하는 data binding ---------------------------------------
## train set에 대한 적합값, validation set에 대한 예측값 포함
val_leukemia_lv2<-as.data.frame(rbind(lv2_leukemia_total,lv2_leukemia_SEX,lv2_leukemia_CAL2,
                                     lv2_leukemia_SEX_CAL2))


# -------------------------------------------------------------------------------------------------------
### 3-1-9) LEVEL L3 data ----------------------------------------------------
lv3_leukemia_total<-data[data$LEVEL=="L3",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LEUKEMIA", "LEUKEMIA")]

lv3_leukemia_total$simple.reg<-0
lv3_leukemia_total$poisson.reg<-0
lv3_leukemia_total$linear.spline<-0
lv3_leukemia_total$quadratic.reg<-0

lv3_leukemia_total$simple.reg.FY<-0
lv3_leukemia_total$poisson.reg.FY<-0
lv3_leukemia_total$linear.spline.FY<-0
lv3_leukemia_total$quadratic.reg.FY<-0

### 각 NAME 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
### 각 model 별 train set에 대한 적합값, validation set에 대한 예측값 도출 --> 저장
name_level<-unique(lv3_leukemia_total$NAME)

for(i in 1:length(name_level)){
  ind.tr<-which(lv3_leukemia_total$YEAR<=2014 & lv3_leukemia_total$NAME==name_level[i])
  ind.val<-which(lv3_leukemia_total$YEAR>2014 & lv3_leukemia_total$NAME==name_level[i])
  
  model.simple.reg<-lm(LEUKEMIA~YEAR, data=lv3_leukemia_total[ind.tr,])
  lv3_leukemia_total$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv3_leukemia_total$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv3_leukemia_total[ind.val,])
  
  model.simple.reg.FY<-lm(FY_LEUKEMIA~YEAR, data=lv3_leukemia_total[ind.tr,])
  lv3_leukemia_total$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv3_leukemia_total$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv3_leukemia_total[ind.val,])
  
  model.poisson.reg<-glm(LEUKEMIA~YEAR, data=lv3_leukemia_total[ind.tr,],family='poisson')
  lv3_leukemia_total$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv3_leukemia_total$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv3_leukemia_total[ind.val,]))
  
  model.poisson.reg.FY<-glm(FY_LEUKEMIA~YEAR, data=lv3_leukemia_total[ind.tr,],family='poisson')
  lv3_leukemia_total$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv3_leukemia_total$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv3_leukemia_total[ind.val,]))
  
  model.quadratic.reg<-lm(LEUKEMIA~poly(YEAR,2), data=lv3_leukemia_total[ind.tr,])
  lv3_leukemia_total$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv3_leukemia_total$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv3_leukemia_total[ind.val,])
  
  model.quadratic.reg.FY<-lm(FY_LEUKEMIA~poly(YEAR,2), data=lv3_leukemia_total[ind.tr,])
  lv3_leukemia_total$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv3_leukemia_total$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv3_leukemia_total[ind.val,])
  
  model.linear.spline<-lm(LEUKEMIA~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_leukemia_total[ind.tr,])
  lv3_leukemia_total$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv3_leukemia_total$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv3_leukemia_total[ind.val,])
  
  model.linear.spline.FY<-lm(FY_LEUKEMIA~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_leukemia_total[ind.tr,])
  lv3_leukemia_total$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv3_leukemia_total$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv3_leukemia_total[ind.val,])
}





### 3-1-10) LEVEL L3a Data --------------------------------------------------
lv3_leukemia_SEX<-data[data$LEVEL=="L3a",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LEUKEMIA", "LEUKEMIA")]

lv3_leukemia_SEX$simple.reg<-0
lv3_leukemia_SEX$poisson.reg<-0
lv3_leukemia_SEX$linear.spline<-0
lv3_leukemia_SEX$quadratic.reg<-0

lv3_leukemia_SEX$simple.reg.FY<-0
lv3_leukemia_SEX$poisson.reg.FY<-0
lv3_leukemia_SEX$linear.spline.FY<-0
lv3_leukemia_SEX$quadratic.reg.FY<-0

### 각 NAME 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
### 각 model 별 train set에 대한 적합값, validation set에 대한 예측값 도출 --> 저장
name_level<-unique(lv3_leukemia_SEX$NAME)

for(i in 1:length(name_level)){
  ind.tr<-which(lv3_leukemia_SEX$YEAR<=2014 & lv3_leukemia_SEX$NAME==name_level[i])
  ind.val<-which(lv3_leukemia_SEX$YEAR>2014 & lv3_leukemia_SEX$NAME==name_level[i])
  
  model.simple.reg<-lm(LEUKEMIA~YEAR, data=lv3_leukemia_SEX[ind.tr,])
  lv3_leukemia_SEX$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv3_leukemia_SEX$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv3_leukemia_SEX[ind.val,])
  
  model.simple.reg.FY<-lm(FY_LEUKEMIA~YEAR, data=lv3_leukemia_SEX[ind.tr,])
  lv3_leukemia_SEX$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv3_leukemia_SEX$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv3_leukemia_SEX[ind.val,])
  
  model.poisson.reg<-glm(LEUKEMIA~YEAR, data=lv3_leukemia_SEX[ind.tr,],family='poisson')
  lv3_leukemia_SEX$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv3_leukemia_SEX$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv3_leukemia_SEX[ind.val,]))
  
  model.poisson.reg.FY<-glm(FY_LEUKEMIA~YEAR, data=lv3_leukemia_SEX[ind.tr,],family='poisson')
  lv3_leukemia_SEX$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv3_leukemia_SEX$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv3_leukemia_SEX[ind.val,]))
  
  model.quadratic.reg<-lm(LEUKEMIA~poly(YEAR,2), data=lv3_leukemia_SEX[ind.tr,])
  lv3_leukemia_SEX$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv3_leukemia_SEX$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv3_leukemia_SEX[ind.val,])
  
  model.quadratic.reg.FY<-lm(FY_LEUKEMIA~poly(YEAR,2), data=lv3_leukemia_SEX[ind.tr,])
  lv3_leukemia_SEX$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv3_leukemia_SEX$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv3_leukemia_SEX[ind.val,])
  
  model.linear.spline<-lm(LEUKEMIA~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_leukemia_SEX[ind.tr,])
  lv3_leukemia_SEX$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv3_leukemia_SEX$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv3_leukemia_SEX[ind.val,])
  
  model.linear.spline.FY<-lm(FY_LEUKEMIA~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_leukemia_SEX[ind.tr,])
  lv3_leukemia_SEX$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv3_leukemia_SEX$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv3_leukemia_SEX[ind.val,])
}




### 3-1-11) LEVEL L3b Data --------------------------------------------------
lv3_leukemia_CAL2<-data[data$LEVEL=="L3b",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LEUKEMIA", "LEUKEMIA")]

lv3_leukemia_CAL2$simple.reg<-0
lv3_leukemia_CAL2$poisson.reg<-0
lv3_leukemia_CAL2$linear.spline<-0
lv3_leukemia_CAL2$quadratic.reg<-0

lv3_leukemia_CAL2$simple.reg.FY<-0
lv3_leukemia_CAL2$poisson.reg.FY<-0
lv3_leukemia_CAL2$linear.spline.FY<-0
lv3_leukemia_CAL2$quadratic.reg.FY<-0

### 각 NAME 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
### 각 model 별 train set에 대한 적합값, validation set에 대한 예측값 도출 --> 저장
name_level<-unique(lv3_leukemia_CAL2$NAME)

for(i in 1:length(name_level)){
  ind.tr<-which(lv3_leukemia_CAL2$YEAR<=2014 & lv3_leukemia_CAL2$NAME==name_level[i])
  ind.val<-which(lv3_leukemia_CAL2$YEAR>2014 & lv3_leukemia_CAL2$NAME==name_level[i])
  
  model.simple.reg<-lm(LEUKEMIA~YEAR, data=lv3_leukemia_CAL2[ind.tr,])
  lv3_leukemia_CAL2$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv3_leukemia_CAL2$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv3_leukemia_CAL2[ind.val,])
  
  model.simple.reg.FY<-lm(FY_LEUKEMIA~YEAR, data=lv3_leukemia_CAL2[ind.tr,])
  lv3_leukemia_CAL2$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv3_leukemia_CAL2$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv3_leukemia_CAL2[ind.val,])
  
  model.poisson.reg<-glm(LEUKEMIA~YEAR, data=lv3_leukemia_CAL2[ind.tr,],family='poisson')
  lv3_leukemia_CAL2$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv3_leukemia_CAL2$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv3_leukemia_CAL2[ind.val,]))
  
  model.poisson.reg.FY<-glm(FY_LEUKEMIA~YEAR, data=lv3_leukemia_CAL2[ind.tr,],family='poisson')
  lv3_leukemia_CAL2$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv3_leukemia_CAL2$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv3_leukemia_CAL2[ind.val,]))
  
  model.quadratic.reg<-lm(LEUKEMIA~poly(YEAR,2), data=lv3_leukemia_CAL2[ind.tr,])
  lv3_leukemia_CAL2$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv3_leukemia_CAL2$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv3_leukemia_CAL2[ind.val,])
  
  model.quadratic.reg.FY<-lm(FY_LEUKEMIA~poly(YEAR,2), data=lv3_leukemia_CAL2[ind.tr,])
  lv3_leukemia_CAL2$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv3_leukemia_CAL2$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv3_leukemia_CAL2[ind.val,])
  
  model.linear.spline<-lm(LEUKEMIA~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_leukemia_CAL2[ind.tr,])
  lv3_leukemia_CAL2$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv3_leukemia_CAL2$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv3_leukemia_CAL2[ind.val,])
  
  model.linear.spline.FY<-lm(FY_LEUKEMIA~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_leukemia_CAL2[ind.tr,])
  lv3_leukemia_CAL2$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv3_leukemia_CAL2$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv3_leukemia_CAL2[ind.val,])

}



### 3-1-12) LEVEL L3ab Data -------------------------------------------------
lv3_leukemia_SEX_CAL2<-data[data$LEVEL=="L3ab",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LEUKEMIA", "LEUKEMIA")]

lv3_leukemia_SEX_CAL2$simple.reg<-0
lv3_leukemia_SEX_CAL2$poisson.reg<-0
lv3_leukemia_SEX_CAL2$linear.spline<-0
lv3_leukemia_SEX_CAL2$quadratic.reg<-0

lv3_leukemia_SEX_CAL2$simple.reg.FY<-0
lv3_leukemia_SEX_CAL2$poisson.reg.FY<-0
lv3_leukemia_SEX_CAL2$linear.spline.FY<-0
lv3_leukemia_SEX_CAL2$quadratic.reg.FY<-0

### 각 NAME 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
### 각 model 별 train set에 대한 적합값, validation set에 대한 예측값 도출 --> 저장
name_level<-unique(lv3_leukemia_SEX_CAL2$NAME)

for(i in 1:length(name_level)){
  ind.tr<-which(lv3_leukemia_SEX_CAL2$YEAR<=2014 & lv3_leukemia_SEX_CAL2$NAME==name_level[i])
  ind.val<-which(lv3_leukemia_SEX_CAL2$YEAR>2014 & lv3_leukemia_SEX_CAL2$NAME==name_level[i])
  
  model.simple.reg<-lm(LEUKEMIA~YEAR, data=lv3_leukemia_SEX_CAL2[ind.tr,])
  lv3_leukemia_SEX_CAL2$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv3_leukemia_SEX_CAL2$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv3_leukemia_SEX_CAL2[ind.val,])
  
  model.simple.reg.FY<-lm(FY_LEUKEMIA~YEAR, data=lv3_leukemia_SEX_CAL2[ind.tr,])
  lv3_leukemia_SEX_CAL2$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv3_leukemia_SEX_CAL2$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv3_leukemia_SEX_CAL2[ind.val,])
  
  model.poisson.reg<-glm(LEUKEMIA~YEAR, data=lv3_leukemia_SEX_CAL2[ind.tr,],family='poisson')
  lv3_leukemia_SEX_CAL2$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv3_leukemia_SEX_CAL2$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv3_leukemia_SEX_CAL2[ind.val,]))
  
  model.poisson.reg.FY<-glm(FY_LEUKEMIA~YEAR, data=lv3_leukemia_SEX_CAL2[ind.tr,],family='poisson')
  lv3_leukemia_SEX_CAL2$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv3_leukemia_SEX_CAL2$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv3_leukemia_SEX_CAL2[ind.val,]))
  
  model.quadratic.reg<-lm(LEUKEMIA~poly(YEAR,2), data=lv3_leukemia_SEX_CAL2[ind.tr,])
  lv3_leukemia_SEX_CAL2$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv3_leukemia_SEX_CAL2$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv3_leukemia_SEX_CAL2[ind.val,])
  
  model.quadratic.reg.FY<-lm(FY_LEUKEMIA~poly(YEAR,2), data=lv3_leukemia_SEX_CAL2[ind.tr,])
  lv3_leukemia_SEX_CAL2$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv3_leukemia_SEX_CAL2$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv3_leukemia_SEX_CAL2[ind.val,])
  
  model.linear.spline<-lm(LEUKEMIA~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_leukemia_SEX_CAL2[ind.tr,])
  lv3_leukemia_SEX_CAL2$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv3_leukemia_SEX_CAL2$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv3_leukemia_SEX_CAL2[ind.val,])
  
  model.linear.spline.FY<-lm(FY_LEUKEMIA~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_leukemia_SEX_CAL2[ind.tr,])
  lv3_leukemia_SEX_CAL2$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv3_leukemia_SEX_CAL2$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv3_leukemia_SEX_CAL2[ind.val,])
}


### Leukemia - L3으로 시작하는 data binding ---------------------------------------
## train set에 대한 적합값, validation set에 대한 예측값 포함
val_leukemia_lv3<-as.data.frame(rbind(lv3_leukemia_total,lv3_leukemia_SEX,lv3_leukemia_CAL2,
                                     lv3_leukemia_SEX_CAL2))




# -------------------------------------------------------------------------
### Leukemia level 별 model 적합 결과 data Merge ---------------------------------------------
val_leukemia_total<-as.data.frame(rbind(val_leukemia_lv1,val_leukemia_lv2,val_leukemia_lv3))
colnames(val_leukemia_total)<-c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR","FY_LEUKEMIA", "LEUKEMIA", "LEUKEMIA_lin", 
                                "LEUKEMIA_poissonLin", "LEUKEMIA_linSpline", "LEUKEMIA_quad","FY_LEUKEMIA_lin", 
                                "FY_LEUKEMIA_poissonLin", "FY_LEUKEMIA_linSpline", "FY_LEUKEMIA_quad")
 



### 3-2. lung data ------------------------------------------------------
### 3-2-1) LEVEL L1 data ------------------------------------------------------
lv1_lung_total<-data[data$LEVEL=="L1",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LUNG", "LUNG")]

lv1_lung_total$simple.reg<-0
lv1_lung_total$poisson.reg<-0
lv1_lung_total$linear.spline<-0
lv1_lung_total$quadratic.reg<-0

lv1_lung_total$simple.reg.FY<-0
lv1_lung_total$poisson.reg.FY<-0
lv1_lung_total$linear.spline.FY<-0
lv1_lung_total$quadratic.reg.FY<-0

### train set, validation set split -----
ind.tr<-which(lv1_lung_total$YEAR<=2014)
ind.val<-which(lv1_lung_total$YEAR>2014)


### 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
### 각 model 별 train set에 대한 적합값, validation set에 대한 예측값 도출 --> 저장
model.simple.reg<-lm(LUNG~YEAR, data=lv1_lung_total[ind.tr,])
lv1_lung_total$simple.reg[ind.tr]<-model.simple.reg$fitted.values
lv1_lung_total$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv1_lung_total[ind.val,])

model.simple.reg.FY<-lm(FY_LUNG~YEAR, data=lv1_lung_total[ind.tr,])
lv1_lung_total$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
lv1_lung_total$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv1_lung_total[ind.val,])

model.poisson.reg<-glm(LUNG~YEAR, data=lv1_lung_total[ind.tr,],family='poisson')
lv1_lung_total$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
lv1_lung_total$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv1_lung_total[ind.val,]))

model.poisson.reg.FY<-glm(FY_LUNG~YEAR, data=lv1_lung_total[ind.tr,],family='poisson')
lv1_lung_total$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
lv1_lung_total$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv1_lung_total[ind.val,]))


model.quadratic.reg<-lm(LUNG~poly(YEAR,2), data=lv1_lung_total[ind.tr,])
lv1_lung_total$quadratic.reg[ind.tr]<-ifelse(model.quadratic.reg$fitted.values<0, 0, model.quadratic.reg$fitted.values)
lv1_lung_total$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv1_lung_total[ind.val,])

model.quadratic.reg.FY<-lm(FY_LUNG~poly(YEAR,2), data=lv1_lung_total[ind.tr,])
lv1_lung_total$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
lv1_lung_total$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv1_lung_total[ind.val,])


model.linear.spline<-lm(LUNG~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_lung_total[ind.tr,])
lv1_lung_total$linear.spline[ind.tr]<-model.linear.spline$fitted.values
lv1_lung_total$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv1_lung_total[ind.val,])

model.linear.spline.FY<-lm(FY_LUNG~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_lung_total[ind.tr,])
lv1_lung_total$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
lv1_lung_total$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv1_lung_total[ind.val,])



### 3-2-2) LEVEL L1a data --------------------------------------------------
lv1_lung_SEX<-data[data$LEVEL=="L1a",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LUNG", "LUNG")]

lv1_lung_SEX$simple.reg<-0
lv1_lung_SEX$poisson.reg<-0
lv1_lung_SEX$linear.spline<-0
lv1_lung_SEX$quadratic.reg<-0

lv1_lung_SEX$simple.reg.FY<-0
lv1_lung_SEX$poisson.reg.FY<-0
lv1_lung_SEX$linear.spline.FY<-0
lv1_lung_SEX$quadratic.reg.FY<-0
SEX_level<-unique(lv1_lung_SEX$SEX)

### SEX 마다 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
### 각 model 별 train set에 대한 적합값, validation set에 대한 예측값 도출 --> 저장
for(i in 1:length(SEX_level)){
  ind.tr<-which(lv1_lung_SEX$YEAR<=2014 & lv1_lung_SEX$SEX==SEX_level[i])
  ind.val<-which(lv1_lung_SEX$YEAR>2014 & lv1_lung_SEX$SEX==SEX_level[i])
  
  model.simple.reg<-lm(LUNG~YEAR, data=lv1_lung_SEX[ind.tr,])
  lv1_lung_SEX$simple.reg[ind.tr]<-ifelse(model.simple.reg$fitted.values<0, 0, model.simple.reg$fitted.values)
  lv1_lung_SEX$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv1_lung_SEX[ind.val,])
  
  model.simple.reg.FY<-lm(FY_LUNG~YEAR, data=lv1_lung_SEX[ind.tr,])
  lv1_lung_SEX$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv1_lung_SEX$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv1_lung_SEX[ind.val,])
  
  model.poisson.reg<-glm(LUNG~YEAR, data=lv1_lung_SEX[ind.tr,],family='poisson')
  lv1_lung_SEX$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv1_lung_SEX$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv1_lung_SEX[ind.val,]))
  
  model.poisson.reg.FY<-glm(FY_LUNG~YEAR, data=lv1_lung_SEX[ind.tr,],family='poisson')
  lv1_lung_SEX$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv1_lung_SEX$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv1_lung_SEX[ind.val,]))
  
  
  model.quadratic.reg<-lm(LUNG~poly(YEAR,2), data=lv1_lung_SEX[ind.tr,])
  lv1_lung_SEX$quadratic.reg[ind.tr]<-ifelse(model.quadratic.reg$fitted.values<0, 0, model.quadratic.reg$fitted.values)
  lv1_lung_SEX$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv1_lung_SEX[ind.val,])
  
  model.quadratic.reg.FY<-lm(FY_LUNG~poly(YEAR,2), data=lv1_lung_SEX[ind.tr,])
  lv1_lung_SEX$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv1_lung_SEX$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv1_lung_SEX[ind.val,])
  
  model.linear.spline<-lm(LUNG~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_lung_SEX[ind.tr,])
  lv1_lung_SEX$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv1_lung_SEX$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv1_lung_SEX[ind.val,])
  
  model.linear.spline.FY<-lm(FY_LUNG~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_lung_SEX[ind.tr,])
  lv1_lung_SEX$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv1_lung_SEX$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv1_lung_SEX[ind.val,])
  
}


### 3-2-3) LEVEL L1b data --------------------------------------------------
lv1_lung_CAL2<-data[data$LEVEL=="L1b",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LUNG", "LUNG")]

lv1_lung_CAL2$simple.reg<-0
lv1_lung_CAL2$poisson.reg<-0
lv1_lung_CAL2$linear.spline<-0
lv1_lung_CAL2$quadratic.reg<-0

lv1_lung_CAL2$simple.reg.FY<-0
lv1_lung_CAL2$poisson.reg.FY<-0
lv1_lung_CAL2$linear.spline.FY<-0
lv1_lung_CAL2$quadratic.reg.FY<-0

CAL2_level<-unique(lv1_lung_CAL2$CAL2)

### CAL2(입사시기) 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
### 각 model 별 train set에 대한 적합값, validation set에 대한 예측값 도출 --> 저장
for(i in 1:length(CAL2_level)){
  ind.tr<-which(lv1_lung_CAL2$YEAR<=2014 & lv1_lung_CAL2$CAL2==CAL2_level[i])
  ind.val<-which(lv1_lung_CAL2$YEAR>2014 & lv1_lung_CAL2$CAL2==CAL2_level[i])
  
  model.poisson.reg<-glm(LUNG~YEAR, data=lv1_lung_CAL2[ind.tr,],family='poisson')
  lv1_lung_CAL2$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv1_lung_CAL2$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv1_lung_CAL2[ind.val,]))
  
  model.poisson.reg.FY<-glm(FY_LUNG~YEAR, data=lv1_lung_CAL2[ind.tr,],family='poisson')
  lv1_lung_CAL2$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv1_lung_CAL2$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv1_lung_CAL2[ind.val,]))
  
  model.simple.reg<-lm(LUNG~YEAR, data=lv1_lung_CAL2[ind.tr,])
  lv1_lung_CAL2$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv1_lung_CAL2$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv1_lung_CAL2[ind.val,])
  
  model.simple.reg.FY<-lm(FY_LUNG~YEAR, data=lv1_lung_CAL2[ind.tr,])
  lv1_lung_CAL2$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv1_lung_CAL2$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv1_lung_CAL2[ind.val,])
  
  model.quadratic.reg<-lm(LUNG~poly(YEAR,2), data=lv1_lung_CAL2[ind.tr,])
  lv1_lung_CAL2$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv1_lung_CAL2$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv1_lung_CAL2[ind.val,])
  
  model.quadratic.reg.FY<-lm(FY_LUNG~poly(YEAR,2), data=lv1_lung_CAL2[ind.tr,])
  lv1_lung_CAL2$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv1_lung_CAL2$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv1_lung_CAL2[ind.val,])
  
  model.linear.spline<-lm(LUNG~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_lung_CAL2[ind.tr,])
  lv1_lung_CAL2$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv1_lung_CAL2$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv1_lung_CAL2[ind.val,])
  
  model.linear.spline.FY<-lm(FY_LUNG~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_lung_CAL2[ind.tr,])
  lv1_lung_CAL2$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv1_lung_CAL2$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv1_lung_CAL2[ind.val,])
  
  
}


### 3-2-4) LEVEL L1ab data --------------------------------------------------
lv1_lung_SEX_CAL2<-data[data$LEVEL=="L1ab",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LUNG", "LUNG")]


SEX_CAL2_level<-unique(lv1_lung_SEX_CAL2$NAME)

lv1_lung_SEX_CAL2$simple.reg<-0
lv1_lung_SEX_CAL2$poisson.reg<-0
lv1_lung_SEX_CAL2$linear.spline<-0
lv1_lung_SEX_CAL2$quadratic.reg<-0

lv1_lung_SEX_CAL2$simple.reg.FY<-0
lv1_lung_SEX_CAL2$poisson.reg.FY<-0
lv1_lung_SEX_CAL2$linear.spline.FY<-0
lv1_lung_SEX_CAL2$quadratic.reg.FY<-0

### 각 SEX, CAL2 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
### 각 model 별 train set에 대한 적합값, validation set에 대한 예측값 도출 --> 저장
for(i in 1:length(SEX_CAL2_level)){
  ind.tr<-which(lv1_lung_SEX_CAL2$YEAR<=2014 & lv1_lung_SEX_CAL2$NAME==SEX_CAL2_level[i])
  ind.val<-which(lv1_lung_SEX_CAL2$YEAR>2014 & lv1_lung_SEX_CAL2$NAME==SEX_CAL2_level[i])
  
  model.simple.reg<-lm(LUNG~YEAR, data=lv1_lung_SEX_CAL2[ind.tr,])
  lv1_lung_SEX_CAL2$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv1_lung_SEX_CAL2$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv1_lung_SEX_CAL2[ind.val,])
  
  model.simple.reg.FY<-lm(FY_LUNG~YEAR, data=lv1_lung_SEX_CAL2[ind.tr,])
  lv1_lung_SEX_CAL2$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv1_lung_SEX_CAL2$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv1_lung_SEX_CAL2[ind.val,])
  
  model.poisson.reg<-glm(LUNG~YEAR, data=lv1_lung_SEX_CAL2[ind.tr,],family='poisson')
  lv1_lung_SEX_CAL2$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv1_lung_SEX_CAL2$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv1_lung_SEX_CAL2[ind.val,]))
  
  model.poisson.reg.FY<-glm(FY_LUNG~YEAR, data=lv1_lung_SEX_CAL2[ind.tr,],family='poisson')
  lv1_lung_SEX_CAL2$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv1_lung_SEX_CAL2$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv1_lung_SEX_CAL2[ind.val,]))
  
  model.quadratic.reg<-lm(LUNG~poly(YEAR,2), data=lv1_lung_SEX_CAL2[ind.tr,])
  lv1_lung_SEX_CAL2$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv1_lung_SEX_CAL2$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv1_lung_SEX_CAL2[ind.val,])
  
  model.quadratic.reg.FY<-lm(FY_LUNG~poly(YEAR,2), data=lv1_lung_SEX_CAL2[ind.tr,])
  lv1_lung_SEX_CAL2$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv1_lung_SEX_CAL2$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv1_lung_SEX_CAL2[ind.val,])
  
  model.linear.spline<-lm(LUNG~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_lung_SEX_CAL2[ind.tr,])
  lv1_lung_SEX_CAL2$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv1_lung_SEX_CAL2$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv1_lung_SEX_CAL2[ind.val,])
  
  model.linear.spline.FY<-lm(FY_LUNG~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_lung_SEX_CAL2[ind.tr,])
  lv1_lung_SEX_CAL2$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv1_lung_SEX_CAL2$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv1_lung_SEX_CAL2[ind.val,])
}



### Lung - L1으로 시작하는 data binding ---------------------------------------
## train set에 대한 적합값, validation set에 대한 예측값 포함
val_lung_lv1<-as.data.frame(rbind(lv1_lung_total,lv1_lung_SEX,lv1_lung_CAL2,
                                  lv1_lung_SEX_CAL2))


# --------------------------------------------------------------------------------------------------------
### 3-2-5) LEVEL L2 data ----------------------------------------------------
lv2_lung_total<-data[data$LEVEL=="L2",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LUNG", "LUNG")]

lv2_lung_total$simple.reg<-0
lv2_lung_total$poisson.reg<-0
lv2_lung_total$linear.spline<-0
lv2_lung_total$quadratic.reg<-0

lv2_lung_total$simple.reg.FY<-0
lv2_lung_total$poisson.reg.FY<-0
lv2_lung_total$linear.spline.FY<-0
lv2_lung_total$quadratic.reg.FY<-0


### 각 NAME 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
### 각 model 별 train set에 대한 적합값, validation set에 대한 예측값 도출 --> 저장
name_level<-unique(lv2_lung_total$NAME)

for(i in 1:length(name_level)){
  ind.tr<-which(lv2_lung_total$YEAR<=2014 & lv2_lung_total$NAME==name_level[i])
  ind.val<-which(lv2_lung_total$YEAR>2014 & lv2_lung_total$NAME==name_level[i])
  
  model.simple.reg<-lm(LUNG~YEAR, data=lv2_lung_total[ind.tr,])
  lv2_lung_total$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv2_lung_total$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv2_lung_total[ind.val,])
  
  model.simple.reg.FY<-lm(FY_LUNG~YEAR, data=lv2_lung_total[ind.tr,])
  lv2_lung_total$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv2_lung_total$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv2_lung_total[ind.val,])
  
  model.poisson.reg<-glm(LUNG~YEAR, data=lv2_lung_total[ind.tr,],family='poisson')
  lv2_lung_total$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv2_lung_total$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv2_lung_total[ind.val,]))
  
  model.poisson.reg.FY<-glm(FY_LUNG~YEAR, data=lv2_lung_total[ind.tr,],family='poisson')
  lv2_lung_total$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv2_lung_total$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv2_lung_total[ind.val,]))
  
  model.quadratic.reg<-lm(LUNG~poly(YEAR,2), data=lv2_lung_total[ind.tr,])
  lv2_lung_total$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv2_lung_total$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv2_lung_total[ind.val,])
  
  model.quadratic.reg.FY<-lm(FY_LUNG~poly(YEAR,2), data=lv2_lung_total[ind.tr,])
  lv2_lung_total$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv2_lung_total$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv2_lung_total[ind.val,])
  
  model.linear.spline<-lm(LUNG~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_lung_total[ind.tr,])
  lv2_lung_total$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv2_lung_total$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv2_lung_total[ind.val,])
  
  model.linear.spline.FY<-lm(FY_LUNG~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_lung_total[ind.tr,])
  lv2_lung_total$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv2_lung_total$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv2_lung_total[ind.val,])
  
}




### 3-2-6) LEVEL L2a Data ---------------------------------------------------
lv2_lung_SEX<-data[data$LEVEL=="L2a",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LUNG", "LUNG")]

lv2_lung_SEX$simple.reg<-0
lv2_lung_SEX$poisson.reg<-0
lv2_lung_SEX$linear.spline<-0
lv2_lung_SEX$quadratic.reg<-0

lv2_lung_SEX$simple.reg.FY<-0
lv2_lung_SEX$poisson.reg.FY<-0
lv2_lung_SEX$linear.spline.FY<-0
lv2_lung_SEX$quadratic.reg.FY<-0



### 각 NAME 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
### 각 model 별 train set에 대한 적합값, validation set에 대한 예측값 도출 --> 저장
name_level<-unique(lv2_lung_SEX$NAME)

for(i in 1:length(name_level)){
  ind.tr<-which(lv2_lung_SEX$YEAR<=2014 & lv2_lung_SEX$NAME==name_level[i])
  ind.val<-which(lv2_lung_SEX$YEAR>2014 & lv2_lung_SEX$NAME==name_level[i])
  
  model.simple.reg<-lm(LUNG~YEAR, data=lv2_lung_SEX[ind.tr,])
  lv2_lung_SEX$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv2_lung_SEX$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv2_lung_SEX[ind.val,])
  
  model.simple.reg.FY<-lm(FY_LUNG~YEAR, data=lv2_lung_SEX[ind.tr,])
  lv2_lung_SEX$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv2_lung_SEX$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv2_lung_SEX[ind.val,])
  
  model.poisson.reg<-glm(LUNG~YEAR, data=lv2_lung_SEX[ind.tr,],family='poisson')
  lv2_lung_SEX$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv2_lung_SEX$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv2_lung_SEX[ind.val,]))
  
  model.poisson.reg.FY<-glm(FY_LUNG~YEAR, data=lv2_lung_SEX[ind.tr,],family='poisson')
  lv2_lung_SEX$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv2_lung_SEX$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv2_lung_SEX[ind.val,]))
  
  model.quadratic.reg<-lm(LUNG~poly(YEAR,2), data=lv2_lung_SEX[ind.tr,])
  lv2_lung_SEX$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv2_lung_SEX$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv2_lung_SEX[ind.val,])
  
  model.quadratic.reg.FY<-lm(FY_LUNG~poly(YEAR,2), data=lv2_lung_SEX[ind.tr,])
  lv2_lung_SEX$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv2_lung_SEX$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv2_lung_SEX[ind.val,])
  
  model.linear.spline<-lm(LUNG~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_lung_SEX[ind.tr,])
  lv2_lung_SEX$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv2_lung_SEX$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv2_lung_SEX[ind.val,])
  
  model.linear.spline.FY<-lm(FY_LUNG~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_lung_SEX[ind.tr,])
  lv2_lung_SEX$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv2_lung_SEX$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv2_lung_SEX[ind.val,])
}





### 3-2-7) LEVEL L2b data ---------------------------------------------------
lv2_lung_CAL2<-data[data$LEVEL=="L2b",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LUNG", "LUNG")]

lv2_lung_CAL2$simple.reg<-0
lv2_lung_CAL2$poisson.reg<-0
lv2_lung_CAL2$linear.spline<-0
lv2_lung_CAL2$quadratic.reg<-0

lv2_lung_CAL2$simple.reg.FY<-0
lv2_lung_CAL2$poisson.reg.FY<-0
lv2_lung_CAL2$linear.spline.FY<-0
lv2_lung_CAL2$quadratic.reg.FY<-0


### 각 NAME 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
### 각 model 별 train set에 대한 적합값, validation set에 대한 예측값 도출 --> 저장
name_level<-unique(lv2_lung_CAL2$NAME)

for(i in 1:length(name_level)){
  ind.tr<-which(lv2_lung_CAL2$YEAR<=2014 & lv2_lung_CAL2$NAME==name_level[i])
  ind.val<-which(lv2_lung_CAL2$YEAR>2014 & lv2_lung_CAL2$NAME==name_level[i])
  
  model.simple.reg<-lm(LUNG~YEAR, data=lv2_lung_CAL2[ind.tr,])
  lv2_lung_CAL2$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv2_lung_CAL2$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv2_lung_CAL2[ind.val,])
  
  model.simple.reg.FY<-lm(FY_LUNG~YEAR, data=lv2_lung_CAL2[ind.tr,])
  lv2_lung_CAL2$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv2_lung_CAL2$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv2_lung_CAL2[ind.val,])
  
  model.poisson.reg<-glm(LUNG~YEAR, data=lv2_lung_CAL2[ind.tr,],family='poisson')
  lv2_lung_CAL2$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv2_lung_CAL2$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv2_lung_CAL2[ind.val,]))
  
  model.poisson.reg.FY<-glm(FY_LUNG~YEAR, data=lv2_lung_CAL2[ind.tr,],family='poisson')
  lv2_lung_CAL2$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv2_lung_CAL2$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv2_lung_CAL2[ind.val,]))
  
  model.quadratic.reg<-lm(LUNG~poly(YEAR,2), data=lv2_lung_CAL2[ind.tr,])
  lv2_lung_CAL2$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv2_lung_CAL2$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv2_lung_CAL2[ind.val,])
  
  model.quadratic.reg.FY<-lm(FY_LUNG~poly(YEAR,2), data=lv2_lung_CAL2[ind.tr,])
  lv2_lung_CAL2$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv2_lung_CAL2$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv2_lung_CAL2[ind.val,])
  
  model.linear.spline<-lm(LUNG~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_lung_CAL2[ind.tr,])
  lv2_lung_CAL2$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv2_lung_CAL2$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv2_lung_CAL2[ind.val,])
  
  model.linear.spline.FY<-lm(FY_LUNG~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_lung_CAL2[ind.tr,])
  lv2_lung_CAL2$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv2_lung_CAL2$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv2_lung_CAL2[ind.val,])
  
}



### 3-2-8) LEVEL L2ab data --------------------------------------------------
lv2_lung_SEX_CAL2<-data[data$LEVEL=="L2ab",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LUNG", "LUNG")]

lv2_lung_SEX_CAL2$simple.reg<-0
lv2_lung_SEX_CAL2$poisson.reg<-0
lv2_lung_SEX_CAL2$linear.spline<-0
lv2_lung_SEX_CAL2$quadratic.reg<-0

lv2_lung_SEX_CAL2$simple.reg.FY<-0
lv2_lung_SEX_CAL2$poisson.reg.FY<-0
lv2_lung_SEX_CAL2$linear.spline.FY<-0
lv2_lung_SEX_CAL2$quadratic.reg.FY<-0

### 각 NAME 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
### 각 model 별 train set에 대한 적합값, validation set에 대한 예측값 도출 --> 저장
name_level<-unique(lv2_lung_SEX_CAL2$NAME)

for(i in 1:length(name_level)){
  ind.tr<-which(lv2_lung_SEX_CAL2$YEAR<=2014 & lv2_lung_SEX_CAL2$NAME==name_level[i])
  ind.val<-which(lv2_lung_SEX_CAL2$YEAR>2014 & lv2_lung_SEX_CAL2$NAME==name_level[i])
  
  model.simple.reg<-lm(LUNG~YEAR, data=lv2_lung_SEX_CAL2[ind.tr,])
  lv2_lung_SEX_CAL2$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv2_lung_SEX_CAL2$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv2_lung_SEX_CAL2[ind.val,])
  
  model.simple.reg.FY<-lm(FY_LUNG~YEAR, data=lv2_lung_SEX_CAL2[ind.tr,])
  lv2_lung_SEX_CAL2$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv2_lung_SEX_CAL2$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv2_lung_SEX_CAL2[ind.val,])
  
  model.poisson.reg<-glm(LUNG~YEAR, data=lv2_lung_SEX_CAL2[ind.tr,],family='poisson')
  lv2_lung_SEX_CAL2$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv2_lung_SEX_CAL2$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv2_lung_SEX_CAL2[ind.val,]))
  
  model.poisson.reg.FY<-glm(FY_LUNG~YEAR, data=lv2_lung_SEX_CAL2[ind.tr,],family='poisson')
  lv2_lung_SEX_CAL2$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv2_lung_SEX_CAL2$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv2_lung_SEX_CAL2[ind.val,]))
  
  model.quadratic.reg<-lm(LUNG~poly(YEAR,2), data=lv2_lung_SEX_CAL2[ind.tr,])
  lv2_lung_SEX_CAL2$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv2_lung_SEX_CAL2$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv2_lung_SEX_CAL2[ind.val,])
  
  model.quadratic.reg.FY<-lm(FY_LUNG~poly(YEAR,2), data=lv2_lung_SEX_CAL2[ind.tr,])
  lv2_lung_SEX_CAL2$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv2_lung_SEX_CAL2$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv2_lung_SEX_CAL2[ind.val,])
  
  model.linear.spline<-lm(LUNG~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_lung_SEX_CAL2[ind.tr,])
  lv2_lung_SEX_CAL2$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv2_lung_SEX_CAL2$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv2_lung_SEX_CAL2[ind.val,])
  
  model.linear.spline.FY<-lm(FY_LUNG~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_lung_SEX_CAL2[ind.tr,])
  lv2_lung_SEX_CAL2$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv2_lung_SEX_CAL2$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv2_lung_SEX_CAL2[ind.val,])
  
}

### Lung - L2으로 시작하는 data binding ---------------------------------------
## train set에 대한 적합값, validation set에 대한 예측값 포함
val_lung_lv2<-as.data.frame(rbind(lv2_lung_total,lv2_lung_SEX,lv2_lung_CAL2,
                                  lv2_lung_SEX_CAL2))


# -------------------------------------------------------------------------------------------------------
### 3-2-9) LEVEL L3 data ----------------------------------------------------
lv3_lung_total<-data[data$LEVEL=="L3",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LUNG", "LUNG")]

lv3_lung_total$simple.reg<-0
lv3_lung_total$poisson.reg<-0
lv3_lung_total$linear.spline<-0
lv3_lung_total$quadratic.reg<-0

lv3_lung_total$simple.reg.FY<-0
lv3_lung_total$poisson.reg.FY<-0
lv3_lung_total$linear.spline.FY<-0
lv3_lung_total$quadratic.reg.FY<-0

### 각 NAME 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
### 각 model 별 train set에 대한 적합값, validation set에 대한 예측값 도출 --> 저장
name_level<-unique(lv3_lung_total$NAME)

for(i in 1:length(name_level)){
  ind.tr<-which(lv3_lung_total$YEAR<=2014 & lv3_lung_total$NAME==name_level[i])
  ind.val<-which(lv3_lung_total$YEAR>2014 & lv3_lung_total$NAME==name_level[i])
  
  model.simple.reg<-lm(LUNG~YEAR, data=lv3_lung_total[ind.tr,])
  lv3_lung_total$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv3_lung_total$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv3_lung_total[ind.val,])
  
  model.simple.reg.FY<-lm(FY_LUNG~YEAR, data=lv3_lung_total[ind.tr,])
  lv3_lung_total$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv3_lung_total$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv3_lung_total[ind.val,])
  
  model.poisson.reg<-glm(LUNG~YEAR, data=lv3_lung_total[ind.tr,],family='poisson')
  lv3_lung_total$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv3_lung_total$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv3_lung_total[ind.val,]))
  
  model.poisson.reg.FY<-glm(FY_LUNG~YEAR, data=lv3_lung_total[ind.tr,],family='poisson')
  lv3_lung_total$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv3_lung_total$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv3_lung_total[ind.val,]))
  
  model.quadratic.reg<-lm(LUNG~poly(YEAR,2), data=lv3_lung_total[ind.tr,])
  lv3_lung_total$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv3_lung_total$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv3_lung_total[ind.val,])
  
  model.quadratic.reg.FY<-lm(FY_LUNG~poly(YEAR,2), data=lv3_lung_total[ind.tr,])
  lv3_lung_total$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv3_lung_total$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv3_lung_total[ind.val,])
  
  model.linear.spline<-lm(LUNG~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_lung_total[ind.tr,])
  lv3_lung_total$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv3_lung_total$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv3_lung_total[ind.val,])
  
  model.linear.spline.FY<-lm(FY_LUNG~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_lung_total[ind.tr,])
  lv3_lung_total$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv3_lung_total$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv3_lung_total[ind.val,])
}





### 3-2-10) LEVEL L3a Data --------------------------------------------------
lv3_lung_SEX<-data[data$LEVEL=="L3a",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LUNG", "LUNG")]

lv3_lung_SEX$simple.reg<-0
lv3_lung_SEX$poisson.reg<-0
lv3_lung_SEX$linear.spline<-0
lv3_lung_SEX$quadratic.reg<-0

lv3_lung_SEX$simple.reg.FY<-0
lv3_lung_SEX$poisson.reg.FY<-0
lv3_lung_SEX$linear.spline.FY<-0
lv3_lung_SEX$quadratic.reg.FY<-0

### 각 NAME 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
### 각 model 별 train set에 대한 적합값, validation set에 대한 예측값 도출 --> 저장
name_level<-unique(lv3_lung_SEX$NAME)

for(i in 1:length(name_level)){
  ind.tr<-which(lv3_lung_SEX$YEAR<=2014 & lv3_lung_SEX$NAME==name_level[i])
  ind.val<-which(lv3_lung_SEX$YEAR>2014 & lv3_lung_SEX$NAME==name_level[i])
  
  model.simple.reg<-lm(LUNG~YEAR, data=lv3_lung_SEX[ind.tr,])
  lv3_lung_SEX$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv3_lung_SEX$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv3_lung_SEX[ind.val,])
  
  model.simple.reg.FY<-lm(FY_LUNG~YEAR, data=lv3_lung_SEX[ind.tr,])
  lv3_lung_SEX$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv3_lung_SEX$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv3_lung_SEX[ind.val,])
  
  model.poisson.reg<-glm(LUNG~YEAR, data=lv3_lung_SEX[ind.tr,],family='poisson')
  lv3_lung_SEX$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv3_lung_SEX$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv3_lung_SEX[ind.val,]))
  
  model.poisson.reg.FY<-glm(FY_LUNG~YEAR, data=lv3_lung_SEX[ind.tr,],family='poisson')
  lv3_lung_SEX$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv3_lung_SEX$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv3_lung_SEX[ind.val,]))
  
  model.quadratic.reg<-lm(LUNG~poly(YEAR,2), data=lv3_lung_SEX[ind.tr,])
  lv3_lung_SEX$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv3_lung_SEX$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv3_lung_SEX[ind.val,])
  
  model.quadratic.reg.FY<-lm(FY_LUNG~poly(YEAR,2), data=lv3_lung_SEX[ind.tr,])
  lv3_lung_SEX$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv3_lung_SEX$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv3_lung_SEX[ind.val,])
  
  model.linear.spline<-lm(LUNG~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_lung_SEX[ind.tr,])
  lv3_lung_SEX$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv3_lung_SEX$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv3_lung_SEX[ind.val,])
  
  model.linear.spline.FY<-lm(FY_LUNG~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_lung_SEX[ind.tr,])
  lv3_lung_SEX$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv3_lung_SEX$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv3_lung_SEX[ind.val,])
}




### 3-2-11) LEVEL L3b Data --------------------------------------------------
lv3_lung_CAL2<-data[data$LEVEL=="L3b",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LUNG", "LUNG")]

lv3_lung_CAL2$simple.reg<-0
lv3_lung_CAL2$poisson.reg<-0
lv3_lung_CAL2$linear.spline<-0
lv3_lung_CAL2$quadratic.reg<-0

lv3_lung_CAL2$simple.reg.FY<-0
lv3_lung_CAL2$poisson.reg.FY<-0
lv3_lung_CAL2$linear.spline.FY<-0
lv3_lung_CAL2$quadratic.reg.FY<-0

### 각 NAME 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
### 각 model 별 train set에 대한 적합값, validation set에 대한 예측값 도출 --> 저장
name_level<-unique(lv3_lung_CAL2$NAME)

for(i in 1:length(name_level)){
  ind.tr<-which(lv3_lung_CAL2$YEAR<=2014 & lv3_lung_CAL2$NAME==name_level[i])
  ind.val<-which(lv3_lung_CAL2$YEAR>2014 & lv3_lung_CAL2$NAME==name_level[i])
  
  model.simple.reg<-lm(LUNG~YEAR, data=lv3_lung_CAL2[ind.tr,])
  lv3_lung_CAL2$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv3_lung_CAL2$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv3_lung_CAL2[ind.val,])
  
  model.simple.reg.FY<-lm(FY_LUNG~YEAR, data=lv3_lung_CAL2[ind.tr,])
  lv3_lung_CAL2$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv3_lung_CAL2$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv3_lung_CAL2[ind.val,])
  
  model.poisson.reg<-glm(LUNG~YEAR, data=lv3_lung_CAL2[ind.tr,],family='poisson')
  lv3_lung_CAL2$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv3_lung_CAL2$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv3_lung_CAL2[ind.val,]))
  
  model.poisson.reg.FY<-glm(FY_LUNG~YEAR, data=lv3_lung_CAL2[ind.tr,],family='poisson')
  lv3_lung_CAL2$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv3_lung_CAL2$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv3_lung_CAL2[ind.val,]))
  
  model.quadratic.reg<-lm(LUNG~poly(YEAR,2), data=lv3_lung_CAL2[ind.tr,])
  lv3_lung_CAL2$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv3_lung_CAL2$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv3_lung_CAL2[ind.val,])
  
  model.quadratic.reg.FY<-lm(FY_LUNG~poly(YEAR,2), data=lv3_lung_CAL2[ind.tr,])
  lv3_lung_CAL2$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv3_lung_CAL2$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv3_lung_CAL2[ind.val,])
  
  model.linear.spline<-lm(LUNG~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_lung_CAL2[ind.tr,])
  lv3_lung_CAL2$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv3_lung_CAL2$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv3_lung_CAL2[ind.val,])
  
  model.linear.spline.FY<-lm(FY_LUNG~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_lung_CAL2[ind.tr,])
  lv3_lung_CAL2$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv3_lung_CAL2$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv3_lung_CAL2[ind.val,])
  
}



### 3-2-12) LEVEL L3ab Data -------------------------------------------------
lv3_lung_SEX_CAL2<-data[data$LEVEL=="L3ab",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LUNG", "LUNG")]

lv3_lung_SEX_CAL2$simple.reg<-0
lv3_lung_SEX_CAL2$poisson.reg<-0
lv3_lung_SEX_CAL2$linear.spline<-0
lv3_lung_SEX_CAL2$quadratic.reg<-0

lv3_lung_SEX_CAL2$simple.reg.FY<-0
lv3_lung_SEX_CAL2$poisson.reg.FY<-0
lv3_lung_SEX_CAL2$linear.spline.FY<-0
lv3_lung_SEX_CAL2$quadratic.reg.FY<-0

### 각 NAME 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
### 각 model 별 train set에 대한 적합값, validation set에 대한 예측값 도출 --> 저장
name_level<-unique(lv3_lung_SEX_CAL2$NAME)

for(i in 1:length(name_level)){
  ind.tr<-which(lv3_lung_SEX_CAL2$YEAR<=2014 & lv3_lung_SEX_CAL2$NAME==name_level[i])
  ind.val<-which(lv3_lung_SEX_CAL2$YEAR>2014 & lv3_lung_SEX_CAL2$NAME==name_level[i])
  
  model.simple.reg<-lm(LUNG~YEAR, data=lv3_lung_SEX_CAL2[ind.tr,])
  lv3_lung_SEX_CAL2$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv3_lung_SEX_CAL2$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv3_lung_SEX_CAL2[ind.val,])
  
  model.simple.reg.FY<-lm(FY_LUNG~YEAR, data=lv3_lung_SEX_CAL2[ind.tr,])
  lv3_lung_SEX_CAL2$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv3_lung_SEX_CAL2$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv3_lung_SEX_CAL2[ind.val,])
  
  model.poisson.reg<-glm(LUNG~YEAR, data=lv3_lung_SEX_CAL2[ind.tr,],family='poisson')
  lv3_lung_SEX_CAL2$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv3_lung_SEX_CAL2$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv3_lung_SEX_CAL2[ind.val,]))
  
  model.poisson.reg.FY<-glm(FY_LUNG~YEAR, data=lv3_lung_SEX_CAL2[ind.tr,],family='poisson')
  lv3_lung_SEX_CAL2$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv3_lung_SEX_CAL2$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv3_lung_SEX_CAL2[ind.val,]))
  
  model.quadratic.reg<-lm(LUNG~poly(YEAR,2), data=lv3_lung_SEX_CAL2[ind.tr,])
  lv3_lung_SEX_CAL2$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv3_lung_SEX_CAL2$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv3_lung_SEX_CAL2[ind.val,])
  
  model.quadratic.reg.FY<-lm(FY_LUNG~poly(YEAR,2), data=lv3_lung_SEX_CAL2[ind.tr,])
  lv3_lung_SEX_CAL2$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv3_lung_SEX_CAL2$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv3_lung_SEX_CAL2[ind.val,])
  
  model.linear.spline<-lm(LUNG~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_lung_SEX_CAL2[ind.tr,])
  lv3_lung_SEX_CAL2$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv3_lung_SEX_CAL2$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv3_lung_SEX_CAL2[ind.val,])
  
  model.linear.spline.FY<-lm(FY_LUNG~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_lung_SEX_CAL2[ind.tr,])
  lv3_lung_SEX_CAL2$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv3_lung_SEX_CAL2$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv3_lung_SEX_CAL2[ind.val,])
}


### Lung - L3으로 시작하는 data binding ---------------------------------------
## train set에 대한 적합값, validation set에 대한 예측값 포함
val_lung_lv3<-as.data.frame(rbind(lv3_lung_total,lv3_lung_SEX,lv3_lung_CAL2,
                                  lv3_lung_SEX_CAL2))



# -------------------------------------------------------------------------
### Lung level 별 model 적합 결과 data Merge ---------------------------------------------
val_lung_total<-as.data.frame(rbind(val_lung_lv1,val_lung_lv2,val_lung_lv3))
colnames(val_lung_total)<-c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR","FY_LUNG", "LUNG", "LUNG_lin", 
                            "LUNG_poissonLin", "LUNG_linSpline", "LUNG_quad","FY_LUNG_lin", 
                            "FY_LUNG_poissonLin", "FY_LUNG_linSpline", "FY_LUNG_quad")




# -------------------------------------------------------------------------
### Leukemia data & Lung cancer data 적합 결과 Merge ----------------------------
validation.dt<-merge(val_leukemia_total,val_lung_total,key=c("ID","NAME","LEVEL","YEAR"),all=TRUE)

### csv file로 저장------------------------------------------------------
write.csv(validation.dt,file='validation_final.csv',row.names=FALSE)
