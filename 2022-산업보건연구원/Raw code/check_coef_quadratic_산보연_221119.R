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


## 3. check the coef of quadratic model --------------------------------------------------------
check_coef<-as.data.frame(matrix(NA,nrow=length(unique(data$NAME)),ncol=2))
colnames(check_coef)<-c("NAME","coef")


### -------------------------------------------------------------------------
### 3-1. leukemia data ------------------------------------------------------

### 3-1-1) LEVEL L1 data ------------------------------------------------------
lv1_leukemia_total<-data[data$LEVEL=="L1",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LEUKEMIA", "LEUKEMIA")]

model.quadratic.reg<-lm(LEUKEMIA~poly(YEAR,2), data=lv1_leukemia_total)
check_coef[1,]<-c(unique(lv1_leukemia_total$NAME),coef(model.quadratic.reg)[3])


### 3-1-2) LEVEL L1a data --------------------------------------------------
lv1_leukemia_SEX<-data[data$LEVEL=="L1a",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LEUKEMIA", "LEUKEMIA")]

SEX_level<-unique(lv1_leukemia_SEX$SEX)


### SEX 마다 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
for(i in 1:length(SEX_level)){
  
  ind.tr<-which(lv1_leukemia_SEX$SEX==SEX_level[i])
  model.quadratic.reg<-lm(LEUKEMIA~poly(YEAR,2), data=lv1_leukemia_SEX[ind.tr,])
  check_coef[(i+1),]<-c(unique(lv1_leukemia_SEX[ind.tr,]$NAME),coef(model.quadratic.reg)[3])

}



### 3-1-3) LEVEL L1b data --------------------------------------------------
lv1_leukemia_CAL2<-data[data$LEVEL=="L1b",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LEUKEMIA", "LEUKEMIA")]
CAL2_level<-unique(lv1_leukemia_CAL2$CAL2)

### CAL2(입사시기) 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
for(i in 1:length(CAL2_level)){
  
  ind.tr<-which(lv1_leukemia_CAL2$CAL2==CAL2_level[i])
  model.quadratic.reg<-lm(LEUKEMIA~poly(YEAR,2), data=lv1_leukemia_CAL2[ind.tr,])
  check_coef[(i+3),]<-c(unique(lv1_leukemia_CAL2[ind.tr,]$NAME),coef(model.quadratic.reg)[3])
  
}



### 3-1-4) LEVEL L1ab data --------------------------------------------------
lv1_leukemia_SEX_CAL2<-data[data$LEVEL=="L1ab",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LEUKEMIA", "LEUKEMIA")]

SEX_CAL2_level<-unique(lv1_leukemia_SEX_CAL2$NAME)

### 각 SEX, CAL2 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
for(i in 1:length(SEX_CAL2_level)){
  ind.tr<-which(lv1_leukemia_SEX_CAL2$NAME==SEX_CAL2_level[i])
  model.quadratic.reg<-lm(LEUKEMIA~poly(YEAR,2), data=lv1_leukemia_SEX_CAL2[ind.tr,])
  check_coef[(i+5),]<-c(unique(lv1_leukemia_SEX_CAL2[ind.tr,]$NAME),coef(model.quadratic.reg)[3])

}



# --------------------------------------------------------------------------------------------------------
### 3-1-5) LEVEL L2 data ----------------------------------------------------
lv2_leukemia_total<-data[data$LEVEL=="L2",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LEUKEMIA", "LEUKEMIA")]

### 각 NAME 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
name_level<-unique(lv2_leukemia_total$NAME)
index<-which(is.na(check_coef$NAME))[1]

for(i in 1:length(name_level)){
  ind.tr<-which(lv2_leukemia_total$NAME==name_level[i])
  model.quadratic.reg<-lm(LEUKEMIA~poly(YEAR,2), data=lv2_leukemia_total[ind.tr,])
  check_coef[(i+index-1),]<-c(name_level[i],coef(model.quadratic.reg)[3])

}



### 3-1-6) LEVEL L2a Data ---------------------------------------------------
lv2_leukemia_SEX<-data[data$LEVEL=="L2a",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LEUKEMIA", "LEUKEMIA")]

### 각 NAME 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
### 각 model 별 train set에 대한 적합값, test set에 대한 예측값 도출 --> 저장
name_level<-unique(lv2_leukemia_SEX$NAME)
index<-which(is.na(check_coef$NAME))[1]

for(i in 1:length(name_level)){
  ind.tr<-which(lv2_leukemia_SEX$NAME==name_level[i])
  model.quadratic.reg<-lm(LEUKEMIA~poly(YEAR,2), data=lv2_leukemia_SEX[ind.tr,])
  check_coef[(i+index-1),]<-c(name_level[i],coef(model.quadratic.reg)[3])

}



### 3-1-7) LEVEL L2b data ---------------------------------------------------
lv2_leukemia_CAL2<-data[data$LEVEL=="L2b",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LEUKEMIA", "LEUKEMIA")]

### 각 NAME 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
name_level<-unique(lv2_leukemia_CAL2$NAME)
index<-which(is.na(check_coef$NAME))[1]

for(i in 1:length(name_level)){
  
  ind.tr<-which(lv2_leukemia_CAL2$NAME==name_level[i])
  model.quadratic.reg<-lm(LEUKEMIA~poly(YEAR,2), data=lv2_leukemia_CAL2[ind.tr,])
  check_coef[(i+index-1),]<-c(name_level[i],coef(model.quadratic.reg)[3])
 
  
}



### 3-1-8) LEVEL L2ab data --------------------------------------------------
lv2_leukemia_SEX_CAL2<-data[data$LEVEL=="L2ab",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LEUKEMIA", "LEUKEMIA")]

### 각 NAME 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
name_level<-unique(lv2_leukemia_SEX_CAL2$NAME)
index<-which(is.na(check_coef$NAME))[1]

for(i in 1:length(name_level)){
  ind.tr<-which(lv2_leukemia_SEX_CAL2$NAME==name_level[i])
  model.quadratic.reg<-lm(LEUKEMIA~poly(YEAR,2), data=lv2_leukemia_SEX_CAL2[ind.tr,])
  check_coef[(i+index-1),]<-c(name_level[i],coef(model.quadratic.reg)[3])
 
}


# -------------------------------------------------------------------------------------------------------
### 3-1-9) LEVEL L3 data ----------------------------------------------------
lv3_leukemia_total<-data[data$LEVEL=="L3",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LEUKEMIA", "LEUKEMIA")]

### 각 NAME 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
name_level<-unique(lv3_leukemia_total$NAME)
index<-which(is.na(check_coef$NAME))[1]

for(i in 1:length(name_level)){

  ind.tr<-which(lv3_leukemia_total$YEAR<=2018 & lv3_leukemia_total$NAME==name_level[i])
  model.quadratic.reg<-lm(LEUKEMIA~poly(YEAR,2), data=lv3_leukemia_total[ind.tr,])
  check_coef[(i+index-1),]<-c(name_level[i],coef(model.quadratic.reg)[3])


}





### 3-1-10) LEVEL L3a Data --------------------------------------------------
lv3_leukemia_SEX<-data[data$LEVEL=="L3a",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LEUKEMIA", "LEUKEMIA")]


### 각 NAME 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
name_level<-unique(lv3_leukemia_SEX$NAME)
index<-which(is.na(check_coef$NAME))[1]

for(i in 1:length(name_level)){
  ind.tr<-which(lv3_leukemia_SEX$NAME==name_level[i])
  model.quadratic.reg<-lm(LEUKEMIA~poly(YEAR,2), data=lv3_leukemia_SEX[ind.tr,])
  check_coef[(i+index-1),]<-c(name_level[i],coef(model.quadratic.reg)[3])
  
}



### 3-1-11) LEVEL L3b Data --------------------------------------------------
lv3_leukemia_CAL2<-data[data$LEVEL=="L3b",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LEUKEMIA", "LEUKEMIA")]

### 각 NAME 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
### 각 model 별 train set에 대한 적합값, test set에 대한 예측값 도출 --> 저장
name_level<-unique(lv3_leukemia_CAL2$NAME)
index<-which(is.na(check_coef$NAME))[1]

for(i in 1:length(name_level)){
  ind.tr<-which(lv3_leukemia_CAL2$YEAR<=2018 & lv3_leukemia_CAL2$NAME==name_level[i])
  model.quadratic.reg<-lm(LEUKEMIA~poly(YEAR,2), data=lv3_leukemia_CAL2[ind.tr,])
  check_coef[(i+index-1),]<-c(name_level[i],coef(model.quadratic.reg)[3])
 
}



### 3-1-12) LEVEL L3ab Data -------------------------------------------------
lv3_leukemia_SEX_CAL2<-data[data$LEVEL=="L3ab",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LEUKEMIA", "LEUKEMIA")]


### 각 NAME 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
name_level<-unique(lv3_leukemia_SEX_CAL2$NAME)
index<-which(is.na(check_coef$NAME))[1]

for(i in 1:length(name_level)){
  ind.tr<-which(lv3_leukemia_SEX_CAL2$YEAR<=2018 & lv3_leukemia_SEX_CAL2$NAME==name_level[i])
  model.quadratic.reg<-lm(LEUKEMIA~poly(YEAR,2), data=lv3_leukemia_SEX_CAL2[ind.tr,])
  check_coef[(i+index-1),]<-c(name_level[i],coef(model.quadratic.reg)[3])
}





# -------------------------------------------------------------------------
### Leukemia level 별 model 적합 결과 coef이 음수인 NAME 확인 ---------------------------------------------
df_leukemia<-check_coef[check_coef$coef<0,]
df_leukemia$disease<-"LEUKEMIA"




### -------------------------------------------------------------------------
### 3-1. lung data ------------------------------------------------------
check_coef<-as.data.frame(matrix(NA,nrow=length(unique(data$NAME)),ncol=2))
colnames(check_coef)<-c("NAME","coef")

### 3-1-1) LEVEL L1 data ------------------------------------------------------
lv1_lung_total<-data[data$LEVEL=="L1",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LUNG", "LUNG")]

model.quadratic.reg<-lm(LUNG~poly(YEAR,2), data=lv1_lung_total)
check_coef[1,]<-c(unique(lv1_lung_total$NAME),coef(model.quadratic.reg)[3])


### 3-1-2) LEVEL L1a data --------------------------------------------------
lv1_lung_SEX<-data[data$LEVEL=="L1a",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LUNG", "LUNG")]

SEX_level<-unique(lv1_lung_SEX$SEX)


### SEX 마다 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
for(i in 1:length(SEX_level)){
  
  ind.tr<-which(lv1_lung_SEX$SEX==SEX_level[i])
  model.quadratic.reg<-lm(LUNG~poly(YEAR,2), data=lv1_lung_SEX[ind.tr,])
  check_coef[(i+1),]<-c(unique(lv1_lung_SEX[ind.tr,]$NAME),coef(model.quadratic.reg)[3])
  
}



### 3-1-3) LEVEL L1b data --------------------------------------------------
lv1_lung_CAL2<-data[data$LEVEL=="L1b",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LUNG", "LUNG")]
CAL2_level<-unique(lv1_lung_CAL2$CAL2)

### CAL2(입사시기) 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
for(i in 1:length(CAL2_level)){
  
  ind.tr<-which(lv1_lung_CAL2$CAL2==CAL2_level[i])
  model.quadratic.reg<-lm(LUNG~poly(YEAR,2), data=lv1_lung_CAL2[ind.tr,])
  check_coef[(i+3),]<-c(unique(lv1_lung_CAL2[ind.tr,]$NAME),coef(model.quadratic.reg)[3])
  
}



### 3-1-4) LEVEL L1ab data --------------------------------------------------
lv1_lung_SEX_CAL2<-data[data$LEVEL=="L1ab",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LUNG", "LUNG")]

SEX_CAL2_level<-unique(lv1_lung_SEX_CAL2$NAME)

### 각 SEX, CAL2 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
for(i in 1:length(SEX_CAL2_level)){
  ind.tr<-which(lv1_lung_SEX_CAL2$NAME==SEX_CAL2_level[i])
  model.quadratic.reg<-lm(LUNG~poly(YEAR,2), data=lv1_lung_SEX_CAL2[ind.tr,])
  check_coef[(i+5),]<-c(unique(lv1_lung_SEX_CAL2[ind.tr,]$NAME),coef(model.quadratic.reg)[3])
  
}



# --------------------------------------------------------------------------------------------------------
### 3-1-5) LEVEL L2 data ----------------------------------------------------
lv2_lung_total<-data[data$LEVEL=="L2",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LUNG", "LUNG")]

### 각 NAME 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
name_level<-unique(lv2_lung_total$NAME)
index<-which(is.na(check_coef$NAME))[1]

for(i in 1:length(name_level)){
  ind.tr<-which(lv2_lung_total$NAME==name_level[i])
  model.quadratic.reg<-lm(LUNG~poly(YEAR,2), data=lv2_lung_total[ind.tr,])
  check_coef[(i+index-1),]<-c(name_level[i],coef(model.quadratic.reg)[3])
  
}



### 3-1-6) LEVEL L2a Data ---------------------------------------------------
lv2_lung_SEX<-data[data$LEVEL=="L2a",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LUNG", "LUNG")]

### 각 NAME 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
### 각 model 별 train set에 대한 적합값, test set에 대한 예측값 도출 --> 저장
name_level<-unique(lv2_lung_SEX$NAME)
index<-which(is.na(check_coef$NAME))[1]

for(i in 1:length(name_level)){
  ind.tr<-which(lv2_lung_SEX$NAME==name_level[i])
  model.quadratic.reg<-lm(LUNG~poly(YEAR,2), data=lv2_lung_SEX[ind.tr,])
  check_coef[(i+index-1),]<-c(name_level[i],coef(model.quadratic.reg)[3])
  
}



### 3-1-7) LEVEL L2b data ---------------------------------------------------
lv2_lung_CAL2<-data[data$LEVEL=="L2b",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LUNG", "LUNG")]

### 각 NAME 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
name_level<-unique(lv2_lung_CAL2$NAME)
index<-which(is.na(check_coef$NAME))[1]

for(i in 1:length(name_level)){
  
  ind.tr<-which(lv2_lung_CAL2$NAME==name_level[i])
  model.quadratic.reg<-lm(LUNG~poly(YEAR,2), data=lv2_lung_CAL2[ind.tr,])
  check_coef[(i+index-1),]<-c(name_level[i],coef(model.quadratic.reg)[3])
  
  
}



### 3-1-8) LEVEL L2ab data --------------------------------------------------
lv2_lung_SEX_CAL2<-data[data$LEVEL=="L2ab",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LUNG", "LUNG")]

### 각 NAME 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
name_level<-unique(lv2_lung_SEX_CAL2$NAME)
index<-which(is.na(check_coef$NAME))[1]

for(i in 1:length(name_level)){
  ind.tr<-which(lv2_lung_SEX_CAL2$NAME==name_level[i])
  model.quadratic.reg<-lm(LUNG~poly(YEAR,2), data=lv2_lung_SEX_CAL2[ind.tr,])
  check_coef[(i+index-1),]<-c(name_level[i],coef(model.quadratic.reg)[3])
  
}


# -------------------------------------------------------------------------------------------------------
### 3-1-9) LEVEL L3 data ----------------------------------------------------
lv3_lung_total<-data[data$LEVEL=="L3",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LUNG", "LUNG")]

### 각 NAME 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
name_level<-unique(lv3_lung_total$NAME)
index<-which(is.na(check_coef$NAME))[1]

for(i in 1:length(name_level)){
  
  ind.tr<-which(lv3_lung_total$YEAR<=2018 & lv3_lung_total$NAME==name_level[i])
  model.quadratic.reg<-lm(LUNG~poly(YEAR,2), data=lv3_lung_total[ind.tr,])
  check_coef[(i+index-1),]<-c(name_level[i],coef(model.quadratic.reg)[3])
  
  
}





### 3-1-10) LEVEL L3a Data --------------------------------------------------
lv3_lung_SEX<-data[data$LEVEL=="L3a",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LUNG", "LUNG")]


### 각 NAME 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
name_level<-unique(lv3_lung_SEX$NAME)
index<-which(is.na(check_coef$NAME))[1]

for(i in 1:length(name_level)){
  ind.tr<-which(lv3_lung_SEX$NAME==name_level[i])
  model.quadratic.reg<-lm(LUNG~poly(YEAR,2), data=lv3_lung_SEX[ind.tr,])
  check_coef[(i+index-1),]<-c(name_level[i],coef(model.quadratic.reg)[3])
  
}



### 3-1-11) LEVEL L3b Data --------------------------------------------------
lv3_lung_CAL2<-data[data$LEVEL=="L3b",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LUNG", "LUNG")]

### 각 NAME 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
### 각 model 별 train set에 대한 적합값, test set에 대한 예측값 도출 --> 저장
name_level<-unique(lv3_lung_CAL2$NAME)
index<-which(is.na(check_coef$NAME))[1]

for(i in 1:length(name_level)){
  ind.tr<-which(lv3_lung_CAL2$YEAR<=2018 & lv3_lung_CAL2$NAME==name_level[i])
  model.quadratic.reg<-lm(LUNG~poly(YEAR,2), data=lv3_lung_CAL2[ind.tr,])
  check_coef[(i+index-1),]<-c(name_level[i],coef(model.quadratic.reg)[3])
  
}



### 3-1-12) LEVEL L3ab Data -------------------------------------------------
lv3_lung_SEX_CAL2<-data[data$LEVEL=="L3ab",c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR", "FY_LUNG", "LUNG")]


### 각 NAME 별 누적발병건수, 추적인년합계를 반응변수로 하여 modeling -----
name_level<-unique(lv3_lung_SEX_CAL2$NAME)
index<-which(is.na(check_coef$NAME))[1]

for(i in 1:length(name_level)){
  ind.tr<-which(lv3_lung_SEX_CAL2$YEAR<=2018 & lv3_lung_SEX_CAL2$NAME==name_level[i])
  model.quadratic.reg<-lm(LUNG~poly(YEAR,2), data=lv3_lung_SEX_CAL2[ind.tr,])
  check_coef[(i+index-1),]<-c(name_level[i],coef(model.quadratic.reg)[3])
}





# -------------------------------------------------------------------------
### lung level 별 model 적합 결과 coef이 음수인 NAME 확인 ---------------------------------------------
df_lung<-check_coef[check_coef$coef<0,]
df_lung$disease<-"LUNG"

check_total<-as.data.frame(rbind(df_leukemia,df_lung))

### csv file로 저장------------------------------------------------------
write.csv(check_total,file='check_coef_quadratic.csv',row.names=FALSE)

