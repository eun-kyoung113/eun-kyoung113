library(tidyverse)
library(data.table)
setDTthreads(24)
library(stringr)
library(splines)

dt_leukemia<-fread("/shared/data/koshri/ref_leukemia_bz_v4.csv")
dt_lung<-fread("/shared/data/koshri/ref_lung_bz_v4.csv")

library(dtplyr)
dt_lz_leukemia<-lazy_dt(dt_leukemia)
dt_lz_lung<-lazy_dt(dt_lung)

## pre-processing
dt_lz_leukemia$parent$SEX = ifelse(dt_lz_leukemia$parent$SEX=="남", "M", "F")
df_leukemia1<-dt_lz_leukemia %>% mutate(CAL2=ifelse( (CAL<=1), "01", "234"))%>%as_tibble()
`%!in%` <- Negate(`%in%`)
df_leukemia1 = df_leukemia1%>%filter(UP1 %!in% c("", "T", "U","NA"))%>%as_tibble()


dt_lz_lung$parent$SEX = ifelse(dt_lz_lung$parent$SEX=="남", "M", "F")
df_lung1<-dt_lz_lung %>% mutate(CAL2=ifelse( (CAL<=1), "01", "234"))%>%filter(UP1 %!in% c("", "T", "U","NA"))%>%as_tibble()


######################### Leukemia data ################################
############################# level 1 #############################
########### 1. total ###########
lv1_leukemia_total<-df_leukemia1%>%group_by(YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  mutate(NAME="X")%>%as_tibble()

new_dt<-data.frame(c(2019:2022),rep(NA,4),rep(NA,4),rep(unique(lv1_leukemia_total$NAME)))
colnames(new_dt)<-colnames(lv1_leukemia_total)
lv1_leukemia_total<-as.data.frame(rbind(lv1_leukemia_total,new_dt))

lv1_leukemia_total$simple.reg<-0
lv1_leukemia_total$poisson.reg<-0
lv1_leukemia_total$linear.spline<-0
lv1_leukemia_total$quadratic.reg<-0

lv1_leukemia_total$simple.reg.FY<-0
lv1_leukemia_total$poisson.reg.FY<-0
lv1_leukemia_total$linear.spline.FY<-0
lv1_leukemia_total$quadratic.reg.FY<-0

ind.tr<-which(lv1_leukemia_total$YEAR<=2018)
ind.val<-which(lv1_leukemia_total$YEAR>2018)

model.simple.reg<-lm(sum_LEUKEMIA_BZ~YEAR, data=lv1_leukemia_total[ind.tr,])
lv1_leukemia_total$simple.reg[ind.tr]<-ifelse(model.simple.reg$fitted.values<0, 0, model.simple.reg$fitted.values)
lv1_leukemia_total$simple.reg[ind.val]<-ifelse(predict(model.simple.reg,newdata=lv1_leukemia_total[ind.val,])<0,0,
                                               predict(model.simple.reg,newdata=lv1_leukemia_total[ind.val,]))


model.simple.reg.FY<-lm(sum_FY_BZ~YEAR, data=lv1_leukemia_total[ind.tr,])
lv1_leukemia_total$simple.reg.FY[ind.tr]<-ifelse(model.simple.reg.FY$fitted.values<0,0,model.simple.reg.FY$fitted.values)
lv1_leukemia_total$simple.reg.FY[ind.val]<-ifelse(predict(model.simple.reg.FY,newdata=lv1_leukemia_total[ind.val,])<0,0,
                                                  predict(model.simple.reg.FY,newdata=lv1_leukemia_total[ind.val,]))

model.poisson.reg<-glm(sum_LEUKEMIA_BZ~YEAR, data=lv1_leukemia_total[ind.tr,],family='poisson')
lv1_leukemia_total$poisson.reg[ind.tr]<-ifelse(model.poisson.reg$fitted.values<0,0,model.poisson.reg$fitted.values)
lv1_leukemia_total$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv1_leukemia_total[ind.val,]))

model.poisson.reg.FY<-glm(sum_FY_BZ~YEAR, data=lv1_leukemia_total[ind.tr,],family='poisson')
lv1_leukemia_total$poisson.reg.FY[ind.tr]<-ifelse(model.poisson.reg.FY$fitted.values<0,0,model.poisson.reg.FY$fitted.values)
lv1_leukemia_total$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv1_leukemia_total[ind.val,]))


model.quadratic.reg<-lm(sum_LEUKEMIA_BZ~poly(YEAR,2), data=lv1_leukemia_total[ind.tr,])
lv1_leukemia_total$quadratic.reg[ind.tr]<-ifelse(model.quadratic.reg$fitted.values<0, 0, model.quadratic.reg$fitted.values)
lv1_leukemia_total$quadratic.reg[ind.val]<-ifelse(predict(model.quadratic.reg,newdata=lv1_leukemia_total[ind.val,])<0,0,
                                                  predict(model.quadratic.reg,newdata=lv1_leukemia_total[ind.val,]))

model.quadratic.reg.FY<-lm(sum_FY_BZ~poly(YEAR,2), data=lv1_leukemia_total[ind.tr,])
lv1_leukemia_total$quadratic.reg.FY[ind.tr]<-ifelse(model.quadratic.reg.FY$fitted.values<0,0,model.quadratic.reg.FY$fitted.values)
lv1_leukemia_total$quadratic.reg.FY[ind.val]<-ifelse(predict(model.quadratic.reg.FY,newdata=lv1_leukemia_total[ind.val,])<0,0,
                                                     predict(model.quadratic.reg.FY,newdata=lv1_leukemia_total[ind.val,]))


model.linear.spline<-lm(sum_LEUKEMIA_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_leukemia_total[ind.tr,])
lv1_leukemia_total$linear.spline[ind.tr]<-ifelse(model.linear.spline$fitted.values<0,0,model.linear.spline$fitted.values)
lv1_leukemia_total$linear.spline[ind.val]<-ifelse(predict(model.linear.spline,newdata=lv1_leukemia_total[ind.val,])<0,0,
                                                  predict(model.linear.spline,newdata=lv1_leukemia_total[ind.val,]))

model.linear.spline.FY<-lm(sum_FY_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_leukemia_total[ind.tr,])
lv1_leukemia_total$linear.spline.FY[ind.tr]<-ifelse(model.linear.spline.FY$fitted.values<0,0,model.linear.spline.FY$fitted.values)
lv1_leukemia_total$linear.spline.FY[ind.val]<-ifelse(predict(model.linear.spline.FY,newdata=lv1_leukemia_total[ind.val,])<0,0,
                                                     predict(model.linear.spline.FY,newdata=lv1_leukemia_total[ind.val,]))




####################################################################################
#################### 2. grouping by SEX ##################
lv1_leukemia_SEX<-df_leukemia1%>%group_by(SEX,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  mutate(NAME=paste0("X","_",SEX))%>%as_tibble()

SEX_level<-unique(lv1_leukemia_SEX$SEX)
year<-c(2019:2022)
new_dt<-data.frame(rep(unique(SEX_level),each=length(year)),rep(year,length(unique(SEX_level))),rep(NA,length(year)*length(unique(SEX_level))),
                   rep(NA,length(year)*length(unique(SEX_level))),rep(unique(lv1_leukemia_SEX$NAME),each=length(year)))

colnames(new_dt)<-colnames(lv1_leukemia_SEX)
lv1_leukemia_SEX<-as.data.frame(rbind(lv1_leukemia_SEX,new_dt))

lv1_leukemia_SEX$simple.reg<-0
lv1_leukemia_SEX$poisson.reg<-0
lv1_leukemia_SEX$linear.spline<-0
lv1_leukemia_SEX$quadratic.reg<-0

lv1_leukemia_SEX$simple.reg.FY<-0
lv1_leukemia_SEX$poisson.reg.FY<-0
lv1_leukemia_SEX$linear.spline.FY<-0
lv1_leukemia_SEX$quadratic.reg.FY<-0

for(i in 1:length(SEX_level)){
  ind.tr<-which(lv1_leukemia_SEX$YEAR<=2018 & lv1_leukemia_SEX$SEX==SEX_level[i])
  ind.val<-which(lv1_leukemia_SEX$YEAR>2018 & lv1_leukemia_SEX$SEX==SEX_level[i])
  
  model.simple.reg<-lm(sum_LEUKEMIA_BZ~YEAR, data=lv1_leukemia_SEX[ind.tr,])
  lv1_leukemia_SEX$simple.reg[ind.tr]<-ifelse(model.simple.reg$fitted.values<0, 0, model.simple.reg$fitted.values)
  lv1_leukemia_SEX$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv1_leukemia_SEX[ind.val,])
  
  model.simple.reg.FY<-lm(sum_FY_BZ~YEAR, data=lv1_leukemia_SEX[ind.tr,])
  lv1_leukemia_SEX$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv1_leukemia_SEX$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv1_leukemia_SEX[ind.val,])
  
  model.poisson.reg<-glm(sum_LEUKEMIA_BZ~YEAR, data=lv1_leukemia_SEX[ind.tr,],family='poisson')
  lv1_leukemia_SEX$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv1_leukemia_SEX$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv1_leukemia_SEX[ind.val,]))
  
  model.poisson.reg.FY<-glm(sum_FY_BZ~YEAR, data=lv1_leukemia_SEX[ind.tr,],family='poisson')
  lv1_leukemia_SEX$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv1_leukemia_SEX$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv1_leukemia_SEX[ind.val,]))
  
  
  model.quadratic.reg<-lm(sum_LEUKEMIA_BZ~poly(YEAR,2), data=lv1_leukemia_SEX[ind.tr,])
  lv1_leukemia_SEX$quadratic.reg[ind.tr]<-ifelse(model.quadratic.reg$fitted.values<0, 0, model.quadratic.reg$fitted.values)
  lv1_leukemia_SEX$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv1_leukemia_SEX[ind.val,])
  
  model.quadratic.reg.FY<-lm(sum_FY_BZ~poly(YEAR,2), data=lv1_leukemia_SEX[ind.tr,])
  lv1_leukemia_SEX$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv1_leukemia_SEX$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv1_leukemia_SEX[ind.val,])
  
  model.linear.spline<-lm(sum_LEUKEMIA_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_leukemia_SEX[ind.tr,])
  lv1_leukemia_SEX$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv1_leukemia_SEX$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv1_leukemia_SEX[ind.val,])
  
  model.linear.spline.FY<-lm(sum_FY_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_leukemia_SEX[ind.tr,])
  lv1_leukemia_SEX$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv1_leukemia_SEX$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv1_leukemia_SEX[ind.val,])
}




################################################################################################
############### 3. grouping by CAL2 ######################### 
lv1_leukemia_CAL2<-df_leukemia1%>%group_by(CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  mutate(NAME=paste0("X","_",CAL2))%>%as_tibble()

CAL2_level<-unique(lv1_leukemia_CAL2$CAL2)

year<-c(2019:2022)
new_dt<-data.frame(rep(unique(CAL2_level),each=length(year)),rep(year,length(unique(CAL2_level))),rep(NA,length(year)*length(unique(CAL2_level))),
                   rep(NA,length(year)*length(unique(CAL2_level))),rep(unique(lv1_leukemia_CAL2$NAME),each=length(year)))

colnames(new_dt)<-colnames(lv1_leukemia_CAL2)
lv1_leukemia_CAL2<-as.data.frame(rbind(lv1_leukemia_CAL2,new_dt))

lv1_leukemia_CAL2$simple.reg<-0
lv1_leukemia_CAL2$poisson.reg<-0
lv1_leukemia_CAL2$linear.spline<-0
lv1_leukemia_CAL2$quadratic.reg<-0

lv1_leukemia_CAL2$simple.reg.FY<-0
lv1_leukemia_CAL2$poisson.reg.FY<-0
lv1_leukemia_CAL2$linear.spline.FY<-0
lv1_leukemia_CAL2$quadratic.reg.FY<-0

CAL2_level<-unique(lv1_leukemia_CAL2$CAL2)

for(i in 1:length(CAL2_level)){
  ind.tr<-which(lv1_leukemia_CAL2$YEAR<=2018 & lv1_leukemia_CAL2$CAL2==CAL2_level[i])
  ind.val<-which(lv1_leukemia_CAL2$YEAR>2018 & lv1_leukemia_CAL2$CAL2==CAL2_level[i])
  
  model.poisson.reg<-glm(sum_LEUKEMIA_BZ~YEAR, data=lv1_leukemia_CAL2[ind.tr,],family='poisson')
  lv1_leukemia_CAL2$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv1_leukemia_CAL2$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv1_leukemia_CAL2[ind.val,]))
  
  model.poisson.reg.FY<-glm(sum_FY_BZ~YEAR, data=lv1_leukemia_CAL2[ind.tr,],family='poisson')
  lv1_leukemia_CAL2$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv1_leukemia_CAL2$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv1_leukemia_CAL2[ind.val,]))
  
  model.simple.reg<-lm(sum_LEUKEMIA_BZ~YEAR, data=lv1_leukemia_CAL2[ind.tr,])
  lv1_leukemia_CAL2$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv1_leukemia_CAL2$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv1_leukemia_CAL2[ind.val,])
  
  model.simple.reg.FY<-lm(sum_FY_BZ~YEAR, data=lv1_leukemia_CAL2[ind.tr,])
  lv1_leukemia_CAL2$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv1_leukemia_CAL2$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv1_leukemia_CAL2[ind.val,])
  
  model.quadratic.reg<-lm(sum_LEUKEMIA_BZ~poly(YEAR,2), data=lv1_leukemia_CAL2[ind.tr,])
  lv1_leukemia_CAL2$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv1_leukemia_CAL2$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv1_leukemia_CAL2[ind.val,])
  
  model.quadratic.reg.FY<-lm(sum_FY_BZ~poly(YEAR,2), data=lv1_leukemia_CAL2[ind.tr,])
  lv1_leukemia_CAL2$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv1_leukemia_CAL2$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv1_leukemia_CAL2[ind.val,])
  
  model.linear.spline<-lm(sum_LEUKEMIA_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_leukemia_CAL2[ind.tr,])
  lv1_leukemia_CAL2$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv1_leukemia_CAL2$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv1_leukemia_CAL2[ind.val,])
  
  model.linear.spline.FY<-lm(sum_FY_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_leukemia_CAL2[ind.tr,])
  lv1_leukemia_CAL2$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv1_leukemia_CAL2$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv1_leukemia_CAL2[ind.val,])
}


################################################################################################
############### 4. grouping by SEX and CAL2 ######################### 
lv1_leukemia_SEX_CAL2<-df_leukemia1%>%group_by(SEX,CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  mutate(NAME=paste0("X","_",SEX,CAL2))%>%as_tibble()

year<-c(2019:2022)

SEX_CAL2_level<-unique(lv1_leukemia_SEX_CAL2$NAME)

new_dt<-data.frame(rep(unique(lv1_leukemia_SEX_CAL2$SEX),each=(length(year)*2)),rep(unique(lv1_leukemia_SEX_CAL2$CAL2),each=(length(year))),
                      rep(year,length(SEX_CAL2_level)),rep(NA,(length(year)*4)),rep(NA,(length(year)*4)),rep(SEX_CAL2_level,each=length(year)))

colnames(new_dt)<-colnames(lv1_leukemia_SEX_CAL2)
lv1_leukemia_SEX_CAL2<-as.data.frame(rbind(lv1_leukemia_SEX_CAL2,new_dt))

lv1_leukemia_SEX_CAL2$simple.reg<-0
lv1_leukemia_SEX_CAL2$poisson.reg<-0
lv1_leukemia_SEX_CAL2$linear.spline<-0
lv1_leukemia_SEX_CAL2$quadratic.reg<-0

lv1_leukemia_SEX_CAL2$simple.reg.FY<-0
lv1_leukemia_SEX_CAL2$poisson.reg.FY<-0
lv1_leukemia_SEX_CAL2$linear.spline.FY<-0
lv1_leukemia_SEX_CAL2$quadratic.reg.FY<-0

for(i in 1:length(SEX_CAL2_level)){
  ind.tr<-which(lv1_leukemia_SEX_CAL2$YEAR<=2018 & lv1_leukemia_SEX_CAL2$NAME==SEX_CAL2_level[i])
  ind.val<-which(lv1_leukemia_SEX_CAL2$YEAR>2018 & lv1_leukemia_SEX_CAL2$NAME==SEX_CAL2_level[i])
  
  model.simple.reg<-lm(sum_LEUKEMIA_BZ~YEAR, data=lv1_leukemia_SEX_CAL2[ind.tr,])
  lv1_leukemia_SEX_CAL2$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv1_leukemia_SEX_CAL2$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv1_leukemia_SEX_CAL2[ind.val,])
  
  model.simple.reg.FY<-lm(sum_FY_BZ~YEAR, data=lv1_leukemia_SEX_CAL2[ind.tr,])
  lv1_leukemia_SEX_CAL2$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv1_leukemia_SEX_CAL2$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv1_leukemia_SEX_CAL2[ind.val,])
  
  model.poisson.reg<-glm(sum_LEUKEMIA_BZ~YEAR, data=lv1_leukemia_SEX_CAL2[ind.tr,],family='poisson')
  lv1_leukemia_SEX_CAL2$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv1_leukemia_SEX_CAL2$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv1_leukemia_SEX_CAL2[ind.val,]))
  
  model.poisson.reg.FY<-glm(sum_FY_BZ~YEAR, data=lv1_leukemia_SEX_CAL2[ind.tr,],family='poisson')
  lv1_leukemia_SEX_CAL2$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv1_leukemia_SEX_CAL2$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv1_leukemia_SEX_CAL2[ind.val,]))
  
  model.quadratic.reg<-lm(sum_LEUKEMIA_BZ~poly(YEAR,2), data=lv1_leukemia_SEX_CAL2[ind.tr,])
  lv1_leukemia_SEX_CAL2$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv1_leukemia_SEX_CAL2$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv1_leukemia_SEX_CAL2[ind.val,])
  
  model.quadratic.reg.FY<-lm(sum_FY_BZ~poly(YEAR,2), data=lv1_leukemia_SEX_CAL2[ind.tr,])
  lv1_leukemia_SEX_CAL2$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv1_leukemia_SEX_CAL2$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv1_leukemia_SEX_CAL2[ind.val,])
  
  model.linear.spline<-lm(sum_LEUKEMIA_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_leukemia_SEX_CAL2[ind.tr,])
  lv1_leukemia_SEX_CAL2$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv1_leukemia_SEX_CAL2$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv1_leukemia_SEX_CAL2[ind.val,])
  
  model.linear.spline.FY<-lm(sum_FY_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_leukemia_SEX_CAL2[ind.tr,])
  lv1_leukemia_SEX_CAL2$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv1_leukemia_SEX_CAL2$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv1_leukemia_SEX_CAL2[ind.val,])
}

lv1_leukemia_test<-as.data.frame(rbind(lv1_leukemia_total,lv1_leukemia_SEX[2:length(lv1_leukemia_SEX)],lv1_leukemia_CAL2[2:length(lv1_leukemia_CAL2)],
                                       lv1_leukemia_SEX_CAL2[3:length(lv1_leukemia_SEX_CAL2)]))






##################### level 2 data ##########################
############# 1. grouping by UP1 #################
lv2_leukemia_total<-df_leukemia1%>%group_by(UP1,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  mutate(NAME=paste0("X",UP1))%>%as_tibble()

year<-c(2019:2022)
new_dt<-data.frame(rep(unique(lv2_leukemia_total$UP1),each=length(year)),rep(year,length(unique(lv2_leukemia_total$UP1))),
                   rep(NA,length(year)*length(unique(lv2_leukemia_total$UP1))),rep(NA,length(year)*length(unique(lv2_leukemia_total$UP1))),
                   rep(unique(lv2_leukemia_total$NAME),each=length(year)))

colnames(new_dt)<-colnames(lv2_leukemia_total)
lv2_leukemia_total<-as.data.frame(rbind(lv2_leukemia_total,new_dt))

lv2_leukemia_total$simple.reg<-0
lv2_leukemia_total$poisson.reg<-0
lv2_leukemia_total$linear.spline<-0
lv2_leukemia_total$quadratic.reg<-0

lv2_leukemia_total$simple.reg.FY<-0
lv2_leukemia_total$poisson.reg.FY<-0
lv2_leukemia_total$linear.spline.FY<-0
lv2_leukemia_total$quadratic.reg.FY<-0

name_level<-unique(lv2_leukemia_total$NAME)

for(i in 1:length(name_level)){
  ind.tr<-which(lv2_leukemia_total$YEAR<=2018 & lv2_leukemia_total$NAME==name_level[i])
  ind.val<-which(lv2_leukemia_total$YEAR>2018 & lv2_leukemia_total$NAME==name_level[i])
  
  model.simple.reg<-lm(sum_LEUKEMIA_BZ~YEAR, data=lv2_leukemia_total[ind.tr,])
  lv2_leukemia_total$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv2_leukemia_total$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv2_leukemia_total[ind.val,])
  
  model.simple.reg.FY<-lm(sum_FY_BZ~YEAR, data=lv2_leukemia_total[ind.tr,])
  lv2_leukemia_total$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv2_leukemia_total$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv2_leukemia_total[ind.val,])
  
  model.poisson.reg<-glm(sum_LEUKEMIA_BZ~YEAR, data=lv2_leukemia_total[ind.tr,],family='poisson')
  lv2_leukemia_total$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv2_leukemia_total$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv2_leukemia_total[ind.val,]))
  
  model.poisson.reg.FY<-glm(sum_FY_BZ~YEAR, data=lv2_leukemia_total[ind.tr,],family='poisson')
  lv2_leukemia_total$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv2_leukemia_total$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv2_leukemia_total[ind.val,]))
  
  model.quadratic.reg<-lm(sum_LEUKEMIA_BZ~poly(YEAR,2), data=lv2_leukemia_total[ind.tr,])
  lv2_leukemia_total$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv2_leukemia_total$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv2_leukemia_total[ind.val,])
  
  model.quadratic.reg.FY<-lm(sum_FY_BZ~poly(YEAR,2), data=lv2_leukemia_total[ind.tr,])
  lv2_leukemia_total$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv2_leukemia_total$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv2_leukemia_total[ind.val,])
  
  model.linear.spline<-lm(sum_LEUKEMIA_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_leukemia_total[ind.tr,])
  lv2_leukemia_total$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv2_leukemia_total$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv2_leukemia_total[ind.val,])
  
  model.linear.spline.FY<-lm(sum_FY_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_leukemia_total[ind.tr,])
  lv2_leukemia_total$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv2_leukemia_total$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv2_leukemia_total[ind.val,])
}







##################################################################################
############# 2. grouping by UP1 and SEX #################
lv2_leukemia_SEX<-df_leukemia1%>%group_by(UP1,SEX,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  mutate(NAME=paste0("X",UP1,"_",SEX))%>%as_tibble()

new_dt<-data.frame(rep(unique(lv2_leukemia_SEX$UP1),each=length(year)*length(unique(lv2_leukemia_SEX$SEX))),
                   rep(unique(lv2_leukemia_SEX$SEX),each=length(year)),
                   rep(year,length(unique(lv2_leukemia_SEX$NAME))),
                   rep(NA,length(unique(lv2_leukemia_SEX$NAME))*length(year)),
                   rep(NA,length(unique(lv2_leukemia_SEX$NAME))*length(year)),
                   rep(unique(lv2_leukemia_SEX$NAME),each=length(year)))

colnames(new_dt)<-colnames(lv2_leukemia_SEX)
lv2_leukemia_SEX<-as.data.frame(rbind(lv2_leukemia_SEX,new_dt))

lv2_leukemia_SEX$simple.reg<-0
lv2_leukemia_SEX$poisson.reg<-0
lv2_leukemia_SEX$linear.spline<-0
lv2_leukemia_SEX$quadratic.reg<-0

lv2_leukemia_SEX$simple.reg.FY<-0
lv2_leukemia_SEX$poisson.reg.FY<-0
lv2_leukemia_SEX$linear.spline.FY<-0
lv2_leukemia_SEX$quadratic.reg.FY<-0

name_level<-unique(lv2_leukemia_SEX$NAME)

for(i in 1:length(name_level)){
  ind.tr<-which(lv2_leukemia_SEX$YEAR<=2018 & lv2_leukemia_SEX$NAME==name_level[i])
  ind.val<-which(lv2_leukemia_SEX$YEAR>2018 & lv2_leukemia_SEX$NAME==name_level[i])
  
  model.simple.reg<-lm(sum_LEUKEMIA_BZ~YEAR, data=lv2_leukemia_SEX[ind.tr,])
  lv2_leukemia_SEX$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv2_leukemia_SEX$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv2_leukemia_SEX[ind.val,])
  
  model.simple.reg.FY<-lm(sum_FY_BZ~YEAR, data=lv2_leukemia_SEX[ind.tr,])
  lv2_leukemia_SEX$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv2_leukemia_SEX$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv2_leukemia_SEX[ind.val,])
  
  model.poisson.reg<-glm(sum_LEUKEMIA_BZ~YEAR, data=lv2_leukemia_SEX[ind.tr,],family='poisson')
  lv2_leukemia_SEX$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv2_leukemia_SEX$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv2_leukemia_SEX[ind.val,]))
  
  model.poisson.reg.FY<-glm(sum_FY_BZ~YEAR, data=lv2_leukemia_SEX[ind.tr,],family='poisson')
  lv2_leukemia_SEX$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv2_leukemia_SEX$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv2_leukemia_SEX[ind.val,]))
  
  model.quadratic.reg<-lm(sum_LEUKEMIA_BZ~poly(YEAR,2), data=lv2_leukemia_SEX[ind.tr,])
  lv2_leukemia_SEX$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv2_leukemia_SEX$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv2_leukemia_SEX[ind.val,])
  
  model.quadratic.reg.FY<-lm(sum_FY_BZ~poly(YEAR,2), data=lv2_leukemia_SEX[ind.tr,])
  lv2_leukemia_SEX$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv2_leukemia_SEX$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv2_leukemia_SEX[ind.val,])
  
  model.linear.spline<-lm(sum_LEUKEMIA_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_leukemia_SEX[ind.tr,])
  lv2_leukemia_SEX$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv2_leukemia_SEX$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv2_leukemia_SEX[ind.val,])
  
  model.linear.spline.FY<-lm(sum_FY_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_leukemia_SEX[ind.tr,])
  lv2_leukemia_SEX$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv2_leukemia_SEX$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv2_leukemia_SEX[ind.val,])
}




################################################################################################
############### 3. grouping by UP1 and CAL2 ######################### 
lv2_leukemia_CAL2<-df_leukemia1%>%group_by(UP1,CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  mutate(NAME=paste0("X",UP1,"_",CAL2))%>%as_tibble()

name_level<-unique(lv2_leukemia_CAL2$NAME)

new_dt<-data.frame(rep(unique(lv2_leukemia_CAL2$UP1),each=length(year)*length(unique(lv2_leukemia_CAL2$CAL2))),
                   rep(unique(lv2_leukemia_CAL2$CAL2),each=length(year)),
                   rep(year,length(unique(lv2_leukemia_CAL2$NAME))),
                   rep(NA,length(unique(lv2_leukemia_CAL2$NAME))*length(year)),
                   rep(NA,length(unique(lv2_leukemia_CAL2$NAME))*length(year)),
                   rep(unique(lv2_leukemia_CAL2$NAME),each=length(year)))

colnames(new_dt)<-colnames(lv2_leukemia_CAL2)
lv2_leukemia_CAL2<-as.data.frame(rbind(lv2_leukemia_CAL2,new_dt))

lv2_leukemia_CAL2$simple.reg<-0
lv2_leukemia_CAL2$poisson.reg<-0
lv2_leukemia_CAL2$linear.spline<-0
lv2_leukemia_CAL2$quadratic.reg<-0

lv2_leukemia_CAL2$simple.reg.FY<-0
lv2_leukemia_CAL2$poisson.reg.FY<-0
lv2_leukemia_CAL2$linear.spline.FY<-0
lv2_leukemia_CAL2$quadratic.reg.FY<-0

for(i in 1:length(name_level)){
  ind.tr<-which(lv2_leukemia_CAL2$YEAR<=2018 & lv2_leukemia_CAL2$NAME==name_level[i])
  ind.val<-which(lv2_leukemia_CAL2$YEAR>2018 & lv2_leukemia_CAL2$NAME==name_level[i])
  
  model.simple.reg<-lm(sum_LEUKEMIA_BZ~YEAR, data=lv2_leukemia_CAL2[ind.tr,])
  lv2_leukemia_CAL2$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv2_leukemia_CAL2$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv2_leukemia_CAL2[ind.val,])
  
  model.simple.reg.FY<-lm(sum_FY_BZ~YEAR, data=lv2_leukemia_CAL2[ind.tr,])
  lv2_leukemia_CAL2$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv2_leukemia_CAL2$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv2_leukemia_CAL2[ind.val,])
  
  model.poisson.reg<-glm(sum_LEUKEMIA_BZ~YEAR, data=lv2_leukemia_CAL2[ind.tr,],family='poisson')
  lv2_leukemia_CAL2$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv2_leukemia_CAL2$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv2_leukemia_CAL2[ind.val,]))
  
  model.poisson.reg.FY<-glm(sum_FY_BZ~YEAR, data=lv2_leukemia_CAL2[ind.tr,],family='poisson')
  lv2_leukemia_CAL2$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv2_leukemia_CAL2$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv2_leukemia_CAL2[ind.val,]))
  
  model.quadratic.reg<-lm(sum_LEUKEMIA_BZ~poly(YEAR,2), data=lv2_leukemia_CAL2[ind.tr,])
  lv2_leukemia_CAL2$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv2_leukemia_CAL2$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv2_leukemia_CAL2[ind.val,])
  
  model.quadratic.reg.FY<-lm(sum_FY_BZ~poly(YEAR,2), data=lv2_leukemia_CAL2[ind.tr,])
  lv2_leukemia_CAL2$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv2_leukemia_CAL2$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv2_leukemia_CAL2[ind.val,])
  
  model.linear.spline<-lm(sum_LEUKEMIA_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_leukemia_CAL2[ind.tr,])
  lv2_leukemia_CAL2$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv2_leukemia_CAL2$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv2_leukemia_CAL2[ind.val,])
  
  model.linear.spline.FY<-lm(sum_FY_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_leukemia_CAL2[ind.tr,])
  lv2_leukemia_CAL2$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv2_leukemia_CAL2$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv2_leukemia_CAL2[ind.val,])
  
}





################################################################################################
############### 3. grouping by UP1 and SEX and CAL2 ######################### 
lv2_leukemia_SEX_CAL2<-df_leukemia1%>%group_by(UP1,SEX,CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  mutate(NAME=paste0("X",UP1,"_",SEX,CAL2))%>%as_tibble()

name_level<-unique(lv2_leukemia_SEX_CAL2$NAME)

new_dt<-data.frame(rep(unique(lv2_leukemia_SEX_CAL2$UP1),each=length(year)*length(unique(lv2_leukemia_SEX_CAL2$SEX))*length(unique(lv2_leukemia_SEX_CAL2$CAL2))),
                   rep(unique(lv2_leukemia_SEX_CAL2$SEX),each=length(year)*length(unique(lv2_leukemia_SEX_CAL2$CAL2))),
                   rep(unique(lv2_leukemia_SEX_CAL2$CAL2),each=length(year)*length(unique(lv2_leukemia_SEX_CAL2$SEX))),
                   rep(year,length(unique(lv2_leukemia_SEX_CAL2$NAME))),
                   rep(NA,length(unique(lv2_leukemia_SEX_CAL2$NAME))*length(year)),
                   rep(NA,length(unique(lv2_leukemia_SEX_CAL2$NAME))*length(year)),
                   rep(unique(lv2_leukemia_SEX_CAL2$NAME),each=length(year)))

colnames(new_dt)<-colnames(lv2_leukemia_SEX_CAL2)
lv2_leukemia_SEX_CAL2<-as.data.frame(rbind(lv2_leukemia_SEX_CAL2,new_dt))

lv2_leukemia_SEX_CAL2$simple.reg<-0
lv2_leukemia_SEX_CAL2$poisson.reg<-0
lv2_leukemia_SEX_CAL2$linear.spline<-0
lv2_leukemia_SEX_CAL2$quadratic.reg<-0

lv2_leukemia_SEX_CAL2$simple.reg.FY<-0
lv2_leukemia_SEX_CAL2$poisson.reg.FY<-0
lv2_leukemia_SEX_CAL2$linear.spline.FY<-0
lv2_leukemia_SEX_CAL2$quadratic.reg.FY<-0

for(i in 1:length(name_level)){
  ind.tr<-which(lv2_leukemia_SEX_CAL2$YEAR<=2018 & lv2_leukemia_SEX_CAL2$NAME==name_level[i])
  ind.val<-which(lv2_leukemia_SEX_CAL2$YEAR>2018 & lv2_leukemia_SEX_CAL2$NAME==name_level[i])
  
  model.simple.reg<-lm(sum_LEUKEMIA_BZ~YEAR, data=lv2_leukemia_SEX_CAL2[ind.tr,])
  lv2_leukemia_SEX_CAL2$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv2_leukemia_SEX_CAL2$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv2_leukemia_SEX_CAL2[ind.val,])
  
  model.simple.reg.FY<-lm(sum_FY_BZ~YEAR, data=lv2_leukemia_SEX_CAL2[ind.tr,])
  lv2_leukemia_SEX_CAL2$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv2_leukemia_SEX_CAL2$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv2_leukemia_SEX_CAL2[ind.val,])
  
  model.poisson.reg<-glm(sum_LEUKEMIA_BZ~YEAR, data=lv2_leukemia_SEX_CAL2[ind.tr,],family='poisson')
  lv2_leukemia_SEX_CAL2$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv2_leukemia_SEX_CAL2$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv2_leukemia_SEX_CAL2[ind.val,]))
  
  model.poisson.reg.FY<-glm(sum_FY_BZ~YEAR, data=lv2_leukemia_SEX_CAL2[ind.tr,],family='poisson')
  lv2_leukemia_SEX_CAL2$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv2_leukemia_SEX_CAL2$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv2_leukemia_SEX_CAL2[ind.val,]))
  
  model.quadratic.reg<-lm(sum_LEUKEMIA_BZ~poly(YEAR,2), data=lv2_leukemia_SEX_CAL2[ind.tr,])
  lv2_leukemia_SEX_CAL2$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv2_leukemia_SEX_CAL2$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv2_leukemia_SEX_CAL2[ind.val,])
  
  model.quadratic.reg.FY<-lm(sum_FY_BZ~poly(YEAR,2), data=lv2_leukemia_SEX_CAL2[ind.tr,])
  lv2_leukemia_SEX_CAL2$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv2_leukemia_SEX_CAL2$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv2_leukemia_SEX_CAL2[ind.val,])
  
  model.linear.spline<-lm(sum_LEUKEMIA_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_leukemia_SEX_CAL2[ind.tr,])
  lv2_leukemia_SEX_CAL2$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv2_leukemia_SEX_CAL2$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv2_leukemia_SEX_CAL2[ind.val,])
  
  model.linear.spline.FY<-lm(sum_FY_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_leukemia_SEX_CAL2[ind.tr,])
  lv2_leukemia_SEX_CAL2$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv2_leukemia_SEX_CAL2$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv2_leukemia_SEX_CAL2[ind.val,])
}

lv2_leukemia_test<-as.data.frame(rbind(lv2_leukemia_total[2:length(lv2_leukemia_total)],lv2_leukemia_SEX[3:length(lv2_leukemia_SEX)],lv2_leukemia_CAL2[3:length(lv2_leukemia_CAL2)],
                                     lv2_leukemia_SEX_CAL2[4:length(lv2_leukemia_SEX_CAL2)]))



#######################################################################################################3
##################### level 3 data ##########################
############# 1. grouping by UP2 #################
lv3_leukemia_total<-df_leukemia1%>%group_by(UP1,UP2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  mutate(NAME=paste0("X",UP1,UP2))%>%as_tibble()

year<-c(2019:2022)
name<-unique(lv3_leukemia_total$NAME)
name2<-cbind(substr(name,2,2),substr(name,3,nchar(name)))
new_dt<-data.frame(rep(name2[,1],each=length(year)),rep(name2[,2],each=length(year)),rep(year,length(unique(lv3_leukemia_total$UP2))),
                   rep(NA,length(year)*length(unique(lv3_leukemia_total$UP2))),rep(NA,length(year)*length(unique(lv3_leukemia_total$UP2))),
                   rep(unique(lv3_leukemia_total$NAME),each=length(year)))

colnames(new_dt)<-colnames(lv3_leukemia_total)
lv3_leukemia_total<-as.data.frame(rbind(lv3_leukemia_total,new_dt))

lv3_leukemia_total$simple.reg<-0
lv3_leukemia_total$poisson.reg<-0
lv3_leukemia_total$linear.spline<-0
lv3_leukemia_total$quadratic.reg<-0

lv3_leukemia_total$simple.reg.FY<-0
lv3_leukemia_total$poisson.reg.FY<-0
lv3_leukemia_total$linear.spline.FY<-0
lv3_leukemia_total$quadratic.reg.FY<-0

name_level<-unique(lv3_leukemia_total$NAME)

for(i in 1:length(name_level)){
  ind.tr<-which(lv3_leukemia_total$YEAR<=2018 & lv3_leukemia_total$NAME==name_level[i])
  ind.val<-which(lv3_leukemia_total$YEAR>2018 & lv3_leukemia_total$NAME==name_level[i])
  
  model.simple.reg<-lm(sum_LEUKEMIA_BZ~YEAR, data=lv3_leukemia_total[ind.tr,])
  lv3_leukemia_total$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv3_leukemia_total$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv3_leukemia_total[ind.val,])
  
  model.simple.reg.FY<-lm(sum_FY_BZ~YEAR, data=lv3_leukemia_total[ind.tr,])
  lv3_leukemia_total$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv3_leukemia_total$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv3_leukemia_total[ind.val,])
  
  model.poisson.reg<-glm(sum_LEUKEMIA_BZ~YEAR, data=lv3_leukemia_total[ind.tr,],family='poisson')
  lv3_leukemia_total$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv3_leukemia_total$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv3_leukemia_total[ind.val,]))
  
  model.poisson.reg.FY<-glm(sum_FY_BZ~YEAR, data=lv3_leukemia_total[ind.tr,],family='poisson')
  lv3_leukemia_total$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv3_leukemia_total$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv3_leukemia_total[ind.val,]))
  
  model.quadratic.reg<-lm(sum_LEUKEMIA_BZ~poly(YEAR,2), data=lv3_leukemia_total[ind.tr,])
  lv3_leukemia_total$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv3_leukemia_total$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv3_leukemia_total[ind.val,])
  
  model.quadratic.reg.FY<-lm(sum_FY_BZ~poly(YEAR,2), data=lv3_leukemia_total[ind.tr,])
  lv3_leukemia_total$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv3_leukemia_total$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv3_leukemia_total[ind.val,])
  
  model.linear.spline<-lm(sum_LEUKEMIA_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_leukemia_total[ind.tr,])
  lv3_leukemia_total$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv3_leukemia_total$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv3_leukemia_total[ind.val,])
  
  model.linear.spline.FY<-lm(sum_FY_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_leukemia_total[ind.tr,])
  lv3_leukemia_total$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv3_leukemia_total$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv3_leukemia_total[ind.val,])

}


##################################################################################
############# 2. grouping by UP2 and SEX #################
lv3_leukemia_SEX<-df_leukemia1%>%group_by(UP1,UP2,SEX,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  mutate(NAME=paste0("X",UP1,UP2,"_",SEX))%>%as_tibble()

name<-unique(lv3_leukemia_SEX$NAME)
name2<-cbind(substr(name,2,2),substr(name,3,(nchar(name)-2)))

new_dt<-data.frame(rep(name2[,1],each=length(year)),
                   rep(name2[,2],each=length(year)),
                   rep(unique(lv3_leukemia_SEX$SEX),each=length(year)),
                   rep(year,length(unique(lv3_leukemia_SEX$NAME))),
                   rep(NA,length(unique(lv3_leukemia_SEX$NAME))*length(year)),
                   rep(NA,length(unique(lv3_leukemia_SEX$NAME))*length(year)),
                   rep(unique(lv3_leukemia_SEX$NAME),each=length(year)))

colnames(new_dt)<-colnames(lv3_leukemia_SEX)
lv3_leukemia_SEX<-as.data.frame(rbind(lv3_leukemia_SEX,new_dt))

lv3_leukemia_SEX$simple.reg<-0
lv3_leukemia_SEX$poisson.reg<-0
lv3_leukemia_SEX$linear.spline<-0
lv3_leukemia_SEX$quadratic.reg<-0

lv3_leukemia_SEX$simple.reg.FY<-0
lv3_leukemia_SEX$poisson.reg.FY<-0
lv3_leukemia_SEX$linear.spline.FY<-0
lv3_leukemia_SEX$quadratic.reg.FY<-0

name_level<-unique(lv3_leukemia_SEX$NAME)

for(i in 1:length(name_level)){
  ind.tr<-which(lv3_leukemia_SEX$YEAR<=2018 & lv3_leukemia_SEX$NAME==name_level[i])
  ind.val<-which(lv3_leukemia_SEX$YEAR>2018 & lv3_leukemia_SEX$NAME==name_level[i])
  
  model.simple.reg<-lm(sum_LEUKEMIA_BZ~YEAR, data=lv3_leukemia_SEX[ind.tr,])
  lv3_leukemia_SEX$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv3_leukemia_SEX$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv3_leukemia_SEX[ind.val,])
  
  model.simple.reg.FY<-lm(sum_FY_BZ~YEAR, data=lv3_leukemia_SEX[ind.tr,])
  lv3_leukemia_SEX$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv3_leukemia_SEX$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv3_leukemia_SEX[ind.val,])
  
  model.poisson.reg<-glm(sum_LEUKEMIA_BZ~YEAR, data=lv3_leukemia_SEX[ind.tr,],family='poisson')
  lv3_leukemia_SEX$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv3_leukemia_SEX$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv3_leukemia_SEX[ind.val,]))
  
  model.poisson.reg.FY<-glm(sum_FY_BZ~YEAR, data=lv3_leukemia_SEX[ind.tr,],family='poisson')
  lv3_leukemia_SEX$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv3_leukemia_SEX$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv3_leukemia_SEX[ind.val,]))
  
  model.quadratic.reg<-lm(sum_LEUKEMIA_BZ~poly(YEAR,2), data=lv3_leukemia_SEX[ind.tr,])
  lv3_leukemia_SEX$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv3_leukemia_SEX$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv3_leukemia_SEX[ind.val,])
  
  model.quadratic.reg.FY<-lm(sum_FY_BZ~poly(YEAR,2), data=lv3_leukemia_SEX[ind.tr,])
  lv3_leukemia_SEX$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv3_leukemia_SEX$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv3_leukemia_SEX[ind.val,])
  
  model.linear.spline<-lm(sum_LEUKEMIA_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_leukemia_SEX[ind.tr,])
  lv3_leukemia_SEX$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv3_leukemia_SEX$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv3_leukemia_SEX[ind.val,])
  
  model.linear.spline.FY<-lm(sum_FY_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_leukemia_SEX[ind.tr,])
  lv3_leukemia_SEX$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv3_leukemia_SEX$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv3_leukemia_SEX[ind.val,])
}


################################################################################################
############### 3. grouping by UP2 and CAL2 ######################### 
lv3_leukemia_CAL2<-df_leukemia1%>%group_by(UP1,UP2,CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  mutate(NAME=paste0("X",UP1,UP2,"_",CAL2))%>%as_tibble()

name<-unique(lv3_leukemia_CAL2$NAME)
name2<-as.data.frame(matrix(0,nrow=length(name),ncol=2))
for(i in 1:length(name)){
  name2[i,]<-c(substr(name[i],2,2),substr(name[i],3,(str_locate(name[i],"_")[1]-1)))
}

name2<-cbind(substr(name,2,2),substr(name,3,(nchar(name)-2)))

new_dt<-data.frame(rep(name2[,1],each=length(year)),
                   rep(name2[,2],each=length(year)),
                   rep(unique(lv3_leukemia_CAL2$CAL2),each=length(year)),
                   rep(year,length(unique(lv3_leukemia_CAL2$NAME))),
                   rep(NA,length(unique(lv3_leukemia_CAL2$NAME))*length(year)),
                   rep(NA,length(unique(lv3_leukemia_CAL2$NAME))*length(year)),
                   rep(unique(lv3_leukemia_CAL2$NAME),each=length(year)))

colnames(new_dt)<-colnames(lv3_leukemia_CAL2)
lv3_leukemia_CAL2<-as.data.frame(rbind(lv3_leukemia_CAL2,new_dt))

name_level<-unique(lv3_leukemia_CAL2$NAME)

lv3_leukemia_CAL2$simple.reg<-0
lv3_leukemia_CAL2$poisson.reg<-0
lv3_leukemia_CAL2$linear.spline<-0
lv3_leukemia_CAL2$quadratic.reg<-0

lv3_leukemia_CAL2$simple.reg.FY<-0
lv3_leukemia_CAL2$poisson.reg.FY<-0
lv3_leukemia_CAL2$linear.spline.FY<-0
lv3_leukemia_CAL2$quadratic.reg.FY<-0

for(i in 1:length(name_level)){
  ind.tr<-which(lv3_leukemia_CAL2$YEAR<=2018 & lv3_leukemia_CAL2$NAME==name_level[i])
  ind.val<-which(lv3_leukemia_CAL2$YEAR>2018 & lv3_leukemia_CAL2$NAME==name_level[i])
  
  model.simple.reg<-lm(sum_LEUKEMIA_BZ~YEAR, data=lv3_leukemia_CAL2[ind.tr,])
  lv3_leukemia_CAL2$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv3_leukemia_CAL2$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv3_leukemia_CAL2[ind.val,])
  
  model.simple.reg.FY<-lm(sum_FY_BZ~YEAR, data=lv3_leukemia_CAL2[ind.tr,])
  lv3_leukemia_CAL2$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv3_leukemia_CAL2$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv3_leukemia_CAL2[ind.val,])
  
  model.poisson.reg<-glm(sum_LEUKEMIA_BZ~YEAR, data=lv3_leukemia_CAL2[ind.tr,],family='poisson')
  lv3_leukemia_CAL2$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv3_leukemia_CAL2$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv3_leukemia_CAL2[ind.val,]))
  
  model.poisson.reg.FY<-glm(sum_FY_BZ~YEAR, data=lv3_leukemia_CAL2[ind.tr,],family='poisson')
  lv3_leukemia_CAL2$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv3_leukemia_CAL2$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv3_leukemia_CAL2[ind.val,]))
  
  model.quadratic.reg<-lm(sum_LEUKEMIA_BZ~poly(YEAR,2), data=lv3_leukemia_CAL2[ind.tr,])
  lv3_leukemia_CAL2$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv3_leukemia_CAL2$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv3_leukemia_CAL2[ind.val,])
  
  model.quadratic.reg.FY<-lm(sum_FY_BZ~poly(YEAR,2), data=lv3_leukemia_CAL2[ind.tr,])
  lv3_leukemia_CAL2$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv3_leukemia_CAL2$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv3_leukemia_CAL2[ind.val,])
  
  model.linear.spline<-lm(sum_LEUKEMIA_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_leukemia_CAL2[ind.tr,])
  lv3_leukemia_CAL2$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv3_leukemia_CAL2$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv3_leukemia_CAL2[ind.val,])
  
  model.linear.spline.FY<-lm(sum_FY_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_leukemia_CAL2[ind.tr,])
  lv3_leukemia_CAL2$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv3_leukemia_CAL2$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv3_leukemia_CAL2[ind.val,])
}





################################################################################################
############### 3. grouping by UP2 and SEX and CAL2 ######################### 
lv3_leukemia_SEX_CAL2<-df_leukemia1%>%group_by(UP1,UP2,SEX,CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  mutate(NAME=paste0("X",UP1,UP2,"_",SEX,CAL2))%>%as_tibble()

name<-unique(lv3_leukemia_SEX_CAL2$NAME)
name2<-as.data.frame(matrix(0,nrow=length(name),ncol=2))

for(i in 1:length(name)){
  name2[i,]<-c(substr(name[i],2,2),substr(name[i],3,(str_locate(name[i],"_")[1]-1)))
}

name2<-cbind(substr(name,2,2),substr(name,3,(nchar(name)-2)))

new_dt<-data.frame(rep(name2[,1],each=length(year)),
                   rep(name2[,2],each=length(year)),
                   rep(unique(lv3_leukemia_SEX_CAL2$SEX),each=length(year)*length(unique(lv3_leukemia_SEX_CAL2$CAL2))),
                   rep(unique(lv3_leukemia_SEX_CAL2$CAL2),each=length(year)),
                   rep(year,length(unique(lv3_leukemia_SEX_CAL2$NAME))),
                   rep(NA,length(unique(lv3_leukemia_SEX_CAL2$NAME))*length(year)),
                   rep(NA,length(unique(lv3_leukemia_SEX_CAL2$NAME))*length(year)),
                   rep(unique(lv3_leukemia_SEX_CAL2$NAME),each=length(year)))

colnames(new_dt)<-colnames(lv3_leukemia_SEX_CAL2)
lv3_leukemia_SEX_CAL2<-as.data.frame(rbind(lv3_leukemia_SEX_CAL2,new_dt))

name_level<-unique(lv3_leukemia_SEX_CAL2$NAME)

lv3_leukemia_SEX_CAL2$simple.reg<-0
lv3_leukemia_SEX_CAL2$poisson.reg<-0
lv3_leukemia_SEX_CAL2$linear.spline<-0
lv3_leukemia_SEX_CAL2$quadratic.reg<-0

lv3_leukemia_SEX_CAL2$simple.reg.FY<-0
lv3_leukemia_SEX_CAL2$poisson.reg.FY<-0
lv3_leukemia_SEX_CAL2$linear.spline.FY<-0
lv3_leukemia_SEX_CAL2$quadratic.reg.FY<-0

for(i in 1:length(name_level)){
  ind.tr<-which(lv3_leukemia_SEX_CAL2$YEAR<=2018 & lv3_leukemia_SEX_CAL2$NAME==name_level[i])
  ind.val<-which(lv3_leukemia_SEX_CAL2$YEAR>2018 & lv3_leukemia_SEX_CAL2$NAME==name_level[i])
  
  model.simple.reg<-lm(sum_LEUKEMIA_BZ~YEAR, data=lv3_leukemia_SEX_CAL2[ind.tr,])
  lv3_leukemia_SEX_CAL2$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv3_leukemia_SEX_CAL2$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv3_leukemia_SEX_CAL2[ind.val,])
  
  model.simple.reg.FY<-lm(sum_FY_BZ~YEAR, data=lv3_leukemia_SEX_CAL2[ind.tr,])
  lv3_leukemia_SEX_CAL2$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv3_leukemia_SEX_CAL2$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv3_leukemia_SEX_CAL2[ind.val,])
  
  model.poisson.reg<-glm(sum_LEUKEMIA_BZ~YEAR, data=lv3_leukemia_SEX_CAL2[ind.tr,],family='poisson')
  lv3_leukemia_SEX_CAL2$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv3_leukemia_SEX_CAL2$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv3_leukemia_SEX_CAL2[ind.val,]))
  
  model.poisson.reg.FY<-glm(sum_FY_BZ~YEAR, data=lv3_leukemia_SEX_CAL2[ind.tr,],family='poisson')
  lv3_leukemia_SEX_CAL2$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv3_leukemia_SEX_CAL2$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv3_leukemia_SEX_CAL2[ind.val,]))
  
  model.quadratic.reg<-lm(sum_LEUKEMIA_BZ~poly(YEAR,2), data=lv3_leukemia_SEX_CAL2[ind.tr,])
  lv3_leukemia_SEX_CAL2$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv3_leukemia_SEX_CAL2$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv3_leukemia_SEX_CAL2[ind.val,])
  
  model.quadratic.reg.FY<-lm(sum_FY_BZ~poly(YEAR,2), data=lv3_leukemia_SEX_CAL2[ind.tr,])
  lv3_leukemia_SEX_CAL2$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv3_leukemia_SEX_CAL2$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv3_leukemia_SEX_CAL2[ind.val,])
  
  model.linear.spline<-lm(sum_LEUKEMIA_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_leukemia_SEX_CAL2[ind.tr,])
  lv3_leukemia_SEX_CAL2$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv3_leukemia_SEX_CAL2$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv3_leukemia_SEX_CAL2[ind.val,])
  
  model.linear.spline.FY<-lm(sum_FY_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_leukemia_SEX_CAL2[ind.tr,])
  lv3_leukemia_SEX_CAL2$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv3_leukemia_SEX_CAL2$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv3_leukemia_SEX_CAL2[ind.val,])
} 

lv3_leukemia_test<-as.data.frame(rbind(lv3_leukemia_total[3:length(lv3_leukemia_total)],lv3_leukemia_SEX[4:length(lv3_leukemia_SEX)],lv3_leukemia_CAL2[4:length(lv3_leukemia_CAL2)],
                                    lv3_leukemia_SEX_CAL2[5:length(lv3_leukemia_SEX_CAL2)]))

####### test_set_leukemia_total ########
validation_v2 <- read_csv("validation_v2.csv")
ID<-validation_v2[,c("ID","NAME")]

leukemia_total_test<-as.data.frame(rbind(lv1_leukemia_test,lv2_leukemia_test,lv3_leukemia_test))
leukemia_total_test<-left_join(leukemia_total_test,ID,by='NAME')
leukemia_total_test<-unique(leukemia_total_test)
colnames(leukemia_total_test)<-c("YEAR", "sum_FY_BZ_LEUKEMIA","sum_LEUKEMIA_BZ","NAME","simple.reg.LEUKEMIA","poisson.reg.LEUKEMIA",     
                                 "linear.spline.LEUKEMIA","quadratic.reg.LEUKEMIA","simple.reg.leukemia.FY","poisson.reg.leukemia.FY",
                                 "linear.spline.leukemia.FY","quadratic.reg.leukemia.FY",
                                 "ID"  )
leukemia_total_test<-leukemia_total_test[,c('ID','NAME',"YEAR","sum_LEUKEMIA_BZ","simple.reg.LEUKEMIA","poisson.reg.LEUKEMIA",
                                "linear.spline.LEUKEMIA","quadratic.reg.LEUKEMIA", "sum_FY_BZ_LEUKEMIA","simple.reg.leukemia.FY","poisson.reg.leukemia.FY","linear.spline.leukemia.FY",
                                "quadratic.reg.leukemia.FY")]

















###################### Lung cancer data ####################################
############################# level 1 #############################
########### 1. total ###########
lv1_lung_total<-df_lung1%>%group_by(YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME="X")%>%as_tibble()

new_dt<-data.frame(c(2019:2022),rep(NA,4),rep(NA,4),rep(unique(lv1_lung_total$NAME)))
colnames(new_dt)<-colnames(lv1_lung_total)
lv1_lung_total<-as.data.frame(rbind(lv1_lung_total,new_dt))

lv1_lung_total$simple.reg<-0
lv1_lung_total$poisson.reg<-0
lv1_lung_total$linear.spline<-0
lv1_lung_total$quadratic.reg<-0

lv1_lung_total$simple.reg.FY<-0
lv1_lung_total$poisson.reg.FY<-0
lv1_lung_total$linear.spline.FY<-0
lv1_lung_total$quadratic.reg.FY<-0

ind.tr<-which(lv1_lung_total$YEAR<=2018)
ind.val<-which(lv1_lung_total$YEAR>2018)

model.simple.reg<-lm(sum_LUNG_BZ~YEAR, data=lv1_lung_total[ind.tr,])
lv1_lung_total$simple.reg[ind.tr]<-ifelse(model.simple.reg$fitted.values<0, 0, model.simple.reg$fitted.values)
lv1_lung_total$simple.reg[ind.val]<-ifelse(predict(model.simple.reg,newdata=lv1_lung_total[ind.val,])<0,0,
                                           predict(model.simple.reg,newdata=lv1_lung_total[ind.val,]))

model.simple.reg.FY<-lm(sum_FY_BZ~YEAR, data=lv1_lung_total[ind.tr,])
lv1_lung_total$simple.reg.FY[ind.tr]<-ifelse(model.simple.reg.FY$fitted.values<0,0,model.simple.reg.FY$fitted.values)
lv1_lung_total$simple.reg.FY[ind.val]<-ifelse(predict(model.simple.reg.FY,newdata=lv1_lung_total[ind.val,])<0,0,
                                              predict(model.simple.reg.FY,newdata=lv1_lung_total[ind.val,]))

model.poisson.reg<-glm(sum_LUNG_BZ~YEAR, data=lv1_lung_total[ind.tr,],family='poisson')
lv1_lung_total$poisson.reg[ind.tr]<-ifelse(model.poisson.reg$fitted.values<0,0,model.poisson.reg$fitted.values)
lv1_lung_total$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv1_lung_total[ind.val,]))

model.poisson.reg.FY<-glm(sum_FY_BZ~YEAR, data=lv1_lung_total[ind.tr,],family='poisson')
lv1_lung_total$poisson.reg.FY[ind.tr]<-ifelse(model.poisson.reg.FY$fitted.values<0,0,model.poisson.reg.FY$fitted.values)
lv1_lung_total$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv1_lung_total[ind.val,]))

model.quadratic.reg<-lm(sum_LUNG_BZ~poly(YEAR,2), data=lv1_lung_total[ind.tr,])
lv1_lung_total$quadratic.reg[ind.tr]<-ifelse(model.quadratic.reg$fitted.values<0, 0, model.quadratic.reg$fitted.values)
lv1_lung_total$quadratic.reg[ind.val]<-ifelse(predict(model.quadratic.reg,newdata=lv1_lung_total[ind.val,])<0,0,
                                              predict(model.quadratic.reg,newdata=lv1_lung_total[ind.val,]))
model.quadratic.reg.FY<-lm(sum_FY_BZ~poly(YEAR,2), data=lv1_lung_total[ind.tr,])
lv1_lung_total$quadratic.reg.FY[ind.tr]<-ifelse(model.quadratic.reg.FY$fitted.values<0,0,model.quadratic.reg.FY$fitted.values)
lv1_lung_total$quadratic.reg.FY[ind.val]<-ifelse(predict(model.quadratic.reg.FY,newdata=lv1_lung_total[ind.val,])<0,0,
                                                 predict(model.quadratic.reg.FY,newdata=lv1_lung_total[ind.val,]))
model.linear.spline<-lm(sum_LUNG_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_lung_total[ind.tr,])
lv1_lung_total$linear.spline[ind.tr]<-ifelse(model.linear.spline$fitted.values<0,0,model.linear.spline$fitted.values)
lv1_lung_total$linear.spline[ind.val]<-ifelse(predict(model.linear.spline,newdata=lv1_lung_total[ind.val,])<0,0,
                                              predict(model.linear.spline,newdata=lv1_lung_total[ind.val,]))
model.linear.spline.FY<-lm(sum_FY_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_lung_total[ind.tr,])
lv1_lung_total$linear.spline.FY[ind.tr]<-ifelse(model.linear.spline.FY$fitted.values<0,0,model.linear.spline.FY$fitted.values)
lv1_lung_total$linear.spline.FY[ind.val]<-ifelse(predict(model.linear.spline.FY,newdata=lv1_lung_total[ind.val,])<0,0,
                                                 predict(model.linear.spline.FY,newdata=lv1_lung_total[ind.val,]))

####################################################################################
#################### 2. grouping by SEX ##################
lv1_lung_SEX<-df_lung1%>%group_by(SEX,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME=paste0("X","_",SEX))%>%as_tibble()

SEX_level<-unique(lv1_lung_SEX$SEX)
year<-c(2019:2022)

new_dt<-data.frame(rep(unique(SEX_level),each=length(year)),rep(year,length(unique(SEX_level))),rep(NA,length(year)*length(unique(SEX_level))),
                   rep(NA,length(year)*length(unique(SEX_level))),rep(unique(lv1_lung_SEX$NAME),each=length(year)))
colnames(new_dt)<-colnames(lv1_lung_SEX)
lv1_lung_SEX<-as.data.frame(rbind(lv1_lung_SEX,new_dt))

lv1_lung_SEX$simple.reg<-0
lv1_lung_SEX$poisson.reg<-0
lv1_lung_SEX$linear.spline<-0
lv1_lung_SEX$quadratic.reg<-0

lv1_lung_SEX$simple.reg.FY<-0
lv1_lung_SEX$poisson.reg.FY<-0
lv1_lung_SEX$linear.spline.FY<-0
lv1_lung_SEX$quadratic.reg.FY<-0

for(i in 1:length(SEX_level)){
  ind.tr<-which(lv1_lung_SEX$YEAR<=2018 & lv1_lung_SEX$SEX==SEX_level[i])
  ind.val<-which(lv1_lung_SEX$YEAR>2018 & lv1_lung_SEX$SEX==SEX_level[i])
  
  model.simple.reg<-lm(sum_LUNG_BZ~YEAR, data=lv1_lung_SEX[ind.tr,])
  lv1_lung_SEX$simple.reg[ind.tr]<-ifelse(model.simple.reg$fitted.values<0, 0, model.simple.reg$fitted.values)
  lv1_lung_SEX$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv1_lung_SEX[ind.val,])
  
  model.simple.reg.FY<-lm(sum_FY_BZ~YEAR, data=lv1_lung_SEX[ind.tr,])
  lv1_lung_SEX$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv1_lung_SEX$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv1_lung_SEX[ind.val,])
  
  model.poisson.reg<-glm(sum_LUNG_BZ~YEAR, data=lv1_lung_SEX[ind.tr,],family='poisson')
  lv1_lung_SEX$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv1_lung_SEX$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv1_lung_SEX[ind.val,]))
  
  model.poisson.reg.FY<-glm(sum_FY_BZ~YEAR, data=lv1_lung_SEX[ind.tr,],family='poisson')
  lv1_lung_SEX$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv1_lung_SEX$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv1_lung_SEX[ind.val,]))
  
  
  model.quadratic.reg<-lm(sum_LUNG_BZ~poly(YEAR,2), data=lv1_lung_SEX[ind.tr,])
  lv1_lung_SEX$quadratic.reg[ind.tr]<-ifelse(model.quadratic.reg$fitted.values<0, 0, model.quadratic.reg$fitted.values)
  lv1_lung_SEX$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv1_lung_SEX[ind.val,])
  
  model.quadratic.reg.FY<-lm(sum_FY_BZ~poly(YEAR,2), data=lv1_lung_SEX[ind.tr,])
  lv1_lung_SEX$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv1_lung_SEX$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv1_lung_SEX[ind.val,])
  
  model.linear.spline<-lm(sum_LUNG_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_lung_SEX[ind.tr,])
  lv1_lung_SEX$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv1_lung_SEX$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv1_lung_SEX[ind.val,])
  
  model.linear.spline.FY<-lm(sum_FY_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_lung_SEX[ind.tr,])
  lv1_lung_SEX$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv1_lung_SEX$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv1_lung_SEX[ind.val,])
}

################################################################################################
############### 3. grouping by CAL2 ######################### 
lv1_lung_CAL2<-df_lung1%>%group_by(CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME=paste0("X","_",CAL2))%>%as_tibble()

CAL2_level<-unique(lv1_lung_CAL2$CAL2)

year<-c(2019:2022)
new_dt<-data.frame(rep(unique(CAL2_level),each=length(year)),rep(year,length(unique(CAL2_level))),rep(NA,length(year)*length(unique(CAL2_level))),
                   rep(NA,length(year)*length(unique(CAL2_level))),rep(unique(lv1_lung_CAL2$NAME),each=length(year)))
colnames(new_dt)<-colnames(lv1_lung_CAL2)
lv1_lung_CAL2<-as.data.frame(rbind(lv1_lung_CAL2,new_dt))

lv1_lung_CAL2$simple.reg<-0
lv1_lung_CAL2$poisson.reg<-0
lv1_lung_CAL2$linear.spline<-0
lv1_lung_CAL2$quadratic.reg<-0

lv1_lung_CAL2$simple.reg.FY<-0
lv1_lung_CAL2$poisson.reg.FY<-0
lv1_lung_CAL2$linear.spline.FY<-0
lv1_lung_CAL2$quadratic.reg.FY<-0

CAL2_level<-unique(lv1_lung_CAL2$CAL2)

for(i in 1:length(CAL2_level)){
  ind.tr<-which(lv1_lung_CAL2$YEAR<=2018 & lv1_lung_CAL2$CAL2==CAL2_level[i])
  ind.val<-which(lv1_lung_CAL2$YEAR>2018 & lv1_lung_CAL2$CAL2==CAL2_level[i])
  
  model.poisson.reg<-glm(sum_LUNG_BZ~YEAR, data=lv1_lung_CAL2[ind.tr,],family='poisson')
  lv1_lung_CAL2$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv1_lung_CAL2$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv1_lung_CAL2[ind.val,]))
  
  model.poisson.reg.FY<-glm(sum_FY_BZ~YEAR, data=lv1_lung_CAL2[ind.tr,],family='poisson')
  lv1_lung_CAL2$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv1_lung_CAL2$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv1_lung_CAL2[ind.val,]))
  
  model.simple.reg<-lm(sum_LUNG_BZ~YEAR, data=lv1_lung_CAL2[ind.tr,])
  lv1_lung_CAL2$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv1_lung_CAL2$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv1_lung_CAL2[ind.val,])
  
  model.simple.reg.FY<-lm(sum_FY_BZ~YEAR, data=lv1_lung_CAL2[ind.tr,])
  lv1_lung_CAL2$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv1_lung_CAL2$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv1_lung_CAL2[ind.val,])
  
  model.quadratic.reg<-lm(sum_LUNG_BZ~poly(YEAR,2), data=lv1_lung_CAL2[ind.tr,])
  lv1_lung_CAL2$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv1_lung_CAL2$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv1_lung_CAL2[ind.val,])
  
  model.quadratic.reg.FY<-lm(sum_FY_BZ~poly(YEAR,2), data=lv1_lung_CAL2[ind.tr,])
  lv1_lung_CAL2$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv1_lung_CAL2$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv1_lung_CAL2[ind.val,])
  
  model.linear.spline<-lm(sum_LUNG_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_lung_CAL2[ind.tr,])
  lv1_lung_CAL2$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv1_lung_CAL2$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv1_lung_CAL2[ind.val,])
  
  model.linear.spline.FY<-lm(sum_FY_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_lung_CAL2[ind.tr,])
  lv1_lung_CAL2$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv1_lung_CAL2$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv1_lung_CAL2[ind.val,])
}

################################################################################################
############### 4. grouping by SEX and CAL2 ######################### 
lv1_lung_SEX_CAL2<-df_lung1%>%group_by(SEX,CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME=paste0("X","_",SEX,CAL2))%>%as_tibble()

year<-c(2019:2022)

SEX_CAL2_level<-unique(lv1_lung_SEX_CAL2$NAME)
new_dt<-data.frame(rep(unique(lv1_lung_SEX_CAL2$SEX),each=(length(year)*2)),rep(unique(lv1_lung_SEX_CAL2$CAL2),each=(length(year))),
                   rep(year,length(SEX_CAL2_level)),rep(NA,(length(year)*4)),rep(NA,(length(year)*4)),rep(SEX_CAL2_level,each=length(year)))
colnames(new_dt)<-colnames(lv1_lung_SEX_CAL2)
lv1_lung_SEX_CAL2<-as.data.frame(rbind(lv1_lung_SEX_CAL2,new_dt))

lv1_lung_SEX_CAL2$simple.reg<-0
lv1_lung_SEX_CAL2$poisson.reg<-0
lv1_lung_SEX_CAL2$linear.spline<-0
lv1_lung_SEX_CAL2$quadratic.reg<-0

lv1_lung_SEX_CAL2$simple.reg.FY<-0
lv1_lung_SEX_CAL2$poisson.reg.FY<-0
lv1_lung_SEX_CAL2$linear.spline.FY<-0
lv1_lung_SEX_CAL2$quadratic.reg.FY<-0

for(i in 1:length(SEX_CAL2_level)){
  ind.tr<-which(lv1_lung_SEX_CAL2$YEAR<=2018 & lv1_lung_SEX_CAL2$NAME==SEX_CAL2_level[i])
  ind.val<-which(lv1_lung_SEX_CAL2$YEAR>2018 & lv1_lung_SEX_CAL2$NAME==SEX_CAL2_level[i])
  
  model.simple.reg<-lm(sum_LUNG_BZ~YEAR, data=lv1_lung_SEX_CAL2[ind.tr,])
  lv1_lung_SEX_CAL2$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv1_lung_SEX_CAL2$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv1_lung_SEX_CAL2[ind.val,])
  
  model.simple.reg.FY<-lm(sum_FY_BZ~YEAR, data=lv1_lung_SEX_CAL2[ind.tr,])
  lv1_lung_SEX_CAL2$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv1_lung_SEX_CAL2$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv1_lung_SEX_CAL2[ind.val,])
  
  model.poisson.reg<-glm(sum_LUNG_BZ~YEAR, data=lv1_lung_SEX_CAL2[ind.tr,],family='poisson')
  lv1_lung_SEX_CAL2$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv1_lung_SEX_CAL2$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv1_lung_SEX_CAL2[ind.val,]))
  
  model.poisson.reg.FY<-glm(sum_FY_BZ~YEAR, data=lv1_lung_SEX_CAL2[ind.tr,],family='poisson')
  lv1_lung_SEX_CAL2$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv1_lung_SEX_CAL2$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv1_lung_SEX_CAL2[ind.val,]))
  
  model.quadratic.reg<-lm(sum_LUNG_BZ~poly(YEAR,2), data=lv1_lung_SEX_CAL2[ind.tr,])
  lv1_lung_SEX_CAL2$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv1_lung_SEX_CAL2$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv1_lung_SEX_CAL2[ind.val,])
  
  model.quadratic.reg.FY<-lm(sum_FY_BZ~poly(YEAR,2), data=lv1_lung_SEX_CAL2[ind.tr,])
  lv1_lung_SEX_CAL2$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv1_lung_SEX_CAL2$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv1_lung_SEX_CAL2[ind.val,])
  
  model.linear.spline<-lm(sum_LUNG_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_lung_SEX_CAL2[ind.tr,])
  lv1_lung_SEX_CAL2$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv1_lung_SEX_CAL2$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv1_lung_SEX_CAL2[ind.val,])
  
  model.linear.spline.FY<-lm(sum_FY_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_lung_SEX_CAL2[ind.tr,])
  lv1_lung_SEX_CAL2$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv1_lung_SEX_CAL2$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv1_lung_SEX_CAL2[ind.val,])
}

lv1_lung_test<-as.data.frame(rbind(lv1_lung_total,lv1_lung_SEX[2:length(lv1_lung_SEX)],lv1_lung_CAL2[2:length(lv1_lung_CAL2)],
                                    lv1_lung_SEX_CAL2[3:length(lv1_lung_SEX_CAL2)]))

##################### level 2 data ##########################
############# 1. grouping by UP1 #################
lv2_lung_total<-df_lung1%>%group_by(UP1,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME=paste0("X",UP1))%>%as_tibble()

year<-c(2019:2022)
new_dt<-data.frame(rep(unique(lv2_lung_total$UP1),each=length(year)),rep(year,length(unique(lv2_lung_total$UP1))),
                   rep(NA,length(year)*length(unique(lv2_lung_total$UP1))),rep(NA,length(year)*length(unique(lv2_lung_total$UP1))),
                   rep(unique(lv2_lung_total$NAME),each=length(year)))
colnames(new_dt)<-colnames(lv2_lung_total)

lv2_lung_total<-as.data.frame(rbind(lv2_lung_total,new_dt))

lv2_lung_total$simple.reg<-0
lv2_lung_total$poisson.reg<-0
lv2_lung_total$linear.spline<-0
lv2_lung_total$quadratic.reg<-0

lv2_lung_total$simple.reg.FY<-0
lv2_lung_total$poisson.reg.FY<-0
lv2_lung_total$linear.spline.FY<-0
lv2_lung_total$quadratic.reg.FY<-0

name_level<-unique(lv2_lung_total$NAME)

for(i in 1:length(name_level)){
  ind.tr<-which(lv2_lung_total$YEAR<=2018 & lv2_lung_total$NAME==name_level[i])
  ind.val<-which(lv2_lung_total$YEAR>2018 & lv2_lung_total$NAME==name_level[i])
  
  model.simple.reg<-lm(sum_LUNG_BZ~YEAR, data=lv2_lung_total[ind.tr,])
  lv2_lung_total$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv2_lung_total$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv2_lung_total[ind.val,])
  
  model.simple.reg.FY<-lm(sum_FY_BZ~YEAR, data=lv2_lung_total[ind.tr,])
  lv2_lung_total$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv2_lung_total$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv2_lung_total[ind.val,])
  
  model.poisson.reg<-glm(sum_LUNG_BZ~YEAR, data=lv2_lung_total[ind.tr,],family='poisson')
  lv2_lung_total$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv2_lung_total$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv2_lung_total[ind.val,]))
  
  model.poisson.reg.FY<-glm(sum_FY_BZ~YEAR, data=lv2_lung_total[ind.tr,],family='poisson')
  lv2_lung_total$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv2_lung_total$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv2_lung_total[ind.val,]))
  
  model.quadratic.reg<-lm(sum_LUNG_BZ~poly(YEAR,2), data=lv2_lung_total[ind.tr,])
  lv2_lung_total$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv2_lung_total$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv2_lung_total[ind.val,])
  
  model.quadratic.reg.FY<-lm(sum_FY_BZ~poly(YEAR,2), data=lv2_lung_total[ind.tr,])
  lv2_lung_total$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv2_lung_total$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv2_lung_total[ind.val,])
  
  model.linear.spline<-lm(sum_LUNG_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_lung_total[ind.tr,])
  lv2_lung_total$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv2_lung_total$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv2_lung_total[ind.val,])
  
  model.linear.spline.FY<-lm(sum_FY_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_lung_total[ind.tr,])
  lv2_lung_total$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv2_lung_total$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv2_lung_total[ind.val,])
}

##################################################################################
############# 2. grouping by UP1 and SEX #################
lv2_lung_SEX<-df_lung1%>%group_by(UP1,SEX,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME=paste0("X",UP1,"_",SEX))%>%as_tibble()

new_dt<-data.frame(rep(unique(lv2_lung_SEX$UP1),each=length(year)*length(unique(lv2_lung_SEX$SEX))),
                   rep(unique(lv2_lung_SEX$SEX),each=length(year)),
                   rep(year,length(unique(lv2_lung_SEX$NAME))),
                   rep(NA,length(unique(lv2_lung_SEX$NAME))*length(year)),
                   rep(NA,length(unique(lv2_lung_SEX$NAME))*length(year)),
                   rep(unique(lv2_lung_SEX$NAME),each=length(year)))

colnames(new_dt)<-colnames(lv2_lung_SEX)
lv2_lung_SEX<-as.data.frame(rbind(lv2_lung_SEX,new_dt))

lv2_lung_SEX$simple.reg<-0
lv2_lung_SEX$poisson.reg<-0
lv2_lung_SEX$linear.spline<-0
lv2_lung_SEX$quadratic.reg<-0

lv2_lung_SEX$simple.reg.FY<-0
lv2_lung_SEX$poisson.reg.FY<-0
lv2_lung_SEX$linear.spline.FY<-0
lv2_lung_SEX$quadratic.reg.FY<-0

name_level<-unique(lv2_lung_SEX$NAME)

for(i in 1:length(name_level)){
  ind.tr<-which(lv2_lung_SEX$YEAR<=2018 & lv2_lung_SEX$NAME==name_level[i])
  ind.val<-which(lv2_lung_SEX$YEAR>2018 & lv2_lung_SEX$NAME==name_level[i])
  
  model.simple.reg<-lm(sum_LUNG_BZ~YEAR, data=lv2_lung_SEX[ind.tr,])
  lv2_lung_SEX$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv2_lung_SEX$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv2_lung_SEX[ind.val,])
  
  model.simple.reg.FY<-lm(sum_FY_BZ~YEAR, data=lv2_lung_SEX[ind.tr,])
  lv2_lung_SEX$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv2_lung_SEX$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv2_lung_SEX[ind.val,])
  
  model.poisson.reg<-glm(sum_LUNG_BZ~YEAR, data=lv2_lung_SEX[ind.tr,],family='poisson')
  lv2_lung_SEX$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv2_lung_SEX$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv2_lung_SEX[ind.val,]))
  
  model.poisson.reg.FY<-glm(sum_FY_BZ~YEAR, data=lv2_lung_SEX[ind.tr,],family='poisson')
  lv2_lung_SEX$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv2_lung_SEX$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv2_lung_SEX[ind.val,]))
  
  model.quadratic.reg<-lm(sum_LUNG_BZ~poly(YEAR,2), data=lv2_lung_SEX[ind.tr,])
  lv2_lung_SEX$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv2_lung_SEX$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv2_lung_SEX[ind.val,])
  
  model.quadratic.reg.FY<-lm(sum_FY_BZ~poly(YEAR,2), data=lv2_lung_SEX[ind.tr,])
  lv2_lung_SEX$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv2_lung_SEX$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv2_lung_SEX[ind.val,])
  
  model.linear.spline<-lm(sum_LUNG_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_lung_SEX[ind.tr,])
  lv2_lung_SEX$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv2_lung_SEX$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv2_lung_SEX[ind.val,])
  
  model.linear.spline.FY<-lm(sum_FY_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_lung_SEX[ind.tr,])
  lv2_lung_SEX$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv2_lung_SEX$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv2_lung_SEX[ind.val,])
}

################################################################################################
############### 3. grouping by UP1 and CAL2 ######################### 
lv2_lung_CAL2<-df_lung1%>%group_by(UP1,CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME=paste0("X",UP1,"_",CAL2))%>%as_tibble()

name_level<-unique(lv2_lung_CAL2$NAME)

new_dt<-data.frame(rep(unique(lv2_lung_CAL2$UP1),each=length(year)*length(unique(lv2_lung_CAL2$CAL2))),
                   rep(unique(lv2_lung_CAL2$CAL2),each=length(year)),
                   rep(year,length(unique(lv2_lung_CAL2$NAME))),
                   rep(NA,length(unique(lv2_lung_CAL2$NAME))*length(year)),
                   rep(NA,length(unique(lv2_lung_CAL2$NAME))*length(year)),
                   rep(unique(lv2_lung_CAL2$NAME),each=length(year)))

colnames(new_dt)<-colnames(lv2_lung_CAL2)
lv2_lung_CAL2<-as.data.frame(rbind(lv2_lung_CAL2,new_dt))

lv2_lung_CAL2$simple.reg<-0
lv2_lung_CAL2$poisson.reg<-0
lv2_lung_CAL2$linear.spline<-0
lv2_lung_CAL2$quadratic.reg<-0

lv2_lung_CAL2$simple.reg.FY<-0
lv2_lung_CAL2$poisson.reg.FY<-0
lv2_lung_CAL2$linear.spline.FY<-0
lv2_lung_CAL2$quadratic.reg.FY<-0

for(i in 1:length(name_level)){
  ind.tr<-which(lv2_lung_CAL2$YEAR<=2018 & lv2_lung_CAL2$NAME==name_level[i])
  ind.val<-which(lv2_lung_CAL2$YEAR>2018 & lv2_lung_CAL2$NAME==name_level[i])
  
  model.simple.reg<-lm(sum_LUNG_BZ~YEAR, data=lv2_lung_CAL2[ind.tr,])
  lv2_lung_CAL2$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv2_lung_CAL2$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv2_lung_CAL2[ind.val,])
  
  model.simple.reg.FY<-lm(sum_FY_BZ~YEAR, data=lv2_lung_CAL2[ind.tr,])
  lv2_lung_CAL2$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv2_lung_CAL2$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv2_lung_CAL2[ind.val,])
  
  model.poisson.reg<-glm(sum_LUNG_BZ~YEAR, data=lv2_lung_CAL2[ind.tr,],family='poisson')
  lv2_lung_CAL2$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv2_lung_CAL2$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv2_lung_CAL2[ind.val,]))
  
  model.poisson.reg.FY<-glm(sum_FY_BZ~YEAR, data=lv2_lung_CAL2[ind.tr,],family='poisson')
  lv2_lung_CAL2$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv2_lung_CAL2$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv2_lung_CAL2[ind.val,]))
  
  model.quadratic.reg<-lm(sum_LUNG_BZ~poly(YEAR,2), data=lv2_lung_CAL2[ind.tr,])
  lv2_lung_CAL2$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv2_lung_CAL2$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv2_lung_CAL2[ind.val,])
  
  model.quadratic.reg.FY<-lm(sum_FY_BZ~poly(YEAR,2), data=lv2_lung_CAL2[ind.tr,])
  lv2_lung_CAL2$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv2_lung_CAL2$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv2_lung_CAL2[ind.val,])
  
  model.linear.spline<-lm(sum_LUNG_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_lung_CAL2[ind.tr,])
  lv2_lung_CAL2$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv2_lung_CAL2$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv2_lung_CAL2[ind.val,])
  
  model.linear.spline.FY<-lm(sum_FY_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_lung_CAL2[ind.tr,])
  lv2_lung_CAL2$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv2_lung_CAL2$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv2_lung_CAL2[ind.val,])
  
}

################################################################################################
############### 3. grouping by UP1 and SEX and CAL2 ######################### 
lv2_lung_SEX_CAL2<-df_lung1%>%group_by(UP1,SEX,CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME=paste0("X",UP1,"_",SEX,CAL2))%>%as_tibble()

name_level<-unique(lv2_lung_SEX_CAL2$NAME)
new_dt<-data.frame(rep(unique(lv2_lung_SEX_CAL2$UP1),each=length(year)*length(unique(lv2_lung_SEX_CAL2$SEX))*length(unique(lv2_lung_SEX_CAL2$CAL2))),
                   rep(unique(lv2_lung_SEX_CAL2$SEX),each=length(year)*length(unique(lv2_lung_SEX_CAL2$CAL2))),
                   rep(unique(lv2_lung_SEX_CAL2$CAL2),each=length(year)*length(unique(lv2_lung_SEX_CAL2$SEX))),
                   rep(year,length(unique(lv2_lung_SEX_CAL2$NAME))),
                   rep(NA,length(unique(lv2_lung_SEX_CAL2$NAME))*length(year)),
                   rep(NA,length(unique(lv2_lung_SEX_CAL2$NAME))*length(year)),
                   rep(unique(lv2_lung_SEX_CAL2$NAME),each=length(year)))

colnames(new_dt)<-colnames(lv2_lung_SEX_CAL2)
lv2_lung_SEX_CAL2<-as.data.frame(rbind(lv2_lung_SEX_CAL2,new_dt))

lv2_lung_SEX_CAL2$simple.reg<-0
lv2_lung_SEX_CAL2$poisson.reg<-0
lv2_lung_SEX_CAL2$linear.spline<-0
lv2_lung_SEX_CAL2$quadratic.reg<-0

lv2_lung_SEX_CAL2$simple.reg.FY<-0
lv2_lung_SEX_CAL2$poisson.reg.FY<-0
lv2_lung_SEX_CAL2$linear.spline.FY<-0
lv2_lung_SEX_CAL2$quadratic.reg.FY<-0

for(i in 1:length(name_level)){
  ind.tr<-which(lv2_lung_SEX_CAL2$YEAR<=2018 & lv2_lung_SEX_CAL2$NAME==name_level[i])
  ind.val<-which(lv2_lung_SEX_CAL2$YEAR>2018 & lv2_lung_SEX_CAL2$NAME==name_level[i])
  
  model.simple.reg<-lm(sum_LUNG_BZ~YEAR, data=lv2_lung_SEX_CAL2[ind.tr,])
  lv2_lung_SEX_CAL2$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv2_lung_SEX_CAL2$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv2_lung_SEX_CAL2[ind.val,])
  
  model.simple.reg.FY<-lm(sum_FY_BZ~YEAR, data=lv2_lung_SEX_CAL2[ind.tr,])
  lv2_lung_SEX_CAL2$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv2_lung_SEX_CAL2$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv2_lung_SEX_CAL2[ind.val,])
  
  model.poisson.reg<-glm(sum_LUNG_BZ~YEAR, data=lv2_lung_SEX_CAL2[ind.tr,],family='poisson')
  lv2_lung_SEX_CAL2$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv2_lung_SEX_CAL2$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv2_lung_SEX_CAL2[ind.val,]))
  
  model.poisson.reg.FY<-glm(sum_FY_BZ~YEAR, data=lv2_lung_SEX_CAL2[ind.tr,],family='poisson')
  lv2_lung_SEX_CAL2$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv2_lung_SEX_CAL2$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv2_lung_SEX_CAL2[ind.val,]))
  
  model.quadratic.reg<-lm(sum_LUNG_BZ~poly(YEAR,2), data=lv2_lung_SEX_CAL2[ind.tr,])
  lv2_lung_SEX_CAL2$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv2_lung_SEX_CAL2$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv2_lung_SEX_CAL2[ind.val,])
  
  model.quadratic.reg.FY<-lm(sum_FY_BZ~poly(YEAR,2), data=lv2_lung_SEX_CAL2[ind.tr,])
  lv2_lung_SEX_CAL2$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv2_lung_SEX_CAL2$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv2_lung_SEX_CAL2[ind.val,])
  
  model.linear.spline<-lm(sum_LUNG_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_lung_SEX_CAL2[ind.tr,])
  lv2_lung_SEX_CAL2$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv2_lung_SEX_CAL2$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv2_lung_SEX_CAL2[ind.val,])
  
  model.linear.spline.FY<-lm(sum_FY_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_lung_SEX_CAL2[ind.tr,])
  lv2_lung_SEX_CAL2$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv2_lung_SEX_CAL2$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv2_lung_SEX_CAL2[ind.val,])
}

lv2_lung_test<-as.data.frame(rbind(lv2_lung_total[2:length(lv2_lung_total)],lv2_lung_SEX[3:length(lv2_lung_SEX)],lv2_lung_CAL2[3:length(lv2_lung_CAL2)],
                                    lv2_lung_SEX_CAL2[4:length(lv2_lung_SEX_CAL2)]))
#######################################################################################################3

##################### level 3 data ##########################
############# 1. grouping by UP2 #################
lv3_lung_total<-df_lung1%>%group_by(UP1,UP2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME=paste0("X",UP1,UP2))%>%as_tibble()

year<-c(2019:2022)
name<-unique(lv3_lung_total$NAME)

name2<-cbind(substr(name,2,2),substr(name,3,nchar(name)))
new_dt<-data.frame(rep(name2[,1],each=length(year)),rep(name2[,2],each=length(year)),rep(year,length(unique(lv3_lung_total$UP2))),
                   rep(NA,length(year)*length(unique(lv3_lung_total$UP2))),rep(NA,length(year)*length(unique(lv3_lung_total$UP2))),
                   rep(unique(lv3_lung_total$NAME),each=length(year)))
colnames(new_dt)<-colnames(lv3_lung_total)
lv3_lung_total<-as.data.frame(rbind(lv3_lung_total,new_dt))

lv3_lung_total$simple.reg<-0
lv3_lung_total$poisson.reg<-0
lv3_lung_total$linear.spline<-0
lv3_lung_total$quadratic.reg<-0

lv3_lung_total$simple.reg.FY<-0
lv3_lung_total$poisson.reg.FY<-0
lv3_lung_total$linear.spline.FY<-0
lv3_lung_total$quadratic.reg.FY<-0

name_level<-unique(lv3_lung_total$NAME)

for(i in 1:length(name_level)){
  ind.tr<-which(lv3_lung_total$YEAR<=2018 & lv3_lung_total$NAME==name_level[i])
  ind.val<-which(lv3_lung_total$YEAR>2018 & lv3_lung_total$NAME==name_level[i])
  
  model.simple.reg<-lm(sum_LUNG_BZ~YEAR, data=lv3_lung_total[ind.tr,])
  lv3_lung_total$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv3_lung_total$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv3_lung_total[ind.val,])
  
  model.simple.reg.FY<-lm(sum_FY_BZ~YEAR, data=lv3_lung_total[ind.tr,])
  lv3_lung_total$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv3_lung_total$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv3_lung_total[ind.val,])
  
  model.poisson.reg<-glm(sum_LUNG_BZ~YEAR, data=lv3_lung_total[ind.tr,],family='poisson')
  lv3_lung_total$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv3_lung_total$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv3_lung_total[ind.val,]))
  
  model.poisson.reg.FY<-glm(sum_FY_BZ~YEAR, data=lv3_lung_total[ind.tr,],family='poisson')
  lv3_lung_total$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv3_lung_total$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv3_lung_total[ind.val,]))
  
  model.quadratic.reg<-lm(sum_LUNG_BZ~poly(YEAR,2), data=lv3_lung_total[ind.tr,])
  lv3_lung_total$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv3_lung_total$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv3_lung_total[ind.val,])
  
  model.quadratic.reg.FY<-lm(sum_FY_BZ~poly(YEAR,2), data=lv3_lung_total[ind.tr,])
  lv3_lung_total$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv3_lung_total$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv3_lung_total[ind.val,])
  
  model.linear.spline<-lm(sum_LUNG_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_lung_total[ind.tr,])
  lv3_lung_total$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv3_lung_total$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv3_lung_total[ind.val,])
  
  model.linear.spline.FY<-lm(sum_FY_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_lung_total[ind.tr,])
  lv3_lung_total$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv3_lung_total$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv3_lung_total[ind.val,])
}

##################################################################################
############# 2. grouping by UP2 and SEX #################
lv3_lung_SEX<-df_lung1%>%group_by(UP1,UP2,SEX,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME=paste0("X",UP1,UP2,"_",SEX))%>%as_tibble()

name<-unique(lv3_lung_SEX$NAME)
name2<-cbind(substr(name,2,2),substr(name,3,(nchar(name)-2)))
new_dt<-data.frame(rep(name2[,1],each=length(year)),
                   rep(name2[,2],each=length(year)),
                   rep(unique(lv3_lung_SEX$SEX),each=length(year)),
                   rep(year,length(unique(lv3_lung_SEX$NAME))),
                   rep(NA,length(unique(lv3_lung_SEX$NAME))*length(year)),
                   rep(NA,length(unique(lv3_lung_SEX$NAME))*length(year)),
                   rep(unique(lv3_lung_SEX$NAME),each=length(year)))

colnames(new_dt)<-colnames(lv3_lung_SEX)
lv3_lung_SEX<-as.data.frame(rbind(lv3_lung_SEX,new_dt))

lv3_lung_SEX$simple.reg<-0
lv3_lung_SEX$poisson.reg<-0
lv3_lung_SEX$linear.spline<-0
lv3_lung_SEX$quadratic.reg<-0

lv3_lung_SEX$simple.reg.FY<-0
lv3_lung_SEX$poisson.reg.FY<-0
lv3_lung_SEX$linear.spline.FY<-0
lv3_lung_SEX$quadratic.reg.FY<-0

name_level<-unique(lv3_lung_SEX$NAME)

for(i in 1:length(name_level)){
  ind.tr<-which(lv3_lung_SEX$YEAR<=2018 & lv3_lung_SEX$NAME==name_level[i])
  ind.val<-which(lv3_lung_SEX$YEAR>2018 & lv3_lung_SEX$NAME==name_level[i])
  
  model.simple.reg<-lm(sum_LUNG_BZ~YEAR, data=lv3_lung_SEX[ind.tr,])
  lv3_lung_SEX$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv3_lung_SEX$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv3_lung_SEX[ind.val,])
  
  model.simple.reg.FY<-lm(sum_FY_BZ~YEAR, data=lv3_lung_SEX[ind.tr,])
  lv3_lung_SEX$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv3_lung_SEX$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv3_lung_SEX[ind.val,])
  
  model.poisson.reg<-glm(sum_LUNG_BZ~YEAR, data=lv3_lung_SEX[ind.tr,],family='poisson')
  lv3_lung_SEX$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv3_lung_SEX$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv3_lung_SEX[ind.val,]))
  
  model.poisson.reg.FY<-glm(sum_FY_BZ~YEAR, data=lv3_lung_SEX[ind.tr,],family='poisson')
  lv3_lung_SEX$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv3_lung_SEX$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv3_lung_SEX[ind.val,]))
  
  model.quadratic.reg<-lm(sum_LUNG_BZ~poly(YEAR,2), data=lv3_lung_SEX[ind.tr,])
  lv3_lung_SEX$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv3_lung_SEX$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv3_lung_SEX[ind.val,])
  
  model.quadratic.reg.FY<-lm(sum_FY_BZ~poly(YEAR,2), data=lv3_lung_SEX[ind.tr,])
  lv3_lung_SEX$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv3_lung_SEX$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv3_lung_SEX[ind.val,])
  
  model.linear.spline<-lm(sum_LUNG_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_lung_SEX[ind.tr,])
  lv3_lung_SEX$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv3_lung_SEX$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv3_lung_SEX[ind.val,])
  
  model.linear.spline.FY<-lm(sum_FY_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_lung_SEX[ind.tr,])
  lv3_lung_SEX$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv3_lung_SEX$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv3_lung_SEX[ind.val,])
}

################################################################################################
############### 3. grouping by UP2 and CAL2 ######################### 
lv3_lung_CAL2<-df_lung1%>%group_by(UP1,UP2,CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME=paste0("X",UP1,UP2,"_",CAL2))%>%as_tibble()

name<-unique(lv3_lung_CAL2$NAME)
name2<-as.data.frame(matrix(0,nrow=length(name),ncol=2))

for(i in 1:length(name)){
  name2[i,]<-c(substr(name[i],2,2),substr(name[i],3,(str_locate(name[i],"_")[1]-1)))
}

name2<-cbind(substr(name,2,2),substr(name,3,(nchar(name)-2)))
new_dt<-data.frame(rep(name2[,1],each=length(year)),
                   rep(name2[,2],each=length(year)),
                   rep(unique(lv3_lung_CAL2$CAL2),each=length(year)),
                   rep(year,length(unique(lv3_lung_CAL2$NAME))),
                   rep(NA,length(unique(lv3_lung_CAL2$NAME))*length(year)),
                   rep(NA,length(unique(lv3_lung_CAL2$NAME))*length(year)),
                   rep(unique(lv3_lung_CAL2$NAME),each=length(year)))

colnames(new_dt)<-colnames(lv3_lung_CAL2)
lv3_lung_CAL2<-as.data.frame(rbind(lv3_lung_CAL2,new_dt))

name_level<-unique(lv3_lung_CAL2$NAME)

lv3_lung_CAL2$simple.reg<-0
lv3_lung_CAL2$poisson.reg<-0
lv3_lung_CAL2$linear.spline<-0
lv3_lung_CAL2$quadratic.reg<-0

lv3_lung_CAL2$simple.reg.FY<-0
lv3_lung_CAL2$poisson.reg.FY<-0
lv3_lung_CAL2$linear.spline.FY<-0
lv3_lung_CAL2$quadratic.reg.FY<-0

for(i in 1:length(name_level)){
  ind.tr<-which(lv3_lung_CAL2$YEAR<=2018 & lv3_lung_CAL2$NAME==name_level[i])
  ind.val<-which(lv3_lung_CAL2$YEAR>2018 & lv3_lung_CAL2$NAME==name_level[i])
  
  model.simple.reg<-lm(sum_LUNG_BZ~YEAR, data=lv3_lung_CAL2[ind.tr,])
  lv3_lung_CAL2$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv3_lung_CAL2$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv3_lung_CAL2[ind.val,])
  
  model.simple.reg.FY<-lm(sum_FY_BZ~YEAR, data=lv3_lung_CAL2[ind.tr,])
  lv3_lung_CAL2$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv3_lung_CAL2$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv3_lung_CAL2[ind.val,])
  
  model.poisson.reg<-glm(sum_LUNG_BZ~YEAR, data=lv3_lung_CAL2[ind.tr,],family='poisson')
  lv3_lung_CAL2$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv3_lung_CAL2$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv3_lung_CAL2[ind.val,]))
  
  model.poisson.reg.FY<-glm(sum_FY_BZ~YEAR, data=lv3_lung_CAL2[ind.tr,],family='poisson')
  lv3_lung_CAL2$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv3_lung_CAL2$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv3_lung_CAL2[ind.val,]))
  
  model.quadratic.reg<-lm(sum_LUNG_BZ~poly(YEAR,2), data=lv3_lung_CAL2[ind.tr,])
  lv3_lung_CAL2$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv3_lung_CAL2$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv3_lung_CAL2[ind.val,])
  
  model.quadratic.reg.FY<-lm(sum_FY_BZ~poly(YEAR,2), data=lv3_lung_CAL2[ind.tr,])
  lv3_lung_CAL2$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv3_lung_CAL2$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv3_lung_CAL2[ind.val,])
  
  model.linear.spline<-lm(sum_LUNG_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_lung_CAL2[ind.tr,])
  lv3_lung_CAL2$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv3_lung_CAL2$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv3_lung_CAL2[ind.val,])
  
  model.linear.spline.FY<-lm(sum_FY_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_lung_CAL2[ind.tr,])
  lv3_lung_CAL2$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv3_lung_CAL2$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv3_lung_CAL2[ind.val,])
}

################################################################################################
############### 3. grouping by UP2 and SEX and CAL2 ######################### 
lv3_lung_SEX_CAL2<-df_lung1%>%group_by(UP1,UP2,SEX,CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME=paste0("X",UP1,UP2,"_",SEX,CAL2))%>%as_tibble()

name<-unique(lv3_lung_SEX_CAL2$NAME)
name2<-as.data.frame(matrix(0,nrow=length(name),ncol=2))

for(i in 1:length(name)){
  name2[i,]<-c(substr(name[i],2,2),substr(name[i],3,(str_locate(name[i],"_")[1]-1)))
}

name2<-cbind(substr(name,2,2),substr(name,3,(nchar(name)-2)))
new_dt<-data.frame(rep(name2[,1],each=length(year)),
                   rep(name2[,2],each=length(year)),
                   rep(unique(lv3_lung_SEX_CAL2$SEX),each=length(year)*length(unique(lv3_lung_SEX_CAL2$CAL2))),
                   rep(unique(lv3_lung_SEX_CAL2$CAL2),each=length(year)),
                   rep(year,length(unique(lv3_lung_SEX_CAL2$NAME))),
                   rep(NA,length(unique(lv3_lung_SEX_CAL2$NAME))*length(year)),
                   rep(NA,length(unique(lv3_lung_SEX_CAL2$NAME))*length(year)),
                   rep(unique(lv3_lung_SEX_CAL2$NAME),each=length(year)))

colnames(new_dt)<-colnames(lv3_lung_SEX_CAL2)
lv3_lung_SEX_CAL2<-as.data.frame(rbind(lv3_lung_SEX_CAL2,new_dt))
name_level<-unique(lv3_lung_SEX_CAL2$NAME)

lv3_lung_SEX_CAL2$simple.reg<-0
lv3_lung_SEX_CAL2$poisson.reg<-0
lv3_lung_SEX_CAL2$linear.spline<-0
lv3_lung_SEX_CAL2$quadratic.reg<-0

lv3_lung_SEX_CAL2$simple.reg.FY<-0
lv3_lung_SEX_CAL2$poisson.reg.FY<-0
lv3_lung_SEX_CAL2$linear.spline.FY<-0
lv3_lung_SEX_CAL2$quadratic.reg.FY<-0

for(i in 1:length(name_level)){
  ind.tr<-which(lv3_lung_SEX_CAL2$YEAR<=2018 & lv3_lung_SEX_CAL2$NAME==name_level[i])
  ind.val<-which(lv3_lung_SEX_CAL2$YEAR>2018 & lv3_lung_SEX_CAL2$NAME==name_level[i])
  
  model.simple.reg<-lm(sum_LUNG_BZ~YEAR, data=lv3_lung_SEX_CAL2[ind.tr,])
  lv3_lung_SEX_CAL2$simple.reg[ind.tr]<-model.simple.reg$fitted.values
  lv3_lung_SEX_CAL2$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv3_lung_SEX_CAL2[ind.val,])
  
  model.simple.reg.FY<-lm(sum_FY_BZ~YEAR, data=lv3_lung_SEX_CAL2[ind.tr,])
  lv3_lung_SEX_CAL2$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv3_lung_SEX_CAL2$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv3_lung_SEX_CAL2[ind.val,])
  
  model.poisson.reg<-glm(sum_LUNG_BZ~YEAR, data=lv3_lung_SEX_CAL2[ind.tr,],family='poisson')
  lv3_lung_SEX_CAL2$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv3_lung_SEX_CAL2$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv3_lung_SEX_CAL2[ind.val,]))
  
  model.poisson.reg.FY<-glm(sum_FY_BZ~YEAR, data=lv3_lung_SEX_CAL2[ind.tr,],family='poisson')
  lv3_lung_SEX_CAL2$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
  lv3_lung_SEX_CAL2$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv3_lung_SEX_CAL2[ind.val,]))
  
  model.quadratic.reg<-lm(sum_LUNG_BZ~poly(YEAR,2), data=lv3_lung_SEX_CAL2[ind.tr,])
  lv3_lung_SEX_CAL2$quadratic.reg[ind.tr]<-model.quadratic.reg$fitted.values
  lv3_lung_SEX_CAL2$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv3_lung_SEX_CAL2[ind.val,])
  
  model.quadratic.reg.FY<-lm(sum_FY_BZ~poly(YEAR,2), data=lv3_lung_SEX_CAL2[ind.tr,])
  lv3_lung_SEX_CAL2$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
  lv3_lung_SEX_CAL2$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv3_lung_SEX_CAL2[ind.val,])
  
  model.linear.spline<-lm(sum_LUNG_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_lung_SEX_CAL2[ind.tr,])
  lv3_lung_SEX_CAL2$linear.spline[ind.tr]<-model.linear.spline$fitted.values
  lv3_lung_SEX_CAL2$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv3_lung_SEX_CAL2[ind.val,])
  
  model.linear.spline.FY<-lm(sum_FY_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_lung_SEX_CAL2[ind.tr,])
  lv3_lung_SEX_CAL2$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
  lv3_lung_SEX_CAL2$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv3_lung_SEX_CAL2[ind.val,])
} 

lv3_lung_test<-as.data.frame(rbind(lv3_lung_total[3:length(lv3_lung_total)],lv3_lung_SEX[4:length(lv3_lung_SEX)],lv3_lung_CAL2[4:length(lv3_lung_CAL2)],
                                   lv3_lung_SEX_CAL2[5:length(lv3_lung_SEX_CAL2)]))


####### test_set_lung_total ########
validation_v2 <- read_csv("validation_v2.csv")
ID<-validation_v2[,c("ID","NAME")]
lung_total_test<-as.data.frame(rbind(lv1_lung_test,lv2_lung_test,lv3_lung_test))
lung_total_test<-left_join(lung_total_test,ID,by='NAME')
lung_total_test<-unique(lung_total_test)

colnames(lung_total_test)<-c("YEAR","sum_FY_BZ_Lung","sum_LUNG_BZ","NAME","simple.reg.LUNG","poisson.reg.LUNG",    
                             "linear.spline.LUNG","quadratic.reg.LUNG","simple.reg.lung.FY","poisson.reg.lung.FY","linear.spline.lung.FY","quadratic.reg.lung.FY",
                             "ID" )
lung_total_test<-lung_total_test[,c('ID','NAME',"YEAR","sum_LUNG_BZ","simple.reg.LUNG","poisson.reg.LUNG",
                             "linear.spline.LUNG","quadratic.reg.LUNG", "sum_FY_BZ_Lung","simple.reg.lung.FY","poisson.reg.lung.FY","linear.spline.lung.FY",
                             "quadratic.reg.lung.FY")]

test.dt<-merge(leukemia_total_test,lung_total_test,key=c("NAME","YEAR"),all=TRUE)

test.dt.change<-test.dt

col.idx<-c(5:12,15:length(test.dt.change))

for(i in 1:nrow(test.dt.change)){
  for(j in 1:length(col.idx)){
    test.dt.change[i,col.idx[j]]<-ifelse(test.dt.change[i,col.idx[j]]<0,0,test.dt.change[i,col.idx[j]])
  }
}

write.csv(test.dt,file="test.csv",row.names=FALSE)
write.csv(test.dt.change,file="test_change_to_zero.csv",row.names = FALSE)
