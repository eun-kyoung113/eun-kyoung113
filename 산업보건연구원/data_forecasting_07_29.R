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

lv1_leukemia_total$simple.reg<-0
lv1_leukemia_total$linear.spline<-0
lv1_leukemia_total$quadratic.reg<-0


ind.tr<-which(lv1_leukemia_total$YEAR<=2014)
ind.val<-which(lv1_leukemia_total$YEAR>2014)

model.simple.reg<-lm(sum_LEUKEMIA_BZ~YEAR, data=lv1_leukemia_total[ind.tr,])
lv1_leukemia_total$simple.reg[ind.tr]<-ifelse(model.simple.reg$fitted.values<0, 0, model.simple.reg$fitted.values)
lv1_leukemia_total$simple.reg[ind.val]<-ifelse(predict(model.simple.reg,newdata=lv1_leukemia_total[ind.val,])<0,0,
                                               predict(model.simple.reg,newdata=lv1_leukemia_total[ind.val,]))



model.quadratic.reg<-lm(sum_LEUKEMIA_BZ~poly(YEAR,2), data=lv1_leukemia_total[ind.tr,])
lv1_leukemia_total$quadratic.reg[ind.tr]<-ifelse(model.quadratic.reg$fitted.values<0, 0, model.quadratic.reg$fitted.values)
lv1_leukemia_total$quadratic.reg[ind.val]<-ifelse(predict(model.quadratic.reg,newdata=lv1_leukemia_total[ind.val,])<0,0,
                                               predict(model.quadratic.reg,newdata=lv1_leukemia_total[ind.val,]))

model.linear.spline<-lm(sum_LEUKEMIA_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_leukemia_total[ind.tr,])
lv1_leukemia_total$linear.spline[ind.tr]<-ifelse(model.linear.spline$fitted.values<0, 0, model.linear.spline$fitted.values)
lv1_leukemia_total$linear.spline[ind.val]<-ifelse(predict(model.linear.spline,newdata=lv1_leukemia_total[ind.val,])<0,0,
                                                  predict(model.linear.spline,newdata=lv1_leukemia_total[ind.val,]))

MAPE_lv1<-matrix(0,nrow=9,ncol=4)
colnames(MAPE_lv1)<-c("NAME","simple reg","linear spline","quadratic reg")

RMSE_lv1<-matrix(0,nrow=9,ncol=4)
colnames(RMSE_lv1)<-c("NAME","simple reg","linear spline","quadratic reg")

MAPE_lv1[1,]<-c(unique(lv1_leukemia_total$NAME),
                round(mean(abs(lv1_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_total$simple.reg[ind.val])/lv1_leukemia_total$sum_LEUKEMIA_BZ[ind.val])*100,2),
                round(mean(abs(lv1_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_total$linear.spline[ind.val])/lv1_leukemia_total$sum_LEUKEMIA_BZ[ind.val])*100,2),
                round(mean(abs(lv1_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_total$quadratic.reg[ind.val])/lv1_leukemia_total$sum_LEUKEMIA_BZ[ind.val])*100,2))
RMSE_lv1[1,]<-c(unique(lv1_leukemia_total$NAME),
                round(sqrt(mean(abs(lv1_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_total$simple.reg[ind.val]))),2),
                round(sqrt(mean(abs(lv1_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_total$linear.spline[ind.val]))),2),
                round(sqrt(mean(abs(lv1_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_total$quadratic.reg[ind.val]))),2))

ggplot()+geom_line(data=lv1_leukemia_total,aes(x=YEAR,y=sum_LEUKEMIA_BZ,color="red"),size=1)+labs(y="rate_of_leukemia")+
  geom_line(data=lv1_leukemia_total,aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv1_leukemia_total,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv1_leukemia_total,aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  labs(x="Year")+
  scale_color_identity(name='rate of leukemia',
                       breaks=c("red","black","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))



####################################################################################
#################### 2. grouping by SEX ##################
lv1_leukemia_SEX<-df_leukemia1%>%group_by(SEX,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  mutate(NAME=paste0("X",SEX))%>%as_tibble()

SEX_level<-unique(lv1_leukemia_SEX$SEX)

lv1_leukemia_SEX$simple.reg<-0
lv1_leukemia_SEX$linear.spline<-0
lv1_leukemia_SEX$quadratic.reg<-0
SEX_level<-unique(lv1_leukemia_SEX$SEX)

for(i in 1:length(SEX_level)){
  ind.tr<-which(lv1_leukemia_SEX$YEAR<=2014 & lv1_leukemia_SEX$SEX==SEX_level[i])
  ind.val<-which(lv1_leukemia_SEX$YEAR>2014 & lv1_leukemia_SEX$SEX==SEX_level[i])

  model.simple.reg<-lm(sum_LEUKEMIA_BZ~YEAR, data=lv1_leukemia_SEX[ind.tr,])
  lv1_leukemia_SEX$simple.reg[ind.tr]<-ifelse(model.simple.reg$fitted.values<0, 0, model.simple.reg$fitted.values)
  lv1_leukemia_SEX$simple.reg[ind.val]<-ifelse(predict(model.simple.reg,newdata=lv1_leukemia_SEX[ind.val,])<0,0,
                                               predict(model.simple.reg,newdata=lv1_leukemia_SEX[ind.val,]))



  model.quadratic.reg<-lm(sum_LEUKEMIA_BZ~poly(YEAR,2), data=lv1_leukemia_SEX[ind.tr,])
  lv1_leukemia_SEX$quadratic.reg[ind.tr]<-ifelse(model.quadratic.reg$fitted.values<0, 0, model.quadratic.reg$fitted.values)
  lv1_leukemia_SEX$quadratic.reg[ind.val]<-ifelse(predict(model.quadratic.reg,newdata=lv1_leukemia_SEX[ind.val,])<0,0,
                                                  predict(model.quadratic.reg,newdata=lv1_leukemia_SEX[ind.val,]))

  model.linear.spline<-lm(sum_LEUKEMIA_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_leukemia_SEX[ind.tr,])
  lv1_leukemia_SEX$linear.spline[ind.tr]<-ifelse(model.linear.spline$fitted.values<0, 0, model.linear.spline$fitted.values)
  lv1_leukemia_SEX$linear.spline[ind.val]<-ifelse(predict(model.linear.spline,newdata=lv1_leukemia_SEX[ind.val,])<0,0,
                                                  predict(model.linear.spline,newdata=lv1_leukemia_SEX[ind.val,]))



  MAPE_lv1[(i+1),]<-c(unique(lv1_leukemia_SEX$NAME[ind.tr]),
                round(mean(abs(lv1_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_SEX$simple.reg[ind.val])/lv1_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val])*100,2),
                round(mean(abs(lv1_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_SEX$linear.spline[ind.val])/lv1_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val])*100,2),
                round(mean(abs(lv1_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_SEX$quadratic.reg[ind.val])/lv1_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val])*100,2))
  
  RMSE_lv1[(i+1),]<-c(unique(lv1_leukemia_SEX$NAME[ind.tr]),
                  round(sqrt(mean(abs(lv1_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_SEX$simple.reg[ind.val]))),2),
                  round(sqrt(mean(abs(lv1_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_SEX$linear.spline[ind.val]))),2),
                  round(sqrt(mean(abs(lv1_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_SEX$quadratic.reg[ind.val]))),2))
  
}



ggplot()+geom_line(data=lv1_leukemia_SEX,aes(x=YEAR,y=sum_LEUKEMIA_BZ,color="red"),size=1)+labs(y="rate_of_leukemia")+
  facet_wrap(~ factor(SEX,levels=SEX_level),scales="free_y")+
  geom_line(data=lv1_leukemia_SEX,aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv1_leukemia_SEX,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv1_leukemia_SEX,aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  labs(x="Year")+
  scale_color_identity(name='rate of leukemia',
                       breaks=c("red","black","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))



################################################################################################
############### 3. grouping by CAL2 ######################### 
lv1_leukemia_CAL2<-df_leukemia1%>%group_by(CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  mutate(NAME=paste0("X",CAL2))%>%as_tibble()

CAL2_level<-unique(lv1_leukemia_CAL2$CAL2)

lv1_leukemia_CAL2$simple.reg<-0
lv1_leukemia_CAL2$linear.spline<-0
lv1_leukemia_CAL2$quadratic.reg<-0
CAL2_level<-unique(lv1_leukemia_CAL2$CAL2)

for(i in 1:length(CAL2_level)){
  ind.tr<-which(lv1_leukemia_CAL2$YEAR<=2014 & lv1_leukemia_CAL2$CAL2==CAL2_level[i])
  ind.val<-which(lv1_leukemia_CAL2$YEAR>2014 & lv1_leukemia_CAL2$CAL2==CAL2_level[i])
  
  model.simple.reg<-lm(sum_LEUKEMIA_BZ~YEAR, data=lv1_leukemia_CAL2[ind.tr,])
  lv1_leukemia_CAL2$simple.reg[ind.tr]<-ifelse(model.simple.reg$fitted.values<0, 0, model.simple.reg$fitted.values)
  lv1_leukemia_CAL2$simple.reg[ind.val]<-ifelse(predict(model.simple.reg,newdata=lv1_leukemia_CAL2[ind.val,])<0,0,
                                               predict(model.simple.reg,newdata=lv1_leukemia_CAL2[ind.val,]))
  
  
  
  model.quadratic.reg<-lm(sum_LEUKEMIA_BZ~poly(YEAR,2), data=lv1_leukemia_CAL2[ind.tr,])
  lv1_leukemia_CAL2$quadratic.reg[ind.tr]<-ifelse(model.quadratic.reg$fitted.values<0, 0, model.quadratic.reg$fitted.values)
  lv1_leukemia_CAL2$quadratic.reg[ind.val]<-ifelse(predict(model.quadratic.reg,newdata=lv1_leukemia_CAL2[ind.val,])<0,0,
                                                  predict(model.quadratic.reg,newdata=lv1_leukemia_CAL2[ind.val,]))
  
  model.linear.spline<-lm(sum_LEUKEMIA_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_leukemia_CAL2[ind.tr,])
  lv1_leukemia_CAL2$linear.spline[ind.tr]<-ifelse(model.linear.spline$fitted.values<0, 0, model.linear.spline$fitted.values)
  lv1_leukemia_CAL2$linear.spline[ind.val]<-ifelse(predict(model.linear.spline,newdata=lv1_leukemia_CAL2[ind.val,])<0,0,
                                                  predict(model.linear.spline,newdata=lv1_leukemia_CAL2[ind.val,]))
  
  
  
  MAPE_lv1[(i+3),]<-c(unique(lv1_leukemia_CAL2$NAME[ind.tr]),
                      round(mean(abs(lv1_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_CAL2$simple.reg[ind.val])/lv1_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2),
                      round(mean(abs(lv1_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_CAL2$linear.spline[ind.val])/lv1_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2),
                      round(mean(abs(lv1_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_CAL2$quadratic.reg[ind.val])/lv1_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2))
  
  RMSE_lv1[(i+3),]<-c(unique(lv1_leukemia_CAL2$NAME[ind.tr]),
                      round(sqrt(mean(abs(lv1_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_CAL2$simple.reg[ind.val]))),2),
                      round(sqrt(mean(abs(lv1_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_CAL2$linear.spline[ind.val]))),2),
                      round(sqrt(mean(abs(lv1_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_CAL2$quadratic.reg[ind.val]))),2))
  
  
}



ggplot()+geom_line(data=lv1_leukemia_CAL2,aes(x=YEAR,y=sum_LEUKEMIA_BZ,color="red"),size=1)+labs(y="rate_of_leukemia")+
  facet_wrap(~ factor(CAL2,levels=CAL2_level),scales="free_y")+
  geom_line(data=lv1_leukemia_CAL2,aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv1_leukemia_CAL2,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv1_leukemia_CAL2,aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  labs(x="Year")+
  scale_color_identity(name='rate of leukemia',
                       breaks=c("red","black","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


################################################################################################
############### 4. grouping by SEX and CAL2 ######################### 
lv1_leukemia_SEX_CAL2<-df_leukemia1%>%group_by(SEX,CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  mutate(NAME=paste0("X",SEX,CAL2))%>%as_tibble()

SEX_CAL2_level<-unique(lv1_leukemia_SEX_CAL2$NAME)

lv1_leukemia_SEX_CAL2$simple.reg<-0
lv1_leukemia_SEX_CAL2$linear.spline<-0
lv1_leukemia_SEX_CAL2$quadratic.reg<-0


for(i in 1:length(SEX_CAL2_level)){
  ind.tr<-which(lv1_leukemia_SEX_CAL2$YEAR<=2014 & lv1_leukemia_SEX_CAL2$NAME==SEX_CAL2_level[i])
  ind.val<-which(lv1_leukemia_SEX_CAL2$YEAR>2014 & lv1_leukemia_SEX_CAL2$NAME==SEX_CAL2_level[i])
  
  model.simple.reg<-lm(sum_LEUKEMIA_BZ~YEAR, data=lv1_leukemia_SEX_CAL2[ind.tr,])
  lv1_leukemia_SEX_CAL2$simple.reg[ind.tr]<-ifelse(model.simple.reg$fitted.values<0, 0, model.simple.reg$fitted.values)
  lv1_leukemia_SEX_CAL2$simple.reg[ind.val]<-ifelse(predict(model.simple.reg,newdata=lv1_leukemia_SEX_CAL2[ind.val,])<0,0,
                                                predict(model.simple.reg,newdata=lv1_leukemia_SEX_CAL2[ind.val,]))
  
  
  
  model.quadratic.reg<-lm(sum_LEUKEMIA_BZ~poly(YEAR,2), data=lv1_leukemia_SEX_CAL2[ind.tr,])
  lv1_leukemia_SEX_CAL2$quadratic.reg[ind.tr]<-ifelse(model.quadratic.reg$fitted.values<0, 0, model.quadratic.reg$fitted.values)
  lv1_leukemia_SEX_CAL2$quadratic.reg[ind.val]<-ifelse(predict(model.quadratic.reg,newdata=lv1_leukemia_SEX_CAL2[ind.val,])<0,0,
                                                   predict(model.quadratic.reg,newdata=lv1_leukemia_SEX_CAL2[ind.val,]))
  
  model.linear.spline<-lm(sum_LEUKEMIA_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_leukemia_SEX_CAL2[ind.tr,])
  lv1_leukemia_SEX_CAL2$linear.spline[ind.tr]<-ifelse(model.linear.spline$fitted.values<0, 0, model.linear.spline$fitted.values)
  lv1_leukemia_SEX_CAL2$linear.spline[ind.val]<-ifelse(predict(model.linear.spline,newdata=lv1_leukemia_SEX_CAL2[ind.val,])<0,0,
                                                   predict(model.linear.spline,newdata=lv1_leukemia_SEX_CAL2[ind.val,]))
  
  
  
  MAPE_lv1[(i+5),]<-c(unique(lv1_leukemia_SEX_CAL2$NAME[ind.tr]),
                      round(mean(abs(lv1_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_SEX_CAL2$simple.reg[ind.val])/lv1_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2),
                      round(mean(abs(lv1_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_SEX_CAL2$linear.spline[ind.val])/lv1_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2),
                      round(mean(abs(lv1_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_SEX_CAL2$quadratic.reg[ind.val])/lv1_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2))
  
  RMSE_lv1[(i+5),]<-c(unique(lv1_leukemia_SEX_CAL2$NAME[ind.tr]),
                      round(sqrt(mean(abs(lv1_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_SEX_CAL2$simple.reg[ind.val]))),2),
                      round(sqrt(mean(abs(lv1_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_SEX_CAL2$linear.spline[ind.val]))),2),
                      round(sqrt(mean(abs(lv1_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_SEX_CAL2$quadratic.reg[ind.val]))),2))
  
}



ggplot()+geom_line(data=lv1_leukemia_SEX_CAL2,aes(x=YEAR,y=sum_LEUKEMIA_BZ,color="red"),size=1)+labs(y="rate_of_leukemia")+
  facet_wrap(~ factor(NAME,levels=SEX_CAL2_level),scales="free_y")+
  geom_line(data=lv1_leukemia_SEX_CAL2,aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv1_leukemia_SEX_CAL2,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv1_leukemia_SEX_CAL2,aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  labs(x="Year")+
  scale_color_identity(name='rate of leukemia',
                       breaks=c("red","black","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

value_eklee_lv1<-as.data.frame(rbind(lv1_leukemia_total,lv1_leukemia_SEX[2:length(lv1_leukemia_SEX)],lv1_leukemia_CAL2[2:length(lv1_leukemia_CAL2)],
                                 lv1_leukemia_SEX_CAL2[3:length(lv1_leukemia_SEX_CAL2)]))




##################### level 2 data ##########################
############# 1. grouping by UP1 #################
lv2_leukemia_total<-df_leukemia1%>%group_by(UP1,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  mutate(NAME=paste0("X",UP1))%>%as_tibble()

MAPE_lv2<-matrix(0,nrow=length(unique(lv2_leukemia_total$NAME))*(1+2+2+4),ncol=4)
colnames(MAPE_lv2)<-c("NAME","simple_reg","linear_spline","quadratic_reg")

RMSE_lv2<-matrix(0,nrow=length(unique(lv2_leukemia_total$NAME))*(1+2+2+4),ncol=4)
colnames(RMSE_lv2)<-c("NAME","simple_reg","linear_spline","quadratic_reg")

lv2_leukemia_total$simple.reg<-0
lv2_leukemia_total$linear.spline<-0
lv2_leukemia_total$quadratic.reg<-0

name_level<-unique(lv2_leukemia_total$NAME)

for(i in 1:length(name_level)){
  ind.tr<-which(lv2_leukemia_total$YEAR<=2014 & lv2_leukemia_total$NAME==name_level[i])
  ind.val<-which(lv2_leukemia_total$YEAR>2014 & lv2_leukemia_total$NAME==name_level[i])
  
  model.simple.reg<-lm(sum_LEUKEMIA_BZ~YEAR, data=lv2_leukemia_total[ind.tr,])
  lv2_leukemia_total$simple.reg[ind.tr]<-ifelse(model.simple.reg$fitted.values<0, 0, model.simple.reg$fitted.values)
  lv2_leukemia_total$simple.reg[ind.val]<-ifelse(predict(model.simple.reg,newdata=lv2_leukemia_total[ind.val,])<0,0,
                                                 predict(model.simple.reg,newdata=lv2_leukemia_total[ind.val,]))
  
  
  
  model.quadratic.reg<-lm(sum_LEUKEMIA_BZ~poly(YEAR,2), data=lv2_leukemia_total[ind.tr,])
  lv2_leukemia_total$quadratic.reg[ind.tr]<-ifelse(model.quadratic.reg$fitted.values<0, 0, model.quadratic.reg$fitted.values)
  lv2_leukemia_total$quadratic.reg[ind.val]<-ifelse(predict(model.quadratic.reg,newdata=lv2_leukemia_total[ind.val,])<0,0,
                                                    predict(model.quadratic.reg,newdata=lv2_leukemia_total[ind.val,]))
  
  model.linear.spline<-lm(sum_LEUKEMIA_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_leukemia_total[ind.tr,])
  lv2_leukemia_total$linear.spline[ind.tr]<-ifelse(model.linear.spline$fitted.values<0, 0, model.linear.spline$fitted.values)
  lv2_leukemia_total$linear.spline[ind.val]<-ifelse(predict(model.linear.spline,newdata=lv2_leukemia_total[ind.val,])<0,0,
                                                    predict(model.linear.spline,newdata=lv2_leukemia_total[ind.val,]))
  

  MAPE_lv2[i,]<-c(unique(lv2_leukemia_total$NAME[ind.tr]),
                  round(mean(abs(lv2_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_total$simple.reg[ind.val])/lv2_leukemia_total$sum_LEUKEMIA_BZ[ind.val])*100,2),
                  round(mean(abs(lv2_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_total$linear.spline[ind.val])/lv2_leukemia_total$sum_LEUKEMIA_BZ[ind.val])*100,2),
                  round(mean(abs(lv2_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_total$quadratic.reg[ind.val])/lv2_leukemia_total$sum_LEUKEMIA_BZ[ind.val])*100,2))
  RMSE_lv2[i,]<-c(unique(lv2_leukemia_total$NAME[ind.tr]),
                  round(sqrt(mean(abs(lv2_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_total$simple.reg[ind.val]))),2),
                  round(sqrt(mean(abs(lv2_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_total$linear.spline[ind.val]))),2),
                  round(sqrt(mean(abs(lv2_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_total$quadratic.reg[ind.val]))),2))
  
  
}

ggplot()+geom_line(data=lv2_leukemia_total,aes(x=YEAR,y=sum_LEUKEMIA_BZ,color="red"),size=1)+labs(y="rate_of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(lv2_leukemia_total$NAME)),scales="free_y")+
  geom_line(data=lv2_leukemia_total,aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv2_leukemia_total,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv2_leukemia_total,aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv2),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of leukemia',
                       breaks=c("red","black","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))



##################################################################################
############# 2. grouping by UP1 and SEX #################
lv2_leukemia_SEX<-df_leukemia1%>%group_by(UP1,SEX,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  mutate(NAME=paste0("X",UP1,SEX))%>%as_tibble()

lv2_leukemia_SEX$simple.reg<-0
lv2_leukemia_SEX$linear.spline<-0
lv2_leukemia_SEX$quadratic.reg<-0

name_level<-unique(lv2_leukemia_SEX$NAME)

for(i in 1:length(name_level)){
  ind.tr<-which(lv2_leukemia_SEX$YEAR<=2014 & lv2_leukemia_SEX$NAME==name_level[i])
  ind.val<-which(lv2_leukemia_SEX$YEAR>2014 & lv2_leukemia_SEX$NAME==name_level[i])
  
  model.simple.reg<-lm(sum_LEUKEMIA_BZ~YEAR, data=lv2_leukemia_SEX[ind.tr,])
  lv2_leukemia_SEX$simple.reg[ind.tr]<-ifelse(model.simple.reg$fitted.values<0, 0, model.simple.reg$fitted.values)
  lv2_leukemia_SEX$simple.reg[ind.val]<-ifelse(predict(model.simple.reg,newdata=lv2_leukemia_SEX[ind.val,])<0,0,
                                                 predict(model.simple.reg,newdata=lv2_leukemia_SEX[ind.val,]))
  
  
  
  model.quadratic.reg<-lm(sum_LEUKEMIA_BZ~poly(YEAR,2), data=lv2_leukemia_SEX[ind.tr,])
  lv2_leukemia_SEX$quadratic.reg[ind.tr]<-ifelse(model.quadratic.reg$fitted.values<0, 0, model.quadratic.reg$fitted.values)
  lv2_leukemia_SEX$quadratic.reg[ind.val]<-ifelse(predict(model.quadratic.reg,newdata=lv2_leukemia_SEX[ind.val,])<0,0,
                                                    predict(model.quadratic.reg,newdata=lv2_leukemia_SEX[ind.val,]))
  
  model.linear.spline<-lm(sum_LEUKEMIA_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_leukemia_SEX[ind.tr,])
  lv2_leukemia_SEX$linear.spline[ind.tr]<-ifelse(model.linear.spline$fitted.values<0, 0, model.linear.spline$fitted.values)
  lv2_leukemia_SEX$linear.spline[ind.val]<-ifelse(predict(model.linear.spline,newdata=lv2_leukemia_SEX[ind.val,])<0,0,
                                                    predict(model.linear.spline,newdata=lv2_leukemia_SEX[ind.val,]))
  
  
  MAPE_lv2[(i+19),]<-c(unique(lv2_leukemia_SEX$NAME[ind.tr]),
                  round(mean(abs(lv2_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_SEX$simple.reg[ind.val])/lv2_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val])*100,2),
                  round(mean(abs(lv2_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_SEX$linear.spline[ind.val])/lv2_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val])*100,2),
                  round(mean(abs(lv2_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_SEX$quadratic.reg[ind.val])/lv2_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val])*100,2))
  RMSE_lv2[(i+19),]<-c(unique(lv2_leukemia_SEX$NAME[ind.tr]),
                  round(sqrt(mean(abs(lv2_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_SEX$simple.reg[ind.val]))),2),
                  round(sqrt(mean(abs(lv2_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_SEX$linear.spline[ind.val]))),2),
                  round(sqrt(mean(abs(lv2_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_SEX$quadratic.reg[ind.val]))),2))
  
  
}

ggplot()+geom_line(data=lv2_leukemia_SEX,aes(x=YEAR,y=sum_LEUKEMIA_BZ,color="red"),size=1)+labs(y="rate_of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(lv2_leukemia_SEX$NAME)),scales="free_y")+
  geom_line(data=lv2_leukemia_SEX,aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv2_leukemia_SEX,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv2_leukemia_SEX,aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv2[20:57,]),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2[20:57,]),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2[20:57,]),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of leukemia',
                       breaks=c("red","black","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))



################################################################################################
############### 3. grouping by UP1 and CAL2 ######################### 
lv2_leukemia_CAL2<-df_leukemia1%>%group_by(UP1,CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  mutate(NAME=paste0("X",UP1,CAL2))%>%as_tibble()

name_level<-unique(lv2_leukemia_CAL2$NAME)

lv2_leukemia_CAL2$simple.reg<-0
lv2_leukemia_CAL2$linear.spline<-0
lv2_leukemia_CAL2$quadratic.reg<-0

for(i in 1:length(name_level)){
  ind.tr<-which(lv2_leukemia_CAL2$YEAR<=2014 & lv2_leukemia_CAL2$NAME==name_level[i])
  ind.val<-which(lv2_leukemia_CAL2$YEAR>2014 & lv2_leukemia_CAL2$NAME==name_level[i])
  
  model.simple.reg<-lm(sum_LEUKEMIA_BZ~YEAR, data=lv2_leukemia_CAL2[ind.tr,])
  lv2_leukemia_CAL2$simple.reg[ind.tr]<-ifelse(model.simple.reg$fitted.values<0, 0, model.simple.reg$fitted.values)
  lv2_leukemia_CAL2$simple.reg[ind.val]<-ifelse(predict(model.simple.reg,newdata=lv2_leukemia_CAL2[ind.val,])<0,0,
                                                predict(model.simple.reg,newdata=lv2_leukemia_CAL2[ind.val,]))
  
  
  
  model.quadratic.reg<-lm(sum_LEUKEMIA_BZ~poly(YEAR,2), data=lv2_leukemia_CAL2[ind.tr,])
  lv2_leukemia_CAL2$quadratic.reg[ind.tr]<-ifelse(model.quadratic.reg$fitted.values<0, 0, model.quadratic.reg$fitted.values)
  lv2_leukemia_CAL2$quadratic.reg[ind.val]<-ifelse(predict(model.quadratic.reg,newdata=lv2_leukemia_CAL2[ind.val,])<0,0,
                                                   predict(model.quadratic.reg,newdata=lv2_leukemia_CAL2[ind.val,]))
  
  model.linear.spline<-lm(sum_LEUKEMIA_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_leukemia_CAL2[ind.tr,])
  lv2_leukemia_CAL2$linear.spline[ind.tr]<-ifelse(model.linear.spline$fitted.values<0, 0, model.linear.spline$fitted.values)
  lv2_leukemia_CAL2$linear.spline[ind.val]<-ifelse(predict(model.linear.spline,newdata=lv2_leukemia_CAL2[ind.val,])<0,0,
                                                   predict(model.linear.spline,newdata=lv2_leukemia_CAL2[ind.val,]))
  
  
  
  MAPE_lv2[(i+57),]<-c(unique(lv2_leukemia_CAL2$NAME[ind.tr]),
                      round(mean(abs(lv2_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_CAL2$simple.reg[ind.val])/lv2_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2),
                      round(mean(abs(lv2_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_CAL2$linear.spline[ind.val])/lv2_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2),
                      round(mean(abs(lv2_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_CAL2$quadratic.reg[ind.val])/lv2_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2))
  
  RMSE_lv2[(i+57),]<-c(unique(lv2_leukemia_CAL2$NAME[ind.tr]),
                      round(sqrt(mean(abs(lv2_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_CAL2$simple.reg[ind.val]))),2),
                      round(sqrt(mean(abs(lv2_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_CAL2$linear.spline[ind.val]))),2),
                      round(sqrt(mean(abs(lv2_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_CAL2$quadratic.reg[ind.val]))),2))
  
  
}



ggplot()+geom_line(data=lv2_leukemia_CAL2,aes(x=YEAR,y=sum_LEUKEMIA_BZ,color="red"),size=1)+labs(y="rate_of_leukemia")+
  facet_wrap(~ factor(NAME,levels=name_level),scales="free_y")+
  geom_line(data=lv2_leukemia_CAL2,aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv2_leukemia_CAL2,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv2_leukemia_CAL2,aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv2[58:95,]),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2[58:95,]),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2[58:95,]),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of leukemia',
                       breaks=c("red","black","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))




################################################################################################
############### 3. grouping by UP1 and SEX and CAL2 ######################### 
lv2_leukemia_SEX_CAL2<-df_leukemia1%>%group_by(UP1,SEX,CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  mutate(NAME=paste0("X",UP1,SEX,CAL2))%>%as_tibble()

name_level<-unique(lv2_leukemia_SEX_CAL2$NAME)

lv2_leukemia_SEX_CAL2$simple.reg<-0
lv2_leukemia_SEX_CAL2$linear.spline<-0
lv2_leukemia_SEX_CAL2$quadratic.reg<-0

for(i in 1:length(name_level)){
  ind.tr<-which(lv2_leukemia_SEX_CAL2$YEAR<=2014 & lv2_leukemia_SEX_CAL2$NAME==name_level[i])
  ind.val<-which(lv2_leukemia_SEX_CAL2$YEAR>2014 & lv2_leukemia_SEX_CAL2$NAME==name_level[i])
  
  model.simple.reg<-lm(sum_LEUKEMIA_BZ~YEAR, data=lv2_leukemia_SEX_CAL2[ind.tr,])
  lv2_leukemia_SEX_CAL2$simple.reg[ind.tr]<-ifelse(model.simple.reg$fitted.values<0, 0, model.simple.reg$fitted.values)
  lv2_leukemia_SEX_CAL2$simple.reg[ind.val]<-ifelse(predict(model.simple.reg,newdata=lv2_leukemia_SEX_CAL2[ind.val,])<0,0,
                                                predict(model.simple.reg,newdata=lv2_leukemia_SEX_CAL2[ind.val,]))
  
  
  
  model.quadratic.reg<-lm(sum_LEUKEMIA_BZ~poly(YEAR,2), data=lv2_leukemia_SEX_CAL2[ind.tr,])
  lv2_leukemia_SEX_CAL2$quadratic.reg[ind.tr]<-ifelse(model.quadratic.reg$fitted.values<0, 0, model.quadratic.reg$fitted.values)
  lv2_leukemia_SEX_CAL2$quadratic.reg[ind.val]<-ifelse(predict(model.quadratic.reg,newdata=lv2_leukemia_SEX_CAL2[ind.val,])<0,0,
                                                   predict(model.quadratic.reg,newdata=lv2_leukemia_SEX_CAL2[ind.val,]))
  
  model.linear.spline<-lm(sum_LEUKEMIA_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_leukemia_SEX_CAL2[ind.tr,])
  lv2_leukemia_SEX_CAL2$linear.spline[ind.tr]<-ifelse(model.linear.spline$fitted.values<0, 0, model.linear.spline$fitted.values)
  lv2_leukemia_SEX_CAL2$linear.spline[ind.val]<-ifelse(predict(model.linear.spline,newdata=lv2_leukemia_SEX_CAL2[ind.val,])<0,0,
                                                   predict(model.linear.spline,newdata=lv2_leukemia_SEX_CAL2[ind.val,]))
  
  
  
  MAPE_lv2[(i+95),]<-c(unique(lv2_leukemia_SEX_CAL2$NAME[ind.tr]),
                       round(mean(abs(lv2_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_SEX_CAL2$simple.reg[ind.val])/lv2_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2),
                       round(mean(abs(lv2_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_SEX_CAL2$linear.spline[ind.val])/lv2_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2),
                       round(mean(abs(lv2_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_SEX_CAL2$quadratic.reg[ind.val])/lv2_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2))
  
  RMSE_lv2[(i+95),]<-c(unique(lv2_leukemia_SEX_CAL2$NAME[ind.tr]),
                       round(sqrt(mean(abs(lv2_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_SEX_CAL2$simple.reg[ind.val]))),2),
                       round(sqrt(mean(abs(lv2_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_SEX_CAL2$linear.spline[ind.val]))),2),
                       round(sqrt(mean(abs(lv2_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_SEX_CAL2$quadratic.reg[ind.val]))),2))
  
  
}



ggplot()+geom_line(data=lv2_leukemia_SEX_CAL2,aes(x=YEAR,y=sum_LEUKEMIA_BZ,color="red"),size=1)+labs(y="rate_of_leukemia")+
  facet_wrap(~ factor(NAME,levels=name_level),scales="free_y")+
  geom_line(data=lv2_leukemia_SEX_CAL2,aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv2_leukemia_SEX_CAL2,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv2_leukemia_SEX_CAL2,aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv2[96:171,]),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2[96:171,]),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2[96:171,]),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of leukemia',
                       breaks=c("red","black","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


value_eklee_lv2<-as.data.frame(rbind(lv2_leukemia_total[2:length(lv2_leukemia_total)],lv2_leukemia_SEX[3:length(lv2_leukemia_SEX)],lv2_leukemia_CAL2[3:length(lv2_leukemia_CAL2)],
                                     lv2_leukemia_SEX_CAL2[4:length(lv2_leukemia_SEX_CAL2)]))



#######################################################################################################3
##################### level 3 data ##########################
############# 1. grouping by UP2 #################
lv3_leukemia_total<-df_leukemia1%>%group_by(UP2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  mutate(NAME=paste0("X",UP2))%>%as_tibble()

MAPE_lv3<-matrix(0,nrow=length(unique(lv3_leukemia_total$NAME))*(1+2+2+4),ncol=4)
colnames(MAPE_lv3)<-c("NAME","simple_reg","linear_spline","quadratic_reg")

RMSE_lv3<-matrix(0,nrow=length(unique(lv3_leukemia_total$NAME))*(1+2+2+4),ncol=4)
colnames(RMSE_lv3)<-c("NAME","simple_reg","linear_spline","quadratic_reg")

lv3_leukemia_total$simple.reg<-0
lv3_leukemia_total$linear.spline<-0
lv3_leukemia_total$quadratic.reg<-0

name_level<-unique(lv3_leukemia_total$NAME)

for(i in 1:length(name_level)){
  ind.tr<-which(lv3_leukemia_total$YEAR<=2014 & lv3_leukemia_total$NAME==name_level[i])
  ind.val<-which(lv3_leukemia_total$YEAR>2014 & lv3_leukemia_total$NAME==name_level[i])
  
  model.simple.reg<-lm(sum_LEUKEMIA_BZ~YEAR, data=lv3_leukemia_total[ind.tr,])
  lv3_leukemia_total$simple.reg[ind.tr]<-ifelse(model.simple.reg$fitted.values<0, 0, model.simple.reg$fitted.values)
  lv3_leukemia_total$simple.reg[ind.val]<-ifelse(predict(model.simple.reg,newdata=lv3_leukemia_total[ind.val,])<0,0,
                                                 predict(model.simple.reg,newdata=lv3_leukemia_total[ind.val,]))
  
  
  
  model.quadratic.reg<-lm(sum_LEUKEMIA_BZ~poly(YEAR,2), data=lv3_leukemia_total[ind.tr,])
  lv3_leukemia_total$quadratic.reg[ind.tr]<-ifelse(model.quadratic.reg$fitted.values<0, 0, model.quadratic.reg$fitted.values)
  lv3_leukemia_total$quadratic.reg[ind.val]<-ifelse(predict(model.quadratic.reg,newdata=lv3_leukemia_total[ind.val,])<0,0,
                                                    predict(model.quadratic.reg,newdata=lv3_leukemia_total[ind.val,]))
  
  model.linear.spline<-lm(sum_LEUKEMIA_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_leukemia_total[ind.tr,])
  lv3_leukemia_total$linear.spline[ind.tr]<-ifelse(model.linear.spline$fitted.values<0, 0, model.linear.spline$fitted.values)
  lv3_leukemia_total$linear.spline[ind.val]<-ifelse(predict(model.linear.spline,newdata=lv3_leukemia_total[ind.val,])<0,0,
                                                    predict(model.linear.spline,newdata=lv3_leukemia_total[ind.val,]))
  
  
  MAPE_lv3[i,]<-c(unique(lv3_leukemia_total$NAME[ind.tr]),
                  round(mean(abs(lv3_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_total$simple.reg[ind.val])/lv3_leukemia_total$sum_LEUKEMIA_BZ[ind.val])*100,2),
                  round(mean(abs(lv3_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_total$linear.spline[ind.val])/lv3_leukemia_total$sum_LEUKEMIA_BZ[ind.val])*100,2),
                  round(mean(abs(lv3_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_total$quadratic.reg[ind.val])/lv3_leukemia_total$sum_LEUKEMIA_BZ[ind.val])*100,2))
  RMSE_lv3[i,]<-c(unique(lv3_leukemia_total$NAME[ind.tr]),
                  round(sqrt(mean(abs(lv3_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_total$simple.reg[ind.val]))),2),
                  round(sqrt(mean(abs(lv3_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_total$linear.spline[ind.val]))),2),
                  round(sqrt(mean(abs(lv3_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_total$quadratic.reg[ind.val]))),2))
  
  
}

ggplot()+geom_line(data=lv3_leukemia_total,aes(x=YEAR,y=sum_LEUKEMIA_BZ,color="red"),size=1)+labs(y="rate_of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(lv3_leukemia_total$NAME)),scales="free_y")+
  geom_line(data=lv3_leukemia_total,aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv3_leukemia_total,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv3_leukemia_total,aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of leukemia',
                       breaks=c("red","black","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))



##################################################################################
############# 2. grouping by UP2 and SEX #################
lv3_leukemia_SEX<-df_leukemia1%>%group_by(UP2,SEX,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  mutate(NAME=paste0("X",UP2,SEX))%>%as_tibble()

lv3_leukemia_SEX$simple.reg<-0
lv3_leukemia_SEX$linear.spline<-0
lv3_leukemia_SEX$quadratic.reg<-0

name_level<-unique(lv3_leukemia_SEX$NAME)

for(i in 1:length(name_level)){
  ind.tr<-which(lv3_leukemia_SEX$YEAR<=2014 & lv3_leukemia_SEX$NAME==name_level[i])
  ind.val<-which(lv3_leukemia_SEX$YEAR>2014 & lv3_leukemia_SEX$NAME==name_level[i])
  
  model.simple.reg<-lm(sum_LEUKEMIA_BZ~YEAR, data=lv3_leukemia_SEX[ind.tr,])
  lv3_leukemia_SEX$simple.reg[ind.tr]<-ifelse(model.simple.reg$fitted.values<0, 0, model.simple.reg$fitted.values)
  lv3_leukemia_SEX$simple.reg[ind.val]<-ifelse(predict(model.simple.reg,newdata=lv3_leukemia_SEX[ind.val,])<0,0,
                                               predict(model.simple.reg,newdata=lv3_leukemia_SEX[ind.val,]))
  
  
  
  model.quadratic.reg<-lm(sum_LEUKEMIA_BZ~poly(YEAR,2), data=lv3_leukemia_SEX[ind.tr,])
  lv3_leukemia_SEX$quadratic.reg[ind.tr]<-ifelse(model.quadratic.reg$fitted.values<0, 0, model.quadratic.reg$fitted.values)
  lv3_leukemia_SEX$quadratic.reg[ind.val]<-ifelse(predict(model.quadratic.reg,newdata=lv3_leukemia_SEX[ind.val,])<0,0,
                                                  predict(model.quadratic.reg,newdata=lv3_leukemia_SEX[ind.val,]))
  
  model.linear.spline<-lm(sum_LEUKEMIA_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_leukemia_SEX[ind.tr,])
  lv3_leukemia_SEX$linear.spline[ind.tr]<-ifelse(model.linear.spline$fitted.values<0, 0, model.linear.spline$fitted.values)
  lv3_leukemia_SEX$linear.spline[ind.val]<-ifelse(predict(model.linear.spline,newdata=lv3_leukemia_SEX[ind.val,])<0,0,
                                                  predict(model.linear.spline,newdata=lv3_leukemia_SEX[ind.val,]))
  
  
  MAPE_lv3[(i+74),]<-c(unique(lv3_leukemia_SEX$NAME[ind.tr]),
                       round(mean(abs(lv3_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_SEX$simple.reg[ind.val])/lv3_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val])*100,2),
                       round(mean(abs(lv3_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_SEX$linear.spline[ind.val])/lv3_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val])*100,2),
                       round(mean(abs(lv3_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_SEX$quadratic.reg[ind.val])/lv3_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val])*100,2))
  RMSE_lv3[(i+74),]<-c(unique(lv3_leukemia_SEX$NAME[ind.tr]),
                       round(sqrt(mean(abs(lv3_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_SEX$simple.reg[ind.val]))),2),
                       round(sqrt(mean(abs(lv3_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_SEX$linear.spline[ind.val]))),2),
                       round(sqrt(mean(abs(lv3_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_SEX$quadratic.reg[ind.val]))),2))
  
  
}

ind.M<-grep("M",MAPE_lv3[,1])
MAPE_lv3_M<-MAPE_lv3[ind.M,]

ind.F<-grep("F",MAPE_lv3[,1])
MAPE_lv3_F<-MAPE_lv3[ind.F,]

ggplot()+geom_line(data=lv3_leukemia_SEX[lv3_leukemia_SEX$SEX=="F",],aes(x=YEAR,y=sum_LEUKEMIA_BZ,color="red"),size=1)+labs(y="rate_of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(lv3_leukemia_SEX$NAME[lv3_leukemia_SEX$SEX=="F"])),scales="free_y")+
  geom_line(data=lv3_leukemia_SEX[lv3_leukemia_SEX$SEX=="F",],aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv3_leukemia_SEX[lv3_leukemia_SEX$SEX=="F",],aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv3_leukemia_SEX[lv3_leukemia_SEX$SEX=="F",],aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_F),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_F),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_F),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of leukemia',
                       breaks=c("red","black","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


ggplot()+geom_line(data=lv3_leukemia_SEX[lv3_leukemia_SEX$SEX=="M",],aes(x=YEAR,y=sum_LEUKEMIA_BZ,color="red"),size=1)+labs(y="rate_of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(lv3_leukemia_SEX$NAME[lv3_leukemia_SEX$SEX=="M"])),scales="free_y")+
  geom_line(data=lv3_leukemia_SEX[lv3_leukemia_SEX$SEX=="M",],aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv3_leukemia_SEX[lv3_leukemia_SEX$SEX=="M",],aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv3_leukemia_SEX[lv3_leukemia_SEX$SEX=="M",],aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_M),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_M),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_M),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of leukemia',
                       breaks=c("red","black","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


################################################################################################
############### 3. grouping by UP2 and CAL2 ######################### 
lv3_leukemia_CAL2<-df_leukemia1%>%group_by(UP2,CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  mutate(NAME=paste0("X",UP2,CAL2))%>%as_tibble()

name_level<-unique(lv3_leukemia_CAL2$NAME)

lv3_leukemia_CAL2$simple.reg<-0
lv3_leukemia_CAL2$linear.spline<-0
lv3_leukemia_CAL2$quadratic.reg<-0

for(i in 1:length(name_level)){
  ind.tr<-which(lv3_leukemia_CAL2$YEAR<=2014 & lv3_leukemia_CAL2$NAME==name_level[i])
  ind.val<-which(lv3_leukemia_CAL2$YEAR>2014 & lv3_leukemia_CAL2$NAME==name_level[i])
  
  model.simple.reg<-lm(sum_LEUKEMIA_BZ~YEAR, data=lv3_leukemia_CAL2[ind.tr,])
  lv3_leukemia_CAL2$simple.reg[ind.tr]<-ifelse(model.simple.reg$fitted.values<0, 0, model.simple.reg$fitted.values)
  lv3_leukemia_CAL2$simple.reg[ind.val]<-ifelse(predict(model.simple.reg,newdata=lv3_leukemia_CAL2[ind.val,])<0,0,
                                                predict(model.simple.reg,newdata=lv3_leukemia_CAL2[ind.val,]))
  
  
  
  model.quadratic.reg<-lm(sum_LEUKEMIA_BZ~poly(YEAR,2), data=lv3_leukemia_CAL2[ind.tr,])
  lv3_leukemia_CAL2$quadratic.reg[ind.tr]<-ifelse(model.quadratic.reg$fitted.values<0, 0, model.quadratic.reg$fitted.values)
  lv3_leukemia_CAL2$quadratic.reg[ind.val]<-ifelse(predict(model.quadratic.reg,newdata=lv3_leukemia_CAL2[ind.val,])<0,0,
                                                   predict(model.quadratic.reg,newdata=lv3_leukemia_CAL2[ind.val,]))
  
  model.linear.spline<-lm(sum_LEUKEMIA_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_leukemia_CAL2[ind.tr,])
  lv3_leukemia_CAL2$linear.spline[ind.tr]<-ifelse(model.linear.spline$fitted.values<0, 0, model.linear.spline$fitted.values)
  lv3_leukemia_CAL2$linear.spline[ind.val]<-ifelse(predict(model.linear.spline,newdata=lv3_leukemia_CAL2[ind.val,])<0,0,
                                                   predict(model.linear.spline,newdata=lv3_leukemia_CAL2[ind.val,]))
  
  
  
  MAPE_lv3[(i+222),]<-c(unique(lv3_leukemia_CAL2$NAME[ind.tr]),
                       round(mean(abs(lv3_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_CAL2$simple.reg[ind.val])/lv3_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2),
                       round(mean(abs(lv3_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_CAL2$linear.spline[ind.val])/lv3_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2),
                       round(mean(abs(lv3_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_CAL2$quadratic.reg[ind.val])/lv3_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2))
  
  RMSE_lv3[(i+222),]<-c(unique(lv3_leukemia_CAL2$NAME[ind.tr]),
                       round(sqrt(mean(abs(lv3_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_CAL2$simple.reg[ind.val]))),2),
                       round(sqrt(mean(abs(lv3_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_CAL2$linear.spline[ind.val]))),2),
                       round(sqrt(mean(abs(lv3_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_CAL2$quadratic.reg[ind.val]))),2))
  
  
}

ind.01<-grep("01",MAPE_lv3[,1])
MAPE_lv3_01<-MAPE_lv3[ind.01,]

ind.234<-grep("234",MAPE_lv3[,1])
MAPE_lv3_234<-MAPE_lv3[ind.234,]


ggplot()+geom_line(data=lv3_leukemia_CAL2[lv3_leukemia_CAL2$CAL2=="01",],aes(x=YEAR,y=sum_LEUKEMIA_BZ,color="red"),size=1)+labs(y="rate_of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(lv3_leukemia_CAL2$NAME[lv3_leukemia_CAL2$CAL2=="01"])),scales="free_y")+
  geom_line(data=lv3_leukemia_CAL2[lv3_leukemia_CAL2$CAL2=="01",],aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv3_leukemia_CAL2[lv3_leukemia_CAL2$CAL2=="01",],aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv3_leukemia_CAL2[lv3_leukemia_CAL2$CAL2=="01",],aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_01),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_01),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_01),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of leukemia',
                       breaks=c("red","black","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))



ggplot()+geom_line(data=lv3_leukemia_CAL2[lv3_leukemia_CAL2$CAL2=="234",],aes(x=YEAR,y=sum_LEUKEMIA_BZ,color="red"),size=1)+labs(y="rate_of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(lv3_leukemia_CAL2$NAME[lv3_leukemia_CAL2$CAL2=="234"])),scales="free_y")+
  geom_line(data=lv3_leukemia_CAL2[lv3_leukemia_CAL2$CAL2=="234",],aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv3_leukemia_CAL2[lv3_leukemia_CAL2$CAL2=="234",],aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv3_leukemia_CAL2[lv3_leukemia_CAL2$CAL2=="234",],aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_234),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_234),mapping = aes(x = 2003, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_234),mapping = aes(x = 2003, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of leukemia',
                       breaks=c("red","black","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


################################################################################################
############### 3. grouping by UP2 and SEX and CAL2 ######################### 
lv3_leukemia_SEX_CAL2<-df_leukemia1%>%group_by(UP2,SEX,CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  mutate(NAME=paste0("X",UP2,SEX,CAL2))%>%as_tibble()

name_level<-unique(lv3_leukemia_SEX_CAL2$NAME)

lv3_leukemia_SEX_CAL2$simple.reg<-0
lv3_leukemia_SEX_CAL2$linear.spline<-0
lv3_leukemia_SEX_CAL2$quadratic.reg<-0

for(i in 1:length(name_level)){
  ind.tr<-which(lv3_leukemia_SEX_CAL2$YEAR<=2014 & lv3_leukemia_SEX_CAL2$NAME==name_level[i])
  ind.val<-which(lv3_leukemia_SEX_CAL2$YEAR>2014 & lv3_leukemia_SEX_CAL2$NAME==name_level[i])
  
  model.simple.reg<-lm(sum_LEUKEMIA_BZ~YEAR, data=lv3_leukemia_SEX_CAL2[ind.tr,])
  lv3_leukemia_SEX_CAL2$simple.reg[ind.tr]<-ifelse(model.simple.reg$fitted.values<0, 0, model.simple.reg$fitted.values)
  lv3_leukemia_SEX_CAL2$simple.reg[ind.val]<-ifelse(predict(model.simple.reg,newdata=lv3_leukemia_SEX_CAL2[ind.val,])<0,0,
                                                    predict(model.simple.reg,newdata=lv3_leukemia_SEX_CAL2[ind.val,]))
  
  
  
  model.quadratic.reg<-lm(sum_LEUKEMIA_BZ~poly(YEAR,2), data=lv3_leukemia_SEX_CAL2[ind.tr,])
  lv3_leukemia_SEX_CAL2$quadratic.reg[ind.tr]<-ifelse(model.quadratic.reg$fitted.values<0, 0, model.quadratic.reg$fitted.values)
  lv3_leukemia_SEX_CAL2$quadratic.reg[ind.val]<-ifelse(predict(model.quadratic.reg,newdata=lv3_leukemia_SEX_CAL2[ind.val,])<0,0,
                                                       predict(model.quadratic.reg,newdata=lv3_leukemia_SEX_CAL2[ind.val,]))
  
  model.linear.spline<-lm(sum_LEUKEMIA_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_leukemia_SEX_CAL2[ind.tr,])
  lv3_leukemia_SEX_CAL2$linear.spline[ind.tr]<-ifelse(model.linear.spline$fitted.values<0, 0, model.linear.spline$fitted.values)
  lv3_leukemia_SEX_CAL2$linear.spline[ind.val]<-ifelse(predict(model.linear.spline,newdata=lv3_leukemia_SEX_CAL2[ind.val,])<0,0,
                                                       predict(model.linear.spline,newdata=lv3_leukemia_SEX_CAL2[ind.val,]))
  
  
  
  MAPE_lv3[(i+370),]<-c(unique(lv3_leukemia_SEX_CAL2$NAME[ind.tr]),
                       round(mean(abs(lv3_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_SEX_CAL2$simple.reg[ind.val])/lv3_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2),
                       round(mean(abs(lv3_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_SEX_CAL2$linear.spline[ind.val])/lv3_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2),
                       round(mean(abs(lv3_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_SEX_CAL2$quadratic.reg[ind.val])/lv3_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2))
  
  RMSE_lv3[(i+370),]<-c(unique(lv3_leukemia_SEX_CAL2$NAME[ind.tr]),
                       round(sqrt(mean(abs(lv3_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_SEX_CAL2$simple.reg[ind.val]))),2),
                       round(sqrt(mean(abs(lv3_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_SEX_CAL2$linear.spline[ind.val]))),2),
                       round(sqrt(mean(abs(lv3_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_SEX_CAL2$quadratic.reg[ind.val]))),2))
  
  
}


ind.M.01<-which(grepl("M",MAPE_lv3[,1]) & grepl("01",MAPE_lv3[,1]))
MAPE_lv3_leukemia_M_01<-MAPE_lv3[ind.M.01,]

ind.F.01<-which(grepl("F",MAPE_lv3[,1]) & grepl("01",MAPE_lv3[,1]))
MAPE_lv3_leukemia_F_01<-MAPE_lv3[ind.F.01,]

ind.M.234<-which(grepl("M",MAPE_lv3[,1]) & grepl("234",MAPE_lv3[,1]))
MAPE_lv3_leukemia_M_234<-MAPE_lv3[ind.M.234,]

ind.F.234<-which(grepl("F",MAPE_lv3[,1]) & grepl("234",MAPE_lv3[,1]))
MAPE_lv3_leukemia_F_234<-MAPE_lv3[ind.F.234,]



########## M & 01 ############
ggplot()+geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="M" & lv3_leukemia_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=sum_LEUKEMIA_BZ,color="red"),size=1)+labs(y="rate_of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(MAPE_lv3_leukemia_M_01[,1])),scales="free_y")+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="M" & lv3_leukemia_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="M" & lv3_leukemia_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="M" & lv3_leukemia_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_M_01),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_M_01),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_M_01),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of leukemia',
                       breaks=c("red","black","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

########## M & 234 ############
ggplot()+geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="M" & lv3_leukemia_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=sum_LEUKEMIA_BZ,color="red"),size=1)+labs(y="rate_of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(MAPE_lv3_leukemia_M_234[,1])),scales="free_y")+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="M" & lv3_leukemia_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="M" & lv3_leukemia_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="M" & lv3_leukemia_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_M_234),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_M_234),mapping = aes(x = 2004, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_M_234),mapping = aes(x = 2004, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of leukemia',
                       breaks=c("red","black","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


########## F & 01 ############
ggplot()+geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="F" & lv3_leukemia_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=sum_LEUKEMIA_BZ,color="red"),size=1)+labs(y="rate_of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(MAPE_lv3_leukemia_F_01[,1])),scales="free_y")+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="F" & lv3_leukemia_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="F" & lv3_leukemia_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="F" & lv3_leukemia_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_F_01),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_F_01),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_F_01),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of leukemia',
                       breaks=c("red","black","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))



########## F & 234 ############
ggplot()+geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="F" & lv3_leukemia_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=sum_LEUKEMIA_BZ,color="red"),size=1)+labs(y="rate_of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(MAPE_lv3_leukemia_F_234[,1])),scales="free_y")+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="F" & lv3_leukemia_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="F" & lv3_leukemia_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="F" & lv3_leukemia_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_F_234),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_F_234),mapping = aes(x = 2004, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_F_234),mapping = aes(x = 2004, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of leukemia',
                       breaks=c("red","black","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))




value_eklee_lv3<-as.data.frame(rbind(lv3_leukemia_total[2:length(lv3_leukemia_total)],lv3_leukemia_SEX[3:length(lv3_leukemia_SEX)],lv3_leukemia_CAL2[3:length(lv3_leukemia_CAL2)],
                                     lv3_leukemia_SEX_CAL2[4:length(lv3_leukemia_SEX_CAL2)]))






















































































######################### Lung cancer data ################################
############################# level 1 #############################
########### 1. total ###########
lv1_lung_total<-df_lung1%>%group_by(YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME="X")%>%as_tibble()

lv1_lung_total$simple.reg<-0
lv1_lung_total$linear.spline<-0
lv1_lung_total$quadratic.reg<-0


ind.tr<-which(lv1_lung_total$YEAR<=2014)
ind.val<-which(lv1_lung_total$YEAR>2014)

model.simple.reg<-lm(sum_LUNG_BZ~YEAR, data=lv1_lung_total[ind.tr,])
lv1_lung_total$simple.reg[ind.tr]<-ifelse(model.simple.reg$fitted.values<0, 0, model.simple.reg$fitted.values)
lv1_lung_total$simple.reg[ind.val]<-ifelse(predict(model.simple.reg,newdata=lv1_lung_total[ind.val,])<0,0,
                                               predict(model.simple.reg,newdata=lv1_lung_total[ind.val,]))



model.quadratic.reg<-lm(sum_LUNG_BZ~poly(YEAR,2), data=lv1_lung_total[ind.tr,])
lv1_lung_total$quadratic.reg[ind.tr]<-ifelse(model.quadratic.reg$fitted.values<0, 0, model.quadratic.reg$fitted.values)
lv1_lung_total$quadratic.reg[ind.val]<-ifelse(predict(model.quadratic.reg,newdata=lv1_lung_total[ind.val,])<0,0,
                                                  predict(model.quadratic.reg,newdata=lv1_lung_total[ind.val,]))

model.linear.spline<-lm(sum_LUNG_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_lung_total[ind.tr,])
lv1_lung_total$linear.spline[ind.tr]<-ifelse(model.linear.spline$fitted.values<0, 0, model.linear.spline$fitted.values)
lv1_lung_total$linear.spline[ind.val]<-ifelse(predict(model.linear.spline,newdata=lv1_lung_total[ind.val,])<0,0,
                                                  predict(model.linear.spline,newdata=lv1_lung_total[ind.val,]))

MAPE_lv1_lung<-matrix(0,nrow=9,ncol=4)
colnames(MAPE_lv1_lung)<-c("NAME","simple reg","linear spline","quadratic reg")

RMSE_lv1_lung<-matrix(0,nrow=9,ncol=4)
colnames(RMSE_lv1_lung)<-c("NAME","simple reg","linear spline","quadratic reg")

MAPE_lv1_lung[1,]<-c(unique(lv1_lung_total$NAME),
                round(mean(abs(lv1_lung_total$sum_LUNG_BZ[ind.val]-lv1_lung_total$simple.reg[ind.val])/lv1_lung_total$sum_LUNG_BZ[ind.val])*100,2),
                round(mean(abs(lv1_lung_total$sum_LUNG_BZ[ind.val]-lv1_lung_total$linear.spline[ind.val])/lv1_lung_total$sum_LUNG_BZ[ind.val])*100,2),
                round(mean(abs(lv1_lung_total$sum_LUNG_BZ[ind.val]-lv1_lung_total$quadratic.reg[ind.val])/lv1_lung_total$sum_LUNG_BZ[ind.val])*100,2))
RMSE_lv1_lung[1,]<-c(unique(lv1_lung_total$NAME),
                round(sqrt(mean(abs(lv1_lung_total$sum_LUNG_BZ[ind.val]-lv1_lung_total$simple.reg[ind.val]))),2),
                round(sqrt(mean(abs(lv1_lung_total$sum_LUNG_BZ[ind.val]-lv1_lung_total$linear.spline[ind.val]))),2),
                round(sqrt(mean(abs(lv1_lung_total$sum_LUNG_BZ[ind.val]-lv1_lung_total$quadratic.reg[ind.val]))),2))

ggplot()+geom_line(data=lv1_lung_total,aes(x=YEAR,y=sum_LUNG_BZ,color="red"),size=1)+labs(y="rate_of_lung")+
  geom_line(data=lv1_lung_total,aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Lung cancer Data comparing methods performance")+
  geom_line(data=lv1_lung_total,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv1_lung_total,aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  labs(x="Year")+
  scale_color_identity(name='rate of lung',
                       breaks=c("red","black","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))



####################################################################################
#################### 2. grouping by SEX ##################
lv1_lung_SEX<-df_lung1%>%group_by(SEX,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME=paste0("X",SEX))%>%as_tibble()

SEX_level<-unique(lv1_lung_SEX$SEX)

lv1_lung_SEX$simple.reg<-0
lv1_lung_SEX$linear.spline<-0
lv1_lung_SEX$quadratic.reg<-0
#SEX_level<-unique(lv1_lung_SEX$SEX)

for(i in 1:length(SEX_level)){
  ind.tr<-which(lv1_lung_SEX$YEAR<=2014 & lv1_lung_SEX$SEX==SEX_level[i])
  ind.val<-which(lv1_lung_SEX$YEAR>2014 & lv1_lung_SEX$SEX==SEX_level[i])
  
  model.simple.reg<-lm(sum_LUNG_BZ~YEAR, data=lv1_lung_SEX[ind.tr,])
  lv1_lung_SEX$simple.reg[ind.tr]<-ifelse(model.simple.reg$fitted.values<0, 0, model.simple.reg$fitted.values)
  lv1_lung_SEX$simple.reg[ind.val]<-ifelse(predict(model.simple.reg,newdata=lv1_lung_SEX[ind.val,])<0,0,
                                               predict(model.simple.reg,newdata=lv1_lung_SEX[ind.val,]))
  
  
  
  model.quadratic.reg<-lm(sum_LUNG_BZ~poly(YEAR,2), data=lv1_lung_SEX[ind.tr,])
  lv1_lung_SEX$quadratic.reg[ind.tr]<-ifelse(model.quadratic.reg$fitted.values<0, 0, model.quadratic.reg$fitted.values)
  lv1_lung_SEX$quadratic.reg[ind.val]<-ifelse(predict(model.quadratic.reg,newdata=lv1_lung_SEX[ind.val,])<0,0,
                                                  predict(model.quadratic.reg,newdata=lv1_lung_SEX[ind.val,]))
  
  model.linear.spline<-lm(sum_LUNG_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_lung_SEX[ind.tr,])
  lv1_lung_SEX$linear.spline[ind.tr]<-ifelse(model.linear.spline$fitted.values<0, 0, model.linear.spline$fitted.values)
  lv1_lung_SEX$linear.spline[ind.val]<-ifelse(predict(model.linear.spline,newdata=lv1_lung_SEX[ind.val,])<0,0,
                                                  predict(model.linear.spline,newdata=lv1_lung_SEX[ind.val,]))
  
  
  
  MAPE_lv1_lung[(i+1),]<-c(unique(lv1_lung_SEX$NAME[ind.tr]),
                      round(mean(abs(lv1_lung_SEX$sum_LUNG_BZ[ind.val]-lv1_lung_SEX$simple.reg[ind.val])/lv1_lung_SEX$sum_LUNG_BZ[ind.val])*100,2),
                      round(mean(abs(lv1_lung_SEX$sum_LUNG_BZ[ind.val]-lv1_lung_SEX$linear.spline[ind.val])/lv1_lung_SEX$sum_LUNG_BZ[ind.val])*100,2),
                      round(mean(abs(lv1_lung_SEX$sum_LUNG_BZ[ind.val]-lv1_lung_SEX$quadratic.reg[ind.val])/lv1_lung_SEX$sum_LUNG_BZ[ind.val])*100,2))
  
  RMSE_lv1_lung[(i+1),]<-c(unique(lv1_lung_SEX$NAME[ind.tr]),
                      round(sqrt(mean(abs(lv1_lung_SEX$sum_LUNG_BZ[ind.val]-lv1_lung_SEX$simple.reg[ind.val]))),2),
                      round(sqrt(mean(abs(lv1_lung_SEX$sum_LUNG_BZ[ind.val]-lv1_lung_SEX$linear.spline[ind.val]))),2),
                      round(sqrt(mean(abs(lv1_lung_SEX$sum_LUNG_BZ[ind.val]-lv1_lung_SEX$quadratic.reg[ind.val]))),2))
  
}



ggplot()+geom_line(data=lv1_lung_SEX,aes(x=YEAR,y=sum_LUNG_BZ,color="red"),size=1)+labs(y="rate_of_lung")+
  facet_wrap(~ factor(SEX,levels=SEX_level),scales="free_y")+
  geom_line(data=lv1_lung_SEX,aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Lung cancer Data comparing methods performance")+
  geom_line(data=lv1_lung_SEX,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv1_lung_SEX,aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  labs(x="Year")+
  scale_color_identity(name='rate of lung',
                       breaks=c("red","black","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))



################################################################################################
############### 3. grouping by CAL2 ######################### 
lv1_lung_CAL2<-df_lung1%>%group_by(CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME=paste0("X",CAL2))%>%as_tibble()

CAL2_level<-unique(lv1_lung_CAL2$CAL2)

lv1_lung_CAL2$simple.reg<-0
lv1_lung_CAL2$linear.spline<-0
lv1_lung_CAL2$quadratic.reg<-0
CAL2_level<-unique(lv1_lung_CAL2$CAL2)

for(i in 1:length(CAL2_level)){
  ind.tr<-which(lv1_lung_CAL2$YEAR<=2014 & lv1_lung_CAL2$CAL2==CAL2_level[i])
  ind.val<-which(lv1_lung_CAL2$YEAR>2014 & lv1_lung_CAL2$CAL2==CAL2_level[i])
  
  model.simple.reg<-lm(sum_LUNG_BZ~YEAR, data=lv1_lung_CAL2[ind.tr,])
  lv1_lung_CAL2$simple.reg[ind.tr]<-ifelse(model.simple.reg$fitted.values<0, 0, model.simple.reg$fitted.values)
  lv1_lung_CAL2$simple.reg[ind.val]<-ifelse(predict(model.simple.reg,newdata=lv1_lung_CAL2[ind.val,])<0,0,
                                                predict(model.simple.reg,newdata=lv1_lung_CAL2[ind.val,]))
  
  
  
  model.quadratic.reg<-lm(sum_LUNG_BZ~poly(YEAR,2), data=lv1_lung_CAL2[ind.tr,])
  lv1_lung_CAL2$quadratic.reg[ind.tr]<-ifelse(model.quadratic.reg$fitted.values<0, 0, model.quadratic.reg$fitted.values)
  lv1_lung_CAL2$quadratic.reg[ind.val]<-ifelse(predict(model.quadratic.reg,newdata=lv1_lung_CAL2[ind.val,])<0,0,
                                                   predict(model.quadratic.reg,newdata=lv1_lung_CAL2[ind.val,]))
  
  model.linear.spline<-lm(sum_LUNG_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_lung_CAL2[ind.tr,])
  lv1_lung_CAL2$linear.spline[ind.tr]<-ifelse(model.linear.spline$fitted.values<0, 0, model.linear.spline$fitted.values)
  lv1_lung_CAL2$linear.spline[ind.val]<-ifelse(predict(model.linear.spline,newdata=lv1_lung_CAL2[ind.val,])<0,0,
                                                   predict(model.linear.spline,newdata=lv1_lung_CAL2[ind.val,]))
  
  
  
  MAPE_lv1_lung[(i+3),]<-c(unique(lv1_lung_CAL2$NAME[ind.tr]),
                      round(mean(abs(lv1_lung_CAL2$sum_LUNG_BZ[ind.val]-lv1_lung_CAL2$simple.reg[ind.val])/lv1_lung_CAL2$sum_LUNG_BZ[ind.val])*100,2),
                      round(mean(abs(lv1_lung_CAL2$sum_LUNG_BZ[ind.val]-lv1_lung_CAL2$linear.spline[ind.val])/lv1_lung_CAL2$sum_LUNG_BZ[ind.val])*100,2),
                      round(mean(abs(lv1_lung_CAL2$sum_LUNG_BZ[ind.val]-lv1_lung_CAL2$quadratic.reg[ind.val])/lv1_lung_CAL2$sum_LUNG_BZ[ind.val])*100,2))
  
  RMSE_lv1_lung[(i+3),]<-c(unique(lv1_lung_CAL2$NAME[ind.tr]),
                      round(sqrt(mean(abs(lv1_lung_CAL2$sum_LUNG_BZ[ind.val]-lv1_lung_CAL2$simple.reg[ind.val]))),2),
                      round(sqrt(mean(abs(lv1_lung_CAL2$sum_LUNG_BZ[ind.val]-lv1_lung_CAL2$linear.spline[ind.val]))),2),
                      round(sqrt(mean(abs(lv1_lung_CAL2$sum_LUNG_BZ[ind.val]-lv1_lung_CAL2$quadratic.reg[ind.val]))),2))
  
  
}



ggplot()+geom_line(data=lv1_lung_CAL2,aes(x=YEAR,y=sum_LUNG_BZ,color="red"),size=1)+labs(y="rate_of_lung")+
  facet_wrap(~ factor(CAL2,levels=CAL2_level),scales="free_y")+
  geom_line(data=lv1_lung_CAL2,aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Lung cancer Data comparing methods performance")+
  geom_line(data=lv1_lung_CAL2,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv1_lung_CAL2,aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  labs(x="Year")+
  scale_color_identity(name='rate of lung',
                       breaks=c("red","black","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


################################################################################################
############### 4. grouping by SEX and CAL2 ######################### 
lv1_lung_SEX_CAL2<-df_lung1%>%group_by(SEX,CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME=paste0("X",SEX,CAL2))%>%as_tibble()

SEX_CAL2_level<-unique(lv1_lung_SEX_CAL2$NAME)

lv1_lung_SEX_CAL2$simple.reg<-0
lv1_lung_SEX_CAL2$linear.spline<-0
lv1_lung_SEX_CAL2$quadratic.reg<-0


for(i in 1:length(SEX_CAL2_level)){
  ind.tr<-which(lv1_lung_SEX_CAL2$YEAR<=2014 & lv1_lung_SEX_CAL2$NAME==SEX_CAL2_level[i])
  ind.val<-which(lv1_lung_SEX_CAL2$YEAR>2014 & lv1_lung_SEX_CAL2$NAME==SEX_CAL2_level[i])
  
  model.simple.reg<-lm(sum_LUNG_BZ~YEAR, data=lv1_lung_SEX_CAL2[ind.tr,])
  lv1_lung_SEX_CAL2$simple.reg[ind.tr]<-ifelse(model.simple.reg$fitted.values<0, 0, model.simple.reg$fitted.values)
  lv1_lung_SEX_CAL2$simple.reg[ind.val]<-ifelse(predict(model.simple.reg,newdata=lv1_lung_SEX_CAL2[ind.val,])<0,0,
                                                    predict(model.simple.reg,newdata=lv1_lung_SEX_CAL2[ind.val,]))
  
  
  
  model.quadratic.reg<-lm(sum_LUNG_BZ~poly(YEAR,2), data=lv1_lung_SEX_CAL2[ind.tr,])
  lv1_lung_SEX_CAL2$quadratic.reg[ind.tr]<-ifelse(model.quadratic.reg$fitted.values<0, 0, model.quadratic.reg$fitted.values)
  lv1_lung_SEX_CAL2$quadratic.reg[ind.val]<-ifelse(predict(model.quadratic.reg,newdata=lv1_lung_SEX_CAL2[ind.val,])<0,0,
                                                       predict(model.quadratic.reg,newdata=lv1_lung_SEX_CAL2[ind.val,]))
  
  model.linear.spline<-lm(sum_LUNG_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_lung_SEX_CAL2[ind.tr,])
  lv1_lung_SEX_CAL2$linear.spline[ind.tr]<-ifelse(model.linear.spline$fitted.values<0, 0, model.linear.spline$fitted.values)
  lv1_lung_SEX_CAL2$linear.spline[ind.val]<-ifelse(predict(model.linear.spline,newdata=lv1_lung_SEX_CAL2[ind.val,])<0,0,
                                                       predict(model.linear.spline,newdata=lv1_lung_SEX_CAL2[ind.val,]))
  
  
  
  MAPE_lv1_lung[(i+5),]<-c(unique(lv1_lung_SEX_CAL2$NAME[ind.tr]),
                      round(mean(abs(lv1_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv1_lung_SEX_CAL2$simple.reg[ind.val])/lv1_lung_SEX_CAL2$sum_LUNG_BZ[ind.val])*100,2),
                      round(mean(abs(lv1_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv1_lung_SEX_CAL2$linear.spline[ind.val])/lv1_lung_SEX_CAL2$sum_LUNG_BZ[ind.val])*100,2),
                      round(mean(abs(lv1_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv1_lung_SEX_CAL2$quadratic.reg[ind.val])/lv1_lung_SEX_CAL2$sum_LUNG_BZ[ind.val])*100,2))
  
  RMSE_lv1_lung[(i+5),]<-c(unique(lv1_lung_SEX_CAL2$NAME[ind.tr]),
                      round(sqrt(mean(abs(lv1_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv1_lung_SEX_CAL2$simple.reg[ind.val]))),2),
                      round(sqrt(mean(abs(lv1_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv1_lung_SEX_CAL2$linear.spline[ind.val]))),2),
                      round(sqrt(mean(abs(lv1_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv1_lung_SEX_CAL2$quadratic.reg[ind.val]))),2))
  
}



ggplot()+geom_line(data=lv1_lung_SEX_CAL2,aes(x=YEAR,y=sum_LUNG_BZ,color="red"),size=1)+labs(y="rate_of_lung")+
  facet_wrap(~ factor(NAME,levels=SEX_CAL2_level),scales="free_y")+
  geom_line(data=lv1_lung_SEX_CAL2,aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Lung cancer Data comparing methods performance")+
  geom_line(data=lv1_lung_SEX_CAL2,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv1_lung_SEX_CAL2,aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  labs(x="Year")+
  scale_color_identity(name='rate of lung',
                       breaks=c("red","black","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

value_eklee_lv1_lung<-as.data.frame(rbind(lv1_lung_total,lv1_lung_SEX[2:length(lv1_lung_SEX)],lv1_lung_CAL2[2:length(lv1_lung_CAL2)],
                                     lv1_lung_SEX_CAL2[3:length(lv1_lung_SEX_CAL2)]))




##################### level 2 data ##########################
############# 1. grouping by UP1 #################
lv2_lung_total<-df_lung1%>%group_by(UP1,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME=paste0("X",UP1))%>%as_tibble()

MAPE_lv2_lung<-matrix(0,nrow=length(unique(lv2_lung_total$NAME))*(1+2+2+4),ncol=4)
colnames(MAPE_lv2_lung)<-c("NAME","simple_reg","linear_spline","quadratic_reg")

RMSE_lv2_lung<-matrix(0,nrow=length(unique(lv2_lung_total$NAME))*(1+2+2+4),ncol=4)
colnames(RMSE_lv2_lung)<-c("NAME","simple_reg","linear_spline","quadratic_reg")

lv2_lung_total$simple.reg<-0
lv2_lung_total$linear.spline<-0
lv2_lung_total$quadratic.reg<-0

name_level<-unique(lv2_lung_total$NAME)

for(i in 1:length(name_level)){
  ind.tr<-which(lv2_lung_total$YEAR<=2014 & lv2_lung_total$NAME==name_level[i])
  ind.val<-which(lv2_lung_total$YEAR>2014 & lv2_lung_total$NAME==name_level[i])
  
  model.simple.reg<-lm(sum_LUNG_BZ~YEAR, data=lv2_lung_total[ind.tr,])
  lv2_lung_total$simple.reg[ind.tr]<-ifelse(model.simple.reg$fitted.values<0, 0, model.simple.reg$fitted.values)
  lv2_lung_total$simple.reg[ind.val]<-ifelse(predict(model.simple.reg,newdata=lv2_lung_total[ind.val,])<0,0,
                                                 predict(model.simple.reg,newdata=lv2_lung_total[ind.val,]))
  
  
  
  model.quadratic.reg<-lm(sum_LUNG_BZ~poly(YEAR,2), data=lv2_lung_total[ind.tr,])
  lv2_lung_total$quadratic.reg[ind.tr]<-ifelse(model.quadratic.reg$fitted.values<0, 0, model.quadratic.reg$fitted.values)
  lv2_lung_total$quadratic.reg[ind.val]<-ifelse(predict(model.quadratic.reg,newdata=lv2_lung_total[ind.val,])<0,0,
                                                    predict(model.quadratic.reg,newdata=lv2_lung_total[ind.val,]))
  
  model.linear.spline<-lm(sum_LUNG_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_lung_total[ind.tr,])
  lv2_lung_total$linear.spline[ind.tr]<-ifelse(model.linear.spline$fitted.values<0, 0, model.linear.spline$fitted.values)
  lv2_lung_total$linear.spline[ind.val]<-ifelse(predict(model.linear.spline,newdata=lv2_lung_total[ind.val,])<0,0,
                                                    predict(model.linear.spline,newdata=lv2_lung_total[ind.val,]))
  
  
  MAPE_lv2_lung[i,]<-c(unique(lv2_lung_total$NAME[ind.tr]),
                  round(mean(abs(lv2_lung_total$sum_LUNG_BZ[ind.val]-lv2_lung_total$simple.reg[ind.val])/lv2_lung_total$sum_LUNG_BZ[ind.val])*100,2),
                  round(mean(abs(lv2_lung_total$sum_LUNG_BZ[ind.val]-lv2_lung_total$linear.spline[ind.val])/lv2_lung_total$sum_LUNG_BZ[ind.val])*100,2),
                  round(mean(abs(lv2_lung_total$sum_LUNG_BZ[ind.val]-lv2_lung_total$quadratic.reg[ind.val])/lv2_lung_total$sum_LUNG_BZ[ind.val])*100,2))
  RMSE_lv2_lung[i,]<-c(unique(lv2_lung_total$NAME[ind.tr]),
                  round(sqrt(mean(abs(lv2_lung_total$sum_LUNG_BZ[ind.val]-lv2_lung_total$simple.reg[ind.val]))),2),
                  round(sqrt(mean(abs(lv2_lung_total$sum_LUNG_BZ[ind.val]-lv2_lung_total$linear.spline[ind.val]))),2),
                  round(sqrt(mean(abs(lv2_lung_total$sum_LUNG_BZ[ind.val]-lv2_lung_total$quadratic.reg[ind.val]))),2))
  
  
}

ggplot()+geom_line(data=lv2_lung_total,aes(x=YEAR,y=sum_LUNG_BZ,color="red"),size=1)+labs(y="rate_of_lung")+
  facet_wrap(~ factor(NAME,levels=unique(lv2_lung_total$NAME)),scales="free_y")+
  geom_line(data=lv2_lung_total,aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Lung cancer Data comparing methods performance")+
  geom_line(data=lv2_lung_total,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv2_lung_total,aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of lung',
                       breaks=c("red","black","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

######################################################################
############### 2. grouping by SEX and UP1 #########################
lv2_lung_SEX<-df_lung1%>%group_by(UP1,SEX,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME=paste0("X",UP1,SEX))%>%as_tibble()
lv2_lung_SEX$simple.reg<-0
lv2_lung_SEX$linear.spline<-0
lv2_lung_SEX$quadratic.reg<-0
name_level<-unique(lv2_lung_SEX$NAME)

for(i in 1:length(name_level)){
  ind.tr<-which(lv2_lung_SEX$YEAR<=2014 & lv2_lung_SEX$NAME==name_level[i])
  ind.val<-which(lv2_lung_SEX$YEAR>2014 & lv2_lung_SEX$NAME==name_level[i])
  
  model.simple.reg<-lm(sum_LUNG_BZ~YEAR, data=lv2_lung_SEX[ind.tr,])
  lv2_lung_SEX$simple.reg[ind.tr]<-ifelse(model.simple.reg$fitted.values<0, 0, model.simple.reg$fitted.values)
  lv2_lung_SEX$simple.reg[ind.val]<-ifelse(predict(model.simple.reg,newdata=lv2_lung_SEX[ind.val,])<0,0,
                                           predict(model.simple.reg,newdata=lv2_lung_SEX[ind.val,]))
  
  
  
  model.quadratic.reg<-lm(sum_LUNG_BZ~poly(YEAR,2), data=lv2_lung_SEX[ind.tr,])
  lv2_lung_SEX$quadratic.reg[ind.tr]<-ifelse(model.quadratic.reg$fitted.values<0, 0, model.quadratic.reg$fitted.values)
  lv2_lung_SEX$quadratic.reg[ind.val]<-ifelse(predict(model.quadratic.reg,newdata=lv2_lung_SEX[ind.val,])<0,0,
                                              predict(model.quadratic.reg,newdata=lv2_lung_SEX[ind.val,]))
  
  model.linear.spline<-lm(sum_LUNG_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_lung_SEX[ind.tr,])
  lv2_lung_SEX$linear.spline[ind.tr]<-ifelse(model.linear.spline$fitted.values<0, 0, model.linear.spline$fitted.values)
  lv2_lung_SEX$linear.spline[ind.val]<-ifelse(predict(model.linear.spline,newdata=lv2_lung_SEX[ind.val,])<0,0,
                                              predict(model.linear.spline,newdata=lv2_lung_SEX[ind.val,]))
  
  
  MAPE_lv2_lung[(i+19),]<-c(unique(lv2_lung_SEX$NAME[ind.tr]),
                       round(mean(abs(lv2_lung_SEX$sum_LUNG_BZ[ind.val]-lv2_lung_SEX$simple.reg[ind.val])/lv2_lung_SEX$sum_LUNG_BZ[ind.val])*100,2),
                       round(mean(abs(lv2_lung_SEX$sum_LUNG_BZ[ind.val]-lv2_lung_SEX$linear.spline[ind.val])/lv2_lung_SEX$sum_LUNG_BZ[ind.val])*100,2),
                       round(mean(abs(lv2_lung_SEX$sum_LUNG_BZ[ind.val]-lv2_lung_SEX$quadratic.reg[ind.val])/lv2_lung_SEX$sum_LUNG_BZ[ind.val])*100,2))
  RMSE_lv2_lung[(i+19),]<-c(unique(lv2_lung_SEX$NAME[ind.tr]),
                       round(sqrt(mean(abs(lv2_lung_SEX$sum_LUNG_BZ[ind.val]-lv2_lung_SEX$simple.reg[ind.val]))),2),
                       round(sqrt(mean(abs(lv2_lung_SEX$sum_LUNG_BZ[ind.val]-lv2_lung_SEX$linear.spline[ind.val]))),2),
                       round(sqrt(mean(abs(lv2_lung_SEX$sum_LUNG_BZ[ind.val]-lv2_lung_SEX$quadratic.reg[ind.val]))),2))
  
  
}

ggplot()+geom_line(data=lv2_lung_SEX,aes(x=YEAR,y=sum_LUNG_BZ,color="red"),size=1)+labs(y="rate_of_lung")+
  facet_wrap(~ factor(NAME,levels=unique(lv2_lung_SEX$NAME)),scales="free_y")+
  geom_line(data=lv2_lung_SEX,aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Lung cancer Data comparing methods performance")+
  geom_line(data=lv2_lung_SEX,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv2_lung_SEX,aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung[20:57,]),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung[20:57,]),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung[20:57,]),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of lung',
                       breaks=c("red","black","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))





################################################################################################
############### 3. grouping by UP1 and CAL2 ######################### 
lv2_lung_CAL2<-df_lung1%>%group_by(UP1,CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME=paste0("X",UP1,CAL2))%>%as_tibble()
name_level<-unique(lv2_lung_CAL2$NAME)
lv2_lung_CAL2$simple.reg<-0
lv2_lung_CAL2$linear.spline<-0
lv2_lung_CAL2$quadratic.reg<-0

for(i in 1:length(name_level)){
  ind.tr<-which(lv2_lung_CAL2$YEAR<=2014 & lv2_lung_CAL2$NAME==name_level[i])
  ind.val<-which(lv2_lung_CAL2$YEAR>2014 & lv2_lung_CAL2$NAME==name_level[i])
  
  model.simple.reg<-lm(sum_LUNG_BZ~YEAR, data=lv2_lung_CAL2[ind.tr,])
  lv2_lung_CAL2$simple.reg[ind.tr]<-ifelse(model.simple.reg$fitted.values<0, 0, model.simple.reg$fitted.values)
  lv2_lung_CAL2$simple.reg[ind.val]<-ifelse(predict(model.simple.reg,newdata=lv2_lung_CAL2[ind.val,])<0,0,
                                            predict(model.simple.reg,newdata=lv2_lung_CAL2[ind.val,]))
  
  
  
  model.quadratic.reg<-lm(sum_LUNG_BZ~poly(YEAR,2), data=lv2_lung_CAL2[ind.tr,])
  lv2_lung_CAL2$quadratic.reg[ind.tr]<-ifelse(model.quadratic.reg$fitted.values<0, 0, model.quadratic.reg$fitted.values)
  lv2_lung_CAL2$quadratic.reg[ind.val]<-ifelse(predict(model.quadratic.reg,newdata=lv2_lung_CAL2[ind.val,])<0,0,
                                               predict(model.quadratic.reg,newdata=lv2_lung_CAL2[ind.val,]))
  
  model.linear.spline<-lm(sum_LUNG_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_lung_CAL2[ind.tr,])
  lv2_lung_CAL2$linear.spline[ind.tr]<-ifelse(model.linear.spline$fitted.values<0, 0, model.linear.spline$fitted.values)
  lv2_lung_CAL2$linear.spline[ind.val]<-ifelse(predict(model.linear.spline,newdata=lv2_lung_CAL2[ind.val,])<0,0,
                                               predict(model.linear.spline,newdata=lv2_lung_CAL2[ind.val,]))
  
  
  
  MAPE_lv2_lung[(i+57),]<-c(unique(lv2_lung_CAL2$NAME[ind.tr]),
                       round(mean(abs(lv2_lung_CAL2$sum_LUNG_BZ[ind.val]-lv2_lung_CAL2$simple.reg[ind.val])/lv2_lung_CAL2$sum_LUNG_BZ[ind.val])*100,2),
                       round(mean(abs(lv2_lung_CAL2$sum_LUNG_BZ[ind.val]-lv2_lung_CAL2$linear.spline[ind.val])/lv2_lung_CAL2$sum_LUNG_BZ[ind.val])*100,2),
                       round(mean(abs(lv2_lung_CAL2$sum_LUNG_BZ[ind.val]-lv2_lung_CAL2$quadratic.reg[ind.val])/lv2_lung_CAL2$sum_LUNG_BZ[ind.val])*100,2))
  
  RMSE_lv2_lung[(i+57),]<-c(unique(lv2_lung_CAL2$NAME[ind.tr]),
                       round(sqrt(mean(abs(lv2_lung_CAL2$sum_LUNG_BZ[ind.val]-lv2_lung_CAL2$simple.reg[ind.val]))),2),
                       round(sqrt(mean(abs(lv2_lung_CAL2$sum_LUNG_BZ[ind.val]-lv2_lung_CAL2$linear.spline[ind.val]))),2),
                       round(sqrt(mean(abs(lv2_lung_CAL2$sum_LUNG_BZ[ind.val]-lv2_lung_CAL2$quadratic.reg[ind.val]))),2))
  
  
}

ggplot()+geom_line(data=lv2_lung_CAL2,aes(x=YEAR,y=sum_LUNG_BZ,color="red"),size=1)+labs(y="rate_of_lung")+
  facet_wrap(~ factor(NAME,levels=name_level),scales="free_y")+
  geom_line(data=lv2_lung_CAL2,aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Lung cancer Data comparing methods performance")+
  geom_line(data=lv2_lung_CAL2,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv2_lung_CAL2,aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung[58:95,]),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung[58:95,]),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung[58:95,]),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of lung',
                       breaks=c("red","black","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))



################################################################################################
############### 3. grouping by UP1 and SEX and CAL2 ######################### 
lv2_lung_SEX_CAL2<-df_lung1%>%group_by(UP1,SEX,CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME=paste0("X",UP1,SEX,CAL2))%>%as_tibble()
name_level<-unique(lv2_lung_SEX_CAL2$NAME)
lv2_lung_SEX_CAL2$simple.reg<-0
lv2_lung_SEX_CAL2$linear.spline<-0
lv2_lung_SEX_CAL2$quadratic.reg<-0

for(i in 1:length(name_level)){
  ind.tr<-which(lv2_lung_SEX_CAL2$YEAR<=2014 & lv2_lung_SEX_CAL2$NAME==name_level[i])
  ind.val<-which(lv2_lung_SEX_CAL2$YEAR>2014 & lv2_lung_SEX_CAL2$NAME==name_level[i])
  
  model.simple.reg<-lm(sum_LUNG_BZ~YEAR, data=lv2_lung_SEX_CAL2[ind.tr,])
  lv2_lung_SEX_CAL2$simple.reg[ind.tr]<-ifelse(model.simple.reg$fitted.values<0, 0, model.simple.reg$fitted.values)
  lv2_lung_SEX_CAL2$simple.reg[ind.val]<-ifelse(predict(model.simple.reg,newdata=lv2_lung_SEX_CAL2[ind.val,])<0,0,
                                                predict(model.simple.reg,newdata=lv2_lung_SEX_CAL2[ind.val,]))
  
  
  
  model.quadratic.reg<-lm(sum_LUNG_BZ~poly(YEAR,2), data=lv2_lung_SEX_CAL2[ind.tr,])
  lv2_lung_SEX_CAL2$quadratic.reg[ind.tr]<-ifelse(model.quadratic.reg$fitted.values<0, 0, model.quadratic.reg$fitted.values)
  lv2_lung_SEX_CAL2$quadratic.reg[ind.val]<-ifelse(predict(model.quadratic.reg,newdata=lv2_lung_SEX_CAL2[ind.val,])<0,0,
                                                   predict(model.quadratic.reg,newdata=lv2_lung_SEX_CAL2[ind.val,]))
  
  model.linear.spline<-lm(sum_LUNG_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv2_lung_SEX_CAL2[ind.tr,])
  lv2_lung_SEX_CAL2$linear.spline[ind.tr]<-ifelse(model.linear.spline$fitted.values<0, 0, model.linear.spline$fitted.values)
  lv2_lung_SEX_CAL2$linear.spline[ind.val]<-ifelse(predict(model.linear.spline,newdata=lv2_lung_SEX_CAL2[ind.val,])<0,0,
                                                   predict(model.linear.spline,newdata=lv2_lung_SEX_CAL2[ind.val,]))
  
  
  
  MAPE_lv2_lung[(i+95),]<-c(unique(lv2_lung_SEX_CAL2$NAME[ind.tr]),
                       round(mean(abs(lv2_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv2_lung_SEX_CAL2$simple.reg[ind.val])/lv2_lung_SEX_CAL2$sum_LUNG_BZ[ind.val])*100,2),
                       round(mean(abs(lv2_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv2_lung_SEX_CAL2$linear.spline[ind.val])/lv2_lung_SEX_CAL2$sum_LUNG_BZ[ind.val])*100,2),
                       round(mean(abs(lv2_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv2_lung_SEX_CAL2$quadratic.reg[ind.val])/lv2_lung_SEX_CAL2$sum_LUNG_BZ[ind.val])*100,2))
  
  RMSE_lv2_lung[(i+95),]<-c(unique(lv2_lung_SEX_CAL2$NAME[ind.tr]),
                       round(sqrt(mean(abs(lv2_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv2_lung_SEX_CAL2$simple.reg[ind.val]))),2),
                       round(sqrt(mean(abs(lv2_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv2_lung_SEX_CAL2$linear.spline[ind.val]))),2),
                       round(sqrt(mean(abs(lv2_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv2_lung_SEX_CAL2$quadratic.reg[ind.val]))),2))
  
  
}

ggplot()+geom_line(data=lv2_lung_SEX_CAL2,aes(x=YEAR,y=sum_LUNG_BZ,color="red"),size=1)+labs(y="rate_of_lung")+
  facet_wrap(~ factor(NAME,levels=name_level),scales="free_y")+
  geom_line(data=lv2_lung_SEX_CAL2,aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv2_lung_SEX_CAL2,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv2_lung_SEX_CAL2,aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung[96:171,]),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung[96:171,]),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung[96:171,]),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of lung',
                       breaks=c("red","black","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

value_eklee_lv2_lung<-as.data.frame(rbind(lv2_lung_total[2:length(lv2_lung_total)],lv2_lung_SEX[3:length(lv2_lung_SEX)],lv2_lung_CAL2[3:length(lv2_lung_CAL2)],
                                     lv2_lung_SEX_CAL2[4:length(lv2_lung_SEX_CAL2)]))



#######################################################################################################3
##################### level 3 data ##########################
############# 1. grouping by UP2 #################
lv3_lung_total<-df_lung1%>%group_by(UP2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME=paste0("X",UP2))%>%as_tibble()

MAPE_lv3_lung<-matrix(0,nrow=length(unique(lv3_lung_total$NAME))*(1+2+2+4),ncol=4)
colnames(MAPE_lv3_lung)<-c("NAME","simple_reg","linear_spline","quadratic_reg")

RMSE_lv3_lung<-matrix(0,nrow=length(unique(lv3_lung_total$NAME))*(1+2+2+4),ncol=4)
colnames(RMSE_lv3_lung)<-c("NAME","simple_reg","linear_spline","quadratic_reg")

lv3_lung_total$simple.reg<-0
lv3_lung_total$linear.spline<-0
lv3_lung_total$quadratic.reg<-0

name_level<-unique(lv3_lung_total$NAME)

for(i in 1:length(name_level)){
  ind.tr<-which(lv3_lung_total$YEAR<=2014 & lv3_lung_total$NAME==name_level[i])
  ind.val<-which(lv3_lung_total$YEAR>2014 & lv3_lung_total$NAME==name_level[i])
  
  model.simple.reg<-lm(sum_LUNG_BZ~YEAR, data=lv3_lung_total[ind.tr,])
  lv3_lung_total$simple.reg[ind.tr]<-ifelse(model.simple.reg$fitted.values<0, 0, model.simple.reg$fitted.values)
  lv3_lung_total$simple.reg[ind.val]<-ifelse(predict(model.simple.reg,newdata=lv3_lung_total[ind.val,])<0,0,
                                             predict(model.simple.reg,newdata=lv3_lung_total[ind.val,]))
  
  
  
  model.quadratic.reg<-lm(sum_LUNG_BZ~poly(YEAR,2), data=lv3_lung_total[ind.tr,])
  lv3_lung_total$quadratic.reg[ind.tr]<-ifelse(model.quadratic.reg$fitted.values<0, 0, model.quadratic.reg$fitted.values)
  lv3_lung_total$quadratic.reg[ind.val]<-ifelse(predict(model.quadratic.reg,newdata=lv3_lung_total[ind.val,])<0,0,
                                                predict(model.quadratic.reg,newdata=lv3_lung_total[ind.val,]))
  
  model.linear.spline<-lm(sum_LUNG_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_lung_total[ind.tr,])
  lv3_lung_total$linear.spline[ind.tr]<-ifelse(model.linear.spline$fitted.values<0, 0, model.linear.spline$fitted.values)
  lv3_lung_total$linear.spline[ind.val]<-ifelse(predict(model.linear.spline,newdata=lv3_lung_total[ind.val,])<0,0,
                                                predict(model.linear.spline,newdata=lv3_lung_total[ind.val,]))
  
  
  MAPE_lv3_lung[i,]<-c(unique(lv3_lung_total$NAME[ind.tr]),
                  round(mean(abs(lv3_lung_total$sum_LUNG_BZ[ind.val]-lv3_lung_total$simple.reg[ind.val])/lv3_lung_total$sum_LUNG_BZ[ind.val])*100,2),
                  round(mean(abs(lv3_lung_total$sum_LUNG_BZ[ind.val]-lv3_lung_total$linear.spline[ind.val])/lv3_lung_total$sum_LUNG_BZ[ind.val])*100,2),
                  round(mean(abs(lv3_lung_total$sum_LUNG_BZ[ind.val]-lv3_lung_total$quadratic.reg[ind.val])/lv3_lung_total$sum_LUNG_BZ[ind.val])*100,2))
  RMSE_lv3_lung[i,]<-c(unique(lv3_lung_total$NAME[ind.tr]),
                  round(sqrt(mean(abs(lv3_lung_total$sum_LUNG_BZ[ind.val]-lv3_lung_total$simple.reg[ind.val]))),2),
                  round(sqrt(mean(abs(lv3_lung_total$sum_LUNG_BZ[ind.val]-lv3_lung_total$linear.spline[ind.val]))),2),
                  round(sqrt(mean(abs(lv3_lung_total$sum_LUNG_BZ[ind.val]-lv3_lung_total$quadratic.reg[ind.val]))),2))
  
  
}

ggplot()+geom_line(data=lv3_lung_total,aes(x=YEAR,y=sum_LUNG_BZ,color="red"),size=1)+labs(y="rate_of_lung")+
  facet_wrap(~ factor(NAME,levels=unique(lv3_lung_total$NAME)),scales="free_y")+
  geom_line(data=lv3_lung_total,aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Lung cancer Data comparing methods performance")+
  geom_line(data=lv3_lung_total,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv3_lung_total,aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of lung',
                       breaks=c("red","black","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


##################################################################################
############# 2. grouping by UP2 and SEX #################
lv3_lung_SEX<-df_lung1%>%group_by(UP2,SEX,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME=paste0("X",UP2,SEX))%>%as_tibble()

lv3_lung_SEX$simple.reg<-0
lv3_lung_SEX$linear.spline<-0
lv3_lung_SEX$quadratic.reg<-0

name_level<-unique(lv3_lung_SEX$NAME)

for(i in 1:length(name_level)){
  ind.tr<-which(lv3_lung_SEX$YEAR<=2014 & lv3_lung_SEX$NAME==name_level[i])
  ind.val<-which(lv3_lung_SEX$YEAR>2014 & lv3_lung_SEX$NAME==name_level[i])
  
  model.simple.reg<-lm(sum_LUNG_BZ~YEAR, data=lv3_lung_SEX[ind.tr,])
  lv3_lung_SEX$simple.reg[ind.tr]<-ifelse(model.simple.reg$fitted.values<0, 0, model.simple.reg$fitted.values)
  lv3_lung_SEX$simple.reg[ind.val]<-ifelse(predict(model.simple.reg,newdata=lv3_lung_SEX[ind.val,])<0,0,
                                           predict(model.simple.reg,newdata=lv3_lung_SEX[ind.val,]))
  
  
  
  model.quadratic.reg<-lm(sum_LUNG_BZ~poly(YEAR,2), data=lv3_lung_SEX[ind.tr,])
  lv3_lung_SEX$quadratic.reg[ind.tr]<-ifelse(model.quadratic.reg$fitted.values<0, 0, model.quadratic.reg$fitted.values)
  lv3_lung_SEX$quadratic.reg[ind.val]<-ifelse(predict(model.quadratic.reg,newdata=lv3_lung_SEX[ind.val,])<0,0,
                                              predict(model.quadratic.reg,newdata=lv3_lung_SEX[ind.val,]))
  
  model.linear.spline<-lm(sum_LUNG_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_lung_SEX[ind.tr,])
  lv3_lung_SEX$linear.spline[ind.tr]<-ifelse(model.linear.spline$fitted.values<0, 0, model.linear.spline$fitted.values)
  lv3_lung_SEX$linear.spline[ind.val]<-ifelse(predict(model.linear.spline,newdata=lv3_lung_SEX[ind.val,])<0,0,
                                              predict(model.linear.spline,newdata=lv3_lung_SEX[ind.val,]))
  
  
  MAPE_lv3_lung[(i+74),]<-c(unique(lv3_lung_SEX$NAME[ind.tr]),
                       round(mean(abs(lv3_lung_SEX$sum_LUNG_BZ[ind.val]-lv3_lung_SEX$simple.reg[ind.val])/lv3_lung_SEX$sum_LUNG_BZ[ind.val])*100,2),
                       round(mean(abs(lv3_lung_SEX$sum_LUNG_BZ[ind.val]-lv3_lung_SEX$linear.spline[ind.val])/lv3_lung_SEX$sum_LUNG_BZ[ind.val])*100,2),
                       round(mean(abs(lv3_lung_SEX$sum_LUNG_BZ[ind.val]-lv3_lung_SEX$quadratic.reg[ind.val])/lv3_lung_SEX$sum_LUNG_BZ[ind.val])*100,2))
  RMSE_lv3_lung[(i+74),]<-c(unique(lv3_lung_SEX$NAME[ind.tr]),
                       round(sqrt(mean(abs(lv3_lung_SEX$sum_LUNG_BZ[ind.val]-lv3_lung_SEX$simple.reg[ind.val]))),2),
                       round(sqrt(mean(abs(lv3_lung_SEX$sum_LUNG_BZ[ind.val]-lv3_lung_SEX$linear.spline[ind.val]))),2),
                       round(sqrt(mean(abs(lv3_lung_SEX$sum_LUNG_BZ[ind.val]-lv3_lung_SEX$quadratic.reg[ind.val]))),2))
  
  
}

ind.M<-grep("M",MAPE_lv3_lung[,1])
MAPE_lv3_M_lung<-MAPE_lv3_lung[ind.M,]

ind.F<-grep("F",MAPE_lv3_lung[,1])
MAPE_lv3_F_lung<-MAPE_lv3_lung[ind.F,]

ggplot()+geom_line(data=lv3_lung_SEX[lv3_lung_SEX$SEX=="F",],aes(x=YEAR,y=sum_LUNG_BZ,color="red"),size=1)+labs(y="rate_of_lung")+
  facet_wrap(~ factor(NAME,levels=unique(lv3_lung_SEX$NAME[lv3_lung_SEX$SEX=="F"])),scales="free_y")+
  geom_line(data=lv3_lung_SEX[lv3_lung_SEX$SEX=="F",],aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Lung cancer Data comparing methods performance")+
  geom_line(data=lv3_lung_SEX[lv3_lung_SEX$SEX=="F",],aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv3_lung_SEX[lv3_lung_SEX$SEX=="F",],aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_F_lung),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_F_lung),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_F_lung),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of lung',
                       breaks=c("red","black","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=lv3_lung_SEX[lv3_lung_SEX$SEX=="M",],aes(x=YEAR,y=sum_LUNG_BZ,color="red"),size=1)+labs(y="rate_of_lung")+
  facet_wrap(~ factor(NAME,levels=unique(lv3_lung_SEX$NAME[lv3_lung_SEX$SEX=="M"])),scales="free_y")+
  geom_line(data=lv3_lung_SEX[lv3_lung_SEX$SEX=="M",],aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Lung cancer Data comparing methods performance")+
  geom_line(data=lv3_lung_SEX[lv3_lung_SEX$SEX=="M",],aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv3_lung_SEX[lv3_lung_SEX$SEX=="M",],aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_M_lung),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_M_lung),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_M_lung),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of lung',
                       breaks=c("red","black","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


################################################################################################
############### 3. grouping by UP2 and CAL2 ######################### 
lv3_lung_CAL2<-df_lung1%>%group_by(UP2,CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME=paste0("X",UP2,CAL2))%>%as_tibble()

name_level<-unique(lv3_lung_CAL2$NAME)

lv3_lung_CAL2$simple.reg<-0
lv3_lung_CAL2$linear.spline<-0
lv3_lung_CAL2$quadratic.reg<-0

for(i in 1:length(name_level)){
  ind.tr<-which(lv3_lung_CAL2$YEAR<=2014 & lv3_lung_CAL2$NAME==name_level[i])
  ind.val<-which(lv3_lung_CAL2$YEAR>2014 & lv3_lung_CAL2$NAME==name_level[i])
  
  model.simple.reg<-lm(sum_LUNG_BZ~YEAR, data=lv3_lung_CAL2[ind.tr,])
  lv3_lung_CAL2$simple.reg[ind.tr]<-ifelse(model.simple.reg$fitted.values<0, 0, model.simple.reg$fitted.values)
  lv3_lung_CAL2$simple.reg[ind.val]<-ifelse(predict(model.simple.reg,newdata=lv3_lung_CAL2[ind.val,])<0,0,
                                            predict(model.simple.reg,newdata=lv3_lung_CAL2[ind.val,]))
  
  
  
  model.quadratic.reg<-lm(sum_LUNG_BZ~poly(YEAR,2), data=lv3_lung_CAL2[ind.tr,])
  lv3_lung_CAL2$quadratic.reg[ind.tr]<-ifelse(model.quadratic.reg$fitted.values<0, 0, model.quadratic.reg$fitted.values)
  lv3_lung_CAL2$quadratic.reg[ind.val]<-ifelse(predict(model.quadratic.reg,newdata=lv3_lung_CAL2[ind.val,])<0,0,
                                               predict(model.quadratic.reg,newdata=lv3_lung_CAL2[ind.val,]))
  
  model.linear.spline<-lm(sum_LUNG_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_lung_CAL2[ind.tr,])
  lv3_lung_CAL2$linear.spline[ind.tr]<-ifelse(model.linear.spline$fitted.values<0, 0, model.linear.spline$fitted.values)
  lv3_lung_CAL2$linear.spline[ind.val]<-ifelse(predict(model.linear.spline,newdata=lv3_lung_CAL2[ind.val,])<0,0,
                                               predict(model.linear.spline,newdata=lv3_lung_CAL2[ind.val,]))
  
  
  
  MAPE_lv3_lung[(i+222),]<-c(unique(lv3_lung_CAL2$NAME[ind.tr]),
                        round(mean(abs(lv3_lung_CAL2$sum_LUNG_BZ[ind.val]-lv3_lung_CAL2$simple.reg[ind.val])/lv3_lung_CAL2$sum_LUNG_BZ[ind.val])*100,2),
                        round(mean(abs(lv3_lung_CAL2$sum_LUNG_BZ[ind.val]-lv3_lung_CAL2$linear.spline[ind.val])/lv3_lung_CAL2$sum_LUNG_BZ[ind.val])*100,2),
                        round(mean(abs(lv3_lung_CAL2$sum_LUNG_BZ[ind.val]-lv3_lung_CAL2$quadratic.reg[ind.val])/lv3_lung_CAL2$sum_LUNG_BZ[ind.val])*100,2))
  
  RMSE_lv3_lung[(i+222),]<-c(unique(lv3_lung_CAL2$NAME[ind.tr]),
                        round(sqrt(mean(abs(lv3_lung_CAL2$sum_LUNG_BZ[ind.val]-lv3_lung_CAL2$simple.reg[ind.val]))),2),
                        round(sqrt(mean(abs(lv3_lung_CAL2$sum_LUNG_BZ[ind.val]-lv3_lung_CAL2$linear.spline[ind.val]))),2),
                        round(sqrt(mean(abs(lv3_lung_CAL2$sum_LUNG_BZ[ind.val]-lv3_lung_CAL2$quadratic.reg[ind.val]))),2))
  
  
}

ind.01<-grep("01",MAPE_lv3_lung[,1])
MAPE_lv3_01_lung<-MAPE_lv3_lung[ind.01,]

ind.234<-grep("234",MAPE_lv3_lung[,1])
MAPE_lv3_234_lung<-MAPE_lv3_lung[ind.234,]

ggplot()+geom_line(data=lv3_lung_CAL2[lv3_lung_CAL2$CAL2=="01",],aes(x=YEAR,y=sum_LUNG_BZ,color="red"),size=1)+labs(y="rate_of_lung")+
  facet_wrap(~ factor(NAME,levels=unique(lv3_lung_CAL2$NAME[lv3_lung_CAL2$CAL2=="01"])),scales="free_y")+
  geom_line(data=lv3_lung_CAL2[lv3_lung_CAL2$CAL2=="01",],aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Lung cancer Data comparing methods performance")+
  geom_line(data=lv3_lung_CAL2[lv3_lung_CAL2$CAL2=="01",],aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv3_lung_CAL2[lv3_lung_CAL2$CAL2=="01",],aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_01_lung),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_01_lung),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_01_lung),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of lung',
                       breaks=c("red","black","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


ggplot()+geom_line(data=lv3_lung_CAL2[lv3_lung_CAL2$CAL2=="234",],aes(x=YEAR,y=sum_LUNG_BZ,color="red"),size=1)+labs(y="rate_of_lung")+
  facet_wrap(~ factor(NAME,levels=unique(lv3_lung_CAL2$NAME[lv3_lung_CAL2$CAL2=="234"])),scales="free_y")+
  geom_line(data=lv3_lung_CAL2[lv3_lung_CAL2$CAL2=="234",],aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Lung cancer Data comparing methods performance")+
  geom_line(data=lv3_lung_CAL2[lv3_lung_CAL2$CAL2=="234",],aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv3_lung_CAL2[lv3_lung_CAL2$CAL2=="234",],aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_234_lung),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_234_lung),mapping = aes(x = 2004, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_234_lung),mapping = aes(x = 2004, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of lung',
                       breaks=c("red","black","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

################################################################################################
############### 3. grouping by UP2 and SEX and CAL2 ######################### 
lv3_lung_SEX_CAL2<-df_lung1%>%group_by(UP2,SEX,CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME=paste0("X",UP2,SEX,CAL2))%>%as_tibble()

name_level<-unique(lv3_lung_SEX_CAL2$NAME)

lv3_lung_SEX_CAL2$simple.reg<-0
lv3_lung_SEX_CAL2$linear.spline<-0
lv3_lung_SEX_CAL2$quadratic.reg<-0

for(i in 1:length(name_level)){
  ind.tr<-which(lv3_lung_SEX_CAL2$YEAR<=2014 & lv3_lung_SEX_CAL2$NAME==name_level[i])
  ind.val<-which(lv3_lung_SEX_CAL2$YEAR>2014 & lv3_lung_SEX_CAL2$NAME==name_level[i])
  
  model.simple.reg<-lm(sum_LUNG_BZ~YEAR, data=lv3_lung_SEX_CAL2[ind.tr,])
  lv3_lung_SEX_CAL2$simple.reg[ind.tr]<-ifelse(model.simple.reg$fitted.values<0, 0, model.simple.reg$fitted.values)
  lv3_lung_SEX_CAL2$simple.reg[ind.val]<-ifelse(predict(model.simple.reg,newdata=lv3_lung_SEX_CAL2[ind.val,])<0,0,
                                                predict(model.simple.reg,newdata=lv3_lung_SEX_CAL2[ind.val,]))
  
  
  
  model.quadratic.reg<-lm(sum_LUNG_BZ~poly(YEAR,2), data=lv3_lung_SEX_CAL2[ind.tr,])
  lv3_lung_SEX_CAL2$quadratic.reg[ind.tr]<-ifelse(model.quadratic.reg$fitted.values<0, 0, model.quadratic.reg$fitted.values)
  lv3_lung_SEX_CAL2$quadratic.reg[ind.val]<-ifelse(predict(model.quadratic.reg,newdata=lv3_lung_SEX_CAL2[ind.val,])<0,0,
                                                   predict(model.quadratic.reg,newdata=lv3_lung_SEX_CAL2[ind.val,]))
  
  model.linear.spline<-lm(sum_LUNG_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv3_lung_SEX_CAL2[ind.tr,])
  lv3_lung_SEX_CAL2$linear.spline[ind.tr]<-ifelse(model.linear.spline$fitted.values<0, 0, model.linear.spline$fitted.values)
  lv3_lung_SEX_CAL2$linear.spline[ind.val]<-ifelse(predict(model.linear.spline,newdata=lv3_lung_SEX_CAL2[ind.val,])<0,0,
                                                   predict(model.linear.spline,newdata=lv3_lung_SEX_CAL2[ind.val,]))
  
  
  
  MAPE_lv3_lung[(i+370),]<-c(unique(lv3_lung_SEX_CAL2$NAME[ind.tr]),
                        round(mean(abs(lv3_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv3_lung_SEX_CAL2$simple.reg[ind.val])/lv3_lung_SEX_CAL2$sum_LUNG_BZ[ind.val])*100,2),
                        round(mean(abs(lv3_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv3_lung_SEX_CAL2$linear.spline[ind.val])/lv3_lung_SEX_CAL2$sum_LUNG_BZ[ind.val])*100,2),
                        round(mean(abs(lv3_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv3_lung_SEX_CAL2$quadratic.reg[ind.val])/lv3_lung_SEX_CAL2$sum_LUNG_BZ[ind.val])*100,2))
  
  RMSE_lv3_lung[(i+370),]<-c(unique(lv3_lung_SEX_CAL2$NAME[ind.tr]),
                        round(sqrt(mean(abs(lv3_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv3_lung_SEX_CAL2$simple.reg[ind.val]))),2),
                        round(sqrt(mean(abs(lv3_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv3_lung_SEX_CAL2$linear.spline[ind.val]))),2),
                        round(sqrt(mean(abs(lv3_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv3_lung_SEX_CAL2$quadratic.reg[ind.val]))),2))
  
  
}

ind.M.01<-which(grepl("M",MAPE_lv3_lung[,1]) & grepl("01",MAPE_lv3_lung[,1]))
MAPE_lv3_lung_M_01<-MAPE_lv3_lung[ind.M.01,]

ind.F.01<-which(grepl("F",MAPE_lv3_lung[,1]) & grepl("01",MAPE_lv3_lung[,1]))
MAPE_lv3_lung_F_01<-MAPE_lv3_lung[ind.F.01,]

ind.M.234<-which(grepl("M",MAPE_lv3_lung[,1]) & grepl("234",MAPE_lv3_lung[,1]))
MAPE_lv3_lung_M_234<-MAPE_lv3_lung[ind.M.234,]

ind.F.234<-which(grepl("F",MAPE_lv3_lung[,1]) & grepl("234",MAPE_lv3_lung[,1]))
MAPE_lv3_lung_F_234<-MAPE_lv3_lung[ind.F.234,]

########## M & 01 ############
ggplot()+geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="M" & lv3_lung_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=sum_LUNG_BZ,color="red"),size=1)+labs(y="rate_of_lung")+
  facet_wrap(~ factor(NAME,levels=unique(MAPE_lv3_lung_M_01[,1])),scales="free_y")+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="M" & lv3_lung_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Lung cancer Data comparing methods performance")+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="M" & lv3_lung_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="M" & lv3_lung_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_M_01),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_M_01),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_M_01),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of lung',
                       breaks=c("red","black","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

########## M & 234 ############
ggplot()+geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="M" & lv3_lung_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=sum_LUNG_BZ,color="red"),size=1)+labs(y="rate_of_lung")+
  facet_wrap(~ factor(NAME,levels=unique(MAPE_lv3_lung_M_234[,1])),scales="free_y")+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="M" & lv3_lung_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Lung cancer Data comparing methods performance")+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="M" & lv3_lung_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="M" & lv3_lung_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_M_234),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_M_234),mapping = aes(x = 2004, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_M_234),mapping = aes(x = 2004, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of lung',
                       breaks=c("red","black","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

########## F & 01 ############
ggplot()+geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="F" & lv3_lung_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=sum_LUNG_BZ,color="red"),size=1)+labs(y="rate_of_lung")+
  facet_wrap(~ factor(NAME,levels=unique(MAPE_lv3_lung_F_01[,1])),scales="free_y")+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="F" & lv3_lung_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Lung cancer Data comparing methods performance")+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="F" & lv3_lung_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="F" & lv3_lung_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_F_01),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_F_01),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_F_01),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of lung',
                       breaks=c("red","black","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

########## F & 234 ############
ggplot()+geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="F" & lv3_lung_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=sum_LUNG_BZ,color="red"),size=1)+labs(y="rate_of_lung")+
  facet_wrap(~ factor(NAME,levels=unique(MAPE_lv3_lung_F_234[,1])),scales="free_y")+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="F" & lv3_lung_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Lung cancer Data comparing methods performance")+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="F" & lv3_lung_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="F" & lv3_lung_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_F_234),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_F_234),mapping = aes(x = 2004, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_F_234),mapping = aes(x = 2004, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of lung',
                       breaks=c("red","black","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

value_eklee_lv3_lung<-as.data.frame(rbind(lv3_lung_total[2:length(lv3_lung_total)],lv3_lung_SEX[3:length(lv3_lung_SEX)],lv3_lung_CAL2[3:length(lv3_lung_CAL2)],
                                     lv3_lung_SEX_CAL2[4:length(lv3_lung_SEX_CAL2)]))