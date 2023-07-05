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
lv1_leukemia_total$poisson.reg<-0
lv1_leukemia_total$linear.spline<-0
lv1_leukemia_total$quadratic.reg<-0

lv1_leukemia_total$simple.reg.FY<-0
lv1_leukemia_total$poisson.reg.FY<-0
lv1_leukemia_total$linear.spline.FY<-0
lv1_leukemia_total$quadratic.reg.FY<-0


ind.tr<-which(lv1_leukemia_total$YEAR<=2014)
ind.val<-which(lv1_leukemia_total$YEAR>2014)

model.simple.reg<-lm(sum_LEUKEMIA_BZ~YEAR, data=lv1_leukemia_total[ind.tr,])
lv1_leukemia_total$simple.reg[ind.tr]<-model.simple.reg$fitted.values
lv1_leukemia_total$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv1_leukemia_total[ind.val,])

model.simple.reg.FY<-lm(sum_FY_BZ~YEAR, data=lv1_leukemia_total[ind.tr,])
lv1_leukemia_total$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
lv1_leukemia_total$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv1_leukemia_total[ind.val,])

model.poisson.reg<-glm(sum_LEUKEMIA_BZ~YEAR, data=lv1_leukemia_total[ind.tr,],family='poisson')
lv1_leukemia_total$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
lv1_leukemia_total$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv1_leukemia_total[ind.val,]))

model.poisson.reg.FY<-glm(sum_FY_BZ~YEAR, data=lv1_leukemia_total[ind.tr,],family='poisson')
lv1_leukemia_total$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
lv1_leukemia_total$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv1_leukemia_total[ind.val,]))


model.quadratic.reg<-lm(sum_LEUKEMIA_BZ~poly(YEAR,2), data=lv1_leukemia_total[ind.tr,])
lv1_leukemia_total$quadratic.reg[ind.tr]<-ifelse(model.quadratic.reg$fitted.values<0, 0, model.quadratic.reg$fitted.values)
lv1_leukemia_total$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv1_leukemia_total[ind.val,])

model.quadratic.reg.FY<-lm(sum_FY_BZ~poly(YEAR,2), data=lv1_leukemia_total[ind.tr,])
lv1_leukemia_total$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
lv1_leukemia_total$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv1_leukemia_total[ind.val,])


model.linear.spline<-lm(sum_LEUKEMIA_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_leukemia_total[ind.tr,])
lv1_leukemia_total$linear.spline[ind.tr]<-model.linear.spline$fitted.values
lv1_leukemia_total$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv1_leukemia_total[ind.val,])

model.linear.spline.FY<-lm(sum_FY_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_leukemia_total[ind.tr,])
lv1_leukemia_total$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
lv1_leukemia_total$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv1_leukemia_total[ind.val,])

MAPE_lv1_leukemia<-matrix(0,nrow=9,ncol=9)
colnames(MAPE_lv1_leukemia)<-c("NAME","simple_reg","poisson_reg","linear_spline","quadratic_reg","simple_reg_FY","poisson_reg_FY","linear_spline_FY",
                               "quadratic_reg_FY")

RMSE_lv1_leukemia<-matrix(0,nrow=9,ncol=9)
colnames(RMSE_lv1_leukemia)<-c("NAME","simple_reg","poisson_reg","linear_spline","quadratic_reg","simple_reg_FY","poisson_reg_FY","linear_spline_FY",
                               "quadratic_reg_FY")

MAPE_lv1_leukemia[1,]<-c(unique(lv1_leukemia_total$NAME),
                         round(mean(abs(lv1_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_total$simple.reg[ind.val])/lv1_leukemia_total$sum_LEUKEMIA_BZ[ind.val])*100,2),
                         round(mean(abs(lv1_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_total$poisson.reg[ind.val])/lv1_leukemia_total$sum_LEUKEMIA_BZ[ind.val])*100,2),
                         round(mean(abs(lv1_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_total$linear.spline[ind.val])/lv1_leukemia_total$sum_LEUKEMIA_BZ[ind.val])*100,2),
                         round(mean(abs(lv1_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_total$quadratic.reg[ind.val])/lv1_leukemia_total$sum_LEUKEMIA_BZ[ind.val])*100,2),
                         round(mean(abs(lv1_leukemia_total$sum_FY_BZ[ind.val]-lv1_leukemia_total$simple.reg.FY[ind.val])/lv1_leukemia_total$sum_FY_BZ[ind.val])*100,2),
                         round(mean(abs(lv1_leukemia_total$sum_FY_BZ[ind.val]-lv1_leukemia_total$poisson.reg.FY[ind.val])/lv1_leukemia_total$sum_FY_BZ[ind.val])*100,2),
                         round(mean(abs(lv1_leukemia_total$sum_FY_BZ[ind.val]-lv1_leukemia_total$linear.spline.FY[ind.val])/lv1_leukemia_total$sum_FY_BZ[ind.val])*100,2),
                         round(mean(abs(lv1_leukemia_total$sum_FY_BZ[ind.val]-lv1_leukemia_total$quadratic.reg.FY[ind.val])/lv1_leukemia_total$sum_FY_BZ[ind.val])*100,2))


RMSE_lv1_leukemia[1,]<-c(unique(lv1_leukemia_total$NAME),
                         round(sqrt(mean(abs(lv1_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_total$simple.reg[ind.val]))),2),
                         round(sqrt(mean(abs(lv1_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_total$poisson.reg[ind.val]))),2),
                         round(sqrt(mean(abs(lv1_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_total$linear.spline[ind.val]))),2),
                         round(sqrt(mean(abs(lv1_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_total$quadratic.reg[ind.val]))),2),
                         round(sqrt(mean(abs(lv1_leukemia_total$sum_FY_BZ[ind.val]-lv1_leukemia_total$simple.reg.FY[ind.val]))),2),
                         round(sqrt(mean(abs(lv1_leukemia_total$sum_FY_BZ[ind.val]-lv1_leukemia_total$poisson.reg.FY[ind.val]))),2),
                         round(sqrt(mean(abs(lv1_leukemia_total$sum_FY_BZ[ind.val]-lv1_leukemia_total$linear.spline.FY[ind.val]))),2),
                         round(sqrt(mean(abs(lv1_leukemia_total$sum_FY_BZ[ind.val]-lv1_leukemia_total$quadratic.reg.FY[ind.val]))),2))

#plotsPath = "allPlots.pdf"
#pdf(file=plotsPath,width=3000,height=2500)  
ggplot()+geom_line(data=lv1_leukemia_total,aes(x=YEAR,y=sum_LEUKEMIA_BZ,color="red"),size=1)+labs(y="#of_leukemia")+
  geom_line(data=lv1_leukemia_total,aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv1_leukemia_total,aes(x=YEAR,y=poisson.reg,color="green"),size=1)+
  geom_line(data=lv1_leukemia_total,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv1_leukemia_total,aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  labs(x="Year")+
  scale_color_identity(name='#of leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=lv1_leukemia_total,aes(x=YEAR,y=sum_FY_BZ,color="red"),size=1)+labs(y="FY_BZ_leukemia")+
  geom_line(data=lv1_leukemia_total,aes(x=YEAR,y=simple.reg.FY,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv1_leukemia_total,aes(x=YEAR,y=poisson.reg.FY,color="green"),size=1)+
  geom_line(data=lv1_leukemia_total,aes(x=YEAR,y=linear.spline.FY,color='darkviolet'),size=1)+
  geom_line(data=lv1_leukemia_total,aes(x=YEAR,y=quadratic.reg.FY,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  labs(x="Year")+
  scale_color_identity(name='FY_BZ leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of FY_BZ","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

####################################################################################
#################### 2. grouping by SEX ##################
lv1_leukemia_SEX<-df_leukemia1%>%group_by(SEX,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  mutate(NAME=paste0("X","_",SEX))%>%as_tibble()

SEX_level<-unique(lv1_leukemia_SEX$SEX)

lv1_leukemia_SEX$simple.reg<-0
lv1_leukemia_SEX$poisson.reg<-0
lv1_leukemia_SEX$linear.spline<-0
lv1_leukemia_SEX$quadratic.reg<-0

lv1_leukemia_SEX$simple.reg.FY<-0
lv1_leukemia_SEX$poisson.reg.FY<-0
lv1_leukemia_SEX$linear.spline.FY<-0
lv1_leukemia_SEX$quadratic.reg.FY<-0
SEX_level<-unique(lv1_leukemia_SEX$SEX)

for(i in 1:length(SEX_level)){
  ind.tr<-which(lv1_leukemia_SEX$YEAR<=2014 & lv1_leukemia_SEX$SEX==SEX_level[i])
  ind.val<-which(lv1_leukemia_SEX$YEAR>2014 & lv1_leukemia_SEX$SEX==SEX_level[i])
  
  model.simple.reg<-lm(sum_LEUKEMIA_BZ~YEAR, data=lv1_leukemia_SEX[ind.tr,])
  lv1_leukemia_SEX$simple.reg[ind.tr]<-ifelse(model.simple.reg$fitted.values<0, 0, model.simple.reg$fitted.values)
  lv1_leukemia_SEX$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv1_leukemia_SEX[ind.val,])
  
  model.simple.reg.FY<-lm(sum_FY_BZ~YEAR, data=lv1_leukemia_SEX[ind.tr,])
  lv1_leukemia_SEX$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
  lv1_leukemia_SEX$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv1_leukemia_SEX[ind.val,])
  
  model.poisson.reg<-glm(sum_LEUKEMIA_BZ~YEAR, data=lv1_leukemia_SEX[ind.tr,],family='poisson')
  lv1_leukemia_SEX$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
  lv1_leukemia_SEX$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv1_leukemia_SEX[ind.val,]))
  
  model.poisson.reg.FY<-lm(sum_FY_BZ~YEAR, data=lv1_leukemia_SEX[ind.tr,],family='poisson')
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
  
  
  
  MAPE_lv1_leukemia[(i+1),]<-c(unique(lv1_leukemia_SEX$NAME[ind.tr]),
                               round(mean(abs(lv1_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_SEX$simple.reg[ind.val])/lv1_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val])*100,2),
                               round(mean(abs(lv1_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_SEX$poisson.reg[ind.val])/lv1_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val])*100,2),
                               round(mean(abs(lv1_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_SEX$linear.spline[ind.val])/lv1_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val])*100,2),
                               round(mean(abs(lv1_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_SEX$quadratic.reg[ind.val])/lv1_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val])*100,2),
                               round(mean(abs(lv1_leukemia_SEX$sum_FY_BZ[ind.val]-lv1_leukemia_SEX$simple.reg.FY[ind.val])/lv1_leukemia_SEX$sum_FY_BZ[ind.val])*100,2),
                               round(mean(abs(lv1_leukemia_SEX$sum_FY_BZ[ind.val]-lv1_leukemia_SEX$poisson.reg.FY[ind.val])/lv1_leukemia_SEX$sum_FY_BZ[ind.val])*100,2),
                               round(mean(abs(lv1_leukemia_SEX$sum_FY_BZ[ind.val]-lv1_leukemia_SEX$linear.spline.FY[ind.val])/lv1_leukemia_SEX$sum_FY_BZ[ind.val])*100,2),
                               round(mean(abs(lv1_leukemia_SEX$sum_FY_BZ[ind.val]-lv1_leukemia_SEX$quadratic.reg.FY[ind.val])/lv1_leukemia_SEX$sum_FY_BZ[ind.val])*100,2))
  
  RMSE_lv1_leukemia[(i+1),]<-c(unique(lv1_leukemia_SEX$NAME[ind.tr]),
                               round(sqrt(mean(abs(lv1_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_SEX$simple.reg[ind.val]))),2),
                               round(sqrt(mean(abs(lv1_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_SEX$poisson.reg[ind.val]))),2),
                               round(sqrt(mean(abs(lv1_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_SEX$linear.spline[ind.val]))),2),
                               round(sqrt(mean(abs(lv1_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_SEX$quadratic.reg[ind.val]))),2),
                               round(sqrt(mean(abs(lv1_leukemia_SEX$sum_FY_BZ[ind.val]-lv1_leukemia_SEX$simple.reg.FY[ind.val]))),2),
                               round(sqrt(mean(abs(lv1_leukemia_SEX$sum_FY_BZ[ind.val]-lv1_leukemia_SEX$poisson.reg.FY[ind.val]))),2),
                               round(sqrt(mean(abs(lv1_leukemia_SEX$sum_FY_BZ[ind.val]-lv1_leukemia_SEX$linear.spline.FY[ind.val]))),2),
                               round(sqrt(mean(abs(lv1_leukemia_SEX$sum_FY_BZ[ind.val]-lv1_leukemia_SEX$quadratic.reg.FY[ind.val]))),2))
  
}



ggplot()+geom_line(data=lv1_leukemia_SEX,aes(x=YEAR,y=sum_LEUKEMIA_BZ,color="red"),size=1)+labs(y="#of_leukemia")+
  facet_wrap(~ factor(SEX,levels=SEX_level),scales="free_y")+
  geom_line(data=lv1_leukemia_SEX,aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv1_leukemia_SEX,aes(x=YEAR,y=poisson.reg,color="green"),size=1)+
  geom_line(data=lv1_leukemia_SEX,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv1_leukemia_SEX,aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  labs(x="Year")+
  scale_color_identity(name='#of leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=lv1_leukemia_SEX,aes(x=YEAR,y=sum_FY_BZ,color="red"),size=1)+labs(y="#of FY_BZ")+
  facet_wrap(~ factor(SEX,levels=SEX_level),scales="free_y")+
  geom_line(data=lv1_leukemia_SEX,aes(x=YEAR,y=simple.reg.FY,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv1_leukemia_SEX,aes(x=YEAR,y=poisson.reg.FY,color="green"),size=1)+
  geom_line(data=lv1_leukemia_SEX,aes(x=YEAR,y=linear.spline.FY,color='darkviolet'),size=1)+
  geom_line(data=lv1_leukemia_SEX,aes(x=YEAR,y=quadratic.reg.FY,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  labs(x="Year")+
  scale_color_identity(name='#of FY_BZ',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of FY_BZ","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))



################################################################################################
############### 3. grouping by CAL2 ######################### 
lv1_leukemia_CAL2<-df_leukemia1%>%group_by(CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  mutate(NAME=paste0("X","_",CAL2))%>%as_tibble()

CAL2_level<-unique(lv1_leukemia_CAL2$CAL2)

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
  ind.tr<-which(lv1_leukemia_CAL2$YEAR<=2014 & lv1_leukemia_CAL2$CAL2==CAL2_level[i])
  ind.val<-which(lv1_leukemia_CAL2$YEAR>2014 & lv1_leukemia_CAL2$CAL2==CAL2_level[i])
  
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
  
  
  
  MAPE_lv1_leukemia[(i+3),]<-c(unique(lv1_leukemia_CAL2$NAME[ind.tr]),
                               round(mean(abs(lv1_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_CAL2$simple.reg[ind.val])/lv1_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2),
                               round(mean(abs(lv1_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_CAL2$poisson.reg[ind.val])/lv1_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2),
                               round(mean(abs(lv1_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_CAL2$linear.spline[ind.val])/lv1_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2),
                               round(mean(abs(lv1_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_CAL2$quadratic.reg[ind.val])/lv1_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2),
                               round(mean(abs(lv1_leukemia_CAL2$sum_FY_BZ[ind.val]-lv1_leukemia_CAL2$simple.reg.FY[ind.val])/lv1_leukemia_CAL2$sum_FY_BZ[ind.val])*100,2),
                               round(mean(abs(lv1_leukemia_CAL2$sum_FY_BZ[ind.val]-lv1_leukemia_CAL2$poisson.reg.FY[ind.val])/lv1_leukemia_CAL2$sum_FY_BZ[ind.val])*100,2),
                               round(mean(abs(lv1_leukemia_CAL2$sum_FY_BZ[ind.val]-lv1_leukemia_CAL2$linear.spline.FY[ind.val])/lv1_leukemia_CAL2$sum_FY_BZ[ind.val])*100,2),
                               round(mean(abs(lv1_leukemia_CAL2$sum_FY_BZ[ind.val]-lv1_leukemia_CAL2$quadratic.reg.FY[ind.val])/lv1_leukemia_CAL2$sum_FY_BZ[ind.val])*100,2))
  
  RMSE_lv1_leukemia[(i+3),]<-c(unique(lv1_leukemia_CAL2$NAME[ind.tr]),
                               round(sqrt(mean(abs(lv1_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_CAL2$simple.reg[ind.val]))),2),
                               round(sqrt(mean(abs(lv1_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_CAL2$poisson.reg[ind.val]))),2),
                               round(sqrt(mean(abs(lv1_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_CAL2$linear.spline[ind.val]))),2),
                               round(sqrt(mean(abs(lv1_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_CAL2$quadratic.reg[ind.val]))),2),
                               round(sqrt(mean(abs(lv1_leukemia_CAL2$sum_FY_BZ[ind.val]-lv1_leukemia_CAL2$simple.reg.FY[ind.val]))),2),
                               round(sqrt(mean(abs(lv1_leukemia_CAL2$sum_FY_BZ[ind.val]-lv1_leukemia_CAL2$poisson.reg.FY[ind.val]))),2),
                               round(sqrt(mean(abs(lv1_leukemia_CAL2$sum_FY_BZ[ind.val]-lv1_leukemia_CAL2$linear.spline.FY[ind.val]))),2),
                               round(sqrt(mean(abs(lv1_leukemia_CAL2$sum_FY_BZ[ind.val]-lv1_leukemia_CAL2$quadratic.reg.FY[ind.val]))),2))
  
  
}



ggplot()+geom_line(data=lv1_leukemia_CAL2,aes(x=YEAR,y=sum_LEUKEMIA_BZ,color="red"),size=1)+labs(y="#of_leukemia")+
  facet_wrap(~ factor(CAL2,levels=CAL2_level),scales="free_y")+
  geom_line(data=lv1_leukemia_CAL2,aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv1_leukemia_CAL2,aes(x=YEAR,y=poisson.reg,color="green"),size=1)+
  geom_line(data=lv1_leukemia_CAL2,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv1_leukemia_CAL2,aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  labs(x="Year")+
  scale_color_identity(name='#of leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=lv1_leukemia_CAL2,aes(x=YEAR,y=sum_FY_BZ,color="red"),size=1)+labs(y="#of FY_BZ")+
  facet_wrap(~ factor(CAL2,levels=CAL2_level),scales="free_y")+
  geom_line(data=lv1_leukemia_CAL2,aes(x=YEAR,y=simple.reg.FY,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv1_leukemia_CAL2,aes(x=YEAR,y=poisson.reg.FY,color="green"),size=1)+
  geom_line(data=lv1_leukemia_CAL2,aes(x=YEAR,y=linear.spline.FY,color='darkviolet'),size=1)+
  geom_line(data=lv1_leukemia_CAL2,aes(x=YEAR,y=quadratic.reg.FY,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  labs(x="Year")+
  scale_color_identity(name='#of FY_BZ',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of FY_BZ","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

################################################################################################
############### 4. grouping by SEX and CAL2 ######################### 
lv1_leukemia_SEX_CAL2<-df_leukemia1%>%group_by(SEX,CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  mutate(NAME=paste0("X","_",SEX,CAL2))%>%as_tibble()

SEX_CAL2_level<-unique(lv1_leukemia_SEX_CAL2$NAME)

lv1_leukemia_SEX_CAL2$simple.reg<-0
lv1_leukemia_SEX_CAL2$poisson.reg<-0
lv1_leukemia_SEX_CAL2$linear.spline<-0
lv1_leukemia_SEX_CAL2$quadratic.reg<-0

lv1_leukemia_SEX_CAL2$simple.reg.FY<-0
lv1_leukemia_SEX_CAL2$poisson.reg.FY<-0
lv1_leukemia_SEX_CAL2$linear.spline.FY<-0
lv1_leukemia_SEX_CAL2$quadratic.reg.FY<-0

for(i in 1:length(SEX_CAL2_level)){
  ind.tr<-which(lv1_leukemia_SEX_CAL2$YEAR<=2014 & lv1_leukemia_SEX_CAL2$NAME==SEX_CAL2_level[i])
  ind.val<-which(lv1_leukemia_SEX_CAL2$YEAR>2014 & lv1_leukemia_SEX_CAL2$NAME==SEX_CAL2_level[i])
  
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
  
  
  
  MAPE_lv1_leukemia[(i+5),]<-c(unique(lv1_leukemia_SEX_CAL2$NAME[ind.tr]),
                               round(mean(abs(lv1_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_SEX_CAL2$simple.reg[ind.val])/lv1_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2),
                               round(mean(abs(lv1_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_SEX_CAL2$poisson.reg[ind.val])/lv1_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2),
                               round(mean(abs(lv1_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_SEX_CAL2$linear.spline[ind.val])/lv1_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2),
                               round(mean(abs(lv1_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_SEX_CAL2$quadratic.reg[ind.val])/lv1_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2),
                               round(mean(abs(lv1_leukemia_SEX_CAL2$sum_FY_BZ[ind.val]-lv1_leukemia_SEX_CAL2$simple.reg.FY[ind.val])/lv1_leukemia_SEX_CAL2$sum_FY_BZ[ind.val])*100,2),
                               round(mean(abs(lv1_leukemia_SEX_CAL2$sum_FY_BZ[ind.val]-lv1_leukemia_SEX_CAL2$poisson.reg.FY[ind.val])/lv1_leukemia_SEX_CAL2$sum_FY_BZ[ind.val])*100,2),
                               round(mean(abs(lv1_leukemia_SEX_CAL2$sum_FY_BZ[ind.val]-lv1_leukemia_SEX_CAL2$linear.spline.FY[ind.val])/lv1_leukemia_SEX_CAL2$sum_FY_BZ[ind.val])*100,2),
                               round(mean(abs(lv1_leukemia_SEX_CAL2$sum_FY_BZ[ind.val]-lv1_leukemia_SEX_CAL2$quadratic.reg.FY[ind.val])/lv1_leukemia_SEX_CAL2$sum_FY_BZ[ind.val])*100,2))
  
  RMSE_lv1_leukemia[(i+5),]<-c(unique(lv1_leukemia_SEX_CAL2$NAME[ind.tr]),
                               round(sqrt(mean(abs(lv1_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_SEX_CAL2$simple.reg[ind.val]))),2),
                               round(sqrt(mean(abs(lv1_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_SEX_CAL2$poisson.reg[ind.val]))),2),
                               round(sqrt(mean(abs(lv1_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_SEX_CAL2$linear.spline[ind.val]))),2),
                               round(sqrt(mean(abs(lv1_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv1_leukemia_SEX_CAL2$quadratic.reg[ind.val]))),2),
                               round(sqrt(mean(abs(lv1_leukemia_SEX_CAL2$sum_FY_BZ[ind.val]-lv1_leukemia_SEX_CAL2$simple.reg.FY[ind.val]))),2),
                               round(sqrt(mean(abs(lv1_leukemia_SEX_CAL2$sum_FY_BZ[ind.val]-lv1_leukemia_SEX_CAL2$poisson.reg.FY[ind.val]))),2),
                               round(sqrt(mean(abs(lv1_leukemia_SEX_CAL2$sum_FY_BZ[ind.val]-lv1_leukemia_SEX_CAL2$linear.spline.FY[ind.val]))),2),
                               round(sqrt(mean(abs(lv1_leukemia_SEX_CAL2$sum_FY_BZ[ind.val]-lv1_leukemia_SEX_CAL2$quadratic.reg.FY[ind.val]))),2))
  
}



ggplot()+geom_line(data=lv1_leukemia_SEX_CAL2,aes(x=YEAR,y=sum_LEUKEMIA_BZ,color="red"),size=1)+labs(y="#of_leukemia")+
  facet_wrap(~ factor(NAME,levels=SEX_CAL2_level),scales="free_y")+
  geom_line(data=lv1_leukemia_SEX_CAL2,aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv1_leukemia_SEX_CAL2,aes(x=YEAR,y=poisson.reg,color="green"),size=1)+
  geom_line(data=lv1_leukemia_SEX_CAL2,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv1_leukemia_SEX_CAL2,aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  labs(x="Year")+
  scale_color_identity(name='#of leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=lv1_leukemia_SEX_CAL2,aes(x=YEAR,y=sum_FY_BZ,color="red"),size=1)+labs(y="#of_FY_BZ")+
  facet_wrap(~ factor(NAME,levels=SEX_CAL2_level),scales="free_y")+
  geom_line(data=lv1_leukemia_SEX_CAL2,aes(x=YEAR,y=simple.reg.FY,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv1_leukemia_SEX_CAL2,aes(x=YEAR,y=poisson.reg.FY,color="green"),size=1)+
  geom_line(data=lv1_leukemia_SEX_CAL2,aes(x=YEAR,y=linear.spline.FY,color='darkviolet'),size=1)+
  geom_line(data=lv1_leukemia_SEX_CAL2,aes(x=YEAR,y=quadratic.reg.FY,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  labs(x="Year")+
  scale_color_identity(name='#of FY_BZ',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of FY_BZ","simple linear regression","poisson regreesion","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

value_eklee_lv1<-as.data.frame(rbind(lv1_leukemia_total,lv1_leukemia_SEX[2:length(lv1_leukemia_SEX)],lv1_leukemia_CAL2[2:length(lv1_leukemia_CAL2)],
                                     lv1_leukemia_SEX_CAL2[3:length(lv1_leukemia_SEX_CAL2)]))

val_leukemia_lv1<-value_eklee_lv1%>%as.data.frame()



##################### level 2 data ##########################
############# 1. grouping by UP1 #################
lv2_leukemia_total<-df_leukemia1%>%group_by(UP1,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  mutate(NAME=paste0("X",UP1))%>%as_tibble()

MAPE_lv2_leukemia<-matrix(0,nrow=length(unique(lv2_leukemia_total$NAME))*(1+2+2+4),ncol=9)
colnames(MAPE_lv2_leukemia)<-c("NAME","simple_reg","poisson_reg","linear_spline","quadratic_reg",
                               "simple_reg.FY","poisson_reg.FY","linear_spline.FY","quadratic_reg.FY")

RMSE_lv2_leukemia<-matrix(0,nrow=length(unique(lv2_leukemia_total$NAME))*(1+2+2+4),ncol=9)
colnames(RMSE_lv2_leukemia)<-c("NAME","simple_reg","poisson_reg","linear_spline","quadratic_reg",
                               "simple_reg.FY","poisson_reg.FY","linear_spline.FY","quadratic_reg.FY")

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
  ind.tr<-which(lv2_leukemia_total$YEAR<=2014 & lv2_leukemia_total$NAME==name_level[i])
  ind.val<-which(lv2_leukemia_total$YEAR>2014 & lv2_leukemia_total$NAME==name_level[i])
  
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
  
  
  MAPE_lv2_leukemia[i,]<-c(unique(lv2_leukemia_total$NAME[ind.tr]),
                           round(mean(abs(lv2_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_total$simple.reg[ind.val])/lv2_leukemia_total$sum_LEUKEMIA_BZ[ind.val])*100,2),
                           round(mean(abs(lv2_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_total$poisson.reg[ind.val])/lv2_leukemia_total$sum_LEUKEMIA_BZ[ind.val])*100,2),
                           round(mean(abs(lv2_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_total$linear.spline[ind.val])/lv2_leukemia_total$sum_LEUKEMIA_BZ[ind.val])*100,2),
                           round(mean(abs(lv2_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_total$quadratic.reg[ind.val])/lv2_leukemia_total$sum_LEUKEMIA_BZ[ind.val])*100,2),
                           round(mean(abs(lv2_leukemia_total$sum_FY_BZ[ind.val]-lv2_leukemia_total$simple.reg.FY[ind.val])/lv2_leukemia_total$sum_FY_BZ[ind.val])*100,2),
                           round(mean(abs(lv2_leukemia_total$sum_FY_BZ[ind.val]-lv2_leukemia_total$poisson.reg.FY[ind.val])/lv2_leukemia_total$sum_FY_BZ[ind.val])*100,2),
                           round(mean(abs(lv2_leukemia_total$sum_FY_BZ[ind.val]-lv2_leukemia_total$linear.spline.FY[ind.val])/lv2_leukemia_total$sum_FY_BZ[ind.val])*100,2),
                           round(mean(abs(lv2_leukemia_total$sum_FY_BZ[ind.val]-lv2_leukemia_total$quadratic.reg.FY[ind.val])/lv2_leukemia_total$sum_FY_BZ[ind.val])*100,2))
  
  RMSE_lv2_leukemia[i,]<-c(unique(lv2_leukemia_total$NAME[ind.tr]),
                           round(sqrt(mean(abs(lv2_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_total$simple.reg[ind.val]))),2),
                           round(sqrt(mean(abs(lv2_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_total$poisson.reg[ind.val]))),2),
                           round(sqrt(mean(abs(lv2_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_total$linear.spline[ind.val]))),2),
                           round(sqrt(mean(abs(lv2_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_total$quadratic.reg[ind.val]))),2),
                           round(sqrt(mean(abs(lv2_leukemia_total$sum_FY_BZ[ind.val]-lv2_leukemia_total$simple.reg.FY[ind.val]))),2),
                           round(sqrt(mean(abs(lv2_leukemia_total$sum_FY_BZ[ind.val]-lv2_leukemia_total$poisson.reg.FY[ind.val]))),2),
                           round(sqrt(mean(abs(lv2_leukemia_total$sum_FY_BZ[ind.val]-lv2_leukemia_total$linear.spline.FY[ind.val]))),2),
                           round(sqrt(mean(abs(lv2_leukemia_total$sum_FY_BZ[ind.val]-lv2_leukemia_total$quadratic.reg.FY[ind.val]))),2))
  
  
}

ggplot()+geom_line(data=lv2_leukemia_total,aes(x=YEAR,y=sum_LEUKEMIA_BZ,color="red"),size=1)+labs(y="#of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(lv2_leukemia_total$NAME)),scales="free_y")+
  geom_line(data=lv2_leukemia_total,aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv2_leukemia_total,aes(x=YEAR,y=poisson.reg,color="green"),size=1)+
  geom_line(data=lv2_leukemia_total,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv2_leukemia_total,aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_leukemia),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_leukemia),mapping = aes(x = 2010, y = Inf, label = poisson_reg),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_leukemia),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_leukemia),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=lv2_leukemia_total,aes(x=YEAR,y=sum_FY_BZ,color="red"),size=1)+labs(y="#of_FY_BZ")+
  facet_wrap(~ factor(NAME,levels=unique(lv2_leukemia_total$NAME)),scales="free_y")+
  geom_line(data=lv2_leukemia_total,aes(x=YEAR,y=simple.reg.FY,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv2_leukemia_total,aes(x=YEAR,y=poisson.reg.FY,color="green"),size=1)+
  geom_line(data=lv2_leukemia_total,aes(x=YEAR,y=linear.spline.FY,color='darkviolet'),size=1)+
  geom_line(data=lv2_leukemia_total,aes(x=YEAR,y=quadratic.reg.FY,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_leukemia),mapping = aes(x = 2010, y = Inf, label = simple_reg.FY),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_leukemia),mapping = aes(x = 2010, y = Inf, label = poisson_reg.FY),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_leukemia),mapping = aes(x = 2002, y = Inf, label = linear_spline.FY),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_leukemia),mapping = aes(x = 2002, y = Inf, label = quadratic_reg.FY),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of FY BZ',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of FY BZ","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))



##################################################################################
############# 2. grouping by UP1 and SEX #################
lv2_leukemia_SEX<-df_leukemia1%>%group_by(UP1,SEX,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  mutate(NAME=paste0("X",UP1,"_",SEX))%>%as_tibble()

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
  ind.tr<-which(lv2_leukemia_SEX$YEAR<=2014 & lv2_leukemia_SEX$NAME==name_level[i])
  ind.val<-which(lv2_leukemia_SEX$YEAR>2014 & lv2_leukemia_SEX$NAME==name_level[i])
  
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
  
  
  MAPE_lv2_leukemia[(i+19),]<-c(unique(lv2_leukemia_SEX$NAME[ind.tr]),
                                round(mean(abs(lv2_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_SEX$simple.reg[ind.val])/lv2_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val])*100,2),
                                round(mean(abs(lv2_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_SEX$poisson.reg[ind.val])/lv2_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val])*100,2),
                                round(mean(abs(lv2_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_SEX$linear.spline[ind.val])/lv2_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val])*100,2),
                                round(mean(abs(lv2_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_SEX$quadratic.reg[ind.val])/lv2_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val])*100,2),
                                round(mean(abs(lv2_leukemia_SEX$sum_FY_BZ[ind.val]-lv2_leukemia_SEX$simple.reg.FY[ind.val])/lv2_leukemia_SEX$sum_FY_BZ[ind.val])*100,2),
                                round(mean(abs(lv2_leukemia_SEX$sum_FY_BZ[ind.val]-lv2_leukemia_SEX$poisson.reg.FY[ind.val])/lv2_leukemia_SEX$sum_FY_BZ[ind.val])*100,2),
                                round(mean(abs(lv2_leukemia_SEX$sum_FY_BZ[ind.val]-lv2_leukemia_SEX$linear.spline.FY[ind.val])/lv2_leukemia_SEX$sum_FY_BZ[ind.val])*100,2),
                                round(mean(abs(lv2_leukemia_SEX$sum_FY_BZ[ind.val]-lv2_leukemia_SEX$quadratic.reg.FY[ind.val])/lv2_leukemia_SEX$sum_FY_BZ[ind.val])*100,2))
  
  RMSE_lv2_leukemia[(i+19),]<-c(unique(lv2_leukemia_SEX$NAME[ind.tr]),
                                round(sqrt(mean(abs(lv2_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_SEX$simple.reg[ind.val]))),2),
                                round(sqrt(mean(abs(lv2_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_SEX$poisson.reg[ind.val]))),2),
                                round(sqrt(mean(abs(lv2_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_SEX$linear.spline[ind.val]))),2),
                                round(sqrt(mean(abs(lv2_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_SEX$quadratic.reg[ind.val]))),2),
                                round(sqrt(mean(abs(lv2_leukemia_SEX$sum_FY_BZ[ind.val]-lv2_leukemia_SEX$simple.reg.FY[ind.val]))),2),
                                round(sqrt(mean(abs(lv2_leukemia_SEX$sum_FY_BZ[ind.val]-lv2_leukemia_SEX$poisson.reg.FY[ind.val]))),2),
                                round(sqrt(mean(abs(lv2_leukemia_SEX$sum_FY_BZ[ind.val]-lv2_leukemia_SEX$linear.spline.FY[ind.val]))),2),
                                round(sqrt(mean(abs(lv2_leukemia_SEX$sum_FY_BZ[ind.val]-lv2_leukemia_SEX$quadratic.reg.FY[ind.val]))),2))
  
  
}

ggplot()+geom_line(data=lv2_leukemia_SEX,aes(x=YEAR,y=sum_LEUKEMIA_BZ,color="red"),size=1)+labs(y="#of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(lv2_leukemia_SEX$NAME)),scales="free_y")+
  geom_line(data=lv2_leukemia_SEX,aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv2_leukemia_SEX,aes(x=YEAR,y=poisson.reg,color="green"),size=1)+
  geom_line(data=lv2_leukemia_SEX,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv2_leukemia_SEX,aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_leukemia[20:57,]),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_leukemia[20:57,]),mapping = aes(x = 2010, y = Inf, label = poisson_reg),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_leukemia[20:57,]),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_leukemia[20:57,]),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=lv2_leukemia_SEX,aes(x=YEAR,y=sum_FY_BZ,color="red"),size=1)+labs(y="#of_FY_BZ")+
  facet_wrap(~ factor(NAME,levels=unique(lv2_leukemia_SEX$NAME)),scales="free_y")+
  geom_line(data=lv2_leukemia_SEX,aes(x=YEAR,y=simple.reg.FY,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv2_leukemia_SEX,aes(x=YEAR,y=poisson.reg.FY,color="green"),size=1)+
  geom_line(data=lv2_leukemia_SEX,aes(x=YEAR,y=linear.spline.FY,color='darkviolet'),size=1)+
  geom_line(data=lv2_leukemia_SEX,aes(x=YEAR,y=quadratic.reg.FY,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_leukemia[20:57,]),mapping = aes(x = 2010, y = Inf, label = simple_reg.FY),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_leukemia[20:57,]),mapping = aes(x = 2010, y = Inf, label = poisson_reg.FY),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_leukemia[20:57,]),mapping = aes(x = 2002, y = Inf, label = linear_spline.FY),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_leukemia[20:57,]),mapping = aes(x = 2002, y = Inf, label = quadratic_reg.FY),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of FY_BZ',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of FY_BZ","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))



################################################################################################
############### 3. grouping by UP1 and CAL2 ######################### 
lv2_leukemia_CAL2<-df_leukemia1%>%group_by(UP1,CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  mutate(NAME=paste0("X",UP1,"_",CAL2))%>%as_tibble()

name_level<-unique(lv2_leukemia_CAL2$NAME)

lv2_leukemia_CAL2$simple.reg<-0
lv2_leukemia_CAL2$poisson.reg<-0
lv2_leukemia_CAL2$linear.spline<-0
lv2_leukemia_CAL2$quadratic.reg<-0

lv2_leukemia_CAL2$simple.reg.FY<-0
lv2_leukemia_CAL2$poisson.reg.FY<-0
lv2_leukemia_CAL2$linear.spline.FY<-0
lv2_leukemia_CAL2$quadratic.reg.FY<-0

for(i in 1:length(name_level)){
  ind.tr<-which(lv2_leukemia_CAL2$YEAR<=2014 & lv2_leukemia_CAL2$NAME==name_level[i])
  ind.val<-which(lv2_leukemia_CAL2$YEAR>2014 & lv2_leukemia_CAL2$NAME==name_level[i])
  
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
  
  MAPE_lv2_leukemia[(i+57),]<-c(unique(lv2_leukemia_CAL2$NAME[ind.tr]),
                                round(mean(abs(lv2_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_CAL2$simple.reg[ind.val])/lv2_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2),
                                round(mean(abs(lv2_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_CAL2$poisson.reg[ind.val])/lv2_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2),
                                round(mean(abs(lv2_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_CAL2$linear.spline[ind.val])/lv2_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2),
                                round(mean(abs(lv2_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_CAL2$quadratic.reg[ind.val])/lv2_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2),
                                round(mean(abs(lv2_leukemia_CAL2$sum_FY_BZ[ind.val]-lv2_leukemia_CAL2$simple.reg.FY[ind.val])/lv2_leukemia_CAL2$sum_FY_BZ[ind.val])*100,2),
                                round(mean(abs(lv2_leukemia_CAL2$sum_FY_BZ[ind.val]-lv2_leukemia_CAL2$poisson.reg.FY[ind.val])/lv2_leukemia_CAL2$sum_FY_BZ[ind.val])*100,2),
                                round(mean(abs(lv2_leukemia_CAL2$sum_FY_BZ[ind.val]-lv2_leukemia_CAL2$linear.spline.FY[ind.val])/lv2_leukemia_CAL2$sum_FY_BZ[ind.val])*100,2),
                                round(mean(abs(lv2_leukemia_CAL2$sum_FY_BZ[ind.val]-lv2_leukemia_CAL2$quadratic.reg.FY[ind.val])/lv2_leukemia_CAL2$sum_FY_BZ[ind.val])*100,2))
  
  RMSE_lv2_leukemia[(i+57),]<-c(unique(lv2_leukemia_CAL2$NAME[ind.tr]),
                                round(sqrt(mean(abs(lv2_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_CAL2$simple.reg[ind.val]))),2),
                                round(sqrt(mean(abs(lv2_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_CAL2$poisson.reg[ind.val]))),2),
                                round(sqrt(mean(abs(lv2_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_CAL2$linear.spline[ind.val]))),2),
                                round(sqrt(mean(abs(lv2_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_CAL2$quadratic.reg[ind.val]))),2),
                                round(sqrt(mean(abs(lv2_leukemia_CAL2$sum_FY_BZ[ind.val]-lv2_leukemia_CAL2$simple.reg.FY[ind.val]))),2),
                                round(sqrt(mean(abs(lv2_leukemia_CAL2$sum_FY_BZ[ind.val]-lv2_leukemia_CAL2$poisson.reg.FY[ind.val]))),2),
                                round(sqrt(mean(abs(lv2_leukemia_CAL2$sum_FY_BZ[ind.val]-lv2_leukemia_CAL2$linear.spline.FY[ind.val]))),2),
                                round(sqrt(mean(abs(lv2_leukemia_CAL2$sum_FY_BZ[ind.val]-lv2_leukemia_CAL2$quadratic.reg.FY[ind.val]))),2))
  
  
}



ggplot()+geom_line(data=lv2_leukemia_CAL2,aes(x=YEAR,y=sum_LEUKEMIA_BZ,color="red"),size=1)+labs(y="#of_leukemia")+
  facet_wrap(~ factor(NAME,levels=name_level),scales="free_y")+
  geom_line(data=lv2_leukemia_CAL2,aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv2_leukemia_CAL2,aes(x=YEAR,y=poisson.reg,color="green"),size=1)+
  geom_line(data=lv2_leukemia_CAL2,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv2_leukemia_CAL2,aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_leukemia[58:95,]),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_leukemia[58:95,]),mapping = aes(x = 2010, y = Inf, label = poisson_reg),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_leukemia[58:95,]),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_leukemia[58:95,]),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=lv2_leukemia_CAL2,aes(x=YEAR,y=sum_FY_BZ,color="red"),size=1)+labs(y="#of_FY_BZ")+
  facet_wrap(~ factor(NAME,levels=name_level),scales="free_y")+
  geom_line(data=lv2_leukemia_CAL2,aes(x=YEAR,y=simple.reg.FY,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv2_leukemia_CAL2,aes(x=YEAR,y=poisson.reg.FY,color="green"),size=1)+
  geom_line(data=lv2_leukemia_CAL2,aes(x=YEAR,y=linear.spline.FY,color='darkviolet'),size=1)+
  geom_line(data=lv2_leukemia_CAL2,aes(x=YEAR,y=quadratic.reg.FY,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_leukemia[58:95,]),mapping = aes(x = 2010, y = Inf, label = simple_reg.FY),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_leukemia[58:95,]),mapping = aes(x = 2010, y = Inf, label = poisson_reg.FY),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_leukemia[58:95,]),mapping = aes(x = 2002, y = Inf, label = linear_spline.FY),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_leukemia[58:95,]),mapping = aes(x = 2002, y = Inf, label = quadratic_reg.FY),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of FY BZ',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))




################################################################################################
############### 3. grouping by UP1 and SEX and CAL2 ######################### 
lv2_leukemia_SEX_CAL2<-df_leukemia1%>%group_by(UP1,SEX,CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  mutate(NAME=paste0("X",UP1,"_",SEX,CAL2))%>%as_tibble()

name_level<-unique(lv2_leukemia_SEX_CAL2$NAME)

lv2_leukemia_SEX_CAL2$simple.reg<-0
lv2_leukemia_SEX_CAL2$poisson.reg<-0
lv2_leukemia_SEX_CAL2$linear.spline<-0
lv2_leukemia_SEX_CAL2$quadratic.reg<-0

lv2_leukemia_SEX_CAL2$simple.reg.FY<-0
lv2_leukemia_SEX_CAL2$poisson.reg.FY<-0
lv2_leukemia_SEX_CAL2$linear.spline.FY<-0
lv2_leukemia_SEX_CAL2$quadratic.reg.FY<-0

for(i in 1:length(name_level)){
  ind.tr<-which(lv2_leukemia_SEX_CAL2$YEAR<=2014 & lv2_leukemia_SEX_CAL2$NAME==name_level[i])
  ind.val<-which(lv2_leukemia_SEX_CAL2$YEAR>2014 & lv2_leukemia_SEX_CAL2$NAME==name_level[i])
  
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
  
  MAPE_lv2_leukemia[(i+95),]<-c(unique(lv2_leukemia_SEX_CAL2$NAME[ind.tr]),
                                round(mean(abs(lv2_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_SEX_CAL2$simple.reg[ind.val])/lv2_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2),
                                round(mean(abs(lv2_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_SEX_CAL2$poisson.reg[ind.val])/lv2_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2),
                                round(mean(abs(lv2_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_SEX_CAL2$linear.spline[ind.val])/lv2_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2),
                                round(mean(abs(lv2_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_SEX_CAL2$quadratic.reg[ind.val])/lv2_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2),
                                round(mean(abs(lv2_leukemia_SEX_CAL2$sum_FY_BZ[ind.val]-lv2_leukemia_SEX_CAL2$simple.reg.FY[ind.val])/lv2_leukemia_SEX_CAL2$sum_FY_BZ[ind.val])*100,2),
                                round(mean(abs(lv2_leukemia_SEX_CAL2$sum_FY_BZ[ind.val]-lv2_leukemia_SEX_CAL2$poisson.reg.FY[ind.val])/lv2_leukemia_SEX_CAL2$sum_FY_BZ[ind.val])*100,2),
                                round(mean(abs(lv2_leukemia_SEX_CAL2$sum_FY_BZ[ind.val]-lv2_leukemia_SEX_CAL2$linear.spline.FY[ind.val])/lv2_leukemia_SEX_CAL2$sum_FY_BZ[ind.val])*100,2),
                                round(mean(abs(lv2_leukemia_SEX_CAL2$sum_FY_BZ[ind.val]-lv2_leukemia_SEX_CAL2$quadratic.reg.FY[ind.val])/lv2_leukemia_SEX_CAL2$sum_FY_BZ[ind.val])*100,2))
  
  RMSE_lv2_leukemia[(i+95),]<-c(unique(lv2_leukemia_SEX_CAL2$NAME[ind.tr]),
                                round(sqrt(mean(abs(lv2_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_SEX_CAL2$simple.reg[ind.val]))),2),
                                round(sqrt(mean(abs(lv2_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_SEX_CAL2$poisson.reg[ind.val]))),2),
                                round(sqrt(mean(abs(lv2_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_SEX_CAL2$linear.spline[ind.val]))),2),
                                round(sqrt(mean(abs(lv2_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv2_leukemia_SEX_CAL2$quadratic.reg[ind.val]))),2),
                                round(sqrt(mean(abs(lv2_leukemia_SEX_CAL2$sum_FY_BZ[ind.val]-lv2_leukemia_SEX_CAL2$simple.reg.FY[ind.val]))),2),
                                round(sqrt(mean(abs(lv2_leukemia_SEX_CAL2$sum_FY_BZ[ind.val]-lv2_leukemia_SEX_CAL2$poisson.reg.FY[ind.val]))),2),
                                round(sqrt(mean(abs(lv2_leukemia_SEX_CAL2$sum_FY_BZ[ind.val]-lv2_leukemia_SEX_CAL2$linear.spline.FY[ind.val]))),2),
                                round(sqrt(mean(abs(lv2_leukemia_SEX_CAL2$sum_FY_BZ[ind.val]-lv2_leukemia_SEX_CAL2$quadratic.reg.FY[ind.val]))),2))
  
  
}

ggplot()+geom_line(data=lv2_leukemia_SEX_CAL2,aes(x=YEAR,y=sum_LEUKEMIA_BZ,color="red"),size=1)+labs(y="#of_leukemia")+
  facet_wrap(~ factor(NAME,levels=name_level),scales="free_y")+
  geom_line(data=lv2_leukemia_SEX_CAL2,aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv2_leukemia_SEX_CAL2,aes(x=YEAR,y=poisson.reg,color="green"),size=1)+
  geom_line(data=lv2_leukemia_SEX_CAL2,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv2_leukemia_SEX_CAL2,aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_leukemia[96:171,]),mapping = aes(x = 2011, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_leukemia[96:171,]),mapping = aes(x = 2011, y = Inf, label = poisson_reg),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_leukemia[96:171,]),mapping = aes(x = 2003, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_leukemia[96:171,]),mapping = aes(x = 2003, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=lv2_leukemia_SEX_CAL2,aes(x=YEAR,y=sum_FY_BZ,color="red"),size=1)+labs(y="#of FY_BZ")+
  facet_wrap(~ factor(NAME,levels=name_level),scales="free_y")+
  geom_line(data=lv2_leukemia_SEX_CAL2,aes(x=YEAR,y=simple.reg.FY,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv2_leukemia_SEX_CAL2,aes(x=YEAR,y=poisson.reg.FY,color="green"),size=1)+
  geom_line(data=lv2_leukemia_SEX_CAL2,aes(x=YEAR,y=linear.spline.FY,color='darkviolet'),size=1)+
  geom_line(data=lv2_leukemia_SEX_CAL2,aes(x=YEAR,y=quadratic.reg.FY,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_leukemia[96:171,]),mapping = aes(x = 2012, y = Inf, label = simple_reg.FY),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_leukemia[96:171,]),mapping = aes(x = 2012, y = Inf, label = poisson_reg.FY),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_leukemia[96:171,]),mapping = aes(x = 2003, y = Inf, label = linear_spline.FY),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_leukemia[96:171,]),mapping = aes(x = 2003, y = Inf, label = quadratic_reg.FY),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of FY_BZ',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of FY_BZ","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


value_eklee_lv2<-as.data.frame(rbind(lv2_leukemia_total[2:length(lv2_leukemia_total)],lv2_leukemia_SEX[3:length(lv2_leukemia_SEX)],lv2_leukemia_CAL2[3:length(lv2_leukemia_CAL2)],
                                     lv2_leukemia_SEX_CAL2[4:length(lv2_leukemia_SEX_CAL2)]))

val_leukemia_lv2<-value_eklee_lv2%>%as.data.frame()





#######################################################################################################3
##################### level 3 data ##########################
############# 1. grouping by UP2 #################
lv3_leukemia_total<-df_leukemia1%>%group_by(UP1,UP2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  mutate(NAME=paste0("X",UP1,UP2))%>%as_tibble()

MAPE_lv3_leukemia<-matrix(0,nrow=length(unique(lv3_leukemia_total$NAME))*(1+2+2+4),ncol=9)
colnames(MAPE_lv3_leukemia)<-c("NAME","simple_reg","poisson_reg","linear_spline","quadratic_reg",
                               "simple_reg.FY","poisson_reg.FY","linear_spline.FY","quadratic_reg.FY")
RMSE_lv3_leukemia<-matrix(0,nrow=length(unique(lv3_leukemia_total$NAME))*(1+2+2+4),ncol=9)
colnames(RMSE_lv3_leukemia)<-c("NAME","simple_reg","poisson_reg","linear_spline","quadratic_reg",
                               "simple_reg.FY","poisson_reg.FY","linear_spline.FY","quadratic_reg.FY")

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
  ind.tr<-which(lv3_leukemia_total$YEAR<=2014 & lv3_leukemia_total$NAME==name_level[i])
  ind.val<-which(lv3_leukemia_total$YEAR>2014 & lv3_leukemia_total$NAME==name_level[i])
  
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
  
  
  MAPE_lv3_leukemia[i,]<-c(unique(lv3_leukemia_total$NAME[ind.tr]),
                           round(mean(abs(lv3_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_total$simple.reg[ind.val])/lv3_leukemia_total$sum_LEUKEMIA_BZ[ind.val])*100,2),
                           round(mean(abs(lv3_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_total$poisson.reg[ind.val])/lv3_leukemia_total$sum_LEUKEMIA_BZ[ind.val])*100,2),
                           round(mean(abs(lv3_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_total$linear.spline[ind.val])/lv3_leukemia_total$sum_LEUKEMIA_BZ[ind.val])*100,2),
                           round(mean(abs(lv3_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_total$quadratic.reg[ind.val])/lv3_leukemia_total$sum_LEUKEMIA_BZ[ind.val])*100,2),
                           round(mean(abs(lv3_leukemia_total$sum_FY_BZ[ind.val]-lv3_leukemia_total$simple.reg.FY[ind.val])/lv3_leukemia_total$sum_FY_BZ[ind.val])*100,2),
                           round(mean(abs(lv3_leukemia_total$sum_FY_BZ[ind.val]-lv3_leukemia_total$poisson.reg.FY[ind.val])/lv3_leukemia_total$sum_FY_BZ[ind.val])*100,2),
                           round(mean(abs(lv3_leukemia_total$sum_FY_BZ[ind.val]-lv3_leukemia_total$linear.spline.FY[ind.val])/lv3_leukemia_total$sum_FY_BZ[ind.val])*100,2),
                           round(mean(abs(lv3_leukemia_total$sum_FY_BZ[ind.val]-lv3_leukemia_total$quadratic.reg.FY[ind.val])/lv3_leukemia_total$sum_FY_BZ[ind.val])*100,2))
  
  RMSE_lv3_leukemia[i,]<-c(unique(lv3_leukemia_total$NAME[ind.tr]),
                           round(sqrt(mean(abs(lv3_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_total$simple.reg[ind.val]))),2),
                           round(sqrt(mean(abs(lv3_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_total$poisson.reg[ind.val]))),2),
                           round(sqrt(mean(abs(lv3_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_total$linear.spline[ind.val]))),2),
                           round(sqrt(mean(abs(lv3_leukemia_total$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_total$quadratic.reg[ind.val]))),2),
                           round(sqrt(mean(abs(lv3_leukemia_total$sum_FY_BZ[ind.val]-lv3_leukemia_total$simple.reg.FY[ind.val]))),2),
                           round(sqrt(mean(abs(lv3_leukemia_total$sum_FY_BZ[ind.val]-lv3_leukemia_total$poisson.reg.FY[ind.val]))),2),
                           round(sqrt(mean(abs(lv3_leukemia_total$sum_FY_BZ[ind.val]-lv3_leukemia_total$linear.spline.FY[ind.val]))),2),
                           round(sqrt(mean(abs(lv3_leukemia_total$sum_FY_BZ[ind.val]-lv3_leukemia_total$quadratic.reg.FY[ind.val]))),2))
  
  
}
ggplot()+geom_line(data=lv3_leukemia_total,aes(x=YEAR,y=sum_LEUKEMIA_BZ,color="red"),size=1)+labs(y="#of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(lv3_leukemia_total$NAME)),scales="free_y")+
  geom_line(data=lv3_leukemia_total,aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv3_leukemia_total,aes(x=YEAR,y=poisson.reg,color="green"),size=1)+
  geom_line(data=lv3_leukemia_total,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv3_leukemia_total,aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia),mapping = aes(x = 2010, y = Inf, label = poisson_reg),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=lv3_leukemia_total,aes(x=YEAR,y=sum_FY_BZ,color="red"),size=1)+labs(y="#of_FY_BZ")+
  facet_wrap(~ factor(NAME,levels=unique(lv3_leukemia_total$NAME)),scales="free_y")+
  geom_line(data=lv3_leukemia_total,aes(x=YEAR,y=simple.reg.FY,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv3_leukemia_total,aes(x=YEAR,y=poisson.reg.FY,color="green"),size=1)+
  geom_line(data=lv3_leukemia_total,aes(x=YEAR,y=linear.spline.FY,color='darkviolet'),size=1)+
  geom_line(data=lv3_leukemia_total,aes(x=YEAR,y=quadratic.reg.FY,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia),mapping = aes(x = 2010, y = Inf, label = simple_reg.FY),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia),mapping = aes(x = 2010, y = Inf, label = poisson_reg.FY),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia),mapping = aes(x = 2002, y = Inf, label = linear_spline.FY),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia),mapping = aes(x = 2002, y = Inf, label = quadratic_reg.FY),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of FY BZ',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of FY BZ","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


##################################################################################
############# 2. grouping by UP2 and SEX #################
lv3_leukemia_SEX<-df_leukemia1%>%group_by(UP1,UP2,SEX,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  mutate(NAME=paste0("X",UP1,UP2,"_",SEX))%>%as_tibble()

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
  ind.tr<-which(lv3_leukemia_SEX$YEAR<=2014 & lv3_leukemia_SEX$NAME==name_level[i])
  ind.val<-which(lv3_leukemia_SEX$YEAR>2014 & lv3_leukemia_SEX$NAME==name_level[i])
  
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
  
  
  MAPE_lv3_leukemia[(i+19),]<-c(unique(lv3_leukemia_SEX$NAME[ind.tr]),
                                round(mean(abs(lv3_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_SEX$simple.reg[ind.val])/lv3_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val])*100,2),
                                round(mean(abs(lv3_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_SEX$poisson.reg[ind.val])/lv3_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val])*100,2),
                                round(mean(abs(lv3_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_SEX$linear.spline[ind.val])/lv3_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val])*100,2),
                                round(mean(abs(lv3_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_SEX$quadratic.reg[ind.val])/lv3_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val])*100,2),
                                round(mean(abs(lv3_leukemia_SEX$sum_FY_BZ[ind.val]-lv3_leukemia_SEX$simple.reg.FY[ind.val])/lv3_leukemia_SEX$sum_FY_BZ[ind.val])*100,2),
                                round(mean(abs(lv3_leukemia_SEX$sum_FY_BZ[ind.val]-lv3_leukemia_SEX$poisson.reg.FY[ind.val])/lv3_leukemia_SEX$sum_FY_BZ[ind.val])*100,2),
                                round(mean(abs(lv3_leukemia_SEX$sum_FY_BZ[ind.val]-lv3_leukemia_SEX$linear.spline.FY[ind.val])/lv3_leukemia_SEX$sum_FY_BZ[ind.val])*100,2),
                                round(mean(abs(lv3_leukemia_SEX$sum_FY_BZ[ind.val]-lv3_leukemia_SEX$quadratic.reg.FY[ind.val])/lv3_leukemia_SEX$sum_FY_BZ[ind.val])*100,2))
  
  RMSE_lv3_leukemia[(i+19),]<-c(unique(lv3_leukemia_SEX$NAME[ind.tr]),
                                round(sqrt(mean(abs(lv3_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_SEX$simple.reg[ind.val]))),2),
                                round(sqrt(mean(abs(lv3_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_SEX$poisson.reg[ind.val]))),2),
                                round(sqrt(mean(abs(lv3_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_SEX$linear.spline[ind.val]))),2),
                                round(sqrt(mean(abs(lv3_leukemia_SEX$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_SEX$quadratic.reg[ind.val]))),2),
                                round(sqrt(mean(abs(lv3_leukemia_SEX$sum_FY_BZ[ind.val]-lv3_leukemia_SEX$simple.reg.FY[ind.val]))),2),
                                round(sqrt(mean(abs(lv3_leukemia_SEX$sum_FY_BZ[ind.val]-lv3_leukemia_SEX$poisson.reg.FY[ind.val]))),2),
                                round(sqrt(mean(abs(lv3_leukemia_SEX$sum_FY_BZ[ind.val]-lv3_leukemia_SEX$linear.spline.FY[ind.val]))),2),
                                round(sqrt(mean(abs(lv3_leukemia_SEX$sum_FY_BZ[ind.val]-lv3_leukemia_SEX$quadratic.reg.FY[ind.val]))),2))
  
  
}



ind.M<-grep("M",MAPE_lv3_leukemia[,1])
MAPE_lv3_leukemia_M<-MAPE_lv3_leukemia[ind.M,]

ind.F<-grep("F",MAPE_lv3_leukemia[,1])
MAPE_lv3_leukemia_F<-MAPE_lv3_leukemia[ind.F,]

ggplot()+geom_line(data=lv3_leukemia_SEX[lv3_leukemia_SEX$SEX=="F",],aes(x=YEAR,y=sum_LEUKEMIA_BZ,color="red"),size=1)+labs(y="#of leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(lv3_leukemia_SEX$NAME[lv3_leukemia_SEX$SEX=="F"])),scales="free_y")+
  geom_line(data=lv3_leukemia_SEX[lv3_leukemia_SEX$SEX=="F",],aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv3_leukemia_SEX[lv3_leukemia_SEX$SEX=="F",],aes(x=YEAR,y=poisson.reg,color="green"),size=1)+
  geom_line(data=lv3_leukemia_SEX[lv3_leukemia_SEX$SEX=="F",],aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv3_leukemia_SEX[lv3_leukemia_SEX$SEX=="F",],aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_F),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_F),mapping = aes(x = 2010, y = Inf, label = poisson_reg),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_F),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_F),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


ggplot()+geom_line(data=lv3_leukemia_SEX[lv3_leukemia_SEX$SEX=="M",],aes(x=YEAR,y=sum_LEUKEMIA_BZ,color="red"),size=1)+labs(y="#of leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(lv3_leukemia_SEX$NAME[lv3_leukemia_SEX$SEX=="M"])),scales="free_y")+
  geom_line(data=lv3_leukemia_SEX[lv3_leukemia_SEX$SEX=="M",],aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv3_leukemia_SEX[lv3_leukemia_SEX$SEX=="M",],aes(x=YEAR,y=poisson.reg,color="green"),size=1)+
  geom_line(data=lv3_leukemia_SEX[lv3_leukemia_SEX$SEX=="M",],aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv3_leukemia_SEX[lv3_leukemia_SEX$SEX=="M",],aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_M),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_M),mapping = aes(x = 2010, y = Inf, label = poisson_reg),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_M),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_M),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))



ggplot()+geom_line(data=lv3_leukemia_SEX[lv3_leukemia_SEX$SEX=="F",],aes(x=YEAR,y=sum_FY_BZ,color="red"),size=1)+labs(y="#of FY_BZ")+
  facet_wrap(~ factor(NAME,levels=unique(lv3_leukemia_SEX$NAME[lv3_leukemia_SEX$SEX=="F"])),scales="free_y")+
  geom_line(data=lv3_leukemia_SEX[lv3_leukemia_SEX$SEX=="F",],aes(x=YEAR,y=simple.reg.FY,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv3_leukemia_SEX[lv3_leukemia_SEX$SEX=="F",],aes(x=YEAR,y=poisson.reg.FY,color="green"),size=1)+
  geom_line(data=lv3_leukemia_SEX[lv3_leukemia_SEX$SEX=="F",],aes(x=YEAR,y=linear.spline.FY,color='darkviolet'),size=1)+
  geom_line(data=lv3_leukemia_SEX[lv3_leukemia_SEX$SEX=="F",],aes(x=YEAR,y=quadratic.reg.FY,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_F),mapping = aes(x = 2010, y = Inf, label = simple_reg.FY),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_F),mapping = aes(x = 2010, y = Inf, label = poisson_reg.FY),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_F),mapping = aes(x = 2002, y = Inf, label = linear_spline.FY),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_F),mapping = aes(x = 2002, y = Inf, label = quadratic_reg.FY),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of FY_BZ',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of FY BZ","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


ggplot()+geom_line(data=lv3_leukemia_SEX[lv3_leukemia_SEX$SEX=="M",],aes(x=YEAR,y=sum_FY_BZ,color="red"),size=1)+labs(y="#of FY_BZ")+
  facet_wrap(~ factor(NAME,levels=unique(lv3_leukemia_SEX$NAME[lv3_leukemia_SEX$SEX=="M"])),scales="free_y")+
  geom_line(data=lv3_leukemia_SEX[lv3_leukemia_SEX$SEX=="M",],aes(x=YEAR,y=simple.reg.FY,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv3_leukemia_SEX[lv3_leukemia_SEX$SEX=="M",],aes(x=YEAR,y=poisson.reg.FY,color="green"),size=1)+
  geom_line(data=lv3_leukemia_SEX[lv3_leukemia_SEX$SEX=="M",],aes(x=YEAR,y=linear.spline.FY,color='darkviolet'),size=1)+
  geom_line(data=lv3_leukemia_SEX[lv3_leukemia_SEX$SEX=="M",],aes(x=YEAR,y=quadratic.reg.FY,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_M),mapping = aes(x = 2010, y = Inf, label = simple_reg.FY),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_M),mapping = aes(x = 2010, y = Inf, label = poisson_reg.FY),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_M),mapping = aes(x = 2002, y = Inf, label = linear_spline.FY),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_M),mapping = aes(x = 2002, y = Inf, label = quadratic_reg.FY),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of FY_BZ',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of FY BZ","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))








################################################################################################
############### 3. grouping by UP2 and CAL2 ######################### 
lv3_leukemia_CAL2<-df_leukemia1%>%group_by(UP1,UP2,CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  mutate(NAME=paste0("X",UP1,UP2,"_",CAL2))%>%as_tibble()

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
  ind.tr<-which(lv3_leukemia_CAL2$YEAR<=2014 & lv3_leukemia_CAL2$NAME==name_level[i])
  ind.val<-which(lv3_leukemia_CAL2$YEAR>2014 & lv3_leukemia_CAL2$NAME==name_level[i])
  
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
  
  MAPE_lv3_leukemia[(i+57),]<-c(unique(lv3_leukemia_CAL2$NAME[ind.tr]),
                                round(mean(abs(lv3_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_CAL2$simple.reg[ind.val])/lv3_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2),
                                round(mean(abs(lv3_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_CAL2$poisson.reg[ind.val])/lv3_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2),
                                round(mean(abs(lv3_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_CAL2$linear.spline[ind.val])/lv3_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2),
                                round(mean(abs(lv3_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_CAL2$quadratic.reg[ind.val])/lv3_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2),
                                round(mean(abs(lv3_leukemia_CAL2$sum_FY_BZ[ind.val]-lv3_leukemia_CAL2$simple.reg.FY[ind.val])/lv3_leukemia_CAL2$sum_FY_BZ[ind.val])*100,2),
                                round(mean(abs(lv3_leukemia_CAL2$sum_FY_BZ[ind.val]-lv3_leukemia_CAL2$poisson.reg.FY[ind.val])/lv3_leukemia_CAL2$sum_FY_BZ[ind.val])*100,2),
                                round(mean(abs(lv3_leukemia_CAL2$sum_FY_BZ[ind.val]-lv3_leukemia_CAL2$linear.spline.FY[ind.val])/lv3_leukemia_CAL2$sum_FY_BZ[ind.val])*100,2),
                                round(mean(abs(lv3_leukemia_CAL2$sum_FY_BZ[ind.val]-lv3_leukemia_CAL2$quadratic.reg.FY[ind.val])/lv3_leukemia_CAL2$sum_FY_BZ[ind.val])*100,2))
  
  RMSE_lv3_leukemia[(i+57),]<-c(unique(lv3_leukemia_CAL2$NAME[ind.tr]),
                                round(sqrt(mean(abs(lv3_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_CAL2$simple.reg[ind.val]))),2),
                                round(sqrt(mean(abs(lv3_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_CAL2$poisson.reg[ind.val]))),2),
                                round(sqrt(mean(abs(lv3_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_CAL2$linear.spline[ind.val]))),2),
                                round(sqrt(mean(abs(lv3_leukemia_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_CAL2$quadratic.reg[ind.val]))),2),
                                round(sqrt(mean(abs(lv3_leukemia_CAL2$sum_FY_BZ[ind.val]-lv3_leukemia_CAL2$simple.reg.FY[ind.val]))),2),
                                round(sqrt(mean(abs(lv3_leukemia_CAL2$sum_FY_BZ[ind.val]-lv3_leukemia_CAL2$poisson.reg.FY[ind.val]))),2),
                                round(sqrt(mean(abs(lv3_leukemia_CAL2$sum_FY_BZ[ind.val]-lv3_leukemia_CAL2$linear.spline.FY[ind.val]))),2),
                                round(sqrt(mean(abs(lv3_leukemia_CAL2$sum_FY_BZ[ind.val]-lv3_leukemia_CAL2$quadratic.reg.FY[ind.val]))),2))
  
  
}


ind.01<-grep("01",MAPE_lv3_leukemia[,1])
MAPE_lv3_leukemia_01<-MAPE_lv3_leukemia[ind.01,]

ind.234<-grep("234",MAPE_lv3_leukemia[,1])
MAPE_lv3_leukemia_234<-MAPE_lv3_leukemia[ind.234,]


ggplot()+geom_line(data=lv3_leukemia_CAL2[lv3_leukemia_CAL2$CAL2=="01",],aes(x=YEAR,y=sum_LEUKEMIA_BZ,color="red"),size=1)+labs(y="#of leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(lv3_leukemia_CAL2$NAME[lv3_leukemia_CAL2$CAL2=="01"])),scales="free_y")+
  geom_line(data=lv3_leukemia_CAL2[lv3_leukemia_CAL2$CAL2=="01",],aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv3_leukemia_CAL2[lv3_leukemia_CAL2$CAL2=="01",],aes(x=YEAR,y=poisson.reg,color="green"),size=1)+
  geom_line(data=lv3_leukemia_CAL2[lv3_leukemia_CAL2$CAL2=="01",],aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv3_leukemia_CAL2[lv3_leukemia_CAL2$CAL2=="01",],aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_01),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_01),mapping = aes(x = 2010, y = Inf, label = poisson_reg),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_01),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_01),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))



ggplot()+geom_line(data=lv3_leukemia_CAL2[lv3_leukemia_CAL2$CAL2=="234",],aes(x=YEAR,y=sum_LEUKEMIA_BZ,color="red"),size=1)+labs(y="#of leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(lv3_leukemia_CAL2$NAME[lv3_leukemia_CAL2$CAL2=="234"])),scales="free_y")+
  geom_line(data=lv3_leukemia_CAL2[lv3_leukemia_CAL2$CAL2=="234",],aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv3_leukemia_CAL2[lv3_leukemia_CAL2$CAL2=="234",],aes(x=YEAR,y=poisson.reg,color="green"),size=1)+
  geom_line(data=lv3_leukemia_CAL2[lv3_leukemia_CAL2$CAL2=="234",],aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv3_leukemia_CAL2[lv3_leukemia_CAL2$CAL2=="234",],aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_234),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_234),mapping = aes(x = 2010, y = Inf, label = poisson_reg),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_234),mapping = aes(x = 2004, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_234),mapping = aes(x = 2004, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))






ggplot()+geom_line(data=lv3_leukemia_CAL2[lv3_leukemia_CAL2$CAL2=="01",],aes(x=YEAR,y=sum_FY_BZ,color="red"),size=1)+labs(y="#of FY BZ")+
  facet_wrap(~ factor(NAME,levels=unique(lv3_leukemia_CAL2$NAME[lv3_leukemia_CAL2$CAL2=="01"])),scales="free_y")+
  geom_line(data=lv3_leukemia_CAL2[lv3_leukemia_CAL2$CAL2=="01",],aes(x=YEAR,y=simple.reg.FY,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv3_leukemia_CAL2[lv3_leukemia_CAL2$CAL2=="01",],aes(x=YEAR,y=poisson.reg.FY,color="green"),size=1)+
  geom_line(data=lv3_leukemia_CAL2[lv3_leukemia_CAL2$CAL2=="01",],aes(x=YEAR,y=linear.spline.FY,color='darkviolet'),size=1)+
  geom_line(data=lv3_leukemia_CAL2[lv3_leukemia_CAL2$CAL2=="01",],aes(x=YEAR,y=quadratic.reg.FY,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_01),mapping = aes(x = 2010, y = Inf, label = simple_reg.FY),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_01),mapping = aes(x = 2010, y = Inf, label = poisson_reg.FY),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_01),mapping = aes(x = 2002, y = Inf, label = linear_spline.FY),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_01),mapping = aes(x = 2002, y = Inf, label = quadratic_reg.FY),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of FY BZ',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of FY BZ","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))



ggplot()+geom_line(data=lv3_leukemia_CAL2[lv3_leukemia_CAL2$CAL2=="234",],aes(x=YEAR,y=sum_FY_BZ,color="red"),size=1)+labs(y="#of FY BZ")+
  facet_wrap(~ factor(NAME,levels=unique(lv3_leukemia_CAL2$NAME[lv3_leukemia_CAL2$CAL2=="234"])),scales="free_y")+
  geom_line(data=lv3_leukemia_CAL2[lv3_leukemia_CAL2$CAL2=="234",],aes(x=YEAR,y=simple.reg.FY,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv3_leukemia_CAL2[lv3_leukemia_CAL2$CAL2=="234",],aes(x=YEAR,y=poisson.reg.FY,color="green"),size=1)+
  geom_line(data=lv3_leukemia_CAL2[lv3_leukemia_CAL2$CAL2=="234",],aes(x=YEAR,y=linear.spline.FY,color='darkviolet'),size=1)+
  geom_line(data=lv3_leukemia_CAL2[lv3_leukemia_CAL2$CAL2=="234",],aes(x=YEAR,y=quadratic.reg.FY,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_234),mapping = aes(x = 2010, y = Inf, label = simple_reg.FY),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_234),mapping = aes(x = 2010, y = Inf, label = poisson_reg.FY),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_234),mapping = aes(x = 2004, y = Inf, label = linear_spline.FY),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_234),mapping = aes(x = 2004, y = Inf, label = quadratic_reg.FY),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of FY BZ',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

################################################################################################
############### 3. grouping by UP2 and SEX and CAL2 ######################### 
lv3_leukemia_SEX_CAL2<-df_leukemia1%>%group_by(UP1,UP2,SEX,CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  mutate(NAME=paste0("X",UP1,UP2,"_",SEX,CAL2))%>%as_tibble()

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
  ind.tr<-which(lv3_leukemia_SEX_CAL2$YEAR<=2014 & lv3_leukemia_SEX_CAL2$NAME==name_level[i])
  ind.val<-which(lv3_leukemia_SEX_CAL2$YEAR>2014 & lv3_leukemia_SEX_CAL2$NAME==name_level[i])
  
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
  
  MAPE_lv3_leukemia[(i+95),]<-c(unique(lv3_leukemia_SEX_CAL2$NAME[ind.tr]),
                                round(mean(abs(lv3_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_SEX_CAL2$simple.reg[ind.val])/lv3_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2),
                                round(mean(abs(lv3_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_SEX_CAL2$poisson.reg[ind.val])/lv3_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2),
                                round(mean(abs(lv3_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_SEX_CAL2$linear.spline[ind.val])/lv3_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2),
                                round(mean(abs(lv3_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_SEX_CAL2$quadratic.reg[ind.val])/lv3_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val])*100,2),
                                round(mean(abs(lv3_leukemia_SEX_CAL2$sum_FY_BZ[ind.val]-lv3_leukemia_SEX_CAL2$simple.reg.FY[ind.val])/lv3_leukemia_SEX_CAL2$sum_FY_BZ[ind.val])*100,2),
                                round(mean(abs(lv3_leukemia_SEX_CAL2$sum_FY_BZ[ind.val]-lv3_leukemia_SEX_CAL2$poisson.reg.FY[ind.val])/lv3_leukemia_SEX_CAL2$sum_FY_BZ[ind.val])*100,2),
                                round(mean(abs(lv3_leukemia_SEX_CAL2$sum_FY_BZ[ind.val]-lv3_leukemia_SEX_CAL2$linear.spline.FY[ind.val])/lv3_leukemia_SEX_CAL2$sum_FY_BZ[ind.val])*100,2),
                                round(mean(abs(lv3_leukemia_SEX_CAL2$sum_FY_BZ[ind.val]-lv3_leukemia_SEX_CAL2$quadratic.reg.FY[ind.val])/lv3_leukemia_SEX_CAL2$sum_FY_BZ[ind.val])*100,2))
  
  RMSE_lv3_leukemia[(i+95),]<-c(unique(lv3_leukemia_SEX_CAL2$NAME[ind.tr]),
                                round(sqrt(mean(abs(lv3_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_SEX_CAL2$simple.reg[ind.val]))),2),
                                round(sqrt(mean(abs(lv3_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_SEX_CAL2$poisson.reg[ind.val]))),2),
                                round(sqrt(mean(abs(lv3_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_SEX_CAL2$linear.spline[ind.val]))),2),
                                round(sqrt(mean(abs(lv3_leukemia_SEX_CAL2$sum_LEUKEMIA_BZ[ind.val]-lv3_leukemia_SEX_CAL2$quadratic.reg[ind.val]))),2),
                                round(sqrt(mean(abs(lv3_leukemia_SEX_CAL2$sum_FY_BZ[ind.val]-lv3_leukemia_SEX_CAL2$simple.reg.FY[ind.val]))),2),
                                round(sqrt(mean(abs(lv3_leukemia_SEX_CAL2$sum_FY_BZ[ind.val]-lv3_leukemia_SEX_CAL2$poisson.reg.FY[ind.val]))),2),
                                round(sqrt(mean(abs(lv3_leukemia_SEX_CAL2$sum_FY_BZ[ind.val]-lv3_leukemia_SEX_CAL2$linear.spline.FY[ind.val]))),2),
                                round(sqrt(mean(abs(lv3_leukemia_SEX_CAL2$sum_FY_BZ[ind.val]-lv3_leukemia_SEX_CAL2$quadratic.reg.FY[ind.val]))),2))
  
  
}

ind.M.01<-which(grepl("M",MAPE_lv3_leukemia[,1]) & grepl("01",MAPE_lv3_leukemia[,1]))
MAPE_lv3_leukemia_M_01<-MAPE_lv3_leukemia[ind.M.01,]

ind.F.01<-which(grepl("F",MAPE_lv3_leukemia[,1]) & grepl("01",MAPE_lv3_leukemia[,1]))
MAPE_lv3_leukemia_F_01<-MAPE_lv3_leukemia[ind.F.01,]

ind.M.234<-which(grepl("M",MAPE_lv3_leukemia[,1]) & grepl("234",MAPE_lv3_leukemia[,1]))
MAPE_lv3_leukemia_M_234<-MAPE_lv3_leukemia[ind.M.234,]

ind.F.234<-which(grepl("F",MAPE_lv3_leukemia[,1]) & grepl("234",MAPE_lv3_leukemia[,1]))
MAPE_lv3_leukemia_F_234<-MAPE_lv3_leukemia[ind.F.234,]



########## M & 01 ############
ggplot()+geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="M" & lv3_leukemia_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=sum_LEUKEMIA_BZ,color="red"),size=1)+labs(y="#of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(MAPE_lv3_leukemia_M_01[,1])),scales="free_y")+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="M" & lv3_leukemia_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="M" & lv3_leukemia_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=poisson.reg,color="green"),size=1)+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="M" & lv3_leukemia_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="M" & lv3_leukemia_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_M_01),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_M_01),mapping = aes(x = 2010, y = Inf, label = poisson_reg),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_M_01),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_M_01),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

########## M & 234 ############
ggplot()+geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="M" & lv3_leukemia_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=sum_LEUKEMIA_BZ,color="red"),size=1)+labs(y="#of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(MAPE_lv3_leukemia_M_234[,1])),scales="free_y")+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="M" & lv3_leukemia_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="M" & lv3_leukemia_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=poisson.reg,color="green"),size=1)+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="M" & lv3_leukemia_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="M" & lv3_leukemia_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_M_234),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_M_234),mapping = aes(x = 2010, y = Inf, label = poisson_reg),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_M_234),mapping = aes(x = 2004, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_M_234),mapping = aes(x = 2004, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


########## F & 01 ############
ggplot()+geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="F" & lv3_leukemia_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=sum_LEUKEMIA_BZ,color="red"),size=1)+labs(y="#of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(MAPE_lv3_leukemia_F_01[,1])),scales="free_y")+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="F" & lv3_leukemia_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="F" & lv3_leukemia_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=poisson.reg,color="green"),size=1)+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="F" & lv3_leukemia_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="F" & lv3_leukemia_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_F_01),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_F_01),mapping = aes(x = 2010, y = Inf, label = poisson_reg),vjust=1.5,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_F_01),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_F_01),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))



########## F & 234 ############
ggplot()+geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="F" & lv3_leukemia_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=sum_LEUKEMIA_BZ,color="red"),size=1)+labs(y="#of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(MAPE_lv3_leukemia_F_234[,1])),scales="free_y")+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="F" & lv3_leukemia_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="F" & lv3_leukemia_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=poisson.reg,color="green"),size=1)+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="F" & lv3_leukemia_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="F" & lv3_leukemia_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_F_234),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_F_234),mapping = aes(x = 2010, y = Inf, label = poisson_reg),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_F_234),mapping = aes(x = 2004, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_F_234),mapping = aes(x = 2004, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))





############## FY BZ ###############
########## M & 01 ############
ggplot()+geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="M" & lv3_leukemia_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=sum_FY_BZ,color="red"),size=1)+labs(y="#of FY BZ")+
  facet_wrap(~ factor(NAME,levels=unique(MAPE_lv3_leukemia_M_01[,1])),scales="free_y")+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="M" & lv3_leukemia_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=simple.reg.FY,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="M" & lv3_leukemia_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=poisson.reg.FY,color="green"),size=1)+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="M" & lv3_leukemia_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=linear.spline.FY,color='darkviolet'),size=1)+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="M" & lv3_leukemia_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=quadratic.reg.FY,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_M_01),mapping = aes(x = 2010, y = Inf, label = simple_reg.FY),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_M_01),mapping = aes(x = 2010, y = Inf, label = poisson_reg.FY),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_M_01),mapping = aes(x = 2002, y = Inf, label = linear_spline.FY),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_M_01),mapping = aes(x = 2002, y = Inf, label = quadratic_reg.FY),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of FY BZ',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

########## M & 234 ############
ggplot()+geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="M" & lv3_leukemia_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=sum_FY_BZ,color="red"),size=1)+labs(y="#of FY BZ")+
  facet_wrap(~ factor(NAME,levels=unique(MAPE_lv3_leukemia_M_234[,1])),scales="free_y")+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="M" & lv3_leukemia_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=simple.reg.FY,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="M" & lv3_leukemia_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=poisson.reg.FY,color="green"),size=1)+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="M" & lv3_leukemia_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=linear.spline.FY,color='darkviolet'),size=1)+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="M" & lv3_leukemia_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=quadratic.reg.FY,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_M_234),mapping = aes(x = 2010, y = Inf, label = simple_reg.FY),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_M_234),mapping = aes(x = 2010, y = Inf, label = poisson_reg.FY),vjust=1.5,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_M_234),mapping = aes(x = 2004, y = Inf, label = linear_spline.FY),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_M_234),mapping = aes(x = 2004, y = Inf, label = quadratic_reg.FY),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of FY BZ',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of leukemia","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


########## F & 01 ############
ggplot()+geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="F" & lv3_leukemia_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=sum_FY_BZ,color="red"),size=1)+labs(y="#of FY BZ")+
  facet_wrap(~ factor(NAME,levels=unique(MAPE_lv3_leukemia_F_01[,1])),scales="free_y")+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="F" & lv3_leukemia_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=simple.reg.FY,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="F" & lv3_leukemia_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=poisson.reg.FY,color="green"),size=1)+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="F" & lv3_leukemia_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=linear.spline.FY,color='darkviolet'),size=1)+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="F" & lv3_leukemia_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=quadratic.reg.FY,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_F_01),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_F_01),mapping = aes(x = 2010, y = Inf, label = poisson_reg),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_F_01),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_F_01),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of FY BZ',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of FY BZ","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))



########## F & 234 ############
ggplot()+geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="F" & lv3_leukemia_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=sum_FY_BZ,color="red"),size=1)+labs(y="#of FY BZ")+
  facet_wrap(~ factor(NAME,levels=unique(MAPE_lv3_leukemia_F_234[,1])),scales="free_y")+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="F" & lv3_leukemia_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=simple.reg.FY,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="F" & lv3_leukemia_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=poisson.reg.FY,color="green"),size=1)+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="F" & lv3_leukemia_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=linear.spline.FY,color='darkviolet'),size=1)+
  geom_line(data=lv3_leukemia_SEX_CAL2[lv3_leukemia_SEX_CAL2$SEX=="F" & lv3_leukemia_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=quadratic.reg.FY,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_F_234),mapping = aes(x = 2010, y = Inf, label = simple_reg.FY),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_F_234),mapping = aes(x = 2010, y = Inf, label = poisson_reg.FY),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_F_234),mapping = aes(x = 2004, y = Inf, label = linear_spline.FY),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_leukemia_F_234),mapping = aes(x = 2004, y = Inf, label = quadratic_reg.FY),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of FY BZ',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of FY BZ","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

value_eklee_lv3<-as.data.frame(rbind(lv3_leukemia_total[3:length(lv3_leukemia_total)],lv3_leukemia_SEX[4:length(lv3_leukemia_SEX)],lv3_leukemia_CAL2[4:length(lv3_leukemia_CAL2)],
                                     lv3_leukemia_SEX_CAL2[5:length(lv3_leukemia_SEX_CAL2)]))

val_leukemia_lv3<-value_eklee_lv3%>%as.data.frame()



####### val_leukemia_total ########
val_leukemia_total<-as.data.frame(rbind(val_leukemia_lv1,val_leukemia_lv2,val_leukemia_lv3))
colnames(val_leukemia_total)<-c("YEAR","sum_FY_BZ_Leukemia","sum_LEUKEMIA_BZ","NAME","simple.reg.LEUKEMIA","poisson.reg.LEUKEMIA",
                                "linear.spline.LEUKEMIA","quadratic.reg.LEUKEMIA", "simple.reg.leukemia.FY","poisson.reg.leukemia.FY","linear.spline.leukemia.FY",
                                "quadratic.reg.leukemia.FY")

MAPE_leukemia_total<-as.data.frame(rbind(MAPE_lv1_leukemia,MAPE_lv2_leukemia,MAPE_lv3_leukemia))
colnames(MAPE_leukemia_total)<-c("NAME","simple.reg.LEUKEMIA","poisson.reg.LEUKEMIA",
                                 "linear.spline.LEUKEMIA","quadratic.reg.LEUKEMIA", "simple.reg.leukemia.FY","poisson.reg.leukemia.FY","linear.spline.leukemia.FY",
                                 "quadratic.reg.leukemia.FY")

RMSE_leukemia_total<-as.data.frame(rbind(RMSE_lv1_leukemia,RMSE_lv2_leukemia,RMSE_lv3_leukemia))
colnames(RMSE_leukemia_total)<-c("NAME","simple.reg.LEUKEMIA","poisson.reg.LEUKEMIA",
                                 "linear.spline.LEUKEMIA","quadratic.reg.LEUKEMIA", "simple.reg.leukemia.FY","poisson.reg.leukemia.FY","linear.spline.leukemia.FY",
                                 "quadratic.reg.leukemia.FY")












































































######################### Lung cancer data ################################
############################# level 1 #############################
########### 1. total ###########
lv1_lung_total<-df_lung1%>%group_by(YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME="X")%>%as_tibble()

lv1_lung_total$simple.reg<-0
lv1_lung_total$poisson.reg<-0
lv1_lung_total$linear.spline<-0
lv1_lung_total$quadratic.reg<-0

lv1_lung_total$simple.reg.FY<-0
lv1_lung_total$poisson.reg.FY<-0
lv1_lung_total$linear.spline.FY<-0
lv1_lung_total$quadratic.reg.FY<-0

ind.tr<-which(lv1_lung_total$YEAR<=2014)
ind.val<-which(lv1_lung_total$YEAR>2014)

model.simple.reg<-lm(sum_LUNG_BZ~YEAR, data=lv1_lung_total[ind.tr,])
lv1_lung_total$simple.reg[ind.tr]<-model.simple.reg$fitted.values
lv1_lung_total$simple.reg[ind.val]<-predict(model.simple.reg,newdata=lv1_lung_total[ind.val,])

model.simple.reg.FY<-lm(sum_FY_BZ~YEAR, data=lv1_lung_total[ind.tr,])
lv1_lung_total$simple.reg.FY[ind.tr]<-model.simple.reg.FY$fitted.values
lv1_lung_total$simple.reg.FY[ind.val]<-predict(model.simple.reg.FY,newdata=lv1_lung_total[ind.val,])

model.poisson.reg<-glm(sum_LUNG_BZ~YEAR, data=lv1_lung_total[ind.tr,],family='poisson')
lv1_lung_total$poisson.reg[ind.tr]<-model.poisson.reg$fitted.values
lv1_lung_total$poisson.reg[ind.val]<-exp(predict(model.poisson.reg,newdata=lv1_lung_total[ind.val,]))

model.poisson.reg.FY<-glm(sum_FY_BZ~YEAR, data=lv1_lung_total[ind.tr,],family='poisson')
lv1_lung_total$poisson.reg.FY[ind.tr]<-model.poisson.reg.FY$fitted.values
lv1_lung_total$poisson.reg.FY[ind.val]<-exp(predict(model.poisson.reg.FY,newdata=lv1_lung_total[ind.val,]))

model.quadratic.reg<-lm(sum_LUNG_BZ~poly(YEAR,2), data=lv1_lung_total[ind.tr,])
lv1_lung_total$quadratic.reg[ind.tr]<-ifelse(model.quadratic.reg$fitted.values<0, 0, model.quadratic.reg$fitted.values)
lv1_lung_total$quadratic.reg[ind.val]<-predict(model.quadratic.reg,newdata=lv1_lung_total[ind.val,])

model.quadratic.reg.FY<-lm(sum_FY_BZ~poly(YEAR,2), data=lv1_lung_total[ind.tr,])
lv1_lung_total$quadratic.reg.FY[ind.tr]<-model.quadratic.reg.FY$fitted.values
lv1_lung_total$quadratic.reg.FY[ind.val]<-predict(model.quadratic.reg.FY,newdata=lv1_lung_total[ind.val,])


model.linear.spline<-lm(sum_LUNG_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_lung_total[ind.tr,])
lv1_lung_total$linear.spline[ind.tr]<-model.linear.spline$fitted.values
lv1_lung_total$linear.spline[ind.val]<-predict(model.linear.spline,newdata=lv1_lung_total[ind.val,])

model.linear.spline.FY<-lm(sum_FY_BZ~bs(YEAR,knots=c(2005,2010),degree=1),data=lv1_lung_total[ind.tr,])
lv1_lung_total$linear.spline.FY[ind.tr]<-model.linear.spline.FY$fitted.values
lv1_lung_total$linear.spline.FY[ind.val]<-predict(model.linear.spline.FY,newdata=lv1_lung_total[ind.val,])

MAPE_lv1_lung<-matrix(0,nrow=9,ncol=9)
colnames(MAPE_lv1_lung)<-c("NAME","simple_reg","poisson_reg","linear_spline","quadratic_reg","simple_reg_FY","poisson_reg_FY","linear_spline_FY",
                           "quadratic_reg_FY")
RMSE_lv1_lung<-matrix(0,nrow=9,ncol=9)
colnames(RMSE_lv1_lung)<-c("NAME","simple_reg","poisson_reg","linear_spline","quadratic_reg","simple_reg_FY","poisson_reg_FY","linear_spline_FY",
                           "quadratic_reg_FY")

MAPE_lv1_lung[1,]<-c(unique(lv1_lung_total$NAME),
                     round(mean(abs(lv1_lung_total$sum_LUNG_BZ[ind.val]-lv1_lung_total$simple.reg[ind.val])/lv1_lung_total$sum_LUNG_BZ[ind.val])*100,2),
                     round(mean(abs(lv1_lung_total$sum_LUNG_BZ[ind.val]-lv1_lung_total$poisson.reg[ind.val])/lv1_lung_total$sum_LUNG_BZ[ind.val])*100,2),
                     round(mean(abs(lv1_lung_total$sum_LUNG_BZ[ind.val]-lv1_lung_total$linear.spline[ind.val])/lv1_lung_total$sum_LUNG_BZ[ind.val])*100,2),
                     round(mean(abs(lv1_lung_total$sum_LUNG_BZ[ind.val]-lv1_lung_total$quadratic.reg[ind.val])/lv1_lung_total$sum_LUNG_BZ[ind.val])*100,2),
                     round(mean(abs(lv1_lung_total$sum_FY_BZ[ind.val]-lv1_lung_total$simple.reg.FY[ind.val])/lv1_lung_total$sum_FY_BZ[ind.val])*100,2),
                     round(mean(abs(lv1_lung_total$sum_FY_BZ[ind.val]-lv1_lung_total$poisson.reg.FY[ind.val])/lv1_lung_total$sum_FY_BZ[ind.val])*100,2),
                     round(mean(abs(lv1_lung_total$sum_FY_BZ[ind.val]-lv1_lung_total$linear.spline.FY[ind.val])/lv1_lung_total$sum_FY_BZ[ind.val])*100,2),
                     round(mean(abs(lv1_lung_total$sum_FY_BZ[ind.val]-lv1_lung_total$quadratic.reg.FY[ind.val])/lv1_lung_total$sum_FY_BZ[ind.val])*100,2))
RMSE_lv1_lung[1,]<-c(unique(lv1_lung_total$NAME),
                     round(sqrt(mean(abs(lv1_lung_total$sum_LUNG_BZ[ind.val]-lv1_lung_total$simple.reg[ind.val]))),2),
                     round(sqrt(mean(abs(lv1_lung_total$sum_LUNG_BZ[ind.val]-lv1_lung_total$poisson.reg[ind.val]))),2),
                     round(sqrt(mean(abs(lv1_lung_total$sum_LUNG_BZ[ind.val]-lv1_lung_total$linear.spline[ind.val]))),2),
                     round(sqrt(mean(abs(lv1_lung_total$sum_LUNG_BZ[ind.val]-lv1_lung_total$quadratic.reg[ind.val]))),2),
                     round(sqrt(mean(abs(lv1_lung_total$sum_FY_BZ[ind.val]-lv1_lung_total$simple.reg.FY[ind.val]))),2),
                     round(sqrt(mean(abs(lv1_lung_total$sum_FY_BZ[ind.val]-lv1_lung_total$poisson.reg.FY[ind.val]))),2),
                     round(sqrt(mean(abs(lv1_lung_total$sum_FY_BZ[ind.val]-lv1_lung_total$linear.spline.FY[ind.val]))),2),
                     round(sqrt(mean(abs(lv1_lung_total$sum_FY_BZ[ind.val]-lv1_lung_total$quadratic.reg.FY[ind.val]))),2))

ggplot()+geom_line(data=lv1_lung_total,aes(x=YEAR,y=sum_LUNG_BZ,color="red"),size=1)+labs(y="#of_lung")+
  geom_line(data=lv1_lung_total,aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Lung Data comparing methods performance")+
  geom_line(data=lv1_lung_total,aes(x=YEAR,y=poisson.reg,color="green"),size=1)+
  geom_line(data=lv1_lung_total,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv1_lung_total,aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  labs(x="Year")+
  scale_color_identity(name='#of lung',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=lv1_lung_total,aes(x=YEAR,y=sum_FY_BZ,color="red"),size=1)+labs(y="FY_BZ_lung")+
  geom_line(data=lv1_lung_total,aes(x=YEAR,y=simple.reg.FY,color="black"),size=1)+ggtitle("Lung Data comparing methods performance")+
  geom_line(data=lv1_lung_total,aes(x=YEAR,y=poisson.reg.FY,color="green"),size=1)+
  geom_line(data=lv1_lung_total,aes(x=YEAR,y=linear.spline.FY,color='darkviolet'),size=1)+
  geom_line(data=lv1_lung_total,aes(x=YEAR,y=quadratic.reg.FY,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  labs(x="Year")+
  scale_color_identity(name='FY_BZ lung',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of FY_BZ","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

####################################################################################
#################### 2. grouping by SEX ##################
lv1_lung_SEX<-df_lung1%>%group_by(SEX,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME=paste0("X","_",SEX))%>%as_tibble()

SEX_level<-unique(lv1_lung_SEX$SEX)

lv1_lung_SEX$simple.reg<-0
lv1_lung_SEX$poisson.reg<-0
lv1_lung_SEX$linear.spline<-0
lv1_lung_SEX$quadratic.reg<-0

lv1_lung_SEX$simple.reg.FY<-0
lv1_lung_SEX$poisson.reg.FY<-0
lv1_lung_SEX$linear.spline.FY<-0
lv1_lung_SEX$quadratic.reg.FY<-0

#SEX_level<-unique(lv1_lung_SEX$SEX)

for(i in 1:length(SEX_level)){
  ind.tr<-which(lv1_lung_SEX$YEAR<=2014 & lv1_lung_SEX$SEX==SEX_level[i])
  ind.val<-which(lv1_lung_SEX$YEAR>2014 & lv1_lung_SEX$SEX==SEX_level[i])
  
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
  
  
  
  MAPE_lv1_lung[(i+1),]<-c(unique(lv1_lung_SEX$NAME[ind.tr]),
                           round(mean(abs(lv1_lung_SEX$sum_LUNG_BZ[ind.val]-lv1_lung_SEX$simple.reg[ind.val])/lv1_lung_SEX$sum_LUNG_BZ[ind.val])*100,2),
                           round(mean(abs(lv1_lung_SEX$sum_LUNG_BZ[ind.val]-lv1_lung_SEX$poisson.reg[ind.val])/lv1_lung_SEX$sum_LUNG_BZ[ind.val])*100,2),
                           round(mean(abs(lv1_lung_SEX$sum_LUNG_BZ[ind.val]-lv1_lung_SEX$linear.spline[ind.val])/lv1_lung_SEX$sum_LUNG_BZ[ind.val])*100,2),
                           round(mean(abs(lv1_lung_SEX$sum_LUNG_BZ[ind.val]-lv1_lung_SEX$quadratic.reg[ind.val])/lv1_lung_SEX$sum_LUNG_BZ[ind.val])*100,2),
                           round(mean(abs(lv1_lung_SEX$sum_FY_BZ[ind.val]-lv1_lung_SEX$simple.reg.FY[ind.val])/lv1_lung_SEX$sum_FY_BZ[ind.val])*100,2),
                           round(mean(abs(lv1_lung_SEX$sum_FY_BZ[ind.val]-lv1_lung_SEX$poisson.reg.FY[ind.val])/lv1_lung_SEX$sum_FY_BZ[ind.val])*100,2),
                           round(mean(abs(lv1_lung_SEX$sum_FY_BZ[ind.val]-lv1_lung_SEX$linear.spline.FY[ind.val])/lv1_lung_SEX$sum_FY_BZ[ind.val])*100,2),
                           round(mean(abs(lv1_lung_SEX$sum_FY_BZ[ind.val]-lv1_lung_SEX$quadratic.reg.FY[ind.val])/lv1_lung_SEX$sum_FY_BZ[ind.val])*100,2))
  
  RMSE_lv1_lung[(i+1),]<-c(unique(lv1_lung_SEX$NAME[ind.tr]),
                           round(sqrt(mean(abs(lv1_lung_SEX$sum_LUNG_BZ[ind.val]-lv1_lung_SEX$simple.reg[ind.val]))),2),
                           round(sqrt(mean(abs(lv1_lung_SEX$sum_LUNG_BZ[ind.val]-lv1_lung_SEX$poisson.reg[ind.val]))),2),
                           round(sqrt(mean(abs(lv1_lung_SEX$sum_LUNG_BZ[ind.val]-lv1_lung_SEX$linear.spline[ind.val]))),2),
                           round(sqrt(mean(abs(lv1_lung_SEX$sum_LUNG_BZ[ind.val]-lv1_lung_SEX$quadratic.reg[ind.val]))),2),
                           round(sqrt(mean(abs(lv1_lung_SEX$sum_FY_BZ[ind.val]-lv1_lung_SEX$simple.reg.FY[ind.val]))),2),
                           round(sqrt(mean(abs(lv1_lung_SEX$sum_FY_BZ[ind.val]-lv1_lung_SEX$poisson.reg.FY[ind.val]))),2),
                           round(sqrt(mean(abs(lv1_lung_SEX$sum_FY_BZ[ind.val]-lv1_lung_SEX$linear.spline.FY[ind.val]))),2),
                           round(sqrt(mean(abs(lv1_lung_SEX$sum_FY_BZ[ind.val]-lv1_lung_SEX$quadratic.reg.FY[ind.val]))),2))
  
}

ggplot()+geom_line(data=lv1_lung_SEX,aes(x=YEAR,y=sum_LUNG_BZ,color="red"),size=1)+labs(y="#of_lung")+
  facet_wrap(~ factor(SEX,levels=SEX_level),scales="free_y")+
  geom_line(data=lv1_lung_SEX,aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Lung Data comparing methods performance")+
  geom_line(data=lv1_lung_SEX,aes(x=YEAR,y=poisson.reg,color="green"),size=1)+
  geom_line(data=lv1_lung_SEX,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv1_lung_SEX,aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  labs(x="Year")+
  scale_color_identity(name='#of lung',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=lv1_lung_SEX,aes(x=YEAR,y=sum_FY_BZ,color="red"),size=1)+labs(y="#of FY_BZ")+
  facet_wrap(~ factor(SEX,levels=SEX_level),scales="free_y")+
  geom_line(data=lv1_lung_SEX,aes(x=YEAR,y=simple.reg.FY,color="black"),size=1)+ggtitle("Lung Data comparing methods performance")+
  geom_line(data=lv1_lung_SEX,aes(x=YEAR,y=poisson.reg.FY,color="green"),size=1)+
  geom_line(data=lv1_lung_SEX,aes(x=YEAR,y=linear.spline.FY,color='darkviolet'),size=1)+
  geom_line(data=lv1_lung_SEX,aes(x=YEAR,y=quadratic.reg.FY,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  labs(x="Year")+
  scale_color_identity(name='#of FY_BZ',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of FY_BZ","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


################################################################################################
############### 3. grouping by CAL2 ######################### 
lv1_lung_CAL2<-df_lung1%>%group_by(CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME=paste0("X","_",CAL2))%>%as_tibble()

CAL2_level<-unique(lv1_lung_CAL2$CAL2)

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
  ind.tr<-which(lv1_lung_CAL2$YEAR<=2014 & lv1_lung_CAL2$CAL2==CAL2_level[i])
  ind.val<-which(lv1_lung_CAL2$YEAR>2014 & lv1_lung_CAL2$CAL2==CAL2_level[i])
  
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
  
  
  
  MAPE_lv1_lung[(i+3),]<-c(unique(lv1_lung_CAL2$NAME[ind.tr]),
                           round(mean(abs(lv1_lung_CAL2$sum_LUNG_BZ[ind.val]-lv1_lung_CAL2$simple.reg[ind.val])/lv1_lung_CAL2$sum_LUNG_BZ[ind.val])*100,2),
                           round(mean(abs(lv1_lung_CAL2$sum_LUNG_BZ[ind.val]-lv1_lung_CAL2$poisson.reg[ind.val])/lv1_lung_CAL2$sum_LUNG_BZ[ind.val])*100,2),
                           round(mean(abs(lv1_lung_CAL2$sum_LUNG_BZ[ind.val]-lv1_lung_CAL2$linear.spline[ind.val])/lv1_lung_CAL2$sum_LUNG_BZ[ind.val])*100,2),
                           round(mean(abs(lv1_lung_CAL2$sum_LUNG_BZ[ind.val]-lv1_lung_CAL2$quadratic.reg[ind.val])/lv1_lung_CAL2$sum_LUNG_BZ[ind.val])*100,2),
                           round(mean(abs(lv1_lung_CAL2$sum_FY_BZ[ind.val]-lv1_lung_CAL2$simple.reg.FY[ind.val])/lv1_lung_CAL2$sum_FY_BZ[ind.val])*100,2),
                           round(mean(abs(lv1_lung_CAL2$sum_FY_BZ[ind.val]-lv1_lung_CAL2$poisson.reg.FY[ind.val])/lv1_lung_CAL2$sum_FY_BZ[ind.val])*100,2),
                           round(mean(abs(lv1_lung_CAL2$sum_FY_BZ[ind.val]-lv1_lung_CAL2$linear.spline.FY[ind.val])/lv1_lung_CAL2$sum_FY_BZ[ind.val])*100,2),
                           round(mean(abs(lv1_lung_CAL2$sum_FY_BZ[ind.val]-lv1_lung_CAL2$quadratic.reg.FY[ind.val])/lv1_lung_CAL2$sum_FY_BZ[ind.val])*100,2))
  
  RMSE_lv1_lung[(i+3),]<-c(unique(lv1_lung_CAL2$NAME[ind.tr]),
                           round(sqrt(mean(abs(lv1_lung_CAL2$sum_LUNG_BZ[ind.val]-lv1_lung_CAL2$simple.reg[ind.val]))),2),
                           round(sqrt(mean(abs(lv1_lung_CAL2$sum_LUNG_BZ[ind.val]-lv1_lung_CAL2$poisson.reg[ind.val]))),2),
                           round(sqrt(mean(abs(lv1_lung_CAL2$sum_LUNG_BZ[ind.val]-lv1_lung_CAL2$linear.spline[ind.val]))),2),
                           round(sqrt(mean(abs(lv1_lung_CAL2$sum_LUNG_BZ[ind.val]-lv1_lung_CAL2$quadratic.reg[ind.val]))),2),
                           round(sqrt(mean(abs(lv1_lung_CAL2$sum_FY_BZ[ind.val]-lv1_lung_CAL2$simple.reg.FY[ind.val]))),2),
                           round(sqrt(mean(abs(lv1_lung_CAL2$sum_FY_BZ[ind.val]-lv1_lung_CAL2$poisson.reg.FY[ind.val]))),2),
                           round(sqrt(mean(abs(lv1_lung_CAL2$sum_FY_BZ[ind.val]-lv1_lung_CAL2$linear.spline.FY[ind.val]))),2),
                           round(sqrt(mean(abs(lv1_lung_CAL2$sum_FY_BZ[ind.val]-lv1_lung_CAL2$quadratic.reg.FY[ind.val]))),2))
  
  
}

ggplot()+geom_line(data=lv1_lung_CAL2,aes(x=YEAR,y=sum_LUNG_BZ,color="red"),size=1)+labs(y="#of_lung")+
  facet_wrap(~ factor(CAL2,levels=CAL2_level),scales="free_y")+
  geom_line(data=lv1_lung_CAL2,aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Lung Data comparing methods performance")+
  geom_line(data=lv1_lung_CAL2,aes(x=YEAR,y=poisson.reg,color="green"),size=1)+
  geom_line(data=lv1_lung_CAL2,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv1_lung_CAL2,aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  labs(x="Year")+
  scale_color_identity(name='#of lung',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=lv1_lung_CAL2,aes(x=YEAR,y=sum_FY_BZ,color="red"),size=1)+labs(y="#of FY_BZ")+
  facet_wrap(~ factor(CAL2,levels=CAL2_level),scales="free_y")+
  geom_line(data=lv1_lung_CAL2,aes(x=YEAR,y=simple.reg.FY,color="black"),size=1)+ggtitle("Lung Data comparing methods performance")+
  geom_line(data=lv1_lung_CAL2,aes(x=YEAR,y=poisson.reg.FY,color="green"),size=1)+
  geom_line(data=lv1_lung_CAL2,aes(x=YEAR,y=linear.spline.FY,color='darkviolet'),size=1)+
  geom_line(data=lv1_lung_CAL2,aes(x=YEAR,y=quadratic.reg.FY,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  labs(x="Year")+
  scale_color_identity(name='#of FY_BZ',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of FY_BZ","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


################################################################################################
############### 4. grouping by SEX and CAL2 ######################### 
lv1_lung_SEX_CAL2<-df_lung1%>%group_by(SEX,CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME=paste0("X","_",SEX,CAL2))%>%as_tibble()

SEX_CAL2_level<-unique(lv1_lung_SEX_CAL2$NAME)

lv1_lung_SEX_CAL2$simple.reg<-0
lv1_lung_SEX_CAL2$poisson.reg<-0
lv1_lung_SEX_CAL2$linear.spline<-0
lv1_lung_SEX_CAL2$quadratic.reg<-0

lv1_lung_SEX_CAL2$simple.reg.FY<-0
lv1_lung_SEX_CAL2$poisson.reg.FY<-0
lv1_lung_SEX_CAL2$linear.spline.FY<-0
lv1_lung_SEX_CAL2$quadratic.reg.FY<-0

for(i in 1:length(SEX_CAL2_level)){
  ind.tr<-which(lv1_lung_SEX_CAL2$YEAR<=2014 & lv1_lung_SEX_CAL2$NAME==SEX_CAL2_level[i])
  ind.val<-which(lv1_lung_SEX_CAL2$YEAR>2014 & lv1_lung_SEX_CAL2$NAME==SEX_CAL2_level[i])
  
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
  
  
  
  MAPE_lv1_lung[(i+5),]<-c(unique(lv1_lung_SEX_CAL2$NAME[ind.tr]),
                           round(mean(abs(lv1_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv1_lung_SEX_CAL2$simple.reg[ind.val])/lv1_lung_SEX_CAL2$sum_LUNG_BZ[ind.val])*100,2),
                           round(mean(abs(lv1_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv1_lung_SEX_CAL2$poisson.reg[ind.val])/lv1_lung_SEX_CAL2$sum_LUNG_BZ[ind.val])*100,2),
                           round(mean(abs(lv1_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv1_lung_SEX_CAL2$linear.spline[ind.val])/lv1_lung_SEX_CAL2$sum_LUNG_BZ[ind.val])*100,2),
                           round(mean(abs(lv1_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv1_lung_SEX_CAL2$quadratic.reg[ind.val])/lv1_lung_SEX_CAL2$sum_LUNG_BZ[ind.val])*100,2),
                           round(mean(abs(lv1_lung_SEX_CAL2$sum_FY_BZ[ind.val]-lv1_lung_SEX_CAL2$simple.reg.FY[ind.val])/lv1_lung_SEX_CAL2$sum_FY_BZ[ind.val])*100,2),
                           round(mean(abs(lv1_lung_SEX_CAL2$sum_FY_BZ[ind.val]-lv1_lung_SEX_CAL2$poisson.reg.FY[ind.val])/lv1_lung_SEX_CAL2$sum_FY_BZ[ind.val])*100,2),
                           round(mean(abs(lv1_lung_SEX_CAL2$sum_FY_BZ[ind.val]-lv1_lung_SEX_CAL2$linear.spline.FY[ind.val])/lv1_lung_SEX_CAL2$sum_FY_BZ[ind.val])*100,2),
                           round(mean(abs(lv1_lung_SEX_CAL2$sum_FY_BZ[ind.val]-lv1_lung_SEX_CAL2$quadratic.reg.FY[ind.val])/lv1_lung_SEX_CAL2$sum_FY_BZ[ind.val])*100,2))
  
  RMSE_lv1_lung[(i+5),]<-c(unique(lv1_lung_SEX_CAL2$NAME[ind.tr]),
                           round(sqrt(mean(abs(lv1_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv1_lung_SEX_CAL2$simple.reg[ind.val]))),2),
                           round(sqrt(mean(abs(lv1_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv1_lung_SEX_CAL2$poisson.reg[ind.val]))),2),
                           round(sqrt(mean(abs(lv1_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv1_lung_SEX_CAL2$linear.spline[ind.val]))),2),
                           round(sqrt(mean(abs(lv1_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv1_lung_SEX_CAL2$quadratic.reg[ind.val]))),2),
                           round(sqrt(mean(abs(lv1_lung_SEX_CAL2$sum_FY_BZ[ind.val]-lv1_lung_SEX_CAL2$simple.reg.FY[ind.val]))),2),
                           round(sqrt(mean(abs(lv1_lung_SEX_CAL2$sum_FY_BZ[ind.val]-lv1_lung_SEX_CAL2$poisson.reg.FY[ind.val]))),2),
                           round(sqrt(mean(abs(lv1_lung_SEX_CAL2$sum_FY_BZ[ind.val]-lv1_lung_SEX_CAL2$linear.spline.FY[ind.val]))),2),
                           round(sqrt(mean(abs(lv1_lung_SEX_CAL2$sum_FY_BZ[ind.val]-lv1_lung_SEX_CAL2$quadratic.reg.FY[ind.val]))),2))
  
}

ggplot()+geom_line(data=lv1_lung_SEX_CAL2,aes(x=YEAR,y=sum_LUNG_BZ,color="red"),size=1)+labs(y="#of_lung")+
  facet_wrap(~ factor(NAME,levels=SEX_CAL2_level),scales="free_y")+
  geom_line(data=lv1_lung_SEX_CAL2,aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Lung Data comparing methods performance")+
  geom_line(data=lv1_lung_SEX_CAL2,aes(x=YEAR,y=poisson.reg,color="green"),size=1)+
  geom_line(data=lv1_lung_SEX_CAL2,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv1_lung_SEX_CAL2,aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  labs(x="Year")+
  scale_color_identity(name='#of lung',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=lv1_lung_SEX_CAL2,aes(x=YEAR,y=sum_FY_BZ,color="red"),size=1)+labs(y="#of_FY_BZ")+
  facet_wrap(~ factor(NAME,levels=SEX_CAL2_level),scales="free_y")+
  geom_line(data=lv1_lung_SEX_CAL2,aes(x=YEAR,y=simple.reg.FY,color="black"),size=1)+ggtitle("Lung Data comparing methods performance")+
  geom_line(data=lv1_lung_SEX_CAL2,aes(x=YEAR,y=poisson.reg.FY,color="green"),size=1)+
  geom_line(data=lv1_lung_SEX_CAL2,aes(x=YEAR,y=linear.spline.FY,color='darkviolet'),size=1)+
  geom_line(data=lv1_lung_SEX_CAL2,aes(x=YEAR,y=quadratic.reg.FY,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  labs(x="Year")+
  scale_color_identity(name='#of FY_BZ',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of FY_BZ","simple linear regression","poisson regreesion","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

value_eklee_lv1<-as.data.frame(rbind(lv1_lung_total,lv1_lung_SEX[2:length(lv1_lung_SEX)],lv1_lung_CAL2[2:length(lv1_lung_CAL2)],
                                     lv1_lung_SEX_CAL2[3:length(lv1_lung_SEX_CAL2)]))
val_lung_lv1<-value_eklee_lv1%>%as.data.frame()

##################### level 2 data ##########################
############# 1. grouping by UP1 #################
lv2_lung_total<-df_lung1%>%group_by(UP1,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME=paste0("X",UP1))%>%as_tibble()

MAPE_lv2_lung<-matrix(0,nrow=length(unique(lv2_lung_total$NAME))*(1+2+2+4),ncol=9)
colnames(MAPE_lv2_lung)<-c("NAME","simple_reg","poisson_reg","linear_spline","quadratic_reg",
                           "simple_reg.FY","poisson_reg.FY","linear_spline.FY","quadratic_reg.FY")

RMSE_lv2_lung<-matrix(0,nrow=length(unique(lv2_lung_total$NAME))*(1+2+2+4),ncol=9)
colnames(RMSE_lv2_leukemia)<-c("NAME","simple_reg","poisson_reg","linear_spline","quadratic_reg",
                               "simple_reg.FY","poisson_reg.FY","linear_spline.FY","quadratic_reg.FY")
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
  ind.tr<-which(lv2_lung_total$YEAR<=2014 & lv2_lung_total$NAME==name_level[i])
  ind.val<-which(lv2_lung_total$YEAR>2014 & lv2_lung_total$NAME==name_level[i])
  
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
  
  
  MAPE_lv2_lung[i,]<-c(unique(lv2_lung_total$NAME[ind.tr]),
                       round(mean(abs(lv2_lung_total$sum_LUNG_BZ[ind.val]-lv2_lung_total$simple.reg[ind.val])/lv2_lung_total$sum_LUNG_BZ[ind.val])*100,2),
                       round(mean(abs(lv2_lung_total$sum_LUNG_BZ[ind.val]-lv2_lung_total$poisson.reg[ind.val])/lv2_lung_total$sum_LUNG_BZ[ind.val])*100,2),
                       round(mean(abs(lv2_lung_total$sum_LUNG_BZ[ind.val]-lv2_lung_total$linear.spline[ind.val])/lv2_lung_total$sum_LUNG_BZ[ind.val])*100,2),
                       round(mean(abs(lv2_lung_total$sum_LUNG_BZ[ind.val]-lv2_lung_total$quadratic.reg[ind.val])/lv2_lung_total$sum_LUNG_BZ[ind.val])*100,2),
                       round(mean(abs(lv2_lung_total$sum_FY_BZ[ind.val]-lv2_lung_total$simple.reg.FY[ind.val])/lv2_lung_total$sum_FY_BZ[ind.val])*100,2),
                       round(mean(abs(lv2_lung_total$sum_FY_BZ[ind.val]-lv2_lung_total$poisson.reg.FY[ind.val])/lv2_lung_total$sum_FY_BZ[ind.val])*100,2),
                       round(mean(abs(lv2_lung_total$sum_FY_BZ[ind.val]-lv2_lung_total$linear.spline.FY[ind.val])/lv2_lung_total$sum_FY_BZ[ind.val])*100,2),
                       round(mean(abs(lv2_lung_total$sum_FY_BZ[ind.val]-lv2_lung_total$quadratic.reg.FY[ind.val])/lv2_lung_total$sum_FY_BZ[ind.val])*100,2))
  
  RMSE_lv2_lung[i,]<-c(unique(lv2_lung_total$NAME[ind.tr]),
                       round(sqrt(mean(abs(lv2_lung_total$sum_LUNG_BZ[ind.val]-lv2_lung_total$simple.reg[ind.val]))),2),
                       round(sqrt(mean(abs(lv2_lung_total$sum_LUNG_BZ[ind.val]-lv2_lung_total$poisson.reg[ind.val]))),2),
                       round(sqrt(mean(abs(lv2_lung_total$sum_LUNG_BZ[ind.val]-lv2_lung_total$linear.spline[ind.val]))),2),
                       round(sqrt(mean(abs(lv2_lung_total$sum_LUNG_BZ[ind.val]-lv2_lung_total$quadratic.reg[ind.val]))),2),
                       round(sqrt(mean(abs(lv2_lung_total$sum_FY_BZ[ind.val]-lv2_lung_total$simple.reg.FY[ind.val]))),2),
                       round(sqrt(mean(abs(lv2_lung_total$sum_FY_BZ[ind.val]-lv2_lung_total$poisson.reg.FY[ind.val]))),2),
                       round(sqrt(mean(abs(lv2_lung_total$sum_FY_BZ[ind.val]-lv2_lung_total$linear.spline.FY[ind.val]))),2),
                       round(sqrt(mean(abs(lv2_lung_total$sum_FY_BZ[ind.val]-lv2_lung_total$quadratic.reg.FY[ind.val]))),2))
  
  
}

ggplot()+geom_line(data=lv2_lung_total,aes(x=YEAR,y=sum_LUNG_BZ,color="red"),size=1)+labs(y="#of_lung")+
  facet_wrap(~ factor(NAME,levels=unique(lv2_lung_total$NAME)),scales="free_y")+
  geom_line(data=lv2_lung_total,aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Lung Data comparing methods performance")+
  geom_line(data=lv2_lung_total,aes(x=YEAR,y=poisson.reg,color="green"),size=1)+
  geom_line(data=lv2_lung_total,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv2_lung_total,aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung),mapping = aes(x = 2010, y = Inf, label = poisson_reg),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of lung',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=lv2_lung_total,aes(x=YEAR,y=sum_FY_BZ,color="red"),size=1)+labs(y="#of_FY_BZ")+
  facet_wrap(~ factor(NAME,levels=unique(lv2_lung_total$NAME)),scales="free_y")+
  geom_line(data=lv2_lung_total,aes(x=YEAR,y=simple.reg.FY,color="black"),size=1)+ggtitle("Lung Data comparing methods performance")+
  geom_line(data=lv2_lung_total,aes(x=YEAR,y=poisson.reg.FY,color="green"),size=1)+
  geom_line(data=lv2_lung_total,aes(x=YEAR,y=linear.spline.FY,color='darkviolet'),size=1)+
  geom_line(data=lv2_lung_total,aes(x=YEAR,y=quadratic.reg.FY,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung),mapping = aes(x = 2010, y = Inf, label = simple_reg.FY),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung),mapping = aes(x = 2010, y = Inf, label = poisson_reg.FY),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung),mapping = aes(x = 2002, y = Inf, label = linear_spline.FY),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung),mapping = aes(x = 2002, y = Inf, label = quadratic_reg.FY),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of FY BZ',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of FY BZ","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


##################################################################################
############# 2. grouping by UP1 and SEX #################
lv2_lung_SEX<-df_lung1%>%group_by(UP1,SEX,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME=paste0("X",UP1,"_",SEX))%>%as_tibble()

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
  ind.tr<-which(lv2_lung_SEX$YEAR<=2014 & lv2_lung_SEX$NAME==name_level[i])
  ind.val<-which(lv2_lung_SEX$YEAR>2014 & lv2_lung_SEX$NAME==name_level[i])
  
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
  
  
  MAPE_lv2_lung[(i+19),]<-c(unique(lv2_lung_SEX$NAME[ind.tr]),
                            round(mean(abs(lv2_lung_SEX$sum_LUNG_BZ[ind.val]-lv2_lung_SEX$simple.reg[ind.val])/lv2_lung_SEX$sum_LUNG_BZ[ind.val])*100,2),
                            round(mean(abs(lv2_lung_SEX$sum_LUNG_BZ[ind.val]-lv2_lung_SEX$poisson.reg[ind.val])/lv2_lung_SEX$sum_LUNG_BZ[ind.val])*100,2),
                            round(mean(abs(lv2_lung_SEX$sum_LUNG_BZ[ind.val]-lv2_lung_SEX$linear.spline[ind.val])/lv2_lung_SEX$sum_LUNG_BZ[ind.val])*100,2),
                            round(mean(abs(lv2_lung_SEX$sum_LUNG_BZ[ind.val]-lv2_lung_SEX$quadratic.reg[ind.val])/lv2_lung_SEX$sum_LUNG_BZ[ind.val])*100,2),
                            round(mean(abs(lv2_lung_SEX$sum_FY_BZ[ind.val]-lv2_lung_SEX$simple.reg.FY[ind.val])/lv2_lung_SEX$sum_FY_BZ[ind.val])*100,2),
                            round(mean(abs(lv2_lung_SEX$sum_FY_BZ[ind.val]-lv2_lung_SEX$poisson.reg.FY[ind.val])/lv2_lung_SEX$sum_FY_BZ[ind.val])*100,2),
                            round(mean(abs(lv2_lung_SEX$sum_FY_BZ[ind.val]-lv2_lung_SEX$linear.spline.FY[ind.val])/lv2_lung_SEX$sum_FY_BZ[ind.val])*100,2),
                            round(mean(abs(lv2_lung_SEX$sum_FY_BZ[ind.val]-lv2_lung_SEX$quadratic.reg.FY[ind.val])/lv2_lung_SEX$sum_FY_BZ[ind.val])*100,2))
  
  RMSE_lv2_lung[(i+19),]<-c(unique(lv2_lung_SEX$NAME[ind.tr]),
                            round(sqrt(mean(abs(lv2_lung_SEX$sum_LUNG_BZ[ind.val]-lv2_lung_SEX$simple.reg[ind.val]))),2),
                            round(sqrt(mean(abs(lv2_lung_SEX$sum_LUNG_BZ[ind.val]-lv2_lung_SEX$poisson.reg[ind.val]))),2),
                            round(sqrt(mean(abs(lv2_lung_SEX$sum_LUNG_BZ[ind.val]-lv2_lung_SEX$linear.spline[ind.val]))),2),
                            round(sqrt(mean(abs(lv2_lung_SEX$sum_LUNG_BZ[ind.val]-lv2_lung_SEX$quadratic.reg[ind.val]))),2),
                            round(sqrt(mean(abs(lv2_lung_SEX$sum_FY_BZ[ind.val]-lv2_lung_SEX$simple.reg.FY[ind.val]))),2),
                            round(sqrt(mean(abs(lv2_lung_SEX$sum_FY_BZ[ind.val]-lv2_lung_SEX$poisson.reg.FY[ind.val]))),2),
                            round(sqrt(mean(abs(lv2_lung_SEX$sum_FY_BZ[ind.val]-lv2_lung_SEX$linear.spline.FY[ind.val]))),2),
                            round(sqrt(mean(abs(lv2_lung_SEX$sum_FY_BZ[ind.val]-lv2_lung_SEX$quadratic.reg.FY[ind.val]))),2))
  
  
}

ggplot()+geom_line(data=lv2_lung_SEX,aes(x=YEAR,y=sum_LUNG_BZ,color="red"),size=1)+labs(y="#of_lung")+
  facet_wrap(~ factor(NAME,levels=unique(lv2_lung_SEX$NAME)),scales="free_y")+
  geom_line(data=lv2_lung_SEX,aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Lung Data comparing methods performance")+
  geom_line(data=lv2_lung_SEX,aes(x=YEAR,y=poisson.reg,color="green"),size=1)+
  geom_line(data=lv2_lung_SEX,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv2_lung_SEX,aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung[20:57,]),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung[20:57,]),mapping = aes(x = 2010, y = Inf, label = poisson_reg),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung[20:57,]),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung[20:57,]),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of lung',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=lv2_lung_SEX,aes(x=YEAR,y=sum_FY_BZ,color="red"),size=1)+labs(y="#of_FY_BZ")+
  facet_wrap(~ factor(NAME,levels=unique(lv2_lung_SEX$NAME)),scales="free_y")+
  geom_line(data=lv2_lung_SEX,aes(x=YEAR,y=simple.reg.FY,color="black"),size=1)+ggtitle("Lung Data comparing methods performance")+
  geom_line(data=lv2_lung_SEX,aes(x=YEAR,y=poisson.reg.FY,color="green"),size=1)+
  geom_line(data=lv2_lung_SEX,aes(x=YEAR,y=linear.spline.FY,color='darkviolet'),size=1)+
  geom_line(data=lv2_lung_SEX,aes(x=YEAR,y=quadratic.reg.FY,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung[20:57,]),mapping = aes(x = 2010, y = Inf, label = simple_reg.FY),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung[20:57,]),mapping = aes(x = 2010, y = Inf, label = poisson_reg.FY),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung[20:57,]),mapping = aes(x = 2002, y = Inf, label = linear_spline.FY),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung[20:57,]),mapping = aes(x = 2002, y = Inf, label = quadratic_reg.FY),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of FY_BZ',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of FY_BZ","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


################################################################################################
############### 3. grouping by UP1 and CAL2 ######################### 
lv2_lung_CAL2<-df_lung1%>%group_by(UP1,CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME=paste0("X",UP1,"_",CAL2))%>%as_tibble()
name_level<-unique(lv2_lung_CAL2$NAME)
lv2_lung_CAL2$simple.reg<-0
lv2_lung_CAL2$poisson.reg<-0
lv2_lung_CAL2$linear.spline<-0
lv2_lung_CAL2$quadratic.reg<-0
lv2_lung_CAL2$simple.reg.FY<-0
lv2_lung_CAL2$poisson.reg.FY<-0
lv2_lung_CAL2$linear.spline.FY<-0
lv2_lung_CAL2$quadratic.reg.FY<-0
for(i in 1:length(name_level)){
  ind.tr<-which(lv2_lung_CAL2$YEAR<=2014 & lv2_lung_CAL2$NAME==name_level[i])
  ind.val<-which(lv2_lung_CAL2$YEAR>2014 & lv2_lung_CAL2$NAME==name_level[i])
  
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
  
  MAPE_lv2_lung[(i+57),]<-c(unique(lv2_lung_CAL2$NAME[ind.tr]),
                            round(mean(abs(lv2_lung_CAL2$sum_LUNG_BZ[ind.val]-lv2_lung_CAL2$simple.reg[ind.val])/lv2_lung_CAL2$sum_LUNG_BZ[ind.val])*100,2),
                            round(mean(abs(lv2_lung_CAL2$sum_LUNG_BZ[ind.val]-lv2_lung_CAL2$poisson.reg[ind.val])/lv2_lung_CAL2$sum_LUNG_BZ[ind.val])*100,2),
                            round(mean(abs(lv2_lung_CAL2$sum_LUNG_BZ[ind.val]-lv2_lung_CAL2$linear.spline[ind.val])/lv2_lung_CAL2$sum_LUNG_BZ[ind.val])*100,2),
                            round(mean(abs(lv2_lung_CAL2$sum_LUNG_BZ[ind.val]-lv2_lung_CAL2$quadratic.reg[ind.val])/lv2_lung_CAL2$sum_LUNG_BZ[ind.val])*100,2),
                            round(mean(abs(lv2_lung_CAL2$sum_FY_BZ[ind.val]-lv2_lung_CAL2$simple.reg.FY[ind.val])/lv2_lung_CAL2$sum_FY_BZ[ind.val])*100,2),
                            round(mean(abs(lv2_lung_CAL2$sum_FY_BZ[ind.val]-lv2_lung_CAL2$poisson.reg.FY[ind.val])/lv2_lung_CAL2$sum_FY_BZ[ind.val])*100,2),
                            round(mean(abs(lv2_lung_CAL2$sum_FY_BZ[ind.val]-lv2_lung_CAL2$linear.spline.FY[ind.val])/lv2_lung_CAL2$sum_FY_BZ[ind.val])*100,2),
                            round(mean(abs(lv2_lung_CAL2$sum_FY_BZ[ind.val]-lv2_lung_CAL2$quadratic.reg.FY[ind.val])/lv2_lung_CAL2$sum_FY_BZ[ind.val])*100,2))
  
  RMSE_lv2_lung[(i+57),]<-c(unique(lv2_lung_CAL2$NAME[ind.tr]),
                            round(sqrt(mean(abs(lv2_lung_CAL2$sum_LUNG_BZ[ind.val]-lv2_lung_CAL2$simple.reg[ind.val]))),2),
                            round(sqrt(mean(abs(lv2_lung_CAL2$sum_LUNG_BZ[ind.val]-lv2_lung_CAL2$poisson.reg[ind.val]))),2),
                            round(sqrt(mean(abs(lv2_lung_CAL2$sum_LUNG_BZ[ind.val]-lv2_lung_CAL2$linear.spline[ind.val]))),2),
                            round(sqrt(mean(abs(lv2_lung_CAL2$sum_LUNG_BZ[ind.val]-lv2_lung_CAL2$quadratic.reg[ind.val]))),2),
                            round(sqrt(mean(abs(lv2_lung_CAL2$sum_FY_BZ[ind.val]-lv2_lung_CAL2$simple.reg.FY[ind.val]))),2),
                            round(sqrt(mean(abs(lv2_lung_CAL2$sum_FY_BZ[ind.val]-lv2_lung_CAL2$poisson.reg.FY[ind.val]))),2),
                            round(sqrt(mean(abs(lv2_lung_CAL2$sum_FY_BZ[ind.val]-lv2_lung_CAL2$linear.spline.FY[ind.val]))),2),
                            round(sqrt(mean(abs(lv2_lung_CAL2$sum_FY_BZ[ind.val]-lv2_lung_CAL2$quadratic.reg.FY[ind.val]))),2))
  
  
}

ggplot()+geom_line(data=lv2_lung_CAL2,aes(x=YEAR,y=sum_LUNG_BZ,color="red"),size=1)+labs(y="#of_lung")+
  facet_wrap(~ factor(NAME,levels=name_level),scales="free_y")+
  geom_line(data=lv2_lung_CAL2,aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Lung Data comparing methods performance")+
  geom_line(data=lv2_lung_CAL2,aes(x=YEAR,y=poisson.reg,color="green"),size=1)+
  geom_line(data=lv2_lung_CAL2,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv2_lung_CAL2,aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung[58:95,]),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung[58:95,]),mapping = aes(x = 2010, y = Inf, label = poisson_reg),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung[58:95,]),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung[58:95,]),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of lung',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=lv2_lung_CAL2,aes(x=YEAR,y=sum_FY_BZ,color="red"),size=1)+labs(y="#of_FY_BZ")+
  facet_wrap(~ factor(NAME,levels=name_level),scales="free_y")+
  geom_line(data=lv2_lung_CAL2,aes(x=YEAR,y=simple.reg.FY,color="black"),size=1)+ggtitle("Lung Data comparing methods performance")+
  geom_line(data=lv2_lung_CAL2,aes(x=YEAR,y=poisson.reg.FY,color="green"),size=1)+
  geom_line(data=lv2_lung_CAL2,aes(x=YEAR,y=linear.spline.FY,color='darkviolet'),size=1)+
  geom_line(data=lv2_lung_CAL2,aes(x=YEAR,y=quadratic.reg.FY,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung[58:95,]),mapping = aes(x = 2010, y = Inf, label = simple_reg.FY),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung[58:95,]),mapping = aes(x = 2010, y = Inf, label = poisson_reg.FY),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung[58:95,]),mapping = aes(x = 2002, y = Inf, label = linear_spline.FY),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung[58:95,]),mapping = aes(x = 2002, y = Inf, label = quadratic_reg.FY),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of FY BZ',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


################################################################################################
############### 3. grouping by UP1 and SEX and CAL2 ######################### 
lv2_lung_SEX_CAL2<-df_lung1%>%group_by(UP1,SEX,CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME=paste0("X",UP1,"_",SEX,CAL2))%>%as_tibble()

name_level<-unique(lv2_lung_SEX_CAL2$NAME)

lv2_lung_SEX_CAL2$simple.reg<-0
lv2_lung_SEX_CAL2$poisson.reg<-0
lv2_lung_SEX_CAL2$linear.spline<-0
lv2_lung_SEX_CAL2$quadratic.reg<-0

lv2_lung_SEX_CAL2$simple.reg.FY<-0
lv2_lung_SEX_CAL2$poisson.reg.FY<-0
lv2_lung_SEX_CAL2$linear.spline.FY<-0
lv2_lung_SEX_CAL2$quadratic.reg.FY<-0

for(i in 1:length(name_level)){
  ind.tr<-which(lv2_lung_SEX_CAL2$YEAR<=2014 & lv2_lung_SEX_CAL2$NAME==name_level[i])
  ind.val<-which(lv2_lung_SEX_CAL2$YEAR>2014 & lv2_lung_SEX_CAL2$NAME==name_level[i])
  
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
  
  MAPE_lv2_lung[(i+95),]<-c(unique(lv2_lung_SEX_CAL2$NAME[ind.tr]),
                            round(mean(abs(lv2_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv2_lung_SEX_CAL2$simple.reg[ind.val])/lv2_lung_SEX_CAL2$sum_LUNG_BZ[ind.val])*100,2),
                            round(mean(abs(lv2_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv2_lung_SEX_CAL2$poisson.reg[ind.val])/lv2_lung_SEX_CAL2$sum_LUNG_BZ[ind.val])*100,2),
                            round(mean(abs(lv2_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv2_lung_SEX_CAL2$linear.spline[ind.val])/lv2_lung_SEX_CAL2$sum_LUNG_BZ[ind.val])*100,2),
                            round(mean(abs(lv2_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv2_lung_SEX_CAL2$quadratic.reg[ind.val])/lv2_lung_SEX_CAL2$sum_LUNG_BZ[ind.val])*100,2),
                            round(mean(abs(lv2_lung_SEX_CAL2$sum_FY_BZ[ind.val]-lv2_lung_SEX_CAL2$simple.reg.FY[ind.val])/lv2_lung_SEX_CAL2$sum_FY_BZ[ind.val])*100,2),
                            round(mean(abs(lv2_lung_SEX_CAL2$sum_FY_BZ[ind.val]-lv2_lung_SEX_CAL2$poisson.reg.FY[ind.val])/lv2_lung_SEX_CAL2$sum_FY_BZ[ind.val])*100,2),
                            round(mean(abs(lv2_lung_SEX_CAL2$sum_FY_BZ[ind.val]-lv2_lung_SEX_CAL2$linear.spline.FY[ind.val])/lv2_lung_SEX_CAL2$sum_FY_BZ[ind.val])*100,2),
                            round(mean(abs(lv2_lung_SEX_CAL2$sum_FY_BZ[ind.val]-lv2_lung_SEX_CAL2$quadratic.reg.FY[ind.val])/lv2_lung_SEX_CAL2$sum_FY_BZ[ind.val])*100,2))
  
  RMSE_lv2_lung[(i+95),]<-c(unique(lv2_lung_SEX_CAL2$NAME[ind.tr]),
                            round(sqrt(mean(abs(lv2_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv2_lung_SEX_CAL2$simple.reg[ind.val]))),2),
                            round(sqrt(mean(abs(lv2_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv2_lung_SEX_CAL2$poisson.reg[ind.val]))),2),
                            round(sqrt(mean(abs(lv2_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv2_lung_SEX_CAL2$linear.spline[ind.val]))),2),
                            round(sqrt(mean(abs(lv2_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv2_lung_SEX_CAL2$quadratic.reg[ind.val]))),2),
                            round(sqrt(mean(abs(lv2_lung_SEX_CAL2$sum_FY_BZ[ind.val]-lv2_lung_SEX_CAL2$simple.reg.FY[ind.val]))),2),
                            round(sqrt(mean(abs(lv2_lung_SEX_CAL2$sum_FY_BZ[ind.val]-lv2_lung_SEX_CAL2$poisson.reg.FY[ind.val]))),2),
                            round(sqrt(mean(abs(lv2_lung_SEX_CAL2$sum_FY_BZ[ind.val]-lv2_lung_SEX_CAL2$linear.spline.FY[ind.val]))),2),
                            round(sqrt(mean(abs(lv2_lung_SEX_CAL2$sum_FY_BZ[ind.val]-lv2_lung_SEX_CAL2$quadratic.reg.FY[ind.val]))),2))
  
  
}

ggplot()+geom_line(data=lv2_lung_SEX_CAL2,aes(x=YEAR,y=sum_LUNG_BZ,color="red"),size=1)+labs(y="#of_lung")+
  facet_wrap(~ factor(NAME,levels=name_level),scales="free_y")+
  geom_line(data=lv2_lung_SEX_CAL2,aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Lung Data comparing methods performance")+
  geom_line(data=lv2_lung_SEX_CAL2,aes(x=YEAR,y=poisson.reg,color="green"),size=1)+
  geom_line(data=lv2_lung_SEX_CAL2,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv2_lung_SEX_CAL2,aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung[96:171,]),mapping = aes(x = 2011, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung[96:171,]),mapping = aes(x = 2011, y = Inf, label = poisson_reg),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung[96:171,]),mapping = aes(x = 2003, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung[96:171,]),mapping = aes(x = 2003, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of lung',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=lv2_lung_SEX_CAL2,aes(x=YEAR,y=sum_FY_BZ,color="red"),size=1)+labs(y="#of FY_BZ")+
  facet_wrap(~ factor(NAME,levels=name_level),scales="free_y")+
  geom_line(data=lv2_lung_SEX_CAL2,aes(x=YEAR,y=simple.reg.FY,color="black"),size=1)+ggtitle("Lung Data comparing methods performance")+
  geom_line(data=lv2_lung_SEX_CAL2,aes(x=YEAR,y=poisson.reg.FY,color="green"),size=1)+
  geom_line(data=lv2_lung_SEX_CAL2,aes(x=YEAR,y=linear.spline.FY,color='darkviolet'),size=1)+
  geom_line(data=lv2_lung_SEX_CAL2,aes(x=YEAR,y=quadratic.reg.FY,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung[96:171,]),mapping = aes(x = 2012, y = Inf, label = simple_reg.FY),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung[96:171,]),mapping = aes(x = 2012, y = Inf, label = poisson_reg.FY),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung[96:171,]),mapping = aes(x = 2003, y = Inf, label = linear_spline.FY),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv2_lung[96:171,]),mapping = aes(x = 2003, y = Inf, label = quadratic_reg.FY),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of FY_BZ',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of FY_BZ","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))
value_eklee_lv2<-as.data.frame(rbind(lv2_lung_total[2:length(lv2_lung_total)],lv2_lung_SEX[3:length(lv2_lung_SEX)],lv2_lung_CAL2[3:length(lv2_lung_CAL2)],
                                     lv2_lung_SEX_CAL2[4:length(lv2_lung_SEX_CAL2)]))
val_lung_lv2<-value_eklee_lv2%>%as.data.frame()


#######################################################################################################3
##################### level 3 data ##########################
############# 1. grouping by UP2 #################
lv3_lung_total<-df_lung1%>%group_by(UP1,UP2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME=paste0("X",UP1,UP2))%>%as_tibble()

MAPE_lv3_lung<-matrix(0,nrow=length(unique(lv3_lung_total$NAME))*(1+2+2+4),ncol=9)
colnames(MAPE_lv3_lung)<-c("NAME","simple_reg","poisson_reg","linear_spline","quadratic_reg",
                           "simple_reg.FY","poisson_reg.FY","linear_spline.FY","quadratic_reg.FY")

RMSE_lv3_lung<-matrix(0,nrow=length(unique(lv3_lung_total$NAME))*(1+2+2+4),ncol=9)
colnames(RMSE_lv3_lung)<-c("NAME","simple_reg","poisson_reg","linear_spline","quadratic_reg",
                           "simple_reg.FY","poisson_reg.FY","linear_spline.FY","quadratic_reg.FY")
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
  ind.tr<-which(lv3_lung_total$YEAR<=2014 & lv3_lung_total$NAME==name_level[i])
  ind.val<-which(lv3_lung_total$YEAR>2014 & lv3_lung_total$NAME==name_level[i])
  
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
  
  
  MAPE_lv3_lung[i,]<-c(unique(lv3_lung_total$NAME[ind.tr]),
                       round(mean(abs(lv3_lung_total$sum_LUNG_BZ[ind.val]-lv3_lung_total$simple.reg[ind.val])/lv3_lung_total$sum_LUNG_BZ[ind.val])*100,2),
                       round(mean(abs(lv3_lung_total$sum_LUNG_BZ[ind.val]-lv3_lung_total$poisson.reg[ind.val])/lv3_lung_total$sum_LUNG_BZ[ind.val])*100,2),
                       round(mean(abs(lv3_lung_total$sum_LUNG_BZ[ind.val]-lv3_lung_total$linear.spline[ind.val])/lv3_lung_total$sum_LUNG_BZ[ind.val])*100,2),
                       round(mean(abs(lv3_lung_total$sum_LUNG_BZ[ind.val]-lv3_lung_total$quadratic.reg[ind.val])/lv3_lung_total$sum_LUNG_BZ[ind.val])*100,2),
                       round(mean(abs(lv3_lung_total$sum_FY_BZ[ind.val]-lv3_lung_total$simple.reg.FY[ind.val])/lv3_lung_total$sum_FY_BZ[ind.val])*100,2),
                       round(mean(abs(lv3_lung_total$sum_FY_BZ[ind.val]-lv3_lung_total$poisson.reg.FY[ind.val])/lv3_lung_total$sum_FY_BZ[ind.val])*100,2),
                       round(mean(abs(lv3_lung_total$sum_FY_BZ[ind.val]-lv3_lung_total$linear.spline.FY[ind.val])/lv3_lung_total$sum_FY_BZ[ind.val])*100,2),
                       round(mean(abs(lv3_lung_total$sum_FY_BZ[ind.val]-lv3_lung_total$quadratic.reg.FY[ind.val])/lv3_lung_total$sum_FY_BZ[ind.val])*100,2))
  
  RMSE_lv3_lung[i,]<-c(unique(lv3_lung_total$NAME[ind.tr]),
                       round(sqrt(mean(abs(lv3_lung_total$sum_LUNG_BZ[ind.val]-lv3_lung_total$simple.reg[ind.val]))),2),
                       round(sqrt(mean(abs(lv3_lung_total$sum_LUNG_BZ[ind.val]-lv3_lung_total$poisson.reg[ind.val]))),2),
                       round(sqrt(mean(abs(lv3_lung_total$sum_LUNG_BZ[ind.val]-lv3_lung_total$linear.spline[ind.val]))),2),
                       round(sqrt(mean(abs(lv3_lung_total$sum_LUNG_BZ[ind.val]-lv3_lung_total$quadratic.reg[ind.val]))),2),
                       round(sqrt(mean(abs(lv3_lung_total$sum_FY_BZ[ind.val]-lv3_lung_total$simple.reg.FY[ind.val]))),2),
                       round(sqrt(mean(abs(lv3_lung_total$sum_FY_BZ[ind.val]-lv3_lung_total$poisson.reg.FY[ind.val]))),2),
                       round(sqrt(mean(abs(lv3_lung_total$sum_FY_BZ[ind.val]-lv3_lung_total$linear.spline.FY[ind.val]))),2),
                       round(sqrt(mean(abs(lv3_lung_total$sum_FY_BZ[ind.val]-lv3_lung_total$quadratic.reg.FY[ind.val]))),2))
  
  
}

ggplot()+geom_line(data=lv3_lung_total,aes(x=YEAR,y=sum_LUNG_BZ,color="red"),size=1)+labs(y="#of_lung")+
  facet_wrap(~ factor(NAME,levels=unique(lv3_lung_total$NAME)),scales="free_y")+
  geom_line(data=lv3_lung_total,aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Lung Data comparing methods performance")+
  geom_line(data=lv3_lung_total,aes(x=YEAR,y=poisson.reg,color="green"),size=1)+
  geom_line(data=lv3_lung_total,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv3_lung_total,aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung),mapping = aes(x = 2010, y = Inf, label = poisson_reg),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of lung',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=lv3_lung_total,aes(x=YEAR,y=sum_FY_BZ,color="red"),size=1)+labs(y="#of_FY_BZ")+
  facet_wrap(~ factor(NAME,levels=unique(lv3_lung_total$NAME)),scales="free_y")+
  geom_line(data=lv3_lung_total,aes(x=YEAR,y=simple.reg.FY,color="black"),size=1)+ggtitle("Lung Data comparing methods performance")+
  geom_line(data=lv3_lung_total,aes(x=YEAR,y=poisson.reg.FY,color="green"),size=1)+
  geom_line(data=lv3_lung_total,aes(x=YEAR,y=linear.spline.FY,color='darkviolet'),size=1)+
  geom_line(data=lv3_lung_total,aes(x=YEAR,y=quadratic.reg.FY,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung),mapping = aes(x = 2010, y = Inf, label = simple_reg.FY),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung),mapping = aes(x = 2010, y = Inf, label = poisson_reg.FY),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung),mapping = aes(x = 2002, y = Inf, label = linear_spline.FY),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung),mapping = aes(x = 2002, y = Inf, label = quadratic_reg.FY),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of FY BZ',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of FY BZ","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


##################################################################################
############# 2. grouping by UP2 and SEX #################
lv3_lung_SEX<-df_lung1%>%group_by(UP1,UP2,SEX,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME=paste0("X",UP1,UP2,"_",SEX))%>%as_tibble()

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
  ind.tr<-which(lv3_lung_SEX$YEAR<=2014 & lv3_lung_SEX$NAME==name_level[i])
  ind.val<-which(lv3_lung_SEX$YEAR>2014 & lv3_lung_SEX$NAME==name_level[i])
  
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
  
  
  MAPE_lv3_lung[(i+19),]<-c(unique(lv3_lung_SEX$NAME[ind.tr]),
                            round(mean(abs(lv3_lung_SEX$sum_LUNG_BZ[ind.val]-lv3_lung_SEX$simple.reg[ind.val])/lv3_lung_SEX$sum_LUNG_BZ[ind.val])*100,2),
                            round(mean(abs(lv3_lung_SEX$sum_LUNG_BZ[ind.val]-lv3_lung_SEX$poisson.reg[ind.val])/lv3_lung_SEX$sum_LUNG_BZ[ind.val])*100,2),
                            round(mean(abs(lv3_lung_SEX$sum_LUNG_BZ[ind.val]-lv3_lung_SEX$linear.spline[ind.val])/lv3_lung_SEX$sum_LUNG_BZ[ind.val])*100,2),
                            round(mean(abs(lv3_lung_SEX$sum_LUNG_BZ[ind.val]-lv3_lung_SEX$quadratic.reg[ind.val])/lv3_lung_SEX$sum_LUNG_BZ[ind.val])*100,2),
                            round(mean(abs(lv3_lung_SEX$sum_FY_BZ[ind.val]-lv3_lung_SEX$simple.reg.FY[ind.val])/lv3_lung_SEX$sum_FY_BZ[ind.val])*100,2),
                            round(mean(abs(lv3_lung_SEX$sum_FY_BZ[ind.val]-lv3_lung_SEX$poisson.reg.FY[ind.val])/lv3_lung_SEX$sum_FY_BZ[ind.val])*100,2),
                            round(mean(abs(lv3_lung_SEX$sum_FY_BZ[ind.val]-lv3_lung_SEX$linear.spline.FY[ind.val])/lv3_lung_SEX$sum_FY_BZ[ind.val])*100,2),
                            round(mean(abs(lv3_lung_SEX$sum_FY_BZ[ind.val]-lv3_lung_SEX$quadratic.reg.FY[ind.val])/lv3_lung_SEX$sum_FY_BZ[ind.val])*100,2))
  
  RMSE_lv3_lung[(i+19),]<-c(unique(lv3_lung_SEX$NAME[ind.tr]),
                            round(sqrt(mean(abs(lv3_lung_SEX$sum_LUNG_BZ[ind.val]-lv3_lung_SEX$simple.reg[ind.val]))),2),
                            round(sqrt(mean(abs(lv3_lung_SEX$sum_LUNG_BZ[ind.val]-lv3_lung_SEX$poisson.reg[ind.val]))),2),
                            round(sqrt(mean(abs(lv3_lung_SEX$sum_LUNG_BZ[ind.val]-lv3_lung_SEX$linear.spline[ind.val]))),2),
                            round(sqrt(mean(abs(lv3_lung_SEX$sum_LUNG_BZ[ind.val]-lv3_lung_SEX$quadratic.reg[ind.val]))),2),
                            round(sqrt(mean(abs(lv3_lung_SEX$sum_FY_BZ[ind.val]-lv3_lung_SEX$simple.reg.FY[ind.val]))),2),
                            round(sqrt(mean(abs(lv3_lung_SEX$sum_FY_BZ[ind.val]-lv3_lung_SEX$poisson.reg.FY[ind.val]))),2),
                            round(sqrt(mean(abs(lv3_lung_SEX$sum_FY_BZ[ind.val]-lv3_lung_SEX$linear.spline.FY[ind.val]))),2),
                            round(sqrt(mean(abs(lv3_lung_SEX$sum_FY_BZ[ind.val]-lv3_lung_SEX$quadratic.reg.FY[ind.val]))),2))
  
  
}

ind.M<-grep("M",MAPE_lv3_lung[,1])
MAPE_lv3_lung_M<-MAPE_lv3_lung[ind.M,]
ind.F<-grep("F",MAPE_lv3_lung[,1])
MAPE_lv3_lung_F<-MAPE_lv3_lung[ind.F,]

ggplot()+geom_line(data=lv3_lung_SEX[lv3_lung_SEX$SEX=="F",],aes(x=YEAR,y=sum_LUNG_BZ,color="red"),size=1)+labs(y="#of lung")+
  facet_wrap(~ factor(NAME,levels=unique(lv3_lung_SEX$NAME[lv3_lung_SEX$SEX=="F"])),scales="free_y")+
  geom_line(data=lv3_lung_SEX[lv3_lung_SEX$SEX=="F",],aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Lung Data comparing methods performance")+
  geom_line(data=lv3_lung_SEX[lv3_lung_SEX$SEX=="F",],aes(x=YEAR,y=poisson.reg,color="green"),size=1)+
  geom_line(data=lv3_lung_SEX[lv3_lung_SEX$SEX=="F",],aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv3_lung_SEX[lv3_lung_SEX$SEX=="F",],aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_F),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_F),mapping = aes(x = 2010, y = Inf, label = poisson_reg),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_F),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_F),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of lung',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=lv3_lung_SEX[lv3_lung_SEX$SEX=="M",],aes(x=YEAR,y=sum_LUNG_BZ,color="red"),size=1)+labs(y="#of lung")+
  facet_wrap(~ factor(NAME,levels=unique(lv3_lung_SEX$NAME[lv3_lung_SEX$SEX=="M"])),scales="free_y")+
  geom_line(data=lv3_lung_SEX[lv3_lung_SEX$SEX=="M",],aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Lung Data comparing methods performance")+
  geom_line(data=lv3_lung_SEX[lv3_lung_SEX$SEX=="M",],aes(x=YEAR,y=poisson.reg,color="green"),size=1)+
  geom_line(data=lv3_lung_SEX[lv3_lung_SEX$SEX=="M",],aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv3_lung_SEX[lv3_lung_SEX$SEX=="M",],aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_M),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_M),mapping = aes(x = 2010, y = Inf, label = poisson_reg),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_M),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_M),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of lung',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=lv3_lung_SEX[lv3_lung_SEX$SEX=="F",],aes(x=YEAR,y=sum_FY_BZ,color="red"),size=1)+labs(y="#of FY_BZ")+
  facet_wrap(~ factor(NAME,levels=unique(lv3_lung_SEX$NAME[lv3_lung_SEX$SEX=="F"])),scales="free_y")+
  geom_line(data=lv3_lung_SEX[lv3_lung_SEX$SEX=="F",],aes(x=YEAR,y=simple.reg.FY,color="black"),size=1)+ggtitle("Lung Data comparing methods performance")+
  geom_line(data=lv3_lung_SEX[lv3_lung_SEX$SEX=="F",],aes(x=YEAR,y=poisson.reg.FY,color="green"),size=1)+
  geom_line(data=lv3_lung_SEX[lv3_lung_SEX$SEX=="F",],aes(x=YEAR,y=linear.spline.FY,color='darkviolet'),size=1)+
  geom_line(data=lv3_lung_SEX[lv3_lung_SEX$SEX=="F",],aes(x=YEAR,y=quadratic.reg.FY,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_F),mapping = aes(x = 2010, y = Inf, label = simple_reg.FY),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_F),mapping = aes(x = 2010, y = Inf, label = poisson_reg.FY),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_F),mapping = aes(x = 2002, y = Inf, label = linear_spline.FY),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_F),mapping = aes(x = 2002, y = Inf, label = quadratic_reg.FY),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of FY_BZ',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of FY BZ","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=lv3_lung_SEX[lv3_lung_SEX$SEX=="M",],aes(x=YEAR,y=sum_FY_BZ,color="red"),size=1)+labs(y="#of FY_BZ")+
  facet_wrap(~ factor(NAME,levels=unique(lv3_lung_SEX$NAME[lv3_lung_SEX$SEX=="M"])),scales="free_y")+
  geom_line(data=lv3_lung_SEX[lv3_lung_SEX$SEX=="M",],aes(x=YEAR,y=simple.reg.FY,color="black"),size=1)+ggtitle("Lung Data comparing methods performance")+
  geom_line(data=lv3_lung_SEX[lv3_lung_SEX$SEX=="M",],aes(x=YEAR,y=poisson.reg.FY,color="green"),size=1)+
  geom_line(data=lv3_lung_SEX[lv3_lung_SEX$SEX=="M",],aes(x=YEAR,y=linear.spline.FY,color='darkviolet'),size=1)+
  geom_line(data=lv3_lung_SEX[lv3_lung_SEX$SEX=="M",],aes(x=YEAR,y=quadratic.reg.FY,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_M),mapping = aes(x = 2010, y = Inf, label = simple_reg.FY),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_M),mapping = aes(x = 2010, y = Inf, label = poisson_reg.FY),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_M),mapping = aes(x = 2002, y = Inf, label = linear_spline.FY),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_M),mapping = aes(x = 2002, y = Inf, label = quadratic_reg.FY),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of FY_BZ',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of FY BZ","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


################################################################################################
############### 3. grouping by UP2 and CAL2 ######################### 
lv3_lung_CAL2<-df_lung1%>%group_by(UP1,UP2,CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME=paste0("X",UP1,UP2,"_",CAL2))%>%as_tibble()
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
  ind.tr<-which(lv3_lung_CAL2$YEAR<=2014 & lv3_lung_CAL2$NAME==name_level[i])
  ind.val<-which(lv3_lung_CAL2$YEAR>2014 & lv3_lung_CAL2$NAME==name_level[i])
  
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
  
  MAPE_lv3_lung[(i+57),]<-c(unique(lv3_lung_CAL2$NAME[ind.tr]),
                            round(mean(abs(lv3_lung_CAL2$sum_LUNG_BZ[ind.val]-lv3_lung_CAL2$simple.reg[ind.val])/lv3_lung_CAL2$sum_LUNG_BZ[ind.val])*100,2),
                            round(mean(abs(lv3_lung_CAL2$sum_LUNG_BZ[ind.val]-lv3_lung_CAL2$poisson.reg[ind.val])/lv3_lung_CAL2$sum_LUNG_BZ[ind.val])*100,2),
                            round(mean(abs(lv3_lung_CAL2$sum_LUNG_BZ[ind.val]-lv3_lung_CAL2$linear.spline[ind.val])/lv3_lung_CAL2$sum_LUNG_BZ[ind.val])*100,2),
                            round(mean(abs(lv3_lung_CAL2$sum_LUNG_BZ[ind.val]-lv3_lung_CAL2$quadratic.reg[ind.val])/lv3_lung_CAL2$sum_LUNG_BZ[ind.val])*100,2),
                            round(mean(abs(lv3_lung_CAL2$sum_FY_BZ[ind.val]-lv3_lung_CAL2$simple.reg.FY[ind.val])/lv3_lung_CAL2$sum_FY_BZ[ind.val])*100,2),
                            round(mean(abs(lv3_lung_CAL2$sum_FY_BZ[ind.val]-lv3_lung_CAL2$poisson.reg.FY[ind.val])/lv3_lung_CAL2$sum_FY_BZ[ind.val])*100,2),
                            round(mean(abs(lv3_lung_CAL2$sum_FY_BZ[ind.val]-lv3_lung_CAL2$linear.spline.FY[ind.val])/lv3_lung_CAL2$sum_FY_BZ[ind.val])*100,2),
                            round(mean(abs(lv3_lung_CAL2$sum_FY_BZ[ind.val]-lv3_lung_CAL2$quadratic.reg.FY[ind.val])/lv3_lung_CAL2$sum_FY_BZ[ind.val])*100,2))
  
  RMSE_lv3_lung[(i+57),]<-c(unique(lv3_lung_CAL2$NAME[ind.tr]),
                            round(sqrt(mean(abs(lv3_lung_CAL2$sum_LUNG_BZ[ind.val]-lv3_lung_CAL2$simple.reg[ind.val]))),2),
                            round(sqrt(mean(abs(lv3_lung_CAL2$sum_LUNG_BZ[ind.val]-lv3_lung_CAL2$poisson.reg[ind.val]))),2),
                            round(sqrt(mean(abs(lv3_lung_CAL2$sum_LUNG_BZ[ind.val]-lv3_lung_CAL2$linear.spline[ind.val]))),2),
                            round(sqrt(mean(abs(lv3_lung_CAL2$sum_LUNG_BZ[ind.val]-lv3_lung_CAL2$quadratic.reg[ind.val]))),2),
                            round(sqrt(mean(abs(lv3_lung_CAL2$sum_FY_BZ[ind.val]-lv3_lung_CAL2$simple.reg.FY[ind.val]))),2),
                            round(sqrt(mean(abs(lv3_lung_CAL2$sum_FY_BZ[ind.val]-lv3_lung_CAL2$poisson.reg.FY[ind.val]))),2),
                            round(sqrt(mean(abs(lv3_lung_CAL2$sum_FY_BZ[ind.val]-lv3_lung_CAL2$linear.spline.FY[ind.val]))),2),
                            round(sqrt(mean(abs(lv3_lung_CAL2$sum_FY_BZ[ind.val]-lv3_lung_CAL2$quadratic.reg.FY[ind.val]))),2))
  
  
}


ind.01<-grep("01",MAPE_lv3_lung[,1])
MAPE_lv3_lung_01<-MAPE_lv3_lung[ind.01,]
ind.234<-grep("234",MAPE_lv3_lung[,1])
MAPE_lv3_lung_234<-MAPE_lv3_lung[ind.234,]

ggplot()+geom_line(data=lv3_lung_CAL2[lv3_lung_CAL2$CAL2=="01",],aes(x=YEAR,y=sum_LUNG_BZ,color="red"),size=1)+labs(y="#of lung")+
  facet_wrap(~ factor(NAME,levels=unique(lv3_lung_CAL2$NAME[lv3_lung_CAL2$CAL2=="01"])),scales="free_y")+
  geom_line(data=lv3_lung_CAL2[lv3_lung_CAL2$CAL2=="01",],aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Lung Data comparing methods performance")+
  geom_line(data=lv3_lung_CAL2[lv3_lung_CAL2$CAL2=="01",],aes(x=YEAR,y=poisson.reg,color="green"),size=1)+
  geom_line(data=lv3_lung_CAL2[lv3_lung_CAL2$CAL2=="01",],aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv3_lung_CAL2[lv3_lung_CAL2$CAL2=="01",],aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_01),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_01),mapping = aes(x = 2010, y = Inf, label = poisson_reg),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_01),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_01),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of lung',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=lv3_lung_CAL2[lv3_lung_CAL2$CAL2=="234",],aes(x=YEAR,y=sum_LUNG_BZ,color="red"),size=1)+labs(y="#of lung")+
  facet_wrap(~ factor(NAME,levels=unique(lv3_lung_CAL2$NAME[lv3_lung_CAL2$CAL2=="234"])),scales="free_y")+
  geom_line(data=lv3_lung_CAL2[lv3_lung_CAL2$CAL2=="234",],aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Lung Data comparing methods performance")+
  geom_line(data=lv3_lung_CAL2[lv3_lung_CAL2$CAL2=="234",],aes(x=YEAR,y=poisson.reg,color="green"),size=1)+
  geom_line(data=lv3_lung_CAL2[lv3_lung_CAL2$CAL2=="234",],aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv3_lung_CAL2[lv3_lung_CAL2$CAL2=="234",],aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_234),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_234),mapping = aes(x = 2010, y = Inf, label = poisson_reg),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_234),mapping = aes(x = 2004, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_234),mapping = aes(x = 2004, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of lung',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=lv3_lung_CAL2[lv3_lung_CAL2$CAL2=="01",],aes(x=YEAR,y=sum_FY_BZ,color="red"),size=1)+labs(y="#of FY BZ")+
  facet_wrap(~ factor(NAME,levels=unique(lv3_lung_CAL2$NAME[lv3_lung_CAL2$CAL2=="01"])),scales="free_y")+
  geom_line(data=lv3_lung_CAL2[lv3_lung_CAL2$CAL2=="01",],aes(x=YEAR,y=simple.reg.FY,color="black"),size=1)+ggtitle("Lung Data comparing methods performance")+
  geom_line(data=lv3_lung_CAL2[lv3_lung_CAL2$CAL2=="01",],aes(x=YEAR,y=poisson.reg.FY,color="green"),size=1)+
  geom_line(data=lv3_lung_CAL2[lv3_lung_CAL2$CAL2=="01",],aes(x=YEAR,y=linear.spline.FY,color='darkviolet'),size=1)+
  geom_line(data=lv3_lung_CAL2[lv3_lung_CAL2$CAL2=="01",],aes(x=YEAR,y=quadratic.reg.FY,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_01),mapping = aes(x = 2010, y = Inf, label = simple_reg.FY),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_01),mapping = aes(x = 2010, y = Inf, label = poisson_reg.FY),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_01),mapping = aes(x = 2002, y = Inf, label = linear_spline.FY),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_01),mapping = aes(x = 2002, y = Inf, label = quadratic_reg.FY),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of FY BZ',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of FY BZ","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=lv3_lung_CAL2[lv3_lung_CAL2$CAL2=="234",],aes(x=YEAR,y=sum_FY_BZ,color="red"),size=1)+labs(y="#of FY BZ")+
  facet_wrap(~ factor(NAME,levels=unique(lv3_lung_CAL2$NAME[lv3_lung_CAL2$CAL2=="234"])),scales="free_y")+
  geom_line(data=lv3_lung_CAL2[lv3_lung_CAL2$CAL2=="234",],aes(x=YEAR,y=simple.reg.FY,color="black"),size=1)+ggtitle("Lung Data comparing methods performance")+
  geom_line(data=lv3_lung_CAL2[lv3_lung_CAL2$CAL2=="234",],aes(x=YEAR,y=poisson.reg.FY,color="green"),size=1)+
  geom_line(data=lv3_lung_CAL2[lv3_lung_CAL2$CAL2=="234",],aes(x=YEAR,y=linear.spline.FY,color='darkviolet'),size=1)+
  geom_line(data=lv3_lung_CAL2[lv3_lung_CAL2$CAL2=="234",],aes(x=YEAR,y=quadratic.reg.FY,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_234),mapping = aes(x = 2010, y = Inf, label = simple_reg.FY),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_234),mapping = aes(x = 2010, y = Inf, label = poisson_reg.FY),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_234),mapping = aes(x = 2003, y = Inf, label = linear_spline.FY),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_234),mapping = aes(x = 2003, y = Inf, label = quadratic_reg.FY),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of FY BZ',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


################################################################################################
############### 3. grouping by UP2 and SEX and CAL2 ######################### 
lv3_lung_SEX_CAL2<-df_lung1%>%group_by(UP1,UP2,SEX,CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME=paste0("X",UP1,UP2,"_",SEX,CAL2))%>%as_tibble()

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
  ind.tr<-which(lv3_lung_SEX_CAL2$YEAR<=2014 & lv3_lung_SEX_CAL2$NAME==name_level[i])
  ind.val<-which(lv3_lung_SEX_CAL2$YEAR>2014 & lv3_lung_SEX_CAL2$NAME==name_level[i])
  
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
  
  MAPE_lv3_lung[(i+95),]<-c(unique(lv3_lung_SEX_CAL2$NAME[ind.tr]),
                            round(mean(abs(lv3_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv3_lung_SEX_CAL2$simple.reg[ind.val])/lv3_lung_SEX_CAL2$sum_LUNG_BZ[ind.val])*100,2),
                            round(mean(abs(lv3_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv3_lung_SEX_CAL2$poisson.reg[ind.val])/lv3_lung_SEX_CAL2$sum_LUNG_BZ[ind.val])*100,2),
                            round(mean(abs(lv3_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv3_lung_SEX_CAL2$linear.spline[ind.val])/lv3_lung_SEX_CAL2$sum_LUNG_BZ[ind.val])*100,2),
                            round(mean(abs(lv3_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv3_lung_SEX_CAL2$quadratic.reg[ind.val])/lv3_lung_SEX_CAL2$sum_LUNG_BZ[ind.val])*100,2),
                            round(mean(abs(lv3_lung_SEX_CAL2$sum_FY_BZ[ind.val]-lv3_lung_SEX_CAL2$simple.reg.FY[ind.val])/lv3_lung_SEX_CAL2$sum_FY_BZ[ind.val])*100,2),
                            round(mean(abs(lv3_lung_SEX_CAL2$sum_FY_BZ[ind.val]-lv3_lung_SEX_CAL2$poisson.reg.FY[ind.val])/lv3_lung_SEX_CAL2$sum_FY_BZ[ind.val])*100,2),
                            round(mean(abs(lv3_lung_SEX_CAL2$sum_FY_BZ[ind.val]-lv3_lung_SEX_CAL2$linear.spline.FY[ind.val])/lv3_lung_SEX_CAL2$sum_FY_BZ[ind.val])*100,2),
                            round(mean(abs(lv3_lung_SEX_CAL2$sum_FY_BZ[ind.val]-lv3_lung_SEX_CAL2$quadratic.reg.FY[ind.val])/lv3_lung_SEX_CAL2$sum_FY_BZ[ind.val])*100,2))
  
  RMSE_lv3_lung[(i+95),]<-c(unique(lv3_lung_SEX_CAL2$NAME[ind.tr]),
                            round(sqrt(mean(abs(lv3_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv3_lung_SEX_CAL2$simple.reg[ind.val]))),2),
                            round(sqrt(mean(abs(lv3_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv3_lung_SEX_CAL2$poisson.reg[ind.val]))),2),
                            round(sqrt(mean(abs(lv3_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv3_lung_SEX_CAL2$linear.spline[ind.val]))),2),
                            round(sqrt(mean(abs(lv3_lung_SEX_CAL2$sum_LUNG_BZ[ind.val]-lv3_lung_SEX_CAL2$quadratic.reg[ind.val]))),2),
                            round(sqrt(mean(abs(lv3_lung_SEX_CAL2$sum_FY_BZ[ind.val]-lv3_lung_SEX_CAL2$simple.reg.FY[ind.val]))),2),
                            round(sqrt(mean(abs(lv3_lung_SEX_CAL2$sum_FY_BZ[ind.val]-lv3_lung_SEX_CAL2$poisson.reg.FY[ind.val]))),2),
                            round(sqrt(mean(abs(lv3_lung_SEX_CAL2$sum_FY_BZ[ind.val]-lv3_lung_SEX_CAL2$linear.spline.FY[ind.val]))),2),
                            round(sqrt(mean(abs(lv3_lung_SEX_CAL2$sum_FY_BZ[ind.val]-lv3_lung_SEX_CAL2$quadratic.reg.FY[ind.val]))),2))
  
  
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
ggplot()+geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="M" & lv3_lung_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=sum_LUNG_BZ,color="red"),size=1)+labs(y="#of_lung")+
  facet_wrap(~ factor(NAME,levels=unique(MAPE_lv3_lung_M_01[,1])),scales="free_y")+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="M" & lv3_lung_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Lung Data comparing methods performance")+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="M" & lv3_lung_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=poisson.reg,color="green"),size=1)+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="M" & lv3_lung_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="M" & lv3_lung_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_M_01),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_M_01),mapping = aes(x = 2010, y = Inf, label = poisson_reg),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_M_01),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_M_01),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of lung',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

########## M & 234 ############
ggplot()+geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="M" & lv3_lung_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=sum_LUNG_BZ,color="red"),size=1)+labs(y="#of_lung")+
  facet_wrap(~ factor(NAME,levels=unique(MAPE_lv3_lung_M_234[,1])),scales="free_y")+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="M" & lv3_lung_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Lung Data comparing methods performance")+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="M" & lv3_lung_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=poisson.reg,color="green"),size=1)+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="M" & lv3_lung_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="M" & lv3_lung_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_M_234),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_M_234),mapping = aes(x = 2010, y = Inf, label = poisson_reg),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_M_234),mapping = aes(x = 2004, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_M_234),mapping = aes(x = 2004, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of lung',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

########## F & 01 ############
ggplot()+geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="F" & lv3_lung_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=sum_LUNG_BZ,color="red"),size=1)+labs(y="#of_lung")+
  facet_wrap(~ factor(NAME,levels=unique(MAPE_lv3_lung_F_01[,1])),scales="free_y")+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="F" & lv3_lung_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Lung Data comparing methods performance")+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="F" & lv3_lung_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=poisson.reg,color="green"),size=1)+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="F" & lv3_lung_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="F" & lv3_lung_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_F_01),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_F_01),mapping = aes(x = 2010, y = Inf, label = poisson_reg),vjust=1.5,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_F_01),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_F_01),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of lung',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

########## F & 234 ############
ggplot()+geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="F" & lv3_lung_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=sum_LUNG_BZ,color="red"),size=1)+labs(y="#of_lung")+
  facet_wrap(~ factor(NAME,levels=unique(MAPE_lv3_lung_F_234[,1])),scales="free_y")+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="F" & lv3_lung_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=simple.reg,color="black"),size=1)+ggtitle("Lung Data comparing methods performance")+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="F" & lv3_lung_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=poisson.reg,color="green"),size=1)+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="F" & lv3_lung_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=linear.spline,color='darkviolet'),size=1)+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="F" & lv3_lung_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=quadratic.reg,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_F_234),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_F_234),mapping = aes(x = 2010, y = Inf, label = poisson_reg),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_F_234),mapping = aes(x = 2004, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_F_234),mapping = aes(x = 2004, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of lung',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

############## FY BZ ###############
########## M & 01 ############
ggplot()+geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="M" & lv3_lung_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=sum_FY_BZ,color="red"),size=1)+labs(y="#of FY BZ")+
  facet_wrap(~ factor(NAME,levels=unique(MAPE_lv3_lung_M_01[,1])),scales="free_y")+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="M" & lv3_lung_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=simple.reg.FY,color="black"),size=1)+ggtitle("Lung Data comparing methods performance")+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="M" & lv3_lung_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=poisson.reg.FY,color="green"),size=1)+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="M" & lv3_lung_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=linear.spline.FY,color='darkviolet'),size=1)+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="M" & lv3_lung_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=quadratic.reg.FY,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_M_01),mapping = aes(x = 2010, y = Inf, label = simple_reg.FY),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_M_01),mapping = aes(x = 2010, y = Inf, label = poisson_reg.FY),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_M_01),mapping = aes(x = 2002, y = Inf, label = linear_spline.FY),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_M_01),mapping = aes(x = 2002, y = Inf, label = quadratic_reg.FY),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of FY BZ',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))
########## M & 234 ############
ggplot()+geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="M" & lv3_lung_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=sum_FY_BZ,color="red"),size=1)+labs(y="#of FY BZ")+
  facet_wrap(~ factor(NAME,levels=unique(MAPE_lv3_lung_M_234[,1])),scales="free_y")+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="M" & lv3_lung_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=simple.reg.FY,color="black"),size=1)+ggtitle("Lung Data comparing methods performance")+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="M" & lv3_lung_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=poisson.reg.FY,color="green"),size=1)+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="M" & lv3_lung_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=linear.spline.FY,color='darkviolet'),size=1)+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="M" & lv3_lung_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=quadratic.reg.FY,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_M_234),mapping = aes(x = 2010, y = Inf, label = simple_reg.FY),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_M_234),mapping = aes(x = 2010, y = Inf, label = poisson_reg.FY),vjust=1.5,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_M_234),mapping = aes(x = 2004, y = Inf, label = linear_spline.FY),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_M_234),mapping = aes(x = 2004, y = Inf, label = quadratic_reg.FY),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of FY BZ',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of lung","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))
########## F & 01 ############
ggplot()+geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="F" & lv3_lung_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=sum_FY_BZ,color="red"),size=1)+labs(y="#of FY BZ")+
  facet_wrap(~ factor(NAME,levels=unique(MAPE_lv3_lung_F_01[,1])),scales="free_y")+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="F" & lv3_lung_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=simple.reg.FY,color="black"),size=1)+ggtitle("Lung Data comparing methods performance")+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="F" & lv3_lung_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=poisson.reg.FY,color="green"),size=1)+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="F" & lv3_lung_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=linear.spline.FY,color='darkviolet'),size=1)+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="F" & lv3_lung_SEX_CAL2$CAL2=="01",],aes(x=YEAR,y=quadratic.reg.FY,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_F_01),mapping = aes(x = 2010, y = Inf, label = simple_reg),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_F_01),mapping = aes(x = 2010, y = Inf, label = poisson_reg),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_F_01),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_F_01),mapping = aes(x = 2002, y = Inf, label = quadratic_reg),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of FY BZ',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of FY BZ","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))
########## F & 234 ############
ggplot()+geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="F" & lv3_lung_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=sum_FY_BZ,color="red"),size=1)+labs(y="#of FY BZ")+
  facet_wrap(~ factor(NAME,levels=unique(MAPE_lv3_lung_F_234[,1])),scales="free_y")+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="F" & lv3_lung_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=simple.reg.FY,color="black"),size=1)+ggtitle("Lung Data comparing methods performance")+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="F" & lv3_lung_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=poisson.reg.FY,color="green"),size=1)+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="F" & lv3_lung_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=linear.spline.FY,color='darkviolet'),size=1)+
  geom_line(data=lv3_lung_SEX_CAL2[lv3_lung_SEX_CAL2$SEX=="F" & lv3_lung_SEX_CAL2$CAL2=="234",],aes(x=YEAR,y=quadratic.reg.FY,color='deeppink1'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_F_234),mapping = aes(x = 2010, y = Inf, label = simple_reg.FY),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_F_234),mapping = aes(x = 2010, y = Inf, label = poisson_reg.FY),vjust=3,colour="darkgreen",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_F_234),mapping = aes(x = 2004, y = Inf, label = linear_spline.FY),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv3_lung_F_234),mapping = aes(x = 2004, y = Inf, label = quadratic_reg.FY),vjust=3,colour="brown1",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='#of FY BZ',
                       breaks=c("red","black","green","darkviolet","deeppink1"),
                       labels=c("true value of FY BZ","simple linear regression","poisson regression","linear spline","quadratic regression"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))
#dev.off() 
value_eklee_lv3<-as.data.frame(rbind(lv3_lung_total[3:length(lv3_lung_total)],lv3_lung_SEX[4:length(lv3_lung_SEX)],lv3_lung_CAL2[4:length(lv3_lung_CAL2)],
                                     lv3_lung_SEX_CAL2[5:length(lv3_lung_SEX_CAL2)]))
val_lung_lv3<-value_eklee_lv3%>%as.data.frame()






########## validation.lung #################
val_lung_total<-as.data.frame(rbind(val_lung_lv1,val_lung_lv2,val_lung_lv3))
colnames(val_lung_total)<-c("YEAR","sum_FY_BZ_Lung","sum_LUNG_BZ","NAME","simple.reg.LUNG","poisson.reg.LUNG",
                            "linear.spline.LUNG","quadratic.reg.LUNG", "simple.reg.lung.FY","poisson.reg.lung.FY","linear.spline.lung.FY",
                            "quadratic.reg.lung.FY")

MAPE_lung_total<-as.data.frame(rbind(MAPE_lv1_lung,MAPE_lv2_lung,MAPE_lv3_lung))
colnames(MAPE_lung_total)<-c("NAME","simple.reg.LUNG","poisson.reg.LUNG",
                             "linear.spline.LUNG","quadratic.reg.LUNG", "simple.reg.lung.FY","poisson.reg.lung.FY","linear.spline.lung.FY",
                             "quadratic.reg.lung.FY")

RMSE_lung_total<-as.data.frame(rbind(RMSE_lv1_lung,RMSE_lv2_lung,RMSE_lv3_lung))
colnames(RMSE_lung_total)<-c("NAME","simple.reg.LUNG","poisson.reg.LUNG",
                             "linear.spline.LUNG","quadratic.reg.LUNG", "simple.reg.lung.FY","poisson.reg.lung.FY","linear.spline.lung.FY",
                             "quadratic.reg.lung.FY")

validation.dt<-merge(val_leukemia_total,val_lung_total,key=c("NAME","YEAR"),all=TRUE)



validation_v2 <- read_csv("validation_v2.csv")
ID<-validation_v2[,c("ID","NAME")]

validation_final<-left_join(validation.dt,ID,by="NAME")
validation.final<-validation_final[,c("ID","NAME","YEAR","sum_LEUKEMIA_BZ","simple.reg.LEUKEMIA","poisson.reg.LEUKEMIA",
                                "linear.spline.LEUKEMIA","quadratic.reg.LEUKEMIA", "sum_FY_BZ_Leukemia","simple.reg.leukemia.FY","poisson.reg.leukemia.FY",
                                "linear.spline.leukemia.FY","quadratic.reg.leukemia.FY","sum_LUNG_BZ","simple.reg.LUNG","poisson.reg.LUNG",
                                "linear.spline.LUNG","quadratic.reg.LUNG", "sum_FY_BZ_Lung","simple.reg.lung.FY","poisson.reg.lung.FY","linear.spline.lung.FY",
                                "quadratic.reg.lung.FY")]
write.csv(unique(validation.final),file='validation_final.csv',row.names=FALSE)

MAPE_leukemia_total<-left_join(MAPE_leukemia_total,ID,by="NAME")
MAPE_leukemia_total<-MAPE_leukemia_total[,c("ID","NAME","simple.reg.LEUKEMIA","poisson.reg.LEUKEMIA",
                                            "linear.spline.LEUKEMIA","quadratic.reg.LEUKEMIA","simple.reg.leukemia.FY","poisson.reg.leukemia.FY",
                                            "linear.spline.leukemia.FY","quadratic.reg.leukemia.FY")]
MAPE_leu<-unique(MAPE_leukemia_total)
write.csv(MAPE_leu,file='MAPE_leukemia_final.csv',row.names = FALSE)

MAPE_lung_total<-left_join(MAPE_lung_total,ID,by="NAME")
MAPE_lung_total<-MAPE_lung_total[,c("ID","NAME","simple.reg.LUNG","poisson.reg.LUNG",
                                    "linear.spline.LUNG","quadratic.reg.LUNG","simple.reg.lung.FY","poisson.reg.lung.FY","linear.spline.lung.FY",
                                    "quadratic.reg.lung.FY")]
MAPE_lung<-unique(MAPE_lung_total)
write.csv(MAPE_lung,file='MAPE_lung_final.csv',row.names = FALSE)







RMSE_leukemia_total<-left_join(RMSE_leukemia_total,ID,by="NAME")
RMSE_leukemia_total<-RMSE_leukemia_total[,c("ID","NAME","simple.reg.LEUKEMIA","poisson.reg.LEUKEMIA",
                                            "linear.spline.LEUKEMIA","quadratic.reg.LEUKEMIA","simple.reg.leukemia.FY","poisson.reg.leukemia.FY",
                                            "linear.spline.leukemia.FY","quadratic.reg.leukemia.FY")]
RMSE_leu<-unique(RMSE_leukemia_total)
write.csv(RMSE_leu,file='RMSE_leukemia_final.csv',row.names = FALSE)

RMSE_lung_total<-left_join(RMSE_lung_total,ID,by="NAME")
RMSE_lung_total<-RMSE_lung_total[,c("ID","NAME","simple.reg.LUNG","poisson.reg.LUNG",
                                    "linear.spline.LUNG","quadratic.reg.LUNG","simple.reg.lung.FY","poisson.reg.lung.FY","linear.spline.lung.FY",
                                    "quadratic.reg.lung.FY")]
RMSE_lung<-unique(RMSE_lung_total)
write.csv(RMSE_lung,file='RMSE_lung_final.csv',row.names = FALSE)
