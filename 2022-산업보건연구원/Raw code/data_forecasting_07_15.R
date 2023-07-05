######## Data forecasting 07/15 ver ######
########## 수정한 부분 ###########
## 1. 1차 spline apply
##################################
library("data.table")
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)
library(ggplot2)
dt_leukemia<-fread("/shared/data/koshri/ref_leukemia_bz_v4.csv")
dt_lung<-fread("/shared/data/koshri/ref_lung_bz_v4.csv")

library(dtplyr)
dt_lz_leukemia<-lazy_dt(dt_leukemia)
dt_lz_lung<-lazy_dt(dt_lung)

dt_lz_leukemia<-dt_lz_leukemia%>%filter(!is.na(UP2))
dt_lz_lung<-dt_lz_lung%>%filter(!is.na(UP2))


############ linear spline regression #############
############# 1. Lung Cancer ##############
library(splines)
dt_lz_lung$parent$UP2<-as.character(dt_lz_lung$parent$UP2)

df_lung<-dt_lz_lung%>%group_by(UP2, YEAR)%>%summarise(sum_LUNG_BZ=sum(LUNG_BZ),sum_FY_BZ=sum(FY_BZ,na.rm = TRUE))%>%
  mutate(rate_by_UP2=sum_LUNG_BZ/sum_FY_BZ)%>%as_tibble()


UP2_level<-unique(df_lung$UP2)

df_lung$fitted_values_spline1<-0

MAPE_lung_spline1<-matrix(0,nrow=length(UP2_level),ncol=2)
colnames(MAPE_lung_spline1)<-c("UP2","MAPE")
MAPE_lung_spline1[,1]<-UP2_level

for(i in 1:length(UP2_level)){
  ind_tr<-which(df_lung$YEAR<=2014 & df_lung$UP2==UP2_level[i])
  ind_val<-which(df_lung$YEAR>2014 & df_lung$UP2==UP2_level[i])
  
  train<-df_lung[ind_tr,]
  val<-df_lung[ind_val,]
  
  model_linear_spline<-lm(rate_by_UP2~bs(YEAR,knots=c(2005,2010),degree=1),data=train)
  
  df_lung$fitted_values_spline1[ind_tr]<-model_linear_spline$fitted.values
  value<-predict(model_linear_spline,newdata=val)
  df_lung$fitted_values_spline1[ind_val]<-value
  
  MAPE_lung_spline1[i,2]<-round(mean(abs(val$rate_by_UP2-df_lung$fitted_values_spline1[ind_val])/val$rate_by_UP2)*100,2)
  
}
MAPE_lung_spline1<-as.data.frame(MAPE_lung_spline1)
MAPE_lung_spline1$MAPE<-ifelse(MAPE_lung_spline1$MAPE=="NaN",-99,MAPE_lung_spline1$MAPE)
MAPE_lung_spline1%>%arrange(desc(MAPE))

gg<-ggplot(df_lung,aes(x=YEAR,y=rate_by_UP2,group=UP2))
gg+geom_line(color='red')+labs(y="rate_of_lung_cancer")+
  facet_wrap(~ factor(UP2,levels=unique(result_YEAR_FY_BZ$UP2)),scales="free_y")+
  geom_line(aes(x=YEAR, y=fitted_values_spline1, group=UP2),linetype='dashed',color='black')+ggtitle("LUNG Cancer Data applying linear spline")+
  geom_vline(xintercept=2014, linetype = 'solid', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=MAPE_lung_spline1,mapping = aes(x = 2010, y = Inf, label = MAPE),vjust=1.5,colour="purple",fontface="bold")


######### 2015~2018 forecasting part focusing #########
val_lung<-df_lung%>%filter(YEAR>2014)%>%as_tibble()

gg1<-ggplot(val_lung,aes(x=YEAR,y=rate_by_UP2,group=UP2))
gg1+geom_line(color='red')+labs(y="rate_of_lung_cancer")+
  facet_wrap(~ factor(UP2,levels=unique(result_YEAR_FY_BZ$UP2)),scales="free_y")+
  geom_line(aes(x=YEAR,y=fitted_values_spline1,group=UP2),linetype='dashed',color='blue')+ggtitle("LUNG Cancer Data applying linear spline")+xlim(2015,2018)+
  geom_text(size=3,data=MAPE_lung_spline1,mapping = aes(x = 2017, y = Inf, label = MAPE),vjust=1.5,colour="purple",fontface="bold")



############ 2. Leukemia Data #################
dt_lz_leukemia$parent$UP2<-as.character(dt_lz_leukemia$parent$UP2)

df_leukemia<-dt_lz_leukemia%>%group_by(UP2, YEAR)%>%summarise(sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ),sum_FY_BZ=sum(FY_BZ,na.rm = TRUE))%>%
  mutate(rate_by_UP2=sum_LEUKEMIA_BZ/sum_FY_BZ)%>%as_tibble()


UP2_level<-unique(lm_leukemia$UP2)

df_leukemia$fitted_values_spline1<-0

MAPE_leukemia_spline1<-matrix(0,nrow=length(UP2_level),ncol=2)
colnames(MAPE_leukemia_spline1)<-c("UP2","MAPE")
MAPE_leukemia_spline1[,1]<-UP2_level

for(i in 1:length(UP2_level)){
  ind_tr<-which(df_leukemia$YEAR<=2014 & df_leukemia$UP2==UP2_level[i])
  ind_val<-which(df_leukemia$YEAR>2014 & df_leukemia$UP2==UP2_level[i])
  
  train<-df_leukemia[ind_tr,]
  val<-df_leukemia[ind_val,]
  
  model_linear_spline<-lm(rate_by_UP2~bs(YEAR,knots=c(2005,2010),degree=1),data=train)
  
  df_leukemia$fitted_values_spline1[ind_tr]<-model_linear_spline$fitted.values
  value<-predict(model_linear_spline,newdata=val)
  df_leukemia$fitted_values_spline1[ind_val]<-value
  
  MAPE_leukemia_spline1[i,2]<-round(mean(abs(val$rate_by_UP2-df_leukemia$fitted_values_spline1[ind_val])/val$rate_by_UP2)*100,2)
  
}
MAPE_leukemia_spline1<-as.data.frame(MAPE_leukemia_spline1)
MAPE_leukemia_spline1$MAPE<-ifelse(MAPE_leukemia_spline1$MAPE=="NaN",-99,MAPE_leukemia_spline1$MAPE)
MAPE_leukemia_spline1%>%arrange(desc(MAPE))

gg<-ggplot(df_leukemia,aes(x=YEAR,y=rate_by_UP2,group=UP2))
gg+geom_line(color='red')+labs(y="rate_of_leukemia")+
  facet_wrap(~ factor(UP2,levels=unique(result_YEAR_FY_BZ$UP2)),scales="free_y")+
  geom_line(aes(x=YEAR,y=fitted_values_spline1,group=UP2),linetype='dashed',color='black')+ggtitle("Leukemia Data applying linear spline")+
  geom_vline(xintercept=2014, linetype = 'solid', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=MAPE_leukemia_spline1,mapping = aes(x = 2010, y = Inf, label = MAPE),vjust=1.5,colour="purple",fontface="bold")

######### 2015~2018 forecasting part focusing #########
val_leukemia<-df_leukemia%>%filter(YEAR>2014)%>%as_tibble()

gg2<-ggplot(val_leukemia,aes(x=YEAR,y=rate_by_UP2,group=UP2))
gg2+geom_line(color='red')+labs(y="rate_of_leukemia")+
  facet_wrap(~ factor(UP2,levels=unique(result_YEAR_FY_BZ$UP2)),scales="free_y")+
  geom_line(aes(x=YEAR,y=fitted_values_spline1,group=UP2),linetype='dashed',color='blue')+ggtitle("Leukemia Data applying linear spline")+
  xlim(2015,2018)+
  geom_text(size=3,data=MAPE_leukemia_spline1,mapping = aes(x = 2017, y = Inf, label = MAPE),vjust=1.5,colour="purple",fontface="bold")






################ 추가사항 : Plotting #######################
######### 1. lung cancer #############
dt_lz_lung$parent$UP2<-as.character(dt_lz_lung$parent$UP2)

lv1_lung<-dt_lz_lung%>%group_by(YEAR)%>%summarise(sum_LUNG_BZ=sum(LUNG_BZ),sum_FY_BZ=sum(FY_BZ,na.rm = TRUE))%>%
  mutate(rate=sum_LUNG_BZ/sum_FY_BZ)%>%as_tibble()

lv2_lung<-dt_lz_lung%>%group_by(UP1,YEAR)%>%summarise(sum_LUNG_BZ=sum(LUNG_BZ),sum_FY_BZ=sum(FY_BZ,na.rm = TRUE))%>%
  mutate(rate_by_UP1=sum_LUNG_BZ/sum_FY_BZ)%>%as_tibble()

lv3_lung<-dt_lz_lung%>%group_by(UP2,YEAR)%>%summarise(sum_LUNG_BZ=sum(LUNG_BZ),sum_FY_BZ=sum(FY_BZ,na.rm = TRUE))%>%
  mutate(rate_by_UP2=sum_LUNG_BZ/sum_FY_BZ)%>%as_tibble()

MAPE_lv1<-matrix(0,nrow=1,ncol=2)
names(MAPE_lv1)<-c("simple_linear","linear_spline")

MAPE_lv2<-matrix(0,nrow=length(unique(lv2_lung$UP1)),ncol=3)
MAPE_lv2[,1]<-unique(lv2_lung$UP1)
colnames(MAPE_lv2)<-c("UP1","simple_linear","linear_spline")

MAPE_lv3<-matrix(0,nrow=length(unique(lv3_lung$UP2)),ncol=3)
MAPE_lv3[,1]<-unique(lv3_lung$UP2)
colnames(MAPE_lv3)<-c("UP2","simple_linear","linear_spline")


######### level1 data ############
ind_tr<-which(lv1_lung$YEAR<=2014)
ind_val<-which(lv1_lung$YEAR>2014)

train_lv1<-lv1_lung[ind_tr,]
val_lv1<-lv1_lung[ind_val,]

lv1_lung$fitted.linear<-0
lv1_lung$fitted.spline1<-0

lv1_linear<-lm(rate~YEAR,data=train_lv1)
lv1_lung$fitted.linear[ind_tr]<-lv1_linear$fitted.values
lv1_lung$fitted.linear[ind_val]<-predict(lv1_linear,newdata=val_lv1)

lv1_spline1<-lm(rate~bs(YEAR,knots=c(2005,2010),degree=1),data=train_lv1)
lv1_lung$fitted.spline1[ind_tr]<-lv1_spline1$fitted.values
lv1_lung$fitted.spline1[ind_val]<-predict(lv1_spline1,newdata=val_lv1)

MAPE_lv1<-c(round(mean(abs(val_lv1$rate-lv1_lung$fitted.linear[ind_val])/val_lv1$rate)*100,2),
            round(mean(abs(val_lv1$rate-lv1_lung$fitted.spline1[ind_val])/val_lv1$rate)*100,2))

ggplot()+geom_line(data=lv1_lung,aes(x=YEAR,y=rate,color="red"),size=1)+labs(y="rate_of_lung")+
  geom_line(data=lv1_lung,aes(x=YEAR,y=fitted.linear,color="black"),size=1)+ggtitle("Lung cancer Data comparing methods performance")+
  geom_line(data=lv1_lung,aes(x=YEAR,y=fitted.spline1,color='darkviolet'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  labs(x="Year")+
  scale_color_identity(name='rate of lung cancer',
                     breaks=c("red","black","darkviolet"),
                   labels=c("true value of lung cancer","simple linear regression","linear spline"),
                   guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))
  #geom_text(size=3,data=as.data.frame(MAPE_lv1),mapping = aes(x = 2010, y = Inf, label = simple_linear),vjust=1.5,colour="blue",fontface="bold")+
  #geom_text(size=3,data=as.data.frame(MAPE_lv1),mapping = aes(x = 2004, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")
  #geom_text(size=3,data=MAPE_lv1,mapping = aes(x = 2010, y = Inf, label = MAPE),vjust=1.5,colour="purple",fontface="bold")




###############################################################################
######### level2 data ############
UP1_level<-unique(lv2_lung$UP1)

lv2_lung$fitted.linear<-0
lv2_lung$fitted.spline1<-0

for(i in 1:length(UP1_level)){
  ind_tr<-which(lv2_lung$YEAR<=2014 & lv2_lung$UP1==UP1_level[i])
  ind_val<-which(lv2_lung$YEAR>2014 & lv2_lung$UP1==UP1_level[i])
  
  train<-lv2_lung[ind_tr,]
  val<-lv2_lung[ind_val,]
  
  model_linear<-lm(rate_by_UP1~YEAR,data=train)
  model_linear_spline1<-lm(rate_by_UP1~bs(YEAR,knots=c(2005,2010),degree=1),data=train)
  
  lv2_lung$fitted.linear[ind_tr]<-model_linear$fitted.values
  lv2_lung$fitted.spline1[ind_tr]<-model_linear_spline1$fitted.values
  
  value.linear<-predict(model_linear,newdata=val)
  lv2_lung$fitted.linear[ind_val]<-value.linear
  
  value.spline<-predict(model_linear_spline1,newdata=val)
  lv2_lung$fitted.spline1[ind_val]<-value.spline
  
  MAPE_lv2[i,c(2,3)]<-c(round(mean(abs(val$rate_by_UP1-lv2_lung$fitted.linear[ind_val])/val$rate_by_UP1)*100,2),
                        round(mean(abs(val$rate_by_UP1-lv2_lung$fitted.spline1[ind_val])/val$rate_by_UP1)*100,2))
  
}
MAPE_lv2<-as.data.frame(MAPE_lv2)
MAPE_lv2$simple_linear<-ifelse(MAPE_lv2$simple_linear=="NaN",-99,MAPE_lv2$simple_linear)
MAPE_lv2$linear_spline<-ifelse(MAPE_lv2$linear_spline=="NaN",-99,MAPE_lv2$linear_spline)
MAPE_lv2%>%arrange(desc(simple_linear),desc(linear_spline))

ggplot()+geom_line(data=lv2_lung,aes(x=YEAR,y=rate_by_UP1,group=UP1,color='red'))+labs(y="rate_of_lung_cancer")+
  facet_wrap(~ factor(UP1,levels=unique(result_YEAR_FY_BZ_UP1$UP1)),scales="free_y")+
  geom_line(data=lv2_lung,aes(x=YEAR,y=fitted.linear,group=UP1,color='black'),linetype='dashed')+ggtitle("Lung cancer data grouping by UP1 comparing methods performance")+
  geom_line(data=lv2_lung,aes(x=YEAR,y=fitted.spline1,group=UP1,color='darkviolet'),linetype='twodash')+
  geom_vline(xintercept=2014, linetype = 'solid', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=MAPE_lv2,mapping = aes(x = 2010, y = Inf, label = simple_linear),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=MAPE_lv2,mapping = aes(x = 2004, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  scale_color_identity(name='rate of lung cancer',
                       breaks=c("red","black","darkviolet"),
                       labels=c("true value of lung cancer","simple linear regression","linear spline"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


###################################################################################
######### 2015~2018 forecasting part focusing #########
val_lung_lv2<-lv2_lung%>%filter(YEAR>2014)%>%as_tibble()

ggplot()+geom_line(data=val_lung_lv2,aes(x=YEAR,y=rate_by_UP1,group=UP1,color='red'))+labs(y="rate_of_lung cancer")+
  facet_wrap(~ factor(UP1,levels=unique(result_YEAR_FY_BZ_UP1$UP1)),scales="free_y")+
  geom_line(data=val_lung_lv2,aes(x=YEAR,y=fitted.linear,group=UP1,color='black'),linetype='dashed')+ggtitle("Lung cancer Data grouped by UP1 applying several methods")+
  xlim(2015,2018)+geom_line(data=val_lung_lv2,aes(x=YEAR,y=fitted.spline1,group=UP1,color='darkviolet'),linetype='solid')+
  geom_text(size=3,data=MAPE_lv2,mapping = aes(x = 2016, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=MAPE_lv2,mapping = aes(x = 2016, y = Inf, label = simple_linear),vjust=3,colour="black",fontface="bold")+
  scale_color_identity(name='rate of lung cancer',
                       breaks=c("red","black","darkviolet"),
                       labels=c("true value of lung cancer","simple linear regression","linear spline"),
                       guide='legend')
######################################################################################



###################################################################################
######### level3 data ############
UP2_level<-unique(lv3_lung$UP2)

lv3_lung$fitted.linear<-0
lv3_lung$fitted.spline1<-0

for(i in 1:length(UP2_level)){
  ind_tr<-which(lv3_lung$YEAR<=2014 & lv3_lung$UP2==UP2_level[i])
  ind_val<-which(lv3_lung$YEAR>2014 & lv3_lung$UP2==UP2_level[i])
  
  train<-lv3_lung[ind_tr,]
  val<-lv3_lung[ind_val,]
  
  model_linear<-lm(rate_by_UP2~YEAR,data=train)
  model_linear_spline1<-lm(rate_by_UP2~bs(YEAR,knots=c(2005,2010),degree=1),data=train)
  
  lv3_lung$fitted.linear[ind_tr]<-model_linear$fitted.values
  lv3_lung$fitted.spline1[ind_tr]<-model_linear_spline1$fitted.values
  
  value.linear<-predict(model_linear,newdata=val)
  lv3_lung$fitted.linear[ind_val]<-value.linear
  
  value.spline<-predict(model_linear_spline1,newdata=val)
  lv3_lung$fitted.spline1[ind_val]<-value.spline
  
  MAPE_lv3[i,c(2,3)]<-c(round(mean(abs(val$rate_by_UP2-lv3_lung$fitted.linear[ind_val])/val$rate_by_UP2)*100,2),
                        round(mean(abs(val$rate_by_UP2-lv3_lung$fitted.spline1[ind_val])/val$rate_by_UP2)*100,2))
  
}
MAPE_lv3<-as.data.frame(MAPE_lv3)
MAPE_lv3$simple_linear<-ifelse(MAPE_lv3$simple_linear=="NaN",-99,MAPE_lv3$simple_linear)
MAPE_lv3$linear_spline<-ifelse(MAPE_lv3$linear_spline=="NaN",-99,MAPE_lv3$linear_spline)
MAPE_lv3%>%arrange(desc(simple_linear),desc(linear_spline))

ggplot()+geom_line(data=lv3_lung,aes(x=YEAR,y=rate_by_UP2,group=UP2,color='red'))+labs(y="rate_of_lung_cancer")+
  facet_wrap(~ factor(UP2,levels=unique(result_YEAR_FY_BZ$UP2)),scales="free_y")+
  geom_line(data=lv3_lung,aes(x=YEAR,y=fitted.linear,group=UP2,color='black'),linetype='dashed')+ggtitle("Lung cancer data grouping by UP2 comparing methods performance")+
  geom_line(data=lv3_lung,aes(x=YEAR,y=fitted.spline1,group=UP2,color='darkviolet'),linetype='solid')+
  geom_vline(xintercept=2014, linetype = 'solid', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=MAPE_lv3,mapping = aes(x = 2010, y = Inf, label = simple_linear),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=MAPE_lv3,mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  scale_color_identity(name='rate of lung cancer',
                       breaks=c("red","black","darkviolet"),
                       labels=c("true value of lung cancer","simple linear regression","linear spline"),
                       guide='legend')+
  theme(legend.title=element_text(size=11),
        legend.text=element_text(size=10))


##################################################################################
######### 2015~2018 forecasting part focusing #########
val_lung_lv3<-lv3_lung%>%filter(YEAR>2014)%>%as_tibble()

ggplot()+geom_line(data=val_lung_lv3,aes(x=YEAR,y=rate_by_UP2,group=UP2,color='red'))+labs(y="rate_of_lung cancer")+
  facet_wrap(~ factor(UP2,levels=unique(result_YEAR_FY_BZ$UP2)),scales="free_y")+
  geom_line(data=val_lung_lv3,aes(x=YEAR,y=fitted.linear,group=UP2,color='black'),linetype='dashed')+ggtitle("Lung cancer Data grouped by UP2 applying several methods")+
  xlim(2015,2018)+geom_line(data=val_lung_lv3,aes(x=YEAR,y=fitted.spline1,group=UP2,color='darkviolet'),linetype='solid')+
  geom_text(size=3,data=MAPE_lv3,mapping = aes(x = 2016, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=MAPE_lv3,mapping = aes(x = 2016, y = Inf, label = simple_linear),vjust=3,colour="black",fontface="bold")+
  scale_color_identity(name='rate of lung cancer',
                       breaks=c("red","black","darkviolet"),
                       labels=c("true value of lung cancer","simple linear regression","linear spline"),
                       guide='legend')
######################################################################################
MAPE_lv3$simple_linear<-as.numeric(MAPE_lv3$simple_linear)
MAPE_lv3$linear_spline<-as.numeric(MAPE_lv3$linear_spline)
MAPE_lv3%>%arrange(desc(linear_spline))










##########################################################################################
##########################################################################################
############## 2. Leukemia data #####################
dt_lz_leukemia$parent$UP2<-as.character(dt_lz_leukemia$parent$UP2)

lv1_leukemia<-dt_lz_leukemia%>%group_by(YEAR)%>%summarise(sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ),sum_FY_BZ=sum(FY_BZ,na.rm = TRUE))%>%
  mutate(rate=sum_LEUKEMIA_BZ/sum_FY_BZ)%>%as_tibble()

lv2_leukemia<-dt_lz_leukemia%>%group_by(UP1,YEAR)%>%summarise(sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ),sum_FY_BZ=sum(FY_BZ,na.rm = TRUE))%>%
  mutate(rate_by_UP1=sum_LEUKEMIA_BZ/sum_FY_BZ)%>%as_tibble()

lv3_leukemia<-dt_lz_leukemia%>%group_by(UP2,YEAR)%>%summarise(sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ),sum_FY_BZ=sum(FY_BZ,na.rm = TRUE))%>%
  mutate(rate_by_UP2=sum_LEUKEMIA_BZ/sum_FY_BZ)%>%as_tibble()

MAPE_lv1<-matrix(0,nrow=1,ncol=2)
names(MAPE_lv1)<-c("simple_linear","linear_spline")

MAPE_lv2<-matrix(0,nrow=length(unique(lv2_leukemia$UP1)),ncol=3)
MAPE_lv2[,1]<-unique(lv2_leukemia$UP1)
colnames(MAPE_lv2)<-c("UP1","simple_linear","linear_spline")

MAPE_lv3<-matrix(0,nrow=length(unique(lv3_leukemia$UP2)),ncol=3)
MAPE_lv3[,1]<-unique(lv3_leukemia$UP2)
colnames(MAPE_lv3)<-c("UP2","simple_linear","linear_spline")


######### level1 data ############
ind_tr<-which(lv1_leukemia$YEAR<=2014)
ind_val<-which(lv1_leukemia$YEAR>2014)

train_lv1<-lv1_leukemia[ind_tr,]
val_lv1<-lv1_leukemia[ind_val,]

lv1_leukemia$fitted.linear<-0
lv1_leukemia$fitted.spline1<-0

lv1_linear<-lm(rate~YEAR,data=train_lv1)
lv1_leukemia$fitted.linear[ind_tr]<-lv1_linear$fitted.values
lv1_leukemia$fitted.linear[ind_val]<-predict(lv1_linear,newdata=val_lv1)

lv1_spline1<-lm(rate~bs(YEAR,knots=c(2005,2010),degree=1),data=train_lv1)
lv1_leukemia$fitted.spline1[ind_tr]<-lv1_spline1$fitted.values
lv1_leukemia$fitted.spline1[ind_val]<-predict(lv1_spline1,newdata=val_lv1)

MAPE_lv1<-c(round(mean(abs(val_lv1$rate-lv1_leukemia$fitted.linear[ind_val])/val_lv1$rate)*100,2),
            round(mean(abs(val_lv1$rate-lv1_leukemia$fitted.spline1[ind_val])/val_lv1$rate)*100,2))

ggplot()+geom_line(data=lv1_leukemia,aes(x=YEAR,y=rate,color="red"),size=1)+labs(y="rate_of_leukemia")+
  geom_line(data=lv1_leukemia,aes(x=YEAR,y=fitted.linear,color="black"),size=1)+ggtitle("Leukemia Data comparing methods performance")+
  geom_line(data=lv1_leukemia,aes(x=YEAR,y=fitted.spline1,color='darkviolet'),size=1)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  labs(x="Year")+
  scale_color_identity(name='rate of leukemia',
                       breaks=c("red","black","darkviolet"),
                       labels=c("true value of leukemia","simple linear regression","linear spline"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))
#geom_text(size=3,data=as.data.frame(MAPE_lv1),mapping = aes(x = 2010, y = Inf, label = simple_linear),vjust=1.5,colour="blue",fontface="bold")+
#geom_text(size=3,data=as.data.frame(MAPE_lv1),mapping = aes(x = 2004, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")
#geom_text(size=3,data=MAPE_lv1,mapping = aes(x = 2010, y = Inf, label = MAPE),vjust=1.5,colour="purple",fontface="bold")




###############################################################################
######### level2 data ############
UP1_level<-unique(lv2_leukemia$UP1)

lv2_leukemia$fitted.linear<-0
lv2_leukemia$fitted.spline1<-0

for(i in 1:length(UP1_level)){
  ind_tr<-which(lv2_leukemia$YEAR<=2014 & lv2_leukemia$UP1==UP1_level[i])
  ind_val<-which(lv2_leukemia$YEAR>2014 & lv2_leukemia$UP1==UP1_level[i])
  
  train<-lv2_leukemia[ind_tr,]
  val<-lv2_leukemia[ind_val,]
  
  model_linear<-lm(rate_by_UP1~YEAR,data=train)
  model_linear_spline1<-lm(rate_by_UP1~bs(YEAR,knots=c(2005,2010),degree=1),data=train)
  
  lv2_leukemia$fitted.linear[ind_tr]<-model_linear$fitted.values
  lv2_leukemia$fitted.spline1[ind_tr]<-model_linear_spline1$fitted.values
  
  value.linear<-predict(model_linear,newdata=val)
  lv2_leukemia$fitted.linear[ind_val]<-value.linear
  
  value.spline<-predict(model_linear_spline1,newdata=val)
  lv2_leukemia$fitted.spline1[ind_val]<-value.spline
  
  MAPE_lv2[i,c(2,3)]<-c(round(mean(abs(val$rate_by_UP1-lv2_leukemia$fitted.linear[ind_val])/val$rate_by_UP1)*100,2),
                        round(mean(abs(val$rate_by_UP1-lv2_leukemia$fitted.spline1[ind_val])/val$rate_by_UP1)*100,2))
  
}
MAPE_lv2<-as.data.frame(MAPE_lv2)
MAPE_lv2$simple_linear<-ifelse(MAPE_lv2$simple_linear=="NaN",-99,MAPE_lv2$simple_linear)
MAPE_lv2$linear_spline<-ifelse(MAPE_lv2$linear_spline=="NaN",-99,MAPE_lv2$linear_spline)
MAPE_lv2%>%arrange(desc(simple_linear),desc(linear_spline))

ggplot()+geom_line(data=lv2_leukemia,aes(x=YEAR,y=rate_by_UP1,group=UP1,color='red'))+labs(y="rate_of_leukemia")+
  facet_wrap(~ factor(UP1,levels=unique(result_YEAR_FY_BZ_UP1$UP1)),scales="free_y")+
  geom_line(data=lv2_leukemia,aes(x=YEAR,y=fitted.linear,group=UP1,color='black'),linetype='dashed')+ggtitle("Leukemia data grouping by UP1 comparing methods performance")+
  geom_line(data=lv2_leukemia,aes(x=YEAR,y=fitted.spline1,group=UP1,color='darkviolet'),linetype='twodash')+
  geom_vline(xintercept=2014, linetype = 'solid', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=MAPE_lv2,mapping = aes(x = 2010, y = Inf, label = simple_linear),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=MAPE_lv2,mapping = aes(x = 2004, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  scale_color_identity(name='rate of leukemia',
                       breaks=c("red","black","darkviolet"),
                       labels=c("true value of leukemia","simple linear regression","linear spline"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


###################################################################################
######### 2015~2018 forecasting part focusing #########
val_leukemia_lv2<-lv2_leukemia%>%filter(YEAR>2014)%>%as_tibble()

ggplot()+geom_line(data=val_leukemia_lv2,aes(x=YEAR,y=rate_by_UP1,group=UP1,color='red'))+labs(y="rate_of_leukemia")+
  facet_wrap(~ factor(UP1,levels=unique(result_YEAR_FY_BZ_UP1$UP1)),scales="free_y")+
  geom_line(data=val_leukemia_lv2,aes(x=YEAR,y=fitted.linear,group=UP1,color='black'),linetype='dashed')+ggtitle("Leukemia Data grouped by UP1 applying several methods")+
  xlim(2015,2018)+geom_line(data=val_leukemia_lv2,aes(x=YEAR,y=fitted.spline1,group=UP1,color='darkviolet'),linetype='solid')+
  geom_text(size=3,data=MAPE_lv2,mapping = aes(x = 2016, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=MAPE_lv2,mapping = aes(x = 2016, y = Inf, label = simple_linear),vjust=3,colour="black",fontface="bold")+
  scale_color_identity(name='rate of leukemia',
                       breaks=c("red","black","darkviolet"),
                       labels=c("true value of leukemia","simple linear regression","linear spline"),
                       guide='legend')
######################################################################################



###################################################################################
######### level3 data ############
UP2_level<-unique(lv3_leukemia$UP2)

lv3_leukemia$fitted.linear<-0
lv3_leukemia$fitted.spline1<-0

for(i in 1:length(UP2_level)){
  ind_tr<-which(lv3_leukemia$YEAR<=2014 & lv3_leukemia$UP2==UP2_level[i])
  ind_val<-which(lv3_leukemia$YEAR>2014 & lv3_leukemia$UP2==UP2_level[i])
  
  train<-lv3_leukemia[ind_tr,]
  val<-lv3_leukemia[ind_val,]
  
  model_linear<-lm(rate_by_UP2~YEAR,data=train)
  model_linear_spline1<-lm(rate_by_UP2~bs(YEAR,knots=c(2005,2010),degree=1),data=train)
  
  lv3_leukemia$fitted.linear[ind_tr]<-model_linear$fitted.values
  lv3_leukemia$fitted.spline1[ind_tr]<-model_linear_spline1$fitted.values
  
  value.linear<-predict(model_linear,newdata=val)
  lv3_leukemia$fitted.linear[ind_val]<-value.linear
  
  value.spline<-predict(model_linear_spline1,newdata=val)
  lv3_leukemia$fitted.spline1[ind_val]<-value.spline
  
  MAPE_lv3[i,c(2,3)]<-c(round(mean(abs(val$rate_by_UP2-lv3_leukemia$fitted.linear[ind_val])/val$rate_by_UP2)*100,2),
                        round(mean(abs(val$rate_by_UP2-lv3_leukemia$fitted.spline1[ind_val])/val$rate_by_UP2)*100,2))
  
}
MAPE_lv3<-as.data.frame(MAPE_lv3)
MAPE_lv3$simple_linear<-ifelse(MAPE_lv3$simple_linear=="NaN",-99,MAPE_lv3$simple_linear)
MAPE_lv3$linear_spline<-ifelse(MAPE_lv3$linear_spline=="NaN",-99,MAPE_lv3$linear_spline)
#MAPE_lv3%>%arrange(desc(simple_linear),desc(linear_spline))

ggplot()+geom_line(data=lv3_leukemia,aes(x=YEAR,y=rate_by_UP2,group=UP2,color='red'))+labs(y="rate_of_leukemia")+
  facet_wrap(~ factor(UP2,levels=unique(result_YEAR_FY_BZ$UP2)),scales="free_y")+
  geom_line(data=lv3_leukemia,aes(x=YEAR,y=fitted.linear,group=UP2,color='black'),linetype='dashed')+ggtitle("Leukemia data grouping by UP2 comparing methods performance")+
  geom_line(data=lv3_leukemia,aes(x=YEAR,y=fitted.spline1,group=UP2,color='darkviolet'),linetype='solid')+
  geom_vline(xintercept=2014, linetype = 'solid', color='blue', size = 1)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.5)+
  geom_text(size=3,data=MAPE_lv3,mapping = aes(x = 2010, y = Inf, label = simple_linear),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=MAPE_lv3,mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  scale_color_identity(name='rate of leukemia',
                       breaks=c("red","black","darkviolet"),
                       labels=c("true value of leukemia","simple linear regression","linear spline"),
                       guide='legend')+
  theme(legend.title=element_text(size=11),
        legend.text=element_text(size=10))


##################################################################################
######### 2015~2018 forecasting part focusing #########
val_leukemia_lv3<-lv3_leukemia%>%filter(YEAR>2014)%>%as_tibble()

ggplot()+geom_line(data=val_leukemia_lv3,aes(x=YEAR,y=rate_by_UP2,group=UP2,color='red'))+labs(y="rate_of_leukemia")+
  facet_wrap(~ factor(UP2,levels=unique(result_YEAR_FY_BZ$UP2)),scales="free_y")+
  geom_line(data=val_leukemia_lv3,aes(x=YEAR,y=fitted.linear,group=UP2,color='black'),linetype='dashed')+ggtitle("Leukemia Data grouped by UP2 applying several methods")+
  xlim(2015,2018)+geom_line(data=val_leukemia_lv3,aes(x=YEAR,y=fitted.spline1,group=UP2,color='darkviolet'),linetype='solid')+
  geom_text(size=3,data=MAPE_lv3,mapping = aes(x = 2016, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  geom_text(size=3,data=MAPE_lv3,mapping = aes(x = 2016, y = Inf, label = simple_linear),vjust=3,colour="black",fontface="bold")+
  scale_color_identity(name='rate of leukemia',
                       breaks=c("red","black","darkviolet"),
                       labels=c("true value of leukemia","simple linear regression","linear spline"),
                       guide='legend')
######################################################################################
MAPE_lv3$simple_linear<-as.numeric(MAPE_lv3$simple_linear)
MAPE_lv3$linear_spline<-as.numeric(MAPE_lv3$linear_spline)
MAPE_lv3%>%arrange(desc(linear_spline))
