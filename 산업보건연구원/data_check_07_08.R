######## Data check 7/8 ver ######
########## 수정한 부분 ###########
## 1. SEX, CAL, ECNY_AGE YEAR, UP2에 따라 시각화
## 2. Simple linear regression apply
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

############# UP2, YEAR에 따라 SEX, CAL, ECNY_AGE 시각화 ##################
dt_gg<-dt_lz_leukemia%>%group_by(UP2, YEAR)%>%as_tibble()

ggplot(as.data.frame(dt_gg),aes(x=YEAR,fill=SEX))+geom_bar(position = "fill")+labs(y="Ratio of sex")+
  facet_wrap(~ factor(UP2,levels=unique(result_YEAR_FY_BZ$UP2)))

ggplot(as.data.frame(dt_gg),aes(x=YEAR,fill=as.factor(CAL)))+geom_bar(position = "fill")+labs(y="Ratio of CAL", fill="CAL")+
  facet_wrap(~ factor(UP2,levels=unique(result_YEAR_FY_BZ$UP2)))

ggplot(as.data.frame(dt_lz_leukemia),aes(x=YEAR,fill=as.factor(ECNY_AGE)))+geom_bar(position = "fill")+labs(y="Ratio of ECNY_AGE", fill="ECNY_AGE")+
  facet_wrap(~ factor(UP2,levels=unique(result_YEAR_FY_BZ$UP2)))


############ linear regression #############
############# 1. Lung Cancer ##############
dt_lz_lung$parent$UP2<-as.character(dt_lz_lung$parent$UP2)

lm_lung<-dt_lz_lung%>%group_by(UP2, YEAR)%>%summarise(sum_LUNG_BZ=sum(LUNG_BZ),sum_FY_BZ=sum(FY_BZ,na.rm = TRUE))%>%
  mutate(rate_by_UP2=sum_LUNG_BZ/sum_FY_BZ)%>%as_tibble()


UP2_level<-unique(lm_lung$UP2)

lm_lung$fitted_values<-0

MAPE_lung<-matrix(0,nrow=length(UP2_level),ncol=2)
colnames(MAPE_lung)<-c("UP2","MAPE")
MAPE_lung[,1]<-UP2_level

for(i in 1:length(UP2_level)){
  ind_tr<-which(lm_lung$YEAR<=2014 & lm_lung$UP2==UP2_level[i])
  ind_val<-which(lm_lung$YEAR>2014 & lm_lung$UP2==UP2_level[i])
  
  train<-lm_lung[ind_tr,]
  val<-lm_lung[ind_val,]
  
  model<-lm(rate_by_UP2~YEAR,data=train)
  
  lm_lung$fitted_values[ind_tr]<-model$fitted.values
  value<-predict(model,newdata=val)
  lm_lung$fitted_values[ind_val]<-value
  
  MAPE_lung[i,2]<-round(mean(abs(val$rate_by_UP2-lm_lung$fitted_values[ind_val])/val$rate_by_UP2)*100,2)
  
}
MAPE_lung<-as.data.frame(MAPE_lung)
MAPE_lung$MAPE<-ifelse(MAPE_lung$MAPE=="NaN",-99,MAPE_lung$MAPE)
MAPE_lung%>%arrange(desc(MAPE))

gg<-ggplot(lm_lung,aes(x=YEAR,y=rate_by_UP2,group=UP2))
gg+geom_line(color='red')+labs(y="rate_of_lung_cancer")+
  facet_wrap(~ factor(UP2,levels=unique(result_YEAR_FY_BZ$UP2)),scales="free_y")+
  geom_line(aes(x=YEAR,y=fitted_values,group=UP2),linetype='dashed',color='black')+ggtitle("LUNG Cancer Data")+
  geom_vline(xintercept=2014, linetype = 'solid', color='blue', size = 1)+
  geom_text(size=3,data=MAPE_lung,mapping = aes(x = 2010, y = Inf, label = MAPE),vjust=1.5,colour="purple",fontface="bold")


######### 2015~2018 forecasting part focusing #########
val_lung<-lm_lung%>%filter(YEAR>2014)%>%as_tibble()

gg1<-ggplot(val_lung,aes(x=YEAR,y=rate_by_UP2,group=UP2))
gg1+geom_line(color='red')+labs(y="rate_of_lung_cancer")+
  facet_wrap(~ factor(UP2,levels=unique(result_YEAR_FY_BZ$UP2)),scales="free_y")+
  geom_line(aes(x=YEAR,y=fitted_values,group=UP2),linetype='dashed',color='blue')+ggtitle("LUNG Cancer Data")+xlim(2015,2018)+
  geom_text(size=3,data=MAPE_lung,mapping = aes(x = 2017, y = Inf, label = MAPE),vjust=1.5,colour="purple",fontface="bold")



############ 2. Leukemia Data #################
dt_lz_leukemia$parent$UP2<-as.character(dt_lz_leukemia$parent$UP2)

lm_leukemia<-dt_lz_leukemia%>%group_by(UP2, YEAR)%>%summarise(sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ),sum_FY_BZ=sum(FY_BZ,na.rm = TRUE))%>%
  mutate(rate_by_UP2=sum_LEUKEMIA_BZ/sum_FY_BZ)%>%as_tibble()


UP2_level<-unique(lm_leukemia$UP2)

lm_leukemia$fitted_values<-0

MAPE_leukemia<-matrix(0,nrow=length(UP2_level),ncol=2)
colnames(MAPE_leukemia)<-c("UP2","MAPE")
MAPE_leukemia[,1]<-UP2_level

for(i in 1:length(UP2_level)){
  ind_tr<-which(lm_leukemia$YEAR<=2014 & lm_leukemia$UP2==UP2_level[i])
  ind_val<-which(lm_leukemia$YEAR>2014 & lm_leukemia$UP2==UP2_level[i])
  
  train<-lm_leukemia[ind_tr,]
  val<-lm_leukemia[ind_val,]
  
  model<-lm(rate_by_UP2~YEAR,data=train)
  
  lm_leukemia$fitted_values[ind_tr]<-model$fitted.values
  value<-predict(model,newdata=val)
  lm_leukemia$fitted_values[ind_val]<-value
  
  MAPE_leukemia[i,2]<-round(mean(abs(val$rate_by_UP2-lm_leukemia$fitted_values[ind_val])/val$rate_by_UP2)*100,2)
  
}
MAPE_leukemia<-as.data.frame(MAPE_leukemia)
MAPE_leukemia$MAPE<-ifelse(MAPE_leukemia$MAPE=="NaN",-99,MAPE_leukemia$MAPE)
MAPE_leukemia%>%arrange(desc(MAPE))

gg<-ggplot(lm_leukemia,aes(x=YEAR,y=rate_by_UP2,group=UP2))
gg+geom_line(color='red')+labs(y="rate_of_leukemia_cancer")+
  facet_wrap(~ factor(UP2,levels=unique(result_YEAR_FY_BZ$UP2)),scales="free_y")+
  geom_line(aes(x=YEAR,y=fitted_values,group=UP2),linetype='dashed',color='black')+ggtitle("Leukemia Data")+
  geom_vline(xintercept=2014, linetype = 'solid', color='blue', size = 1)+
  geom_text(size=3,data=MAPE_leukemia,mapping = aes(x = 2010, y = Inf, label = MAPE),vjust=1.5,colour="purple",fontface="bold")

######### 2015~2018 forecasting part focusing #########
val_leukemia<-lm_leukemia%>%filter(YEAR>2014)%>%as_tibble()

gg2<-ggplot(val_leukemia,aes(x=YEAR,y=rate_by_UP2,group=UP2))
gg2+geom_line(color='red')+labs(y="rate_of_leukemia")+
  facet_wrap(~ factor(UP2,levels=unique(result_YEAR_FY_BZ$UP2)),scales="free_y")+
  geom_line(aes(x=YEAR,y=fitted_values,group=UP2),linetype='dashed',color='blue')+ggtitle("Leukemia Data")+xlim(2015,2018)+
  geom_text(size=3,data=MAPE_leukemia,mapping = aes(x = 2017, y = Inf, label = MAPE),vjust=1.5,colour="purple",fontface="bold")
