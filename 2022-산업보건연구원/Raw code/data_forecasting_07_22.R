## import data

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
df_leukemia1 = df_leukemia1%>%filter(UP1 != "")%>%as_tibble()


dt_lz_lung$parent$SEX = ifelse(dt_lz_lung$parent$SEX=="남", "M", "F")
df_lung1<-dt_lz_lung %>% mutate(CAL2=ifelse( (CAL<=1), "01", "234"))%>%filter(UP1 != "")%>%as_tibble()


########################## level 4 ##################################
######### leukemia data #######
lv4_leukemia<-df_leukemia1%>%group_by(UP1,UP2,SEX,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  mutate(NAME=paste0("X",UP1,UP2,SEX),rate=sum_LEUKEMIA_BZ/sum_FY_BZ)%>%as_tibble()

lv4_FY_BZ_leukemia<-lv4_leukemia%>%filter(YEAR==2018)%>%arrange(desc(sum_FY_BZ))%>%as_tibble()
lv4_FY_BZ_leukemia_M<-lv4_FY_BZ_leukemia%>%filter(SEX=="M")%>%as_tibble()
lv4_FY_BZ_leukemia_F<-lv4_FY_BZ_leukemia%>%filter(SEX=="F")%>%as_tibble()

lv4_leukemia$simple.linear<-0
lv4_leukemia$linear.spline<-0

name_value<-unique(lv4_leukemia$NAME)
MAPE_lv4_leukemia<-matrix(0,nrow=length(name_value),ncol=3)
MAPE_lv4_leukemia[,1]<-name_value
colnames(MAPE_lv4_leukemia)<-c("NAME","simple_linear","linear_spline")

for(i in 1:length(name_value)){
  ind.tr<-which(lv4_leukemia$NAME==name_value[i] & lv4_leukemia$YEAR<=2014)
  ind.val<-which(lv4_leukemia$NAME==name_value[i] & lv4_leukemia$YEAR>2014)
  
  model.simple<-lm(rate~YEAR,data=lv4_leukemia[ind.tr,])
  lv4_leukemia$simple.linear[ind.tr]<-model.simple$fitted.values
  lv4_leukemia$simple.linear[ind.val]<-predict(model.simple,newdata=lv4_leukemia[ind.val,])
  
  model.spline<-lm(rate~bs(YEAR,knots=c(2005,2010),degree=1),data=lv4_leukemia[ind.tr,])
  lv4_leukemia$linear.spline[ind.tr]<-model.spline$fitted.values
  lv4_leukemia$linear.spline[ind.val]<-predict(model.spline,newdata=lv4_leukemia[ind.val,])
  
  MAPE_lv4_leukemia[i,c(2,3)]<-c(round(mean(abs(lv4_leukemia$rate[ind.val]-lv4_leukemia$simple.linear[ind.val])/lv4_leukemia$rate[ind.val])*100,2),
                                 round(mean(abs(lv4_leukemia$rate[ind.val]-lv4_leukemia$linear.spline[ind.val])/lv4_leukemia$rate[ind.val])*100,2))
}
MAPE_lv4_leukemia<-as.data.frame(MAPE_lv4_leukemia)
MAPE_lv4_leukemia$simple_linear<-ifelse(MAPE_lv4_leukemia$simple_linear=="NaN",-99,MAPE_lv4_leukemia$simple_linear)
MAPE_lv4_leukemia$linear_spline<-ifelse(MAPE_lv4_leukemia$linear_spline=="NaN",-99,MAPE_lv4_leukemia$linear_spline)

lv4_leukemia_M<-lv4_leukemia%>%filter(SEX=='M')%>%as_tibble()
lv4_leukemia_F<-lv4_leukemia%>%filter(SEX=='F')%>%as_tibble()

ind.M<-grep("M",MAPE_lv4_leukemia[,1])
MAPE_lv4_leukemia_M<-MAPE_lv4_leukemia[ind.M,]

ind.F<-grep("F",MAPE_lv4_leukemia[,1])
MAPE_lv4_leukemia_F<-MAPE_lv4_leukemia[ind.F,]

ggplot()+geom_line(data=lv4_leukemia_M,aes(x=YEAR,y=rate,color="red"),size=0.5)+labs(y="rate_of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(lv4_FY_BZ_leukemia_M$NAME)),scales="free_y")+
  geom_line(data=lv4_leukemia_M,aes(x=YEAR,y=simple.linear,color="black"),size=0.5)+ggtitle("Leukemia level4 Data(Male) comparing methods performance")+
  geom_line(data=lv4_leukemia_M,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=0.5)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 0.3)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.2)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.2)+
  geom_text(size=3,data=as.data.frame(MAPE_lv4_leukemia_M),mapping = aes(x = 2010, y = Inf, label = simple_linear),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv4_leukemia_M),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of leukemia Male Data',
                       breaks=c("red","black","darkviolet"),
                       labels=c("true value of leukemia","simple linear regression","linear spline"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))




ggplot()+geom_line(data=lv4_leukemia_F,aes(x=YEAR,y=rate,color="red"),size=0.5)+labs(y="rate_of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(lv4_FY_BZ_leukemia_F$NAME)),scales="free_y")+
  geom_line(data=lv4_leukemia_F,aes(x=YEAR,y=simple.linear,color="black"),size=0.5)+ggtitle("Leukemia level4 Data(Female) comparing methods performance")+
  geom_line(data=lv4_leukemia_F,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=0.5)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 0.3)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.2)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.2)+
  geom_text(size=3,data=as.data.frame(MAPE_lv4_leukemia_F),mapping = aes(x = 2010, y = Inf, label = simple_linear),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv4_leukemia_F),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of leukemia Female Data',
                       breaks=c("red","black","darkviolet"),
                       labels=c("true value of leukemia","simple linear regression","linear spline"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

MAPE_lv4_leukemia$simple_linear<-as.numeric(MAPE_lv4_leukemia$simple_linear)
MAPE_lv4_leukemia$linear_spline<-as.numeric(MAPE_lv4_leukemia$linear_spline)
MAPE_lv4_leukemia$min_MAPE<-ifelse(MAPE_lv4_leukemia$simple_linear<=MAPE_lv4_leukemia$linear_spline,MAPE_lv4_leukemia$simple_linear,MAPE_lv4_leukemia$linear_spline)

index<-seq(1,nrow(MAPE_lv4_leukemia),by=2)
lv4_leukemia_mean_MAPE<-matrix(0,nrow=length(index),ncol=3)
lv4_leukemia_mean_MAPE[,1]<-substr(MAPE_lv4_leukemia[index,1],1,nchar(MAPE_lv4_leukemia[index,1])-1)
colnames(lv4_leukemia_mean_MAPE)<-c("NAME","Female","Male")

for(i in 1:length(index)){
  lv4_leukemia_mean_MAPE[i,2]<-MAPE_lv4_leukemia$min_MAPE[index[i]]
  lv4_leukemia_mean_MAPE[i,3]<-MAPE_lv4_leukemia$min_MAPE[(index[i]+1)]
}
lv4_leukemia_mean_MAPE<-as.data.frame(lv4_leukemia_mean_MAPE)
lv4_leukemia_mean_MAPE%>%mutate(diff=abs(abs(as.numeric(Female))-abs(as.numeric(Male))))%>%arrange(desc(diff))


################################################################################
######## lung cancer data ##########
lv4_lung<-df_lung1%>%group_by(UP1,UP2,SEX,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME=paste0("X",UP1,UP2,SEX),rate=sum_LUNG_BZ/sum_FY_BZ)%>%as_tibble()

lv4_FY_BZ_lung<-lv4_lung%>%filter(YEAR==2018)%>%arrange(desc(sum_FY_BZ))%>%as_tibble()
lv4_FY_BZ_lung_M<-lv4_FY_BZ_lung%>%filter(SEX=="M")%>%as_tibble()
lv4_FY_BZ_lung_F<-lv4_FY_BZ_lung%>%filter(SEX=="F")%>%as_tibble()

lv4_lung$simple.linear<-0
lv4_lung$linear.spline<-0

name_value<-unique(lv4_lung$NAME)
MAPE_lv4_lung<-matrix(0,nrow=length(name_value),ncol=3)
MAPE_lv4_lung[,1]<-name_value
colnames(MAPE_lv4_lung)<-c("NAME","simple_linear","linear_spline")

for(i in 1:length(name_value)){
  ind.tr<-which(lv4_lung$NAME==name_value[i] & lv4_lung$YEAR<=2014)
  ind.val<-which(lv4_lung$NAME==name_value[i] & lv4_lung$YEAR>2014)
  
  model.simple<-lm(rate~YEAR,data=lv4_lung[ind.tr,])
  lv4_lung$simple.linear[ind.tr]<-model.simple$fitted.values
  lv4_lung$simple.linear[ind.val]<-predict(model.simple,newdata=lv4_lung[ind.val,])
  
  model.spline<-lm(rate~bs(YEAR,knots=c(2005,2010),degree=1),data=lv4_lung[ind.tr,])
  lv4_lung$linear.spline[ind.tr]<-model.spline$fitted.values
  lv4_lung$linear.spline[ind.val]<-predict(model.spline,newdata=lv4_lung[ind.val,])
  
  MAPE_lv4_lung[i,c(2,3)]<-c(round(mean(abs(lv4_lung$rate[ind.val]-lv4_lung$simple.linear[ind.val])/lv4_lung$rate[ind.val])*100,2),
                                 round(mean(abs(lv4_lung$rate[ind.val]-lv4_lung$linear.spline[ind.val])/lv4_lung$rate[ind.val])*100,2))
}
MAPE_lv4_lung<-as.data.frame(MAPE_lv4_lung)
MAPE_lv4_lung$simple_linear<-ifelse(MAPE_lv4_lung$simple_linear=="NaN",-99,MAPE_lv4_lung$simple_linear)
MAPE_lv4_lung$linear_spline<-ifelse(MAPE_lv4_lung$linear_spline=="NaN",-99,MAPE_lv4_lung$linear_spline)

lv4_lung_M<-lv4_lung%>%filter(SEX=='M')%>%as_tibble()
lv4_lung_F<-lv4_lung%>%filter(SEX=='F')%>%as_tibble()

ind.M<-grep("M",MAPE_lv4_lung[,1])
MAPE_lv4_lung_M<-MAPE_lv4_lung[ind.M,]

ind.F<-grep("F",MAPE_lv4_lung[,1])
MAPE_lv4_lung_F<-MAPE_lv4_lung[ind.F,]

ggplot()+geom_line(data=lv4_lung_M,aes(x=YEAR,y=rate,color="red"),size=0.5)+labs(y="rate_of_lung")+
  facet_wrap(~ factor(NAME,levels=unique(lv4_FY_BZ_lung_M$NAME)),scales="free_y")+
  geom_line(data=lv4_lung_M,aes(x=YEAR,y=simple.linear,color="black"),size=0.5)+ggtitle("Lung cancer level4 Data(Male) comparing methods performance")+
  geom_line(data=lv4_lung_M,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=0.5)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 0.3)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.2)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.2)+
  geom_text(size=3,data=as.data.frame(MAPE_lv4_lung_M),mapping = aes(x = 2010, y = Inf, label = simple_linear),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv4_lung_M),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of lung cancer Male Data',
                       breaks=c("red","black","darkviolet"),
                       labels=c("true value of leukemia","simple linear regression","linear spline"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))




ggplot()+geom_line(data=lv4_lung_F,aes(x=YEAR,y=rate,color="red"),size=0.5)+labs(y="rate_of_lung_cancer")+
  facet_wrap(~ factor(NAME,levels=unique(lv4_FY_BZ_lung_F$NAME)),scales="free_y")+
  geom_line(data=lv4_lung_F,aes(x=YEAR,y=simple.linear,color="black"),size=0.5)+ggtitle("Lung cancer level4 Data(Female) comparing methods performance")+
  geom_line(data=lv4_lung_F,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=0.5)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 0.3)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.2)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.2)+
  geom_text(size=3,data=as.data.frame(MAPE_lv4_lung_F),mapping = aes(x = 2010, y = Inf, label = simple_linear),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv4_lung_F),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of lung cancer Female Data',
                       breaks=c("red","black","darkviolet"),
                       labels=c("true value of leukemia","simple linear regression","linear spline"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

MAPE_lv4_lung$simple_linear<-as.numeric(MAPE_lv4_lung$simple_linear)
MAPE_lv4_lung$linear_spline<-as.numeric(MAPE_lv4_lung$linear_spline)
MAPE_lv4_lung$min_MAPE<-ifelse(MAPE_lv4_lung$simple_linear<=MAPE_lv4_lung$linear_spline,MAPE_lv4_lung$simple_linear,MAPE_lv4_lung$linear_spline)

index<-seq(1,nrow(MAPE_lv4_lung),by=2)
lv4_lung_mean_MAPE<-matrix(0,nrow=length(index),ncol=3)
lv4_lung_mean_MAPE[,1]<-substr(MAPE_lv4_lung[index,1],1,nchar(MAPE_lv4_lung[index,1])-1)
colnames(lv4_lung_mean_MAPE)<-c("NAME","Female","Male")

for(i in 1:length(index)){
  lv4_lung_mean_MAPE[i,2]<-MAPE_lv4_lung$min_MAPE[index[i]]
  lv4_lung_mean_MAPE[i,3]<-MAPE_lv4_lung$min_MAPE[(index[i]+1)]
}
lv4_lung_mean_MAPE<-as.data.frame(lv4_lung_mean_MAPE)
lv4_lung_mean_MAPE%>%mutate(diff=abs(abs(as.numeric(Female))-abs(as.numeric(Male))))%>%arrange(desc(diff))






######################### level 5 ##################################
######### leukemia data #######
lv5_leukemia<-df_leukemia1%>%group_by(UP1,UP2,SEX,CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  mutate(NAME=paste0("X",UP1,UP2,SEX,CAL2),rate=sum_LEUKEMIA_BZ/sum_FY_BZ)%>%as_tibble()

lv5_FY_BZ_leukemia<-lv5_leukemia%>%filter(YEAR==2018)%>%arrange(desc(sum_FY_BZ))%>%as_tibble()
lv5_FY_BZ_leukemia_M_01<-lv5_FY_BZ_leukemia%>%filter(SEX=="M" & CAL2=="01")%>%as_tibble()
lv5_FY_BZ_leukemia_F_01<-lv5_FY_BZ_leukemia%>%filter(SEX=="F" & CAL2=="01")%>%as_tibble()
lv5_FY_BZ_leukemia_M_234<-lv5_FY_BZ_leukemia%>%filter(SEX=="M" & CAL2=="234")%>%as_tibble()
lv5_FY_BZ_leukemia_F_234<-lv5_FY_BZ_leukemia%>%filter(SEX=="F" & CAL2=="234")%>%as_tibble()

lv5_leukemia$simple.linear<-0
lv5_leukemia$linear.spline<-0

name_value<-unique(lv5_leukemia$NAME)
MAPE_lv5_leukemia<-matrix(0,nrow=length(name_value),ncol=3)
MAPE_lv5_leukemia[,1]<-name_value
colnames(MAPE_lv5_leukemia)<-c("NAME","simple_linear","linear_spline")

for(i in 1:length(name_value)){
  ind.tr<-which(lv5_leukemia$NAME==name_value[i] & lv5_leukemia$YEAR<=2014)
  ind.val<-which(lv5_leukemia$NAME==name_value[i] & lv5_leukemia$YEAR>2014)
  
  model.simple<-lm(rate~YEAR,data=lv5_leukemia[ind.tr,])
  lv5_leukemia$simple.linear[ind.tr]<-model.simple$fitted.values
  lv5_leukemia$simple.linear[ind.val]<-predict(model.simple,newdata=lv5_leukemia[ind.val,])
  
  model.spline<-lm(rate~bs(YEAR,knots=c(2005,2010),degree=1),data=lv5_leukemia[ind.tr,])
  lv5_leukemia$linear.spline[ind.tr]<-model.spline$fitted.values
  lv5_leukemia$linear.spline[ind.val]<-predict(model.spline,newdata=lv5_leukemia[ind.val,])
  
  MAPE_lv5_leukemia[i,c(2,3)]<-c(round(mean(abs(lv5_leukemia$rate[ind.val]-lv5_leukemia$simple.linear[ind.val])/lv5_leukemia$rate[ind.val])*100,2),
                                 round(mean(abs(lv5_leukemia$rate[ind.val]-lv5_leukemia$linear.spline[ind.val])/lv5_leukemia$rate[ind.val])*100,2))
}
MAPE_lv5_leukemia<-as.data.frame(MAPE_lv5_leukemia)
MAPE_lv5_leukemia$simple_linear<-ifelse(MAPE_lv5_leukemia$simple_linear=="NaN",-99,MAPE_lv5_leukemia$simple_linear)
MAPE_lv5_leukemia$linear_spline<-ifelse(MAPE_lv5_leukemia$linear_spline=="NaN",-99,MAPE_lv5_leukemia$linear_spline)

lv5_leukemia_M_01<-lv5_leukemia%>%filter(SEX=='M' & CAL2=="01")%>%as_tibble()
lv5_leukemia_F_01<-lv5_leukemia%>%filter(SEX=='F' & CAL2=="01")%>%as_tibble()
lv5_leukemia_M_234<-lv5_leukemia%>%filter(SEX=='M' & CAL2=="234")%>%as_tibble()
lv5_leukemia_F_234<-lv5_leukemia%>%filter(SEX=='F' & CAL2=="234")%>%as_tibble()

ind.M.01<-which(grepl("M",MAPE_lv5_leukemia[,1]) & grepl("01",MAPE_lv5_leukemia[,1]))
MAPE_lv5_leukemia_M_01<-MAPE_lv5_leukemia[ind.M.01,]

ind.F.01<-which(grepl("F",MAPE_lv5_leukemia[,1]) & grepl("01",MAPE_lv5_leukemia[,1]))
MAPE_lv5_leukemia_F_01<-MAPE_lv5_leukemia[ind.F.01,]

ind.M.234<-which(grepl("M",MAPE_lv5_leukemia[,1]) & grepl("234",MAPE_lv5_leukemia[,1]))
MAPE_lv5_leukemia_M_234<-MAPE_lv5_leukemia[ind.M.234,]

ind.F.234<-which(grepl("F",MAPE_lv5_leukemia[,1]) & grepl("234",MAPE_lv5_leukemia[,1]))
MAPE_lv5_leukemia_F_234<-MAPE_lv5_leukemia[ind.F.234,]

ggplot()+geom_line(data=lv5_leukemia_M_01,aes(x=YEAR,y=rate,color="red"),size=0.5)+labs(y="rate_of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(lv5_FY_BZ_leukemia_M_01$NAME)),scales="free_y")+
  geom_line(data=lv5_leukemia_M_01,aes(x=YEAR,y=simple.linear,color="black"),size=0.5)+ggtitle("Leukemia level5 Data(Male and CAL01) comparing methods performance")+
  geom_line(data=lv5_leukemia_M_01,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=0.5)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 0.3)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.2)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.2)+
  geom_text(size=3,data=as.data.frame(MAPE_lv5_leukemia_M_01),mapping = aes(x = 2010, y = Inf, label = simple_linear),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv5_leukemia_M_01),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of leukemia Male and CAL01 Data',
                       breaks=c("red","black","darkviolet"),
                       labels=c("true value of leukemia","simple linear regression","linear spline"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


ggplot()+geom_line(data=lv5_leukemia_F_01,aes(x=YEAR,y=rate,color="red"),size=0.5)+labs(y="rate_of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(lv5_FY_BZ_leukemia_F_01$NAME)),scales="free_y")+
  geom_line(data=lv5_leukemia_F_01,aes(x=YEAR,y=simple.linear,color="black"),size=0.5)+ggtitle("Leukemia level5 Data(Female and CAL01) comparing methods performance")+
  geom_line(data=lv5_leukemia_F_01,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=0.5)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 0.3)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.2)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.2)+
  geom_text(size=3,data=as.data.frame(MAPE_lv5_leukemia_F_01),mapping = aes(x = 2011, y = Inf, label = simple_linear),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv5_leukemia_F_01),mapping = aes(x = 2003, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of leukemia Female and CAL01 Data',
                       breaks=c("red","black","darkviolet"),
                       labels=c("true value of leukemia","simple linear regression","linear spline"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))



ggplot()+geom_line(data=lv5_leukemia_M_234,aes(x=YEAR,y=rate,color="red"),size=0.5)+labs(y="rate_of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(lv5_FY_BZ_leukemia_M_234$NAME)),scales="free_y")+
  geom_line(data=lv5_leukemia_M_234,aes(x=YEAR,y=simple.linear,color="black"),size=0.5)+ggtitle("Leukemia level5 Data(Male and CAL234) comparing methods performance")+
  geom_line(data=lv5_leukemia_M_234,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=0.5)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 0.3)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.2)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.2)+
  geom_text(size=3,data=as.data.frame(MAPE_lv5_leukemia_M_234),mapping = aes(x = 2012, y = Inf, label = simple_linear),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv5_leukemia_M_234),mapping = aes(x = 2005, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of leukemia Male and CAL234 Data',
                       breaks=c("red","black","darkviolet"),
                       labels=c("true value of leukemia","simple linear regression","linear spline"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))



ggplot()+geom_line(data=lv5_leukemia_F_234,aes(x=YEAR,y=rate,color="red"),size=0.5)+labs(y="rate_of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(lv5_FY_BZ_leukemia_F_234$NAME)),scales="free_y")+
  geom_line(data=lv5_leukemia_F_234,aes(x=YEAR,y=simple.linear,color="black"),size=0.5)+ggtitle("Leukemia level5 Data(Female and CAL234) comparing methods performance")+
  geom_line(data=lv5_leukemia_F_234,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=0.5)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 0.3)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.2)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.2)+
  geom_text(size=3,data=as.data.frame(MAPE_lv5_leukemia_F_234),mapping = aes(x = 2013, y = Inf, label = simple_linear),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv5_leukemia_F_234),mapping = aes(x = 2005, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of leukemia Female and CAL234 Data',
                       breaks=c("red","black","darkviolet"),
                       labels=c("true value of leukemia","simple linear regression","linear spline"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))





MAPE_lv5_leukemia$simple_linear<-as.numeric(MAPE_lv5_leukemia$simple_linear)
MAPE_lv5_leukemia$linear_spline<-as.numeric(MAPE_lv5_leukemia$linear_spline)
MAPE_lv5_leukemia$min_MAPE<-ifelse(MAPE_lv5_leukemia$simple_linear<=MAPE_lv5_leukemia$linear_spline,MAPE_lv5_leukemia$simple_linear,
                                   MAPE_lv5_leukemia$linear_spline)

index<-seq(1,nrow(MAPE_lv5_leukemia),by=4)
lv5_leukemia_mean_MAPE<-matrix(0,nrow=length(index),ncol=5)
lv5_leukemia_mean_MAPE[,1]<-substr(MAPE_lv5_leukemia[index,1],1,4)
colnames(lv5_leukemia_mean_MAPE)<-c("NAME","Female01","Female234","Male01","Male234")

for(i in 1:length(index)){
  lv5_leukemia_mean_MAPE[i,2]<-MAPE_lv5_leukemia$min_MAPE[index[i]]
  lv5_leukemia_mean_MAPE[i,3]<-MAPE_lv5_leukemia$min_MAPE[(index[i]+1)]
  lv5_leukemia_mean_MAPE[i,4]<-MAPE_lv5_leukemia$min_MAPE[(index[i]+2)]
  lv5_leukemia_mean_MAPE[i,5]<-MAPE_lv5_leukemia$min_MAPE[(index[i]+3)]
}

lv5_leukemia_mean_MAPE<-as.data.frame(lv5_leukemia_mean_MAPE)
lv5_leukemia_mean_MAPE%>%mutate(diff_M=abs(abs(as.numeric(Male01))-abs(as.numeric(Male234))),
                                diff_F=abs(abs(as.numeric(Female01))-abs(as.numeric(Female234))))%>%arrange(desc(diff_M),desc(diff_F))








###########################################################################################################################
######### lung cancer data #######
lv5_lung<-df_lung1%>%group_by(UP1,UP2,SEX,CAL2,YEAR)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE),sum_LUNG_BZ=sum(LUNG_BZ))%>%
  mutate(NAME=paste0("X",UP1,UP2,SEX,CAL2),rate=sum_LUNG_BZ/sum_FY_BZ)%>%as_tibble()

lv5_FY_BZ_lung<-lv5_lung%>%filter(YEAR==2018)%>%arrange(desc(sum_FY_BZ))%>%as_tibble()
lv5_FY_BZ_lung_M_01<-lv5_FY_BZ_lung%>%filter(SEX=="M" & CAL2=="01")%>%as_tibble()
lv5_FY_BZ_lung_F_01<-lv5_FY_BZ_lung%>%filter(SEX=="F" & CAL2=="01")%>%as_tibble()
lv5_FY_BZ_lung_M_234<-lv5_FY_BZ_lung%>%filter(SEX=="M" & CAL2=="234")%>%as_tibble()
lv5_FY_BZ_lung_F_234<-lv5_FY_BZ_lung%>%filter(SEX=="F" & CAL2=="234")%>%as_tibble()

lv5_lung$simple.linear<-0
lv5_lung$linear.spline<-0

name_value<-unique(lv5_lung$NAME)
MAPE_lv5_lung<-matrix(0,nrow=length(name_value),ncol=3)
MAPE_lv5_lung[,1]<-name_value
colnames(MAPE_lv5_lung)<-c("NAME","simple_linear","linear_spline")

for(i in 1:length(name_value)){
  ind.tr<-which(lv5_lung$NAME==name_value[i] & lv5_lung$YEAR<=2014)
  ind.val<-which(lv5_lung$NAME==name_value[i] & lv5_lung$YEAR>2014)
  
  model.simple<-lm(rate~YEAR,data=lv5_lung[ind.tr,])
  lv5_lung$simple.linear[ind.tr]<-model.simple$fitted.values
  lv5_lung$simple.linear[ind.val]<-predict(model.simple,newdata=lv5_lung[ind.val,])
  
  model.spline<-lm(rate~bs(YEAR,knots=c(2005,2010),degree=1),data=lv5_lung[ind.tr,])
  lv5_lung$linear.spline[ind.tr]<-model.spline$fitted.values
  lv5_lung$linear.spline[ind.val]<-predict(model.spline,newdata=lv5_lung[ind.val,])
  
  MAPE_lv5_lung[i,c(2,3)]<-c(round(mean(abs(lv5_lung$rate[ind.val]-lv5_lung$simple.linear[ind.val])/lv5_lung$rate[ind.val])*100,2),
                                 round(mean(abs(lv5_lung$rate[ind.val]-lv5_lung$linear.spline[ind.val])/lv5_lung$rate[ind.val])*100,2))
}
MAPE_lv5_lung<-as.data.frame(MAPE_lv5_lung)
MAPE_lv5_lung$simple_linear<-ifelse(MAPE_lv5_lung$simple_linear=="NaN",-99,MAPE_lv5_lung$simple_linear)
MAPE_lv5_lung$linear_spline<-ifelse(MAPE_lv5_lung$linear_spline=="NaN",-99,MAPE_lv5_lung$linear_spline)

lv5_lung_M_01<-lv5_lung%>%filter(SEX=='M' & CAL2=="01")%>%as_tibble()
lv5_lung_F_01<-lv5_lung%>%filter(SEX=='F' & CAL2=="01")%>%as_tibble()
lv5_lung_M_234<-lv5_lung%>%filter(SEX=='M' & CAL2=="234")%>%as_tibble()
lv5_lung_F_234<-lv5_lung%>%filter(SEX=='F' & CAL2=="234")%>%as_tibble()

ind.M.01<-which(grepl("M",MAPE_lv5_lung[,1]) & grepl("01",MAPE_lv5_lung[,1]))
MAPE_lv5_lung_M_01<-MAPE_lv5_lung[ind.M.01,]

ind.F.01<-which(grepl("F",MAPE_lv5_lung[,1]) & grepl("01",MAPE_lv5_lung[,1]))
MAPE_lv5_lung_F_01<-MAPE_lv5_lung[ind.F.01,]

ind.M.234<-which(grepl("M",MAPE_lv5_lung[,1]) & grepl("234",MAPE_lv5_lung[,1]))
MAPE_lv5_lung_M_234<-MAPE_lv5_lung[ind.M.234,]

ind.F.234<-which(grepl("F",MAPE_lv5_lung[,1]) & grepl("234",MAPE_lv5_lung[,1]))
MAPE_lv5_lung_F_234<-MAPE_lv5_lung[ind.F.234,]

ggplot()+geom_line(data=lv5_lung_M_01,aes(x=YEAR,y=rate,color="red"),size=0.5)+labs(y="rate_of_lung cancer")+
  facet_wrap(~ factor(NAME,levels=unique(lv5_FY_BZ_lung_M_01$NAME)),scales="free_y")+
  geom_line(data=lv5_lung_M_01,aes(x=YEAR,y=simple.linear,color="black"),size=0.5)+ggtitle("Lung cancer level5 Data(Male and CAL01) comparing methods performance")+
  geom_line(data=lv5_lung_M_01,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=0.5)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 0.3)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.2)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.2)+
  geom_text(size=3,data=as.data.frame(MAPE_lv5_lung_M_01),mapping = aes(x = 2010, y = Inf, label = simple_linear),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv5_lung_M_01),mapping = aes(x = 2002, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of lung cancer Male and CAL01 Data',
                       breaks=c("red","black","darkviolet"),
                       labels=c("true value of lung cancer","simple linear regression","linear spline"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


ggplot()+geom_line(data=lv5_lung_F_01,aes(x=YEAR,y=rate,color="red"),size=0.5)+labs(y="rate_of_lung cancer")+
  facet_wrap(~ factor(NAME,levels=unique(lv5_FY_BZ_lung_F_01$NAME)),scales="free_y")+
  geom_line(data=lv5_lung_F_01,aes(x=YEAR,y=simple.linear,color="black"),size=0.5)+ggtitle("Lung cancer level5 Data(Female and CAL01) comparing methods performance")+
  geom_line(data=lv5_lung_F_01,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=0.5)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 0.3)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.2)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.2)+
  geom_text(size=3,data=as.data.frame(MAPE_lv5_lung_F_01),mapping = aes(x = 2011, y = Inf, label = simple_linear),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv5_lung_F_01),mapping = aes(x = 2003, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of lung cancer Female and CAL01 Data',
                       breaks=c("red","black","darkviolet"),
                       labels=c("true value of lung cancer","simple linear regression","linear spline"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))



ggplot()+geom_line(data=lv5_lung_M_234,aes(x=YEAR,y=rate,color="red"),size=0.5)+labs(y="rate_of_lung cancer")+
  facet_wrap(~ factor(NAME,levels=unique(lv5_FY_BZ_lung_M_234$NAME)),scales="free_y")+
  geom_line(data=lv5_lung_M_234,aes(x=YEAR,y=simple.linear,color="black"),size=0.5)+ggtitle("Lung cancer level5 Data(Male and CAL234) comparing methods performance")+
  geom_line(data=lv5_lung_M_234,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=0.5)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 0.3)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.2)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.2)+
  geom_text(size=3,data=as.data.frame(MAPE_lv5_lung_M_234),mapping = aes(x = 2012, y = Inf, label = simple_linear),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv5_lung_M_234),mapping = aes(x = 2005, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of lung cancer Male and CAL234 Data',
                       breaks=c("red","black","darkviolet"),
                       labels=c("true value of lung cancer","simple linear regression","linear spline"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))



ggplot()+geom_line(data=lv5_lung_F_234,aes(x=YEAR,y=rate,color="red"),size=0.5)+labs(y="rate_of_lung cancer")+
  facet_wrap(~ factor(NAME,levels=unique(lv5_FY_BZ_lung_F_234$NAME)),scales="free_y")+
  geom_line(data=lv5_lung_F_234,aes(x=YEAR,y=simple.linear,color="black"),size=0.5)+ggtitle("Lung cancer level5 Data(Female and CAL234) comparing methods performance")+
  geom_line(data=lv5_lung_F_234,aes(x=YEAR,y=linear.spline,color='darkviolet'),size=0.5)+
  geom_vline(xintercept=2014, linetype = 'twodash', color='blue', size = 0.3)+
  geom_vline(xintercept=2005, linetype = 'dashed', color='chocolate1', size = 0.2)+
  geom_vline(xintercept=2010, linetype = 'dashed', color='chocolate1', size = 0.2)+
  geom_text(size=3,data=as.data.frame(MAPE_lv5_lung_F_234),mapping = aes(x = 2013, y = Inf, label = simple_linear),vjust=1.5,colour="black",fontface="bold")+
  geom_text(size=3,data=as.data.frame(MAPE_lv5_lung_F_234),mapping = aes(x = 2005, y = Inf, label = linear_spline),vjust=1.5,colour="purple",fontface="bold")+
  labs(x="Year")+
  scale_color_identity(name='rate of lung cancer Female and CAL234 Data',
                       breaks=c("red","black","darkviolet"),
                       labels=c("true value of lung cancer","simple linear regression","linear spline"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))





MAPE_lv5_lung$simple_linear<-as.numeric(MAPE_lv5_lung$simple_linear)
MAPE_lv5_lung$linear_spline<-as.numeric(MAPE_lv5_lung$linear_spline)
MAPE_lv5_lung$min_MAPE<-ifelse(MAPE_lv5_lung$simple_linear<=MAPE_lv5_lung$linear_spline,MAPE_lv5_lung$simple_linear,
                                   MAPE_lv5_lung$linear_spline)

index<-seq(1,nrow(MAPE_lv5_lung),by=4)
lv5_lung_mean_MAPE<-matrix(0,nrow=length(index),ncol=5)
lv5_lung_mean_MAPE[,1]<-substr(MAPE_lv5_lung[index,1],1,4)
colnames(lv5_lung_mean_MAPE)<-c("NAME","Female01","Female234","Male01","Male234")

for(i in 1:length(index)){
  lv5_lung_mean_MAPE[i,2]<-MAPE_lv5_lung$min_MAPE[index[i]]
  lv5_lung_mean_MAPE[i,3]<-MAPE_lv5_lung$min_MAPE[(index[i]+1)]
  lv5_lung_mean_MAPE[i,4]<-MAPE_lv5_lung$min_MAPE[(index[i]+2)]
  lv5_lung_mean_MAPE[i,5]<-MAPE_lv5_lung$min_MAPE[(index[i]+3)]
}

lv5_lung_mean_MAPE<-as.data.frame(lv5_lung_mean_MAPE)
lv5_lung_mean_MAPE%>%mutate(diff_M=abs(abs(as.numeric(Male01))-abs(as.numeric(Male234))),
                                diff_F=abs(abs(as.numeric(Female01))-abs(as.numeric(Female234))))%>%arrange(desc(diff_M))
