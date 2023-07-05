library(dtplyr)
library(tidyverse)
library(data.table)
setDTthreads(24)
library(stringr)
library(splines)

##### test set 시각화 ####
test_set<-read.csv("test_postprocessed.csv")

validation_v2 <- read.csv("validation_v2.csv")
LEVEL<-validation_v2[,c("NAME","LEVEL")]
LEVEL<-unique(LEVEL)

result<-left_join(test_set,LEVEL,by='NAME')


## 1-1. LEVEL L1 Data 시각화 --------------------------------------------------
L1<-result%>%filter(LEVEL=="L1")%>%as_tibble()

pdf("picture.pdf")

ggplot()+geom_line(data=L1,aes(x=YEAR,y=LEUKEMIA,color="red"),size=2)+labs(y="#of_leukemia")+
  geom_line(data=L1,aes(x=YEAR,y=LEUKEMIA_lin,color="black"),size=1)+ggtitle("Performance of several methods about LEUKEMIA")+
  geom_line(data=L1,aes(x=YEAR,y=LEUKEMIA_linSpline,color="green"),size=1)+
  geom_line(data=L1,aes(x=YEAR,y=LEUKEMIA_quad,color='darkviolet'),size=1)+
  geom_line(data=L1,aes(x=YEAR,y=LEUKEMIA_ensem,color='deeppink1'),size=1)+
  geom_line(data=L1,aes(x=YEAR,y=LEUKEMIA_ensem_adj,color='Cyan'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='#of leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1","Cyan"),
                       labels=c("true value of leukemia","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=L1,aes(x=YEAR,y=LUNG,color="red"),size=2)+labs(y="#of_lung")+
  geom_line(data=L1,aes(x=YEAR,y=LUNG_lin,color="black"),size=1)+ggtitle("Performance of several methods about LUNG")+
  geom_line(data=L1,aes(x=YEAR,y=LUNG_linSpline,color="green"),size=1)+
  geom_line(data=L1,aes(x=YEAR,y=LUNG_quad,color='darkviolet'),size=1)+
  geom_line(data=L1,aes(x=YEAR,y=LUNG_ensem,color='deeppink1'),size=1)+
  geom_line(data=L1,aes(x=YEAR,y=LUNG_ensem_adj,color='Cyan'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='#of lung',
                       breaks=c("red","black","green","darkviolet","deeppink1","Cyan"),
                       labels=c("true value of lung cancer","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=L1,aes(x=YEAR,y=FY_LEUKEMIA,color="red"),size=2)+labs(y="FY_BZ_leukemia")+
  geom_line(data=L1,aes(x=YEAR,y=FY_LEUKEMIA_lin,color="black"),size=1)+ggtitle("Performance of several methods about FY_LEUKEMIA")+
  geom_line(data=L1,aes(x=YEAR,y=FY_LEUKEMIA_linSpline,color="green"),size=1)+
  geom_line(data=L1,aes(x=YEAR,y=FY_LEUKEMIA_quad,color='darkviolet'),size=1)+
  geom_line(data=L1,aes(x=YEAR,y=FY_LEUKEMIA_ensem,color='deeppink1'),size=1)+
  geom_line(data=L1,aes(x=YEAR,y=FY_LEUKEMIA_ensem_adj,color='Olive Drab'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='FY_BZ leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1","Olive Drab"),
                       labels=c("true value of FY_BZ_LEUKEMIA","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


ggplot()+geom_line(data=L1,aes(x=YEAR,y=FY_LUNG,color="red"),size=2)+labs(y="FY_BZ_LUNG")+
  geom_line(data=L1,aes(x=YEAR,y=FY_LUNG_lin,color="black"),size=1)+ggtitle("Performance of several methods about FY_LUNG")+
  geom_line(data=L1,aes(x=YEAR,y=FY_LUNG_linSpline,color="green"),size=1)+
  geom_line(data=L1,aes(x=YEAR,y=FY_LUNG_quad,color='darkviolet'),size=1)+
  geom_line(data=L1,aes(x=YEAR,y=FY_LUNG_ensem,color='deeppink1'),size=1)+
  geom_line(data=L1,aes(x=YEAR,y=FY_LUNG_ensem_adj,color='Olive Drab'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='FY_BZ lung',
                       breaks=c("red","black","green","darkviolet","deeppink1","Olive Drab"),
                       labels=c("true value of FY_BZ_LEUKEMIA","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))







## 1-2. LEVEL L1a Data 시각화 -------------------------------------------------
L1a<-result%>%filter(LEVEL=="L1a")%>%as_tibble()

ggplot()+geom_line(data=L1a,aes(x=YEAR,y=LEUKEMIA,color="red"),size=2)+labs(y="#of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(L1a$NAME)),scales="free_y")+
  geom_line(data=L1a,aes(x=YEAR,y=LEUKEMIA_lin,color="black"),size=1)+ggtitle("Performance of several methods about LEUKEMIA")+
  geom_line(data=L1a,aes(x=YEAR,y=LEUKEMIA_linSpline,color="green"),size=1)+
  geom_line(data=L1a,aes(x=YEAR,y=LEUKEMIA_quad,color='darkviolet'),size=1)+
  geom_line(data=L1a,aes(x=YEAR,y=LEUKEMIA_ensem,color='deeppink1'),size=1)+
  geom_line(data=L1a,aes(x=YEAR,y=LEUKEMIA_ensem_adj,color='Cyan'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='#of leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1","Cyan"),
                       labels=c("true value of leukemia","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=L1a,aes(x=YEAR,y=LUNG,color="red"),size=2)+labs(y="#of_lung")+
  facet_wrap(~ factor(NAME,levels=unique(L1a$NAME)),scales="free_y")+
  geom_line(data=L1a,aes(x=YEAR,y=LUNG_lin,color="black"),size=1)+ggtitle("Performance of several methods about LUNG")+
  geom_line(data=L1a,aes(x=YEAR,y=LUNG_linSpline,color="green"),size=1)+
  geom_line(data=L1a,aes(x=YEAR,y=LUNG_quad,color='darkviolet'),size=1)+
  geom_line(data=L1a,aes(x=YEAR,y=LUNG_ensem,color='deeppink1'),size=1)+
  geom_line(data=L1a,aes(x=YEAR,y=LUNG_ensem_adj,color='Cyan'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='#of lung',
                       breaks=c("red","black","green","darkviolet","deeppink1","Cyan"),
                       labels=c("true value of lung cancer","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=L1a,aes(x=YEAR,y=FY_LEUKEMIA,color="red"),size=2)+labs(y="FY_BZ_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(L1a$NAME)),scales="free_y")+
  geom_line(data=L1a,aes(x=YEAR,y=FY_LEUKEMIA_lin,color="black"),size=1)+ggtitle("Performance of several methods about FY_LEUKEMIA")+
  geom_line(data=L1a,aes(x=YEAR,y=FY_LEUKEMIA_linSpline,color="green"),size=1)+
  geom_line(data=L1a,aes(x=YEAR,y=FY_LEUKEMIA_quad,color='darkviolet'),size=1)+
  geom_line(data=L1a,aes(x=YEAR,y=FY_LEUKEMIA_ensem,color='deeppink1'),size=1)+
  geom_line(data=L1a,aes(x=YEAR,y=FY_LEUKEMIA_ensem_adj,color='Olive Drab'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='FY_BZ leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1","Olive Drab"),
                       labels=c("true value of FY_BZ_LEUKEMIA","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


ggplot()+geom_line(data=L1a,aes(x=YEAR,y=FY_LUNG,color="red"),size=2)+labs(y="FY_BZ_LUNG")+
  facet_wrap(~ factor(NAME,levels=unique(L1a$NAME)),scales="free_y")+
  geom_line(data=L1a,aes(x=YEAR,y=FY_LUNG_lin,color="black"),size=1)+ggtitle("Performance of several methods about FY_LUNG")+
  geom_line(data=L1a,aes(x=YEAR,y=FY_LUNG_linSpline,color="green"),size=1)+
  geom_line(data=L1a,aes(x=YEAR,y=FY_LUNG_quad,color='darkviolet'),size=1)+
  geom_line(data=L1a,aes(x=YEAR,y=FY_LUNG_ensem,color='deeppink1'),size=1)+
  geom_line(data=L1a,aes(x=YEAR,y=FY_LUNG_ensem_adj,color='Olive Drab'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='FY_BZ lung',
                       breaks=c("red","black","green","darkviolet","deeppink1","Olive Drab"),
                       labels=c("true value of FY_BZ_LEUKEMIA","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


## 1-3. LEVEL L1b Data 시각화 -------------------------------------------------
L1b<-result%>%filter(LEVEL=="L1b")%>%as_tibble()

ggplot()+geom_line(data=L1b,aes(x=YEAR,y=LEUKEMIA,color="red"),size=2)+labs(y="#of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(L1b$NAME)),scales="free_y")+
  geom_line(data=L1b,aes(x=YEAR,y=LEUKEMIA_lin,color="black"),size=1)+ggtitle("Performance of several methods about LEUKEMIA")+
  geom_line(data=L1b,aes(x=YEAR,y=LEUKEMIA_linSpline,color="green"),size=1)+
  geom_line(data=L1b,aes(x=YEAR,y=LEUKEMIA_quad,color='darkviolet'),size=1)+
  geom_line(data=L1b,aes(x=YEAR,y=LEUKEMIA_ensem,color='deeppink1'),size=1)+
  geom_line(data=L1b,aes(x=YEAR,y=LEUKEMIA_ensem_adj,color='Cyan'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='#of leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1","Cyan"),
                       labels=c("true value of leukemia","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=L1b,aes(x=YEAR,y=LUNG,color="red"),size=2)+labs(y="#of_lung")+
  facet_wrap(~ factor(NAME,levels=unique(L1b$NAME)),scales="free_y")+
  geom_line(data=L1b,aes(x=YEAR,y=LUNG_lin,color="black"),size=1)+ggtitle("Performance of several methods about LUNG")+
  geom_line(data=L1b,aes(x=YEAR,y=LUNG_linSpline,color="green"),size=1)+
  geom_line(data=L1b,aes(x=YEAR,y=LUNG_quad,color='darkviolet'),size=1)+
  geom_line(data=L1b,aes(x=YEAR,y=LUNG_ensem,color='deeppink1'),size=1)+
  geom_line(data=L1b,aes(x=YEAR,y=LUNG_ensem_adj,color='Cyan'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='#of lung',
                       breaks=c("red","black","green","darkviolet","deeppink1","Cyan"),
                       labels=c("true value of lung cancer","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=L1b,aes(x=YEAR,y=FY_LEUKEMIA,color="red"),size=2)+labs(y="FY_BZ_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(L1b$NAME)),scales="free_y")+
  geom_line(data=L1b,aes(x=YEAR,y=FY_LEUKEMIA_lin,color="black"),size=1)+ggtitle("Performance of several methods about FY_LEUKEMIA")+
  geom_line(data=L1b,aes(x=YEAR,y=FY_LEUKEMIA_linSpline,color="green"),size=1)+
  geom_line(data=L1b,aes(x=YEAR,y=FY_LEUKEMIA_quad,color='darkviolet'),size=1)+
  geom_line(data=L1b,aes(x=YEAR,y=FY_LEUKEMIA_ensem,color='deeppink1'),size=1)+
  geom_line(data=L1b,aes(x=YEAR,y=FY_LEUKEMIA_ensem_adj,color='Olive Drab'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='FY_BZ leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1","Olive Drab"),
                       labels=c("true value of FY_BZ_LEUKEMIA","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


ggplot()+geom_line(data=L1b,aes(x=YEAR,y=FY_LUNG,color="red"),size=2)+labs(y="FY_BZ_LUNG")+
  facet_wrap(~ factor(NAME,levels=unique(L1b$NAME)),scales="free_y")+
  geom_line(data=L1b,aes(x=YEAR,y=FY_LUNG_lin,color="black"),size=1)+ggtitle("Performance of several methods about FY_LUNG")+
  geom_line(data=L1b,aes(x=YEAR,y=FY_LUNG_linSpline,color="green"),size=1)+
  geom_line(data=L1b,aes(x=YEAR,y=FY_LUNG_quad,color='darkviolet'),size=1)+
  geom_line(data=L1b,aes(x=YEAR,y=FY_LUNG_ensem,color='deeppink1'),size=1)+
  geom_line(data=L1b,aes(x=YEAR,y=FY_LUNG_ensem_adj,color='Olive Drab'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='FY_BZ lung',
                       breaks=c("red","black","green","darkviolet","deeppink1","Olive Drab"),
                       labels=c("true value of FY_BZ_LEUKEMIA","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))



dev.off()



## 1-4. LEVEL L1ab Data 시각화 -------------------------------------------------
L1ab<-result%>%filter(LEVEL=="L1ab")%>%as_tibble()

ggplot()+geom_line(data=L1ab,aes(x=YEAR,y=LEUKEMIA,color="red"),size=2)+labs(y="#of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(L1ab$NAME)),scales="free_y")+
  geom_line(data=L1ab,aes(x=YEAR,y=LEUKEMIA_lin,color="black"),size=1)+ggtitle("Performance of several methods about LEUKEMIA")+
  geom_line(data=L1ab,aes(x=YEAR,y=LEUKEMIA_linSpline,color="green"),size=1)+
  geom_line(data=L1ab,aes(x=YEAR,y=LEUKEMIA_quad,color='darkviolet'),size=1)+
  geom_line(data=L1ab,aes(x=YEAR,y=LEUKEMIA_ensem,color='deeppink1'),size=1)+
  geom_line(data=L1ab,aes(x=YEAR,y=LEUKEMIA_ensem_adj,color='Cyan'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='#of leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1","Cyan"),
                       labels=c("true value of leukemia","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=L1ab,aes(x=YEAR,y=LUNG,color="red"),size=2)+labs(y="#of_lung")+
  facet_wrap(~ factor(NAME,levels=unique(L1ab$NAME)),scales="free_y")+
  geom_line(data=L1ab,aes(x=YEAR,y=LUNG_lin,color="black"),size=1)+ggtitle("Performance of several methods about LUNG")+
  geom_line(data=L1ab,aes(x=YEAR,y=LUNG_linSpline,color="green"),size=1)+
  geom_line(data=L1ab,aes(x=YEAR,y=LUNG_quad,color='darkviolet'),size=1)+
  geom_line(data=L1ab,aes(x=YEAR,y=LUNG_ensem,color='deeppink1'),size=1)+
  geom_line(data=L1ab,aes(x=YEAR,y=LUNG_ensem_adj,color='Cyan'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='#of lung',
                       breaks=c("red","black","green","darkviolet","deeppink1","Cyan"),
                       labels=c("true value of lung cancer","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=L1ab,aes(x=YEAR,y=FY_LEUKEMIA,color="red"),size=2)+labs(y="FY_BZ_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(L1ab$NAME)),scales="free_y")+
  geom_line(data=L1ab,aes(x=YEAR,y=FY_LEUKEMIA_lin,color="black"),size=1)+ggtitle("Performance of several methods about FY_LEUKEMIA")+
  geom_line(data=L1ab,aes(x=YEAR,y=FY_LEUKEMIA_linSpline,color="green"),size=1)+
  geom_line(data=L1ab,aes(x=YEAR,y=FY_LEUKEMIA_quad,color='darkviolet'),size=1)+
  geom_line(data=L1ab,aes(x=YEAR,y=FY_LEUKEMIA_ensem,color='deeppink1'),size=1)+
  geom_line(data=L1ab,aes(x=YEAR,y=FY_LEUKEMIA_ensem_adj,color='Olive Drab'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='FY_BZ leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1","Olive Drab"),
                       labels=c("true value of FY_BZ_LEUKEMIA","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


ggplot()+geom_line(data=L1ab,aes(x=YEAR,y=FY_LUNG,color="red"),size=2)+labs(y="FY_BZ_LUNG")+
  facet_wrap(~ factor(NAME,levels=unique(L1ab$NAME)),scales="free_y")+
  geom_line(data=L1ab,aes(x=YEAR,y=FY_LUNG_lin,color="black"),size=1)+ggtitle("Performance of several methods about FY_LUNG")+
  geom_line(data=L1ab,aes(x=YEAR,y=FY_LUNG_linSpline,color="green"),size=1)+
  geom_line(data=L1ab,aes(x=YEAR,y=FY_LUNG_quad,color='darkviolet'),size=1)+
  geom_line(data=L1ab,aes(x=YEAR,y=FY_LUNG_ensem,color='deeppink1'),size=1)+
  geom_line(data=L1ab,aes(x=YEAR,y=FY_LUNG_ensem_adj,color='Olive Drab'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='FY_BZ lung',
                       breaks=c("red","black","green","darkviolet","deeppink1","Olive Drab"),
                       labels=c("true value of FY_BZ_LEUKEMIA","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))








## 1-5. LEVEL L2 Data 시각화 -------------------------------------------------
L2<-result%>%filter(LEVEL=="L2")%>%mutate(UP1=substr(NAME,2,2))%>%as_tibble()

library(readr)
code<-read_csv("한국표준산업분류10차.csv",locale = locale("ko", encoding = "euc-kr"))
code_UP1 <- code[c("대분류(21)","...2")]
code_UP1<-na.omit(code_UP1)
colnames(code_UP1)<-c("UP1","name")
code_UP1<-code_UP1[2:nrow(code_UP1),]
L2_join<-left_join(L2,code_UP1,by="UP1")

L2_join<-L2_join%>%mutate(NAME2=paste0(UP1," : ",name))%>%as_tibble()

ggplot()+geom_line(data=L2_join,aes(x=YEAR,y=LEUKEMIA,color="red"),size=2)+labs(y="#of_leukemia")+
  facet_wrap(~ factor(NAME2,levels=unique(L2_join$NAME2)),scales="free_y")+
  theme(strip.text.x = element_text(size = 6))+
  geom_line(data=L2_join,aes(x=YEAR,y=LEUKEMIA_lin,color="black"),size=1)+ggtitle("Performance of several methods about LEUKEMIA")+
  geom_line(data=L2_join,aes(x=YEAR,y=LEUKEMIA_linSpline,color="green"),size=1)+
  geom_line(data=L2_join,aes(x=YEAR,y=LEUKEMIA_quad,color='darkviolet'),size=1)+
  geom_line(data=L2_join,aes(x=YEAR,y=LEUKEMIA_ensem,color='deeppink1'),size=1)+
  geom_line(data=L2_join,aes(x=YEAR,y=LEUKEMIA_ensem_adj,color='Cyan'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='#of leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1","Cyan"),
                       labels=c("true value of leukemia","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=L2_join,aes(x=YEAR,y=LUNG,color="red"),size=2)+labs(y="#of_lung")+
  facet_wrap(~ factor(NAME2,levels=unique(L2_join$NAME2)),scales="free_y")+
  theme(strip.text.x = element_text(size = 6))+
  geom_line(data=L2_join,aes(x=YEAR,y=LUNG_lin,color="black"),size=1)+ggtitle("Performance of several methods about LUNG")+
  geom_line(data=L2_join,aes(x=YEAR,y=LUNG_linSpline,color="green"),size=1)+
  geom_line(data=L2_join,aes(x=YEAR,y=LUNG_quad,color='darkviolet'),size=1)+
  geom_line(data=L2_join,aes(x=YEAR,y=LUNG_ensem,color='deeppink1'),size=1)+
  geom_line(data=L2_join,aes(x=YEAR,y=LUNG_ensem_adj,color='Cyan'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='#of lung',
                       breaks=c("red","black","green","darkviolet","deeppink1","Cyan"),
                       labels=c("true value of lung cancer","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=L2_join,aes(x=YEAR,y=FY_LEUKEMIA,color="red"),size=2)+labs(y="FY_BZ_leukemia")+
  facet_wrap(~ factor(NAME2,levels=unique(L2_join$NAME2)),scales="free_y")+
  theme(strip.text.x = element_text(size = 6))+
  geom_line(data=L2_join,aes(x=YEAR,y=FY_LEUKEMIA_lin,color="black"),size=1)+ggtitle("Performance of several methods about FY_LEUKEMIA")+
  geom_line(data=L2_join,aes(x=YEAR,y=FY_LEUKEMIA_linSpline,color="green"),size=1)+
  geom_line(data=L2_join,aes(x=YEAR,y=FY_LEUKEMIA_quad,color='darkviolet'),size=1)+
  geom_line(data=L2_join,aes(x=YEAR,y=FY_LEUKEMIA_ensem,color='deeppink1'),size=1)+
  geom_line(data=L2_join,aes(x=YEAR,y=FY_LEUKEMIA_ensem_adj,color='Olive Drab'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='FY_BZ leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1","Olive Drab"),
                       labels=c("true value of FY_BZ_LEUKEMIA","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


ggplot()+geom_line(data=L2_join,aes(x=YEAR,y=FY_LUNG,color="red"),size=2)+labs(y="FY_BZ_LUNG")+
  facet_wrap(~ factor(NAME2,levels=unique(L2_join$NAME2)),scales="free_y")+
  theme(strip.text.x = element_text(size = 6))+
  geom_line(data=L2_join,aes(x=YEAR,y=FY_LUNG_lin,color="black"),size=1)+ggtitle("Performance of several methods about FY_LUNG")+
  geom_line(data=L2_join,aes(x=YEAR,y=FY_LUNG_linSpline,color="green"),size=1)+
  geom_line(data=L2_join,aes(x=YEAR,y=FY_LUNG_quad,color='darkviolet'),size=1)+
  geom_line(data=L2_join,aes(x=YEAR,y=FY_LUNG_ensem,color='deeppink1'),size=1)+
  geom_line(data=L2_join,aes(x=YEAR,y=FY_LUNG_ensem_adj,color='Olive Drab'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='FY_BZ lung',
                       breaks=c("red","black","green","darkviolet","deeppink1","Olive Drab"),
                       labels=c("true value of FY_BZ_LEUKEMIA","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))





## 1-6. LEVEL L2a Data 시각화 -------------------------------------------------
L2a<-result%>%filter(LEVEL=="L2a")%>%mutate(UP1=substr(NAME,2,2))%>%as_tibble()
L2a_join<-left_join(L2a,code_UP1,by="UP1")
L2a_join<-L2a_join%>%mutate(NAME2=paste0(substr(NAME,4,nchar(NAME)),"&",name))%>%as_tibble()

ggplot()+geom_line(data=L2a_join,aes(x=YEAR,y=LEUKEMIA,color="red"),size=2)+labs(y="#of_leukemia")+
  facet_wrap(~ factor(NAME2,levels=unique(L2a_join$NAME2)),scales="free_y")+
  theme(strip.text.x = element_text(size = 5))+
  geom_line(data=L2a_join,aes(x=YEAR,y=LEUKEMIA_lin,color="black"),size=1)+ggtitle("Performance of several methods about LEUKEMIA")+
  geom_line(data=L2a_join,aes(x=YEAR,y=LEUKEMIA_linSpline,color="green"),size=1)+
  geom_line(data=L2a_join,aes(x=YEAR,y=LEUKEMIA_quad,color='darkviolet'),size=1)+
  geom_line(data=L2a_join,aes(x=YEAR,y=LEUKEMIA_ensem,color='deeppink1'),size=1)+
  geom_line(data=L2a_join,aes(x=YEAR,y=LEUKEMIA_ensem_adj,color='Cyan'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='#of leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1","Cyan"),
                       labels=c("true value of leukemia","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=L2a,aes(x=YEAR,y=LUNG,color="red"),size=2)+labs(y="#of_lung")+
  facet_wrap(~ factor(NAME,levels=unique(L2a$NAME)),scales="free_y")+
  geom_line(data=L2a,aes(x=YEAR,y=LUNG_lin,color="black"),size=1)+ggtitle("Performance of several methods about LUNG")+
  geom_line(data=L2a,aes(x=YEAR,y=LUNG_linSpline,color="green"),size=1)+
  geom_line(data=L2a,aes(x=YEAR,y=LUNG_quad,color='darkviolet'),size=1)+
  geom_line(data=L2a,aes(x=YEAR,y=LUNG_ensem,color='deeppink1'),size=1)+
  geom_line(data=L2a,aes(x=YEAR,y=LUNG_ensem_adj,color='Cyan'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='#of lung',
                       breaks=c("red","black","green","darkviolet","deeppink1","Cyan"),
                       labels=c("true value of lung cancer","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=L2a,aes(x=YEAR,y=FY_LEUKEMIA,color="red"),size=2)+labs(y="FY_BZ_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(L2a$NAME)),scales="free_y")+
  geom_line(data=L2a,aes(x=YEAR,y=FY_LEUKEMIA_lin,color="black"),size=1)+ggtitle("Performance of several methods about FY_LEUKEMIA")+
  geom_line(data=L2a,aes(x=YEAR,y=FY_LEUKEMIA_linSpline,color="green"),size=1)+
  geom_line(data=L2a,aes(x=YEAR,y=FY_LEUKEMIA_quad,color='darkviolet'),size=1)+
  geom_line(data=L2a,aes(x=YEAR,y=FY_LEUKEMIA_ensem,color='deeppink1'),size=1)+
  geom_line(data=L2a,aes(x=YEAR,y=FY_LEUKEMIA_ensem_adj,color='Olive Drab'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='FY_BZ leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1","Olive Drab"),
                       labels=c("true value of FY_BZ_LEUKEMIA","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


ggplot()+geom_line(data=L2a,aes(x=YEAR,y=FY_LUNG,color="red"),size=2)+labs(y="FY_BZ_LUNG")+
  facet_wrap(~ factor(NAME,levels=unique(L2a$NAME)),scales="free_y")+
  geom_line(data=L2a,aes(x=YEAR,y=FY_LUNG_lin,color="black"),size=1)+ggtitle("Performance of several methods about FY_LUNG")+
  geom_line(data=L2a,aes(x=YEAR,y=FY_LUNG_linSpline,color="green"),size=1)+
  geom_line(data=L2a,aes(x=YEAR,y=FY_LUNG_quad,color='darkviolet'),size=1)+
  geom_line(data=L2a,aes(x=YEAR,y=FY_LUNG_ensem,color='deeppink1'),size=1)+
  geom_line(data=L2a,aes(x=YEAR,y=FY_LUNG_ensem_adj,color='Olive Drab'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='FY_BZ lung',
                       breaks=c("red","black","green","darkviolet","deeppink1","Olive Drab"),
                       labels=c("true value of FY_BZ_LEUKEMIA","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))






## 1-7. LEVEL L2b Data 시각화 -------------------------------------------------
L2b<-result%>%filter(LEVEL=="L2b")%>%as_tibble()

ggplot()+geom_line(data=L2b,aes(x=YEAR,y=LEUKEMIA,color="red"),size=2)+labs(y="#of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(L2b$NAME)),scales="free_y")+
  geom_line(data=L2b,aes(x=YEAR,y=LEUKEMIA_lin,color="black"),size=1)+ggtitle("Performance of several methods about LEUKEMIA")+
  geom_line(data=L2b,aes(x=YEAR,y=LEUKEMIA_linSpline,color="green"),size=1)+
  geom_line(data=L2b,aes(x=YEAR,y=LEUKEMIA_quad,color='darkviolet'),size=1)+
  geom_line(data=L2b,aes(x=YEAR,y=LEUKEMIA_ensem,color='deeppink1'),size=1)+
  geom_line(data=L2b,aes(x=YEAR,y=LEUKEMIA_ensem_adj,color='Cyan'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='#of leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1","Cyan"),
                       labels=c("true value of leukemia","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=L2b,aes(x=YEAR,y=LUNG,color="red"),size=2)+labs(y="#of_lung")+
  facet_wrap(~ factor(NAME,levels=unique(L2b$NAME)),scales="free_y")+
  geom_line(data=L2b,aes(x=YEAR,y=LUNG_lin,color="black"),size=1)+ggtitle("Performance of several methods about LUNG")+
  geom_line(data=L2b,aes(x=YEAR,y=LUNG_linSpline,color="green"),size=1)+
  geom_line(data=L2b,aes(x=YEAR,y=LUNG_quad,color='darkviolet'),size=1)+
  geom_line(data=L2b,aes(x=YEAR,y=LUNG_ensem,color='deeppink1'),size=1)+
  geom_line(data=L2b,aes(x=YEAR,y=LUNG_ensem_adj,color='Cyan'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='#of lung',
                       breaks=c("red","black","green","darkviolet","deeppink1","Cyan"),
                       labels=c("true value of lung cancer","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=L2b,aes(x=YEAR,y=FY_LEUKEMIA,color="red"),size=2)+labs(y="FY_BZ_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(L2b$NAME)),scales="free_y")+
  geom_line(data=L2b,aes(x=YEAR,y=FY_LEUKEMIA_lin,color="black"),size=1)+ggtitle("Performance of several methods about FY_LEUKEMIA")+
  geom_line(data=L2b,aes(x=YEAR,y=FY_LEUKEMIA_linSpline,color="green"),size=1)+
  geom_line(data=L2b,aes(x=YEAR,y=FY_LEUKEMIA_quad,color='darkviolet'),size=1)+
  geom_line(data=L2b,aes(x=YEAR,y=FY_LEUKEMIA_ensem,color='deeppink1'),size=1)+
  geom_line(data=L2b,aes(x=YEAR,y=FY_LEUKEMIA_ensem_adj,color='Olive Drab'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='FY_BZ leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1","Olive Drab"),
                       labels=c("true value of FY_BZ_LEUKEMIA","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


ggplot()+geom_line(data=L2b,aes(x=YEAR,y=FY_LUNG,color="red"),size=2)+labs(y="FY_BZ_LUNG")+
  facet_wrap(~ factor(NAME,levels=unique(L2b$NAME)),scales="free_y")+
  geom_line(data=L2b,aes(x=YEAR,y=FY_LUNG_lin,color="black"),size=1)+ggtitle("Performance of several methods about FY_LUNG")+
  geom_line(data=L2b,aes(x=YEAR,y=FY_LUNG_linSpline,color="green"),size=1)+
  geom_line(data=L2b,aes(x=YEAR,y=FY_LUNG_quad,color='darkviolet'),size=1)+
  geom_line(data=L2b,aes(x=YEAR,y=FY_LUNG_ensem,color='deeppink1'),size=1)+
  geom_line(data=L2b,aes(x=YEAR,y=FY_LUNG_ensem_adj,color='Olive Drab'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='FY_BZ lung',
                       breaks=c("red","black","green","darkviolet","deeppink1","Olive Drab"),
                       labels=c("true value of FY_BZ_LEUKEMIA","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))



## 1-8. LEVEL L2ab Data 시각화 -------------------------------------------------
L2ab<-result%>%filter(LEVEL=="L2ab")%>%as_tibble()

ggplot()+geom_line(data=L2ab,aes(x=YEAR,y=LEUKEMIA,color="red"),size=2)+labs(y="#of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(L2ab$NAME)),scales="free_y")+
  geom_line(data=L2ab,aes(x=YEAR,y=LEUKEMIA_lin,color="black"),size=1)+ggtitle("Performance of several methods about LEUKEMIA")+
  geom_line(data=L2ab,aes(x=YEAR,y=LEUKEMIA_linSpline,color="green"),size=1)+
  geom_line(data=L2ab,aes(x=YEAR,y=LEUKEMIA_quad,color='darkviolet'),size=1)+
  geom_line(data=L2ab,aes(x=YEAR,y=LEUKEMIA_ensem,color='deeppink1'),size=1)+
  geom_line(data=L2ab,aes(x=YEAR,y=LEUKEMIA_ensem_adj,color='Cyan'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='#of leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1","Cyan"),
                       labels=c("true value of leukemia","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=L2ab,aes(x=YEAR,y=LUNG,color="red"),size=2)+labs(y="#of_lung")+
  facet_wrap(~ factor(NAME,levels=unique(L2ab$NAME)),scales="free_y")+
  geom_line(data=L2ab,aes(x=YEAR,y=LUNG_lin,color="black"),size=1)+ggtitle("Performance of several methods about LUNG")+
  geom_line(data=L2ab,aes(x=YEAR,y=LUNG_linSpline,color="green"),size=1)+
  geom_line(data=L2ab,aes(x=YEAR,y=LUNG_quad,color='darkviolet'),size=1)+
  geom_line(data=L2ab,aes(x=YEAR,y=LUNG_ensem,color='deeppink1'),size=1)+
  geom_line(data=L2ab,aes(x=YEAR,y=LUNG_ensem_adj,color='Cyan'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='#of lung',
                       breaks=c("red","black","green","darkviolet","deeppink1","Cyan"),
                       labels=c("true value of lung cancer","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=L2ab,aes(x=YEAR,y=FY_LEUKEMIA,color="red"),size=2)+labs(y="FY_BZ_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(L2ab$NAME)),scales="free_y")+
  geom_line(data=L2ab,aes(x=YEAR,y=FY_LEUKEMIA_lin,color="black"),size=1)+ggtitle("Performance of several methods about FY_LEUKEMIA")+
  geom_line(data=L2ab,aes(x=YEAR,y=FY_LEUKEMIA_linSpline,color="green"),size=1)+
  geom_line(data=L2ab,aes(x=YEAR,y=FY_LEUKEMIA_quad,color='darkviolet'),size=1)+
  geom_line(data=L2ab,aes(x=YEAR,y=FY_LEUKEMIA_ensem,color='deeppink1'),size=1)+
  geom_line(data=L2ab,aes(x=YEAR,y=FY_LEUKEMIA_ensem_adj,color='Olive Drab'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='FY_BZ leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1","Olive Drab"),
                       labels=c("true value of FY_BZ_LEUKEMIA","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


ggplot()+geom_line(data=L2ab,aes(x=YEAR,y=FY_LUNG,color="red"),size=2)+labs(y="FY_BZ_LUNG")+
  facet_wrap(~ factor(NAME,levels=unique(L2ab$NAME)),scales="free_y")+
  geom_line(data=L2ab,aes(x=YEAR,y=FY_LUNG_lin,color="black"),size=1)+ggtitle("Performance of several methods about FY_LUNG")+
  geom_line(data=L2ab,aes(x=YEAR,y=FY_LUNG_linSpline,color="green"),size=1)+
  geom_line(data=L2ab,aes(x=YEAR,y=FY_LUNG_quad,color='darkviolet'),size=1)+
  geom_line(data=L2ab,aes(x=YEAR,y=FY_LUNG_ensem,color='deeppink1'),size=1)+
  geom_line(data=L2ab,aes(x=YEAR,y=FY_LUNG_ensem_adj,color='Olive Drab'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='FY_BZ lung',
                       breaks=c("red","black","green","darkviolet","deeppink1","Olive Drab"),
                       labels=c("true value of FY_BZ_LEUKEMIA","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))






## 1-9. LEVEL L3 Data 시각화 -------------------------------------------------
L3<-result%>%filter(LEVEL=="L3")%>%as_tibble()

ggplot()+geom_line(data=L3,aes(x=YEAR,y=LEUKEMIA,color="red"),size=2)+labs(y="#of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(L3$NAME)),scales="free_y")+
  geom_line(data=L3,aes(x=YEAR,y=LEUKEMIA_lin,color="black"),size=1)+ggtitle("Performance of several methods about LEUKEMIA")+
  geom_line(data=L3,aes(x=YEAR,y=LEUKEMIA_linSpline,color="green"),size=1)+
  geom_line(data=L3,aes(x=YEAR,y=LEUKEMIA_quad,color='darkviolet'),size=1)+
  geom_line(data=L3,aes(x=YEAR,y=LEUKEMIA_ensem,color='deeppink1'),size=1)+
  geom_line(data=L3,aes(x=YEAR,y=LEUKEMIA_ensem_adj,color='Cyan'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='#of leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1","Cyan"),
                       labels=c("true value of leukemia","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=L3,aes(x=YEAR,y=LUNG,color="red"),size=2)+labs(y="#of_lung")+
  facet_wrap(~ factor(NAME,levels=unique(L3$NAME)),scales="free_y")+
  geom_line(data=L3,aes(x=YEAR,y=LUNG_lin,color="black"),size=1)+ggtitle("Performance of several methods about LUNG")+
  geom_line(data=L3,aes(x=YEAR,y=LUNG_linSpline,color="green"),size=1)+
  geom_line(data=L3,aes(x=YEAR,y=LUNG_quad,color='darkviolet'),size=1)+
  geom_line(data=L3,aes(x=YEAR,y=LUNG_ensem,color='deeppink1'),size=1)+
  geom_line(data=L3,aes(x=YEAR,y=LUNG_ensem_adj,color='Cyan'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='#of lung',
                       breaks=c("red","black","green","darkviolet","deeppink1","Cyan"),
                       labels=c("true value of lung cancer","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=L3,aes(x=YEAR,y=FY_LEUKEMIA,color="red"),size=2)+labs(y="FY_BZ_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(L3$NAME)),scales="free_y")+
  geom_line(data=L3,aes(x=YEAR,y=FY_LEUKEMIA_lin,color="black"),size=1)+ggtitle("Performance of several methods about FY_LEUKEMIA")+
  geom_line(data=L3,aes(x=YEAR,y=FY_LEUKEMIA_linSpline,color="green"),size=1)+
  geom_line(data=L3,aes(x=YEAR,y=FY_LEUKEMIA_quad,color='darkviolet'),size=1)+
  geom_line(data=L3,aes(x=YEAR,y=FY_LEUKEMIA_ensem,color='deeppink1'),size=1)+
  geom_line(data=L3,aes(x=YEAR,y=FY_LEUKEMIA_ensem_adj,color='Olive Drab'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='FY_BZ leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1","Olive Drab"),
                       labels=c("true value of FY_BZ_LEUKEMIA","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


ggplot()+geom_line(data=L3,aes(x=YEAR,y=FY_LUNG,color="red"),size=2)+labs(y="FY_BZ_LUNG")+
  facet_wrap(~ factor(NAME,levels=unique(L3$NAME)),scales="free_y")+
  geom_line(data=L3,aes(x=YEAR,y=FY_LUNG_lin,color="black"),size=1)+ggtitle("Performance of several methods about FY_LUNG")+
  geom_line(data=L3,aes(x=YEAR,y=FY_LUNG_linSpline,color="green"),size=1)+
  geom_line(data=L3,aes(x=YEAR,y=FY_LUNG_quad,color='darkviolet'),size=1)+
  geom_line(data=L3,aes(x=YEAR,y=FY_LUNG_ensem,color='deeppink1'),size=1)+
  geom_line(data=L3,aes(x=YEAR,y=FY_LUNG_ensem_adj,color='Olive Drab'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='FY_BZ lung',
                       breaks=c("red","black","green","darkviolet","deeppink1","Olive Drab"),
                       labels=c("true value of FY_BZ_LEUKEMIA","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))





## 1-10. LEVEL L3a Data 시각화 -------------------------------------------------
L3a<-result%>%filter(LEVEL=="L3a")%>%as_tibble()

ind.M<-grep("M",L3a$NAME)
ind.F<-grep("F",L3a$NAME)


# Female ------------------------------------------------------------------
ggplot()+geom_line(data=L3a[ind.F,],aes(x=YEAR,y=LEUKEMIA,color="red"),size=2)+labs(y="#of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(L3a$NAME[ind.F])),scales="free_y")+
  geom_line(data=L3a[ind.F,],aes(x=YEAR,y=LEUKEMIA_lin,color="black"),size=1)+ggtitle("Performance of several methods about LEUKEMIA")+
  geom_line(data=L3a[ind.F,],aes(x=YEAR,y=LEUKEMIA_linSpline,color="green"),size=1)+
  geom_line(data=L3a[ind.F,],aes(x=YEAR,y=LEUKEMIA_quad,color='darkviolet'),size=1)+
  geom_line(data=L3a[ind.F,],aes(x=YEAR,y=LEUKEMIA_ensem,color='deeppink1'),size=1)+
  geom_line(data=L3a[ind.F,],aes(x=YEAR,y=LEUKEMIA_ensem_adj,color='Cyan'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='#of leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1","Cyan"),
                       labels=c("true value of leukemia","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=L3a[ind.F,],aes(x=YEAR,y=LUNG,color="red"),size=2)+labs(y="#of_lung")+
  facet_wrap(~ factor(NAME,levels=unique(L3a$NAME[ind.F])),scales="free_y")+
  geom_line(data=L3a[ind.F,],aes(x=YEAR,y=LUNG_lin,color="black"),size=1)+ggtitle("Performance of several methods about LUNG")+
  geom_line(data=L3a[ind.F,],aes(x=YEAR,y=LUNG_linSpline,color="green"),size=1)+
  geom_line(data=L3a[ind.F,],aes(x=YEAR,y=LUNG_quad,color='darkviolet'),size=1)+
  geom_line(data=L3a[ind.F,],aes(x=YEAR,y=LUNG_ensem,color='deeppink1'),size=1)+
  geom_line(data=L3a[ind.F,],aes(x=YEAR,y=LUNG_ensem_adj,color='Cyan'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='#of lung',
                       breaks=c("red","black","green","darkviolet","deeppink1","Cyan"),
                       labels=c("true value of lung cancer","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=L3a[ind.F,],aes(x=YEAR,y=FY_LEUKEMIA,color="red"),size=2)+labs(y="FY_BZ_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(L3a$NAME[ind.F])),scales="free_y")+
  geom_line(data=L3a[ind.F,],aes(x=YEAR,y=FY_LEUKEMIA_lin,color="black"),size=1)+ggtitle("Performance of several methods about FY_LEUKEMIA")+
  geom_line(data=L3a[ind.F,],aes(x=YEAR,y=FY_LEUKEMIA_linSpline,color="green"),size=1)+
  geom_line(data=L3a[ind.F,],aes(x=YEAR,y=FY_LEUKEMIA_quad,color='darkviolet'),size=1)+
  geom_line(data=L3a[ind.F,],aes(x=YEAR,y=FY_LEUKEMIA_ensem,color='deeppink1'),size=1)+
  geom_line(data=L3a[ind.F,],aes(x=YEAR,y=FY_LEUKEMIA_ensem_adj,color='Olive Drab'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='FY_BZ leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1","Olive Drab"),
                       labels=c("true value of FY_BZ_LEUKEMIA","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


ggplot()+geom_line(data=L3a[ind.F,],aes(x=YEAR,y=FY_LUNG,color="red"),size=2)+labs(y="FY_BZ_LUNG")+
  facet_wrap(~ factor(NAME,levels=unique(L3a$NAME[ind.F])),scales="free_y")+
  geom_line(data=L3a[ind.F,],aes(x=YEAR,y=FY_LUNG_lin,color="black"),size=1)+ggtitle("Performance of several methods about FY_LUNG")+
  geom_line(data=L3a[ind.F,],aes(x=YEAR,y=FY_LUNG_linSpline,color="green"),size=1)+
  geom_line(data=L3a[ind.F,],aes(x=YEAR,y=FY_LUNG_quad,color='darkviolet'),size=1)+
  geom_line(data=L3a[ind.F,],aes(x=YEAR,y=FY_LUNG_ensem,color='deeppink1'),size=1)+
  geom_line(data=L3a[ind.F,],aes(x=YEAR,y=FY_LUNG_ensem_adj,color='Olive Drab'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='FY_BZ lung',
                       breaks=c("red","black","green","darkviolet","deeppink1","Olive Drab"),
                       labels=c("true value of FY_BZ_LEUKEMIA","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


# Male ------------------------------------------------------------------
ggplot()+geom_line(data=L3a[ind.M,],aes(x=YEAR,y=LEUKEMIA,color="red"),size=2)+labs(y="#of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(L3a$NAME[ind.M])),scales="free_y")+
  geom_line(data=L3a[ind.M,],aes(x=YEAR,y=LEUKEMIA_lin,color="black"),size=1)+ggtitle("Performance of several methods about LEUKEMIA")+
  geom_line(data=L3a[ind.M,],aes(x=YEAR,y=LEUKEMIA_linSpline,color="green"),size=1)+
  geom_line(data=L3a[ind.M,],aes(x=YEAR,y=LEUKEMIA_quad,color='darkviolet'),size=1)+
  geom_line(data=L3a[ind.M,],aes(x=YEAR,y=LEUKEMIA_ensem,color='deeppink1'),size=1)+
  geom_line(data=L3a[ind.M,],aes(x=YEAR,y=LEUKEMIA_ensem_adj,color='Cyan'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='#of leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1","Cyan"),
                       labels=c("true value of leukemia","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=L3a[ind.M,],aes(x=YEAR,y=LUNG,color="red"),size=2)+labs(y="#of_lung")+
  facet_wrap(~ factor(NAME,levels=unique(L3a$NAME[ind.M])),scales="free_y")+
  geom_line(data=L3a[ind.M,],aes(x=YEAR,y=LUNG_lin,color="black"),size=1)+ggtitle("Performance of several methods about LUNG")+
  geom_line(data=L3a[ind.M,],aes(x=YEAR,y=LUNG_linSpline,color="green"),size=1)+
  geom_line(data=L3a[ind.M,],aes(x=YEAR,y=LUNG_quad,color='darkviolet'),size=1)+
  geom_line(data=L3a[ind.M,],aes(x=YEAR,y=LUNG_ensem,color='deeppink1'),size=1)+
  geom_line(data=L3a[ind.M,],aes(x=YEAR,y=LUNG_ensem_adj,color='Cyan'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='#of lung',
                       breaks=c("red","black","green","darkviolet","deeppink1","Cyan"),
                       labels=c("true value of lung cancer","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=L3a[ind.M,],aes(x=YEAR,y=FY_LEUKEMIA,color="red"),size=2)+labs(y="FY_BZ_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(L3a$NAME[ind.M])),scales="free_y")+
  geom_line(data=L3a[ind.M,],aes(x=YEAR,y=FY_LEUKEMIA_lin,color="black"),size=1)+ggtitle("Performance of several methods about FY_LEUKEMIA")+
  geom_line(data=L3a[ind.M,],aes(x=YEAR,y=FY_LEUKEMIA_linSpline,color="green"),size=1)+
  geom_line(data=L3a[ind.M,],aes(x=YEAR,y=FY_LEUKEMIA_quad,color='darkviolet'),size=1)+
  geom_line(data=L3a[ind.M,],aes(x=YEAR,y=FY_LEUKEMIA_ensem,color='deeppink1'),size=1)+
  geom_line(data=L3a[ind.M,],aes(x=YEAR,y=FY_LEUKEMIA_ensem_adj,color='Olive Drab'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='FY_BZ leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1","Olive Drab"),
                       labels=c("true value of FY_BZ_LEUKEMIA","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


ggplot()+geom_line(data=L3a[ind.M,],aes(x=YEAR,y=FY_LUNG,color="red"),size=2)+labs(y="FY_BZ_LUNG")+
  facet_wrap(~ factor(NAME,levels=unique(L3a$NAME[ind.M])),scales="free_y")+
  geom_line(data=L3a[ind.M,],aes(x=YEAR,y=FY_LUNG_lin,color="black"),size=1)+ggtitle("Performance of several methods about FY_LUNG")+
  geom_line(data=L3a[ind.M,],aes(x=YEAR,y=FY_LUNG_linSpline,color="green"),size=1)+
  geom_line(data=L3a[ind.M,],aes(x=YEAR,y=FY_LUNG_quad,color='darkviolet'),size=1)+
  geom_line(data=L3a[ind.M,],aes(x=YEAR,y=FY_LUNG_ensem,color='deeppink1'),size=1)+
  geom_line(data=L3a[ind.M,],aes(x=YEAR,y=FY_LUNG_ensem_adj,color='Olive Drab'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='FY_BZ lung',
                       breaks=c("red","black","green","darkviolet","deeppink1","Olive Drab"),
                       labels=c("true value of FY_BZ_LEUKEMIA","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))




## 1-11. LEVEL L3b Data 시각화 -------------------------------------------------
L3b<-result%>%filter(LEVEL=="L3b")%>%as_tibble()

ind.01<-grep("01",L3b$NAME)
ind.234<-grep("234",L3b$NAME)


# CAL2 = 01 ---------------------------------------------------------------
ggplot()+geom_line(data=L3b[ind.01,],aes(x=YEAR,y=LEUKEMIA,color="red"),size=2)+labs(y="#of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(L3b$NAME[ind.01])),scales="free_y")+
  geom_line(data=L3b[ind.01,],aes(x=YEAR,y=LEUKEMIA_lin,color="black"),size=1)+ggtitle("Performance of several methods about LEUKEMIA")+
  geom_line(data=L3b[ind.01,],aes(x=YEAR,y=LEUKEMIA_linSpline,color="green"),size=1)+
  geom_line(data=L3b[ind.01,],aes(x=YEAR,y=LEUKEMIA_quad,color='darkviolet'),size=1)+
  geom_line(data=L3b[ind.01,],aes(x=YEAR,y=LEUKEMIA_ensem,color='deeppink1'),size=1)+
  geom_line(data=L3b[ind.01,],aes(x=YEAR,y=LEUKEMIA_ensem_adj,color='Cyan'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='#of leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1","Cyan"),
                       labels=c("true value of leukemia","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=L3b[ind.01,],aes(x=YEAR,y=LUNG,color="red"),size=2)+labs(y="#of_lung")+
  facet_wrap(~ factor(NAME,levels=unique(L3b$NAME[ind.01])),scales="free_y")+
  geom_line(data=L3b[ind.01,],aes(x=YEAR,y=LUNG_lin,color="black"),size=1)+ggtitle("Performance of several methods about LUNG")+
  geom_line(data=L3b[ind.01,],aes(x=YEAR,y=LUNG_linSpline,color="green"),size=1)+
  geom_line(data=L3b[ind.01,],aes(x=YEAR,y=LUNG_quad,color='darkviolet'),size=1)+
  geom_line(data=L3b[ind.01,],aes(x=YEAR,y=LUNG_ensem,color='deeppink1'),size=1)+
  geom_line(data=L3b[ind.01,],aes(x=YEAR,y=LUNG_ensem_adj,color='Cyan'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='#of lung',
                       breaks=c("red","black","green","darkviolet","deeppink1","Cyan"),
                       labels=c("true value of lung cancer","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=L3b[ind.01,],aes(x=YEAR,y=FY_LEUKEMIA,color="red"),size=2)+labs(y="FY_BZ_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(L3b$NAME[ind.01])),scales="free_y")+
  geom_line(data=L3b[ind.01,],aes(x=YEAR,y=FY_LEUKEMIA_lin,color="black"),size=1)+ggtitle("Performance of several methods about FY_LEUKEMIA")+
  geom_line(data=L3b[ind.01,],aes(x=YEAR,y=FY_LEUKEMIA_linSpline,color="green"),size=1)+
  geom_line(data=L3b[ind.01,],aes(x=YEAR,y=FY_LEUKEMIA_quad,color='darkviolet'),size=1)+
  geom_line(data=L3b[ind.01,],aes(x=YEAR,y=FY_LEUKEMIA_ensem,color='deeppink1'),size=1)+
  geom_line(data=L3b[ind.01,],aes(x=YEAR,y=FY_LEUKEMIA_ensem_adj,color='Olive Drab'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='FY_BZ leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1","Olive Drab"),
                       labels=c("true value of FY_BZ_LEUKEMIA","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


ggplot()+geom_line(data=L3b[ind.01,],aes(x=YEAR,y=FY_LUNG,color="red"),size=2)+labs(y="FY_BZ_LUNG")+
  facet_wrap(~ factor(NAME,levels=unique(L3b$NAME[ind.01])),scales="free_y")+
  geom_line(data=L3b[ind.01,],aes(x=YEAR,y=FY_LUNG_lin,color="black"),size=1)+ggtitle("Performance of several methods about FY_LUNG")+
  geom_line(data=L3b[ind.01,],aes(x=YEAR,y=FY_LUNG_linSpline,color="green"),size=1)+
  geom_line(data=L3b[ind.01,],aes(x=YEAR,y=FY_LUNG_quad,color='darkviolet'),size=1)+
  geom_line(data=L3b[ind.01,],aes(x=YEAR,y=FY_LUNG_ensem,color='deeppink1'),size=1)+
  geom_line(data=L3b[ind.01,],aes(x=YEAR,y=FY_LUNG_ensem_adj,color='Olive Drab'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='FY_BZ lung',
                       breaks=c("red","black","green","darkviolet","deeppink1","Olive Drab"),
                       labels=c("true value of FY_BZ_LEUKEMIA","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


# CAL2 = 234 ---------------------------------------------------------------
ggplot()+geom_line(data=L3b[ind.234,],aes(x=YEAR,y=LEUKEMIA,color="red"),size=2)+labs(y="#of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(L3b$NAME[ind.234])),scales="free_y")+
  geom_line(data=L3b[ind.234,],aes(x=YEAR,y=LEUKEMIA_lin,color="black"),size=1)+ggtitle("Performance of several methods about LEUKEMIA")+
  geom_line(data=L3b[ind.234,],aes(x=YEAR,y=LEUKEMIA_linSpline,color="green"),size=1)+
  geom_line(data=L3b[ind.234,],aes(x=YEAR,y=LEUKEMIA_quad,color='darkviolet'),size=1)+
  geom_line(data=L3b[ind.234,],aes(x=YEAR,y=LEUKEMIA_ensem,color='deeppink1'),size=1)+
  geom_line(data=L3b[ind.234,],aes(x=YEAR,y=LEUKEMIA_ensem_adj,color='Cyan'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='#of leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1","Cyan"),
                       labels=c("true value of leukemia","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=L3b[ind.234,],aes(x=YEAR,y=LUNG,color="red"),size=2)+labs(y="#of_lung")+
  facet_wrap(~ factor(NAME,levels=unique(L3b$NAME[ind.234])),scales="free_y")+
  geom_line(data=L3b[ind.234,],aes(x=YEAR,y=LUNG_lin,color="black"),size=1)+ggtitle("Performance of several methods about LUNG")+
  geom_line(data=L3b[ind.234,],aes(x=YEAR,y=LUNG_linSpline,color="green"),size=1)+
  geom_line(data=L3b[ind.234,],aes(x=YEAR,y=LUNG_quad,color='darkviolet'),size=1)+
  geom_line(data=L3b[ind.234,],aes(x=YEAR,y=LUNG_ensem,color='deeppink1'),size=1)+
  geom_line(data=L3b[ind.234,],aes(x=YEAR,y=LUNG_ensem_adj,color='Cyan'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='#of lung',
                       breaks=c("red","black","green","darkviolet","deeppink1","Cyan"),
                       labels=c("true value of lung cancer","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=L3b[ind.234,],aes(x=YEAR,y=FY_LEUKEMIA,color="red"),size=2)+labs(y="FY_BZ_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(L3b$NAME[ind.234])),scales="free_y")+
  geom_line(data=L3b[ind.234,],aes(x=YEAR,y=FY_LEUKEMIA_lin,color="black"),size=1)+ggtitle("Performance of several methods about FY_LEUKEMIA")+
  geom_line(data=L3b[ind.234,],aes(x=YEAR,y=FY_LEUKEMIA_linSpline,color="green"),size=1)+
  geom_line(data=L3b[ind.234,],aes(x=YEAR,y=FY_LEUKEMIA_quad,color='darkviolet'),size=1)+
  geom_line(data=L3b[ind.234,],aes(x=YEAR,y=FY_LEUKEMIA_ensem,color='deeppink1'),size=1)+
  geom_line(data=L3b[ind.234,],aes(x=YEAR,y=FY_LEUKEMIA_ensem_adj,color='Olive Drab'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='FY_BZ leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1","Olive Drab"),
                       labels=c("true value of FY_BZ_LEUKEMIA","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


ggplot()+geom_line(data=L3b[ind.234,],aes(x=YEAR,y=FY_LUNG,color="red"),size=2)+labs(y="FY_BZ_LUNG")+
  facet_wrap(~ factor(NAME,levels=unique(L3b$NAME[ind.234])),scales="free_y")+
  geom_line(data=L3b[ind.234,],aes(x=YEAR,y=FY_LUNG_lin,color="black"),size=1)+ggtitle("Performance of several methods about FY_LUNG")+
  geom_line(data=L3b[ind.234,],aes(x=YEAR,y=FY_LUNG_linSpline,color="green"),size=1)+
  geom_line(data=L3b[ind.234,],aes(x=YEAR,y=FY_LUNG_quad,color='darkviolet'),size=1)+
  geom_line(data=L3b[ind.234,],aes(x=YEAR,y=FY_LUNG_ensem,color='deeppink1'),size=1)+
  geom_line(data=L3b[ind.234,],aes(x=YEAR,y=FY_LUNG_ensem_adj,color='Olive Drab'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='FY_BZ lung',
                       breaks=c("red","black","green","darkviolet","deeppink1","Olive Drab"),
                       labels=c("true value of FY_BZ_LEUKEMIA","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))



## 1-12. LEVEL L3ab Data 시각화 -------------------------------------------------
L3ab<-result%>%filter(LEVEL=="L3ab")%>%as_tibble()

ind.M.01<-which(grepl("M",L3ab$NAME) & grepl("01",L3ab$NAME))

ind.F.01<-which(grepl("F",L3ab$NAME) & grepl("01",L3ab$NAME))

ind.M.234<-which(grepl("M",L3ab$NAME) & grepl("234",L3ab$NAME))

ind.F.234<-which(grepl("F",L3ab$NAME) & grepl("234",L3ab$NAME))


# M & 01 ------------------------------------------------------------------
ggplot()+geom_line(data=L3ab[ind.M.01,],aes(x=YEAR,y=LEUKEMIA,color="red"),size=2)+labs(y="#of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(L3ab$NAME[ind.M.01])),scales="free_y")+
  geom_line(data=L3ab[ind.M.01,],aes(x=YEAR,y=LEUKEMIA_lin,color="black"),size=1)+ggtitle("Performance of several methods about LEUKEMIA")+
  geom_line(data=L3ab[ind.M.01,],aes(x=YEAR,y=LEUKEMIA_linSpline,color="green"),size=1)+
  geom_line(data=L3ab[ind.M.01,],aes(x=YEAR,y=LEUKEMIA_quad,color='darkviolet'),size=1)+
  geom_line(data=L3ab[ind.M.01,],aes(x=YEAR,y=LEUKEMIA_ensem,color='deeppink1'),size=1)+
  geom_line(data=L3ab[ind.M.01,],aes(x=YEAR,y=LEUKEMIA_ensem_adj,color='Cyan'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='#of leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1","Cyan"),
                       labels=c("true value of leukemia","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=L3ab[ind.M.01,],aes(x=YEAR,y=LUNG,color="red"),size=2)+labs(y="#of_lung")+
  facet_wrap(~ factor(NAME,levels=unique(L3ab$NAME[ind.M.01])),scales="free_y")+
  geom_line(data=L3ab[ind.M.01,],aes(x=YEAR,y=LUNG_lin,color="black"),size=1)+ggtitle("Performance of several methods about LUNG")+
  geom_line(data=L3ab[ind.M.01,],aes(x=YEAR,y=LUNG_linSpline,color="green"),size=1)+
  geom_line(data=L3ab[ind.M.01,],aes(x=YEAR,y=LUNG_quad,color='darkviolet'),size=1)+
  geom_line(data=L3ab[ind.M.01,],aes(x=YEAR,y=LUNG_ensem,color='deeppink1'),size=1)+
  geom_line(data=L3ab[ind.M.01,],aes(x=YEAR,y=LUNG_ensem_adj,color='Cyan'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='#of lung',
                       breaks=c("red","black","green","darkviolet","deeppink1","Cyan"),
                       labels=c("true value of lung cancer","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=L3ab[ind.M.01,],aes(x=YEAR,y=FY_LEUKEMIA,color="red"),size=2)+labs(y="FY_BZ_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(L3ab$NAME[ind.M.01])),scales="free_y")+
  geom_line(data=L3ab[ind.M.01,],aes(x=YEAR,y=FY_LEUKEMIA_lin,color="black"),size=1)+ggtitle("Performance of several methods about FY_LEUKEMIA")+
  geom_line(data=L3ab[ind.M.01,],aes(x=YEAR,y=FY_LEUKEMIA_linSpline,color="green"),size=1)+
  geom_line(data=L3ab[ind.M.01,],aes(x=YEAR,y=FY_LEUKEMIA_quad,color='darkviolet'),size=1)+
  geom_line(data=L3ab[ind.M.01,],aes(x=YEAR,y=FY_LEUKEMIA_ensem,color='deeppink1'),size=1)+
  geom_line(data=L3ab[ind.M.01,],aes(x=YEAR,y=FY_LEUKEMIA_ensem_adj,color='Olive Drab'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='FY_BZ leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1","Olive Drab"),
                       labels=c("true value of FY_BZ_LEUKEMIA","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


ggplot()+geom_line(data=L3ab[ind.M.01,],aes(x=YEAR,y=FY_LUNG,color="red"),size=2)+labs(y="FY_BZ_LUNG")+
  facet_wrap(~ factor(NAME,levels=unique(L3ab$NAME[ind.M.01])),scales="free_y")+
  geom_line(data=L3ab[ind.M.01,],aes(x=YEAR,y=FY_LUNG_lin,color="black"),size=1)+ggtitle("Performance of several methods about FY_LUNG")+
  geom_line(data=L3ab[ind.M.01,],aes(x=YEAR,y=FY_LUNG_linSpline,color="green"),size=1)+
  geom_line(data=L3ab[ind.M.01,],aes(x=YEAR,y=FY_LUNG_quad,color='darkviolet'),size=1)+
  geom_line(data=L3ab[ind.M.01,],aes(x=YEAR,y=FY_LUNG_ensem,color='deeppink1'),size=1)+
  geom_line(data=L3ab[ind.M.01,],aes(x=YEAR,y=FY_LUNG_ensem_adj,color='Olive Drab'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='FY_BZ lung',
                       breaks=c("red","black","green","darkviolet","deeppink1","Olive Drab"),
                       labels=c("true value of FY_BZ_LEUKEMIA","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))




# M & 234 ------------------------------------------------------------------
ggplot()+geom_line(data=L3ab[ind.M.234,],aes(x=YEAR,y=LEUKEMIA,color="red"),size=2)+labs(y="#of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(L3ab$NAME[ind.M.234])),scales="free_y")+
  geom_line(data=L3ab[ind.M.234,],aes(x=YEAR,y=LEUKEMIA_lin,color="black"),size=1)+ggtitle("Performance of several methods about LEUKEMIA")+
  geom_line(data=L3ab[ind.M.234,],aes(x=YEAR,y=LEUKEMIA_linSpline,color="green"),size=1)+
  geom_line(data=L3ab[ind.M.234,],aes(x=YEAR,y=LEUKEMIA_quad,color='darkviolet'),size=1)+
  geom_line(data=L3ab[ind.M.234,],aes(x=YEAR,y=LEUKEMIA_ensem,color='deeppink1'),size=1)+
  geom_line(data=L3ab[ind.M.234,],aes(x=YEAR,y=LEUKEMIA_ensem_adj,color='Cyan'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='#of leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1","Cyan"),
                       labels=c("true value of leukemia","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=L3ab[ind.M.234,],aes(x=YEAR,y=LUNG,color="red"),size=2)+labs(y="#of_lung")+
  facet_wrap(~ factor(NAME,levels=unique(L3ab$NAME[ind.M.234])),scales="free_y")+
  geom_line(data=L3ab[ind.M.234,],aes(x=YEAR,y=LUNG_lin,color="black"),size=1)+ggtitle("Performance of several methods about LUNG")+
  geom_line(data=L3ab[ind.M.234,],aes(x=YEAR,y=LUNG_linSpline,color="green"),size=1)+
  geom_line(data=L3ab[ind.M.234,],aes(x=YEAR,y=LUNG_quad,color='darkviolet'),size=1)+
  geom_line(data=L3ab[ind.M.234,],aes(x=YEAR,y=LUNG_ensem,color='deeppink1'),size=1)+
  geom_line(data=L3ab[ind.M.234,],aes(x=YEAR,y=LUNG_ensem_adj,color='Cyan'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='#of lung',
                       breaks=c("red","black","green","darkviolet","deeppink1","Cyan"),
                       labels=c("true value of lung cancer","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=L3ab[ind.M.234,],aes(x=YEAR,y=FY_LEUKEMIA,color="red"),size=2)+labs(y="FY_BZ_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(L3ab$NAME[ind.M.234])),scales="free_y")+
  geom_line(data=L3ab[ind.M.234,],aes(x=YEAR,y=FY_LEUKEMIA_lin,color="black"),size=1)+ggtitle("Performance of several methods about FY_LEUKEMIA")+
  geom_line(data=L3ab[ind.M.234,],aes(x=YEAR,y=FY_LEUKEMIA_linSpline,color="green"),size=1)+
  geom_line(data=L3ab[ind.M.234,],aes(x=YEAR,y=FY_LEUKEMIA_quad,color='darkviolet'),size=1)+
  geom_line(data=L3ab[ind.M.234,],aes(x=YEAR,y=FY_LEUKEMIA_ensem,color='deeppink1'),size=1)+
  geom_line(data=L3ab[ind.M.234,],aes(x=YEAR,y=FY_LEUKEMIA_ensem_adj,color='Olive Drab'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='FY_BZ leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1","Olive Drab"),
                       labels=c("true value of FY_BZ_LEUKEMIA","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


ggplot()+geom_line(data=L3ab[ind.M.234,],aes(x=YEAR,y=FY_LUNG,color="red"),size=2)+labs(y="FY_BZ_LUNG")+
  facet_wrap(~ factor(NAME,levels=unique(L3ab$NAME[ind.M.234])),scales="free_y")+
  geom_line(data=L3ab[ind.M.234,],aes(x=YEAR,y=FY_LUNG_lin,color="black"),size=1)+ggtitle("Performance of several methods about FY_LUNG")+
  geom_line(data=L3ab[ind.M.234,],aes(x=YEAR,y=FY_LUNG_linSpline,color="green"),size=1)+
  geom_line(data=L3ab[ind.M.234,],aes(x=YEAR,y=FY_LUNG_quad,color='darkviolet'),size=1)+
  geom_line(data=L3ab[ind.M.234,],aes(x=YEAR,y=FY_LUNG_ensem,color='deeppink1'),size=1)+
  geom_line(data=L3ab[ind.M.234,],aes(x=YEAR,y=FY_LUNG_ensem_adj,color='Olive Drab'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='FY_BZ lung',
                       breaks=c("red","black","green","darkviolet","deeppink1","Olive Drab"),
                       labels=c("true value of FY_BZ_LEUKEMIA","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))




# F & 01 ------------------------------------------------------------------
ggplot()+geom_line(data=L3ab[ind.F.01,],aes(x=YEAR,y=LEUKEMIA,color="red"),size=2)+labs(y="#of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(L3ab$NAME[ind.F.01])),scales="free_y")+
  geom_line(data=L3ab[ind.F.01,],aes(x=YEAR,y=LEUKEMIA_lin,color="black"),size=1)+ggtitle("Performance of several methods about LEUKEMIA")+
  geom_line(data=L3ab[ind.F.01,],aes(x=YEAR,y=LEUKEMIA_linSpline,color="green"),size=1)+
  geom_line(data=L3ab[ind.F.01,],aes(x=YEAR,y=LEUKEMIA_quad,color='darkviolet'),size=1)+
  geom_line(data=L3ab[ind.F.01,],aes(x=YEAR,y=LEUKEMIA_ensem,color='deeppink1'),size=1)+
  geom_line(data=L3ab[ind.F.01,],aes(x=YEAR,y=LEUKEMIA_ensem_adj,color='Cyan'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='#of leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1","Cyan"),
                       labels=c("true value of leukemia","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=L3ab[ind.F.01,],aes(x=YEAR,y=LUNG,color="red"),size=2)+labs(y="#of_lung")+
  facet_wrap(~ factor(NAME,levels=unique(L3ab$NAME[ind.F.01])),scales="free_y")+
  geom_line(data=L3ab[ind.F.01,],aes(x=YEAR,y=LUNG_lin,color="black"),size=1)+ggtitle("Performance of several methods about LUNG")+
  geom_line(data=L3ab[ind.F.01,],aes(x=YEAR,y=LUNG_linSpline,color="green"),size=1)+
  geom_line(data=L3ab[ind.F.01,],aes(x=YEAR,y=LUNG_quad,color='darkviolet'),size=1)+
  geom_line(data=L3ab[ind.F.01,],aes(x=YEAR,y=LUNG_ensem,color='deeppink1'),size=1)+
  geom_line(data=L3ab[ind.F.01,],aes(x=YEAR,y=LUNG_ensem_adj,color='Cyan'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='#of lung',
                       breaks=c("red","black","green","darkviolet","deeppink1","Cyan"),
                       labels=c("true value of lung cancer","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=L3ab[ind.F.01,],aes(x=YEAR,y=FY_LEUKEMIA,color="red"),size=2)+labs(y="FY_BZ_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(L3ab$NAME[ind.F.01])),scales="free_y")+
  geom_line(data=L3ab[ind.F.01,],aes(x=YEAR,y=FY_LEUKEMIA_lin,color="black"),size=1)+ggtitle("Performance of several methods about FY_LEUKEMIA")+
  geom_line(data=L3ab[ind.F.01,],aes(x=YEAR,y=FY_LEUKEMIA_linSpline,color="green"),size=1)+
  geom_line(data=L3ab[ind.F.01,],aes(x=YEAR,y=FY_LEUKEMIA_quad,color='darkviolet'),size=1)+
  geom_line(data=L3ab[ind.F.01,],aes(x=YEAR,y=FY_LEUKEMIA_ensem,color='deeppink1'),size=1)+
  geom_line(data=L3ab[ind.F.01,],aes(x=YEAR,y=FY_LEUKEMIA_ensem_adj,color='Olive Drab'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='FY_BZ leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1","Olive Drab"),
                       labels=c("true value of FY_BZ_LEUKEMIA","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


ggplot()+geom_line(data=L3ab[ind.F.01,],aes(x=YEAR,y=FY_LUNG,color="red"),size=2)+labs(y="FY_BZ_LUNG")+
  facet_wrap(~ factor(NAME,levels=unique(L3ab$NAME[ind.F.01])),scales="free_y")+
  geom_line(data=L3ab[ind.F.01,],aes(x=YEAR,y=FY_LUNG_lin,color="black"),size=1)+ggtitle("Performance of several methods about FY_LUNG")+
  geom_line(data=L3ab[ind.F.01,],aes(x=YEAR,y=FY_LUNG_linSpline,color="green"),size=1)+
  geom_line(data=L3ab[ind.F.01,],aes(x=YEAR,y=FY_LUNG_quad,color='darkviolet'),size=1)+
  geom_line(data=L3ab[ind.F.01,],aes(x=YEAR,y=FY_LUNG_ensem,color='deeppink1'),size=1)+
  geom_line(data=L3ab[ind.F.01,],aes(x=YEAR,y=FY_LUNG_ensem_adj,color='Olive Drab'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='FY_BZ lung',
                       breaks=c("red","black","green","darkviolet","deeppink1","Olive Drab"),
                       labels=c("true value of FY_BZ_LEUKEMIA","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))







# F & 234 ------------------------------------------------------------------
ggplot()+geom_line(data=L3ab[ind.F.234,],aes(x=YEAR,y=LEUKEMIA,color="red"),size=2)+labs(y="#of_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(L3ab$NAME[ind.F.234])),scales="free_y")+
  geom_line(data=L3ab[ind.F.234,],aes(x=YEAR,y=LEUKEMIA_lin,color="black"),size=1)+ggtitle("Performance of several methods about LEUKEMIA")+
  geom_line(data=L3ab[ind.F.234,],aes(x=YEAR,y=LEUKEMIA_linSpline,color="green"),size=1)+
  geom_line(data=L3ab[ind.F.234,],aes(x=YEAR,y=LEUKEMIA_quad,color='darkviolet'),size=1)+
  geom_line(data=L3ab[ind.F.234,],aes(x=YEAR,y=LEUKEMIA_ensem,color='deeppink1'),size=1)+
  geom_line(data=L3ab[ind.F.234,],aes(x=YEAR,y=LEUKEMIA_ensem_adj,color='Cyan'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='#of leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1","Cyan"),
                       labels=c("true value of leukemia","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=L3ab[ind.F.234,],aes(x=YEAR,y=LUNG,color="red"),size=2)+labs(y="#of_lung")+
  facet_wrap(~ factor(NAME,levels=unique(L3ab$NAME[ind.F.234])),scales="free_y")+
  geom_line(data=L3ab[ind.F.234,],aes(x=YEAR,y=LUNG_lin,color="black"),size=1)+ggtitle("Performance of several methods about LUNG")+
  geom_line(data=L3ab[ind.F.234,],aes(x=YEAR,y=LUNG_linSpline,color="green"),size=1)+
  geom_line(data=L3ab[ind.F.234,],aes(x=YEAR,y=LUNG_quad,color='darkviolet'),size=1)+
  geom_line(data=L3ab[ind.F.234,],aes(x=YEAR,y=LUNG_ensem,color='deeppink1'),size=1)+
  geom_line(data=L3ab[ind.F.234,],aes(x=YEAR,y=LUNG_ensem_adj,color='Cyan'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='#of lung',
                       breaks=c("red","black","green","darkviolet","deeppink1","Cyan"),
                       labels=c("true value of lung cancer","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

ggplot()+geom_line(data=L3ab[ind.F.234,],aes(x=YEAR,y=FY_LEUKEMIA,color="red"),size=2)+labs(y="FY_BZ_leukemia")+
  facet_wrap(~ factor(NAME,levels=unique(L3ab$NAME[ind.F.234])),scales="free_y")+
  geom_line(data=L3ab[ind.F.234,],aes(x=YEAR,y=FY_LEUKEMIA_lin,color="black"),size=1)+ggtitle("Performance of several methods about FY_LEUKEMIA")+
  geom_line(data=L3ab[ind.F.234,],aes(x=YEAR,y=FY_LEUKEMIA_linSpline,color="green"),size=1)+
  geom_line(data=L3ab[ind.F.234,],aes(x=YEAR,y=FY_LEUKEMIA_quad,color='darkviolet'),size=1)+
  geom_line(data=L3ab[ind.F.234,],aes(x=YEAR,y=FY_LEUKEMIA_ensem,color='deeppink1'),size=1)+
  geom_line(data=L3ab[ind.F.234,],aes(x=YEAR,y=FY_LEUKEMIA_ensem_adj,color='Olive Drab'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='FY_BZ leukemia',
                       breaks=c("red","black","green","darkviolet","deeppink1","Olive Drab"),
                       labels=c("true value of FY_BZ_LEUKEMIA","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


ggplot()+geom_line(data=L3ab[ind.F.234,],aes(x=YEAR,y=FY_LUNG,color="red"),size=2)+labs(y="FY_BZ_LUNG")+
  facet_wrap(~ factor(NAME,levels=unique(L3ab$NAME[ind.F.234])),scales="free_y")+
  geom_line(data=L3ab[ind.F.234,],aes(x=YEAR,y=FY_LUNG_lin,color="black"),size=1)+ggtitle("Performance of several methods about FY_LUNG")+
  geom_line(data=L3ab[ind.F.234,],aes(x=YEAR,y=FY_LUNG_linSpline,color="green"),size=1)+
  geom_line(data=L3ab[ind.F.234,],aes(x=YEAR,y=FY_LUNG_quad,color='darkviolet'),size=1)+
  geom_line(data=L3ab[ind.F.234,],aes(x=YEAR,y=FY_LUNG_ensem,color='deeppink1'),size=1)+
  geom_line(data=L3ab[ind.F.234,],aes(x=YEAR,y=FY_LUNG_ensem_adj,color='Olive Drab'),size=1)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='blue', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='FY_BZ lung',
                       breaks=c("red","black","green","darkviolet","deeppink1","Olive Drab"),
                       labels=c("true value of FY_BZ_LEUKEMIA","Predict by simple linear regression","Predict by linear spline","Predict by quadratic regression",
                                "Ensembled value", "Ensemble adjusted value"),
                       guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))
