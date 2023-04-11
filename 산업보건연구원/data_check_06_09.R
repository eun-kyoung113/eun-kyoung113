### data check 6/9 ver ###

dt<-fread("/shared/data/koshri/ref_leukemia_workplace1.csv")
dt$rate_LEUKEMIA_BZ_by_FY_BZ<-dt$LEUKEMIA_BZ/dt$FY_BZ

library(dtplyr)
dt_lz<-lazy_dt(dt)

#library(readr)
#code2<-read_csv("업종코드2.csv",locale = locale("ko", encoding = "euc-kr"))
#code_names <- code2[c("UP2","NAME")]

##### last follow up year #####
result_YEAR_LEUKEMIA<-dt_lz%>%filter(YEAR==2018)%>%group_by(UP2)%>%summarise(sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ))%>%
  arrange(desc(sum_LEUKEMIA_BZ))%>%as_tibble()
head(result_YEAR_LEUKEMIA)

result_YEAR_FY_BZ<-dt_lz%>%filter(YEAR==2018)%>%group_by(UP2)%>%summarise(sum_FY_BZ=sum(FY_BZ,na.rm=TRUE))%>%
  arrange(desc(sum_FY_BZ))%>%as_tibble()
head(result_YEAR_FY_BZ)

##### graph update, rate formula ####
dt_lz$parent$UP2<-as.character(dt_lz$parent$UP2)
result_UP2<-dt_lz%>%group_by(UP2, YEAR)%>%summarise(sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ),sum_FY_BZ=sum(FY_BZ,na.rm = TRUE),
                                                    mean_rate_LEUKEMIA_BZ=mean(rate_LEUKEMIA_BZ_by_FY_BZ,na.rm=TRUE),n=n())%>%
  mutate(rate_by_UP2=sum_LEUKEMIA_BZ/sum_FY_BZ)%>%as_tibble()


gg<-ggplot(result_link, aes(x=YEAR, y=rate_by_UP2,group=UP2))

gg+geom_line(color='blue')+facet_wrap(~ factor(UP2,levels=unique(result_YEAR_FY_BZ$UP2)),scales="free_y")+
  geom_line(aes(x=YEAR,y=mean_rate_LEUKEMIA_BZ,group=UP2),color='red')

ggplot(result_UP2,aes(x=YEAR,y=sum_LEUKEMIA_BZ,group=UP2,colour=UP2),scales="free_y")+
  geom_line()+facet_wrap(~ factor(UP2,levels=unique(result_YEAR_FY_BZ$UP2)))

ggplot(result_UP2,aes(x=YEAR,y=sum_FY_BZ,group=UP2,colour=UP2),scales="free_y")+
  geom_line()+facet_wrap(~ factor(UP2,levels=unique(result_YEAR_FY_BZ$UP2)))

ggplot(result_UP2,aes(x=YEAR,y=n,group=UP2,colour=UP2),scales="free_y")+
  geom_line()+facet_wrap(~ factor(UP2,levels=unique(result_YEAR_FY_BZ$UP2)))


##### graph check #####
check_UP2<-c("39","97","2","42","37","19","84","70","31")
result_peak<-dt_lz%>%filter(UP2 %in% check_UP2)%>%group_by(YEAR,UP2)%>%
  summarise(sum_LEUKEMIA=sum(LEUKEMIA_BZ),n=n(),sum_FY_BZ=sum(FY_BZ,na.rm=TRUE))%>%as_tibble()

ggplot(result_peak,aes(x=YEAR,y=sum_LEUKEMIA,group=UP2,colour=UP2))+
  geom_line()+facet_wrap(~factor(result_YEAR_FY_BZ$UP2))

ggplot(result_peak,aes(x=YEAR,y=n,group=UP2,colour=UP2))+
  geom_line()+facet_wrap(~UP2)

ggplot(result_peak,aes(x=YEAR,y=sum_FY_BZ,group=UP2,colour=UP2))+
  geom_line()+facet_wrap(~UP2)


check3<-c("39","2","97","37","19")
result_peak2<-dt_lz%>%filter(UP2 %in% check3)%>%group_by(YEAR,UP2)%>%
  summarise(sum_LEUKEMIA=sum(LEUKEMIA_BZ),n=n(),sum_FY_BZ=sum(FY_BZ,na.rm=TRUE))%>%as_tibble()

ggplot(result_peak2,aes(x=YEAR,y=sum_LEUKEMIA,group=UP2,colour=UP2))+
  geom_line()+facet_wrap(~UP2)+scale_y_continuous(limits=c(0,50))

ggplot(result_peak2,aes(x=YEAR,y=n,group=UP2,colour=UP2))+
  geom_line()+facet_wrap(~UP2)+scale_y_continuous(limits=c(0,10000))

ggplot(result_peak2,aes(x=YEAR,y=sum_FY_BZ,group=UP2,colour=UP2))+
  geom_line()+facet_wrap(~UP2)+scale_y_continuous(limits=c(0,1e+06))





####### last follow up year에 대해 백혈병 발생률, 발생 건수 top 10 파악 ########
result_2018<-dt_lz%>%filter(YEAR==2018)%>%group_by(UP2)%>%summarise(sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ),sum_FY_BZ=sum(FY_BZ,na.rm = TRUE),
                                                                             mean_rate_LEUKEMIA_BZ=mean(rate_LEUKEMIA_BZ_by_FY_BZ,na.rm=TRUE))%>%
  mutate(rate_by_UP2=sum_LEUKEMIA_BZ/sum_FY_BZ)%>%as_tibble()
result_2018%>%arrange(desc(rate_by_UP2))%>%as_tibble()
result_2018%>%arrange(desc(mean_rate_LEUKEMIA_BZ))%>%as_tibble()
result_2018%>%arrange(desc(sum_LEUKEMIA_BZ))%>%as_tibble()
