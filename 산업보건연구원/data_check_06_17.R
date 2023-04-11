#### data check 6/17 ver ####
dt_leukemia<-fread("/shared/data/koshri/ref_leukemia_bz_v2.csv")
dt_lung<-fread("/shared/data/koshri/ref_lung_bz_v2.csv")

library(dtplyr)
dt_lz_leukemia<-lazy_dt(dt_leukemia)
dt_lz_lung<-lazy_dt(dt_lung)



########################### Leukemia ######################################
dt_lz_leukemia$parent$UP2<-as.character(dt_lz_leukemia$parent$UP2)
dt_lz_leukemia<-dt_lz_leukemia%>%filter(!is.na(UP2))
key_all<-dt_lz_leukemia%>%group_by(UP2, SEX, CAL, ECNY_AGE, YEAR)%>%summarise(sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ),sum_FY_BZ=sum(FY_BZ,na.rm = TRUE))%>%
  mutate(rate_by_UP2=sum_LEUKEMIA_BZ/sum_FY_BZ)%>%as_tibble()

counterfactual<-dt_lz_leukemia%>%group_by(SEX, CAL, ECNY_AGE, YEAR)%>%summarise(sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ),sum_FY_BZ=sum(FY_BZ,na.rm = TRUE))%>%
  as_tibble()

merge_leukemia<-left_join(key_all,counterfactual,by=c("SEX","CAL","ECNY_AGE","YEAR"))
merge_leukemia_add<-merge_leukemia%>%mutate(weight=(sum_FY_BZ.x/sum_FY_BZ.y),
                                            weight_FY_BZ=weight*sum_FY_BZ.y, weight_leukemia=weight*sum_LEUKEMIA_BZ.y)%>%as_tibble()
head(merge_leukemia_add)
weight_leukemia<-merge_leukemia_add%>%group_by(UP2, YEAR)%>%summarise(sum_FY_BZ_x=sum(sum_FY_BZ.x), sum_LEUKEMIA_BZ_x=sum(sum_LEUKEMIA_BZ.x),
                                                                      sum_weight_FY_BZ=sum(weight_FY_BZ), sum_weight_LEUKEMIA_BZ=sum(weight_leukemia))%>%
  mutate(origin_rate=sum_LEUKEMIA_BZ_x/sum_FY_BZ_x, weight_rate=sum_weight_LEUKEMIA_BZ/sum_weight_FY_BZ)%>%as_tibble()

head(weight_leukemia)

gg<-ggplot(weight_leukemia, aes(x=YEAR, y=origin_rate, 
                                   group=UP2))

gg+geom_line(color='red')+labs(y="rate_of_LEUKEMIA")+
  facet_wrap(~ factor(UP2,levels=unique(result_YEAR_FY_BZ$UP2)),scales="free_y")+
  geom_line(aes(x=YEAR,y=weight_rate,
                group=UP2),color='black')

gg+geom_line(color='red')+labs(y="rate_of_LEUKEMIA")+
  facet_wrap(~ factor(UP2,levels=unique(result_YEAR_FY_BZ$UP2)))+
  geom_line(aes(x=YEAR,y=weight_rate,
                group=UP2),color='black')

weight_leukemia_sort1<-weight_leukemia%>%filter(YEAR==2018)%>%mutate(SIR=abs(origin_rate/weight_rate))%>%
  arrange(desc(SIR))%>%as_tibble()
weight_leukemia_sort1

weight_leukemia_sort2<-weight_leukemia%>%filter(YEAR==2018)%>%
  arrange(desc(origin_rate))%>%as_tibble()
weight_leukemia_sort2

weight_leukemia_sort3<-weight_leukemia%>%filter(YEAR==2018)%>%mutate(RR=abs(origin_rate-weight_rate))%>%
  arrange(desc(RR))%>%as_tibble()
weight_leukemia_sort3



