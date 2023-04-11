######## Data check 6/20 ver ######
########## 수정한 부분 ###########
## 1. UP2가 NA인 관측치의 관측값은 모두 NA인 것을 확인 --> 삭제 ##
##################################
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


##################### 5년 누적 발생률 계산 ##########################
weight_leukemia_year_sort<-weight_leukemia%>%arrange(UP2, YEAR)%>%as_tibble()

head(weight_leukemia_year_sort)

year_5_dt<-as.data.frame(weight_leukemia_year_sort)
year_5_dt$sum_FY_BZ_5<-year_5_dt$sum_FY_BZ_x
year_5_dt$sum_LEUKEMIA_BZ_5<-year_5_dt$sum_LEUKEMIA_BZ_x
year_5_dt$sum_weight_FY_BZ_5<-year_5_dt$sum_weight_FY_BZ
year_5_dt$sum_weight_LEUKEMIA_BZ_5<-year_5_dt$sum_weight_LEUKEMIA_BZ

UP2_level<-unique(year_5_dt$UP2)

for(i in 1:length(UP2_level)){
  min_year<-min(year_5_dt$YEAR[which(year_5_dt$UP2==UP2_level[i])])
  
  for(j in seq(min_year,2013,by=1)){
    ind1<-which((year_5_dt$UP2==UP2_level[i]) & (year_5_dt$YEAR==j))
    ind2<-which((year_5_dt$UP2==UP2_level[i]) & (year_5_dt$YEAR==(j+5)))
    year_5_dt$sum_FY_BZ_5[ind2]<-year_5_dt$sum_FY_BZ_x[ind2]-year_5_dt$sum_FY_BZ_x[ind1]
    year_5_dt$sum_LEUKEMIA_BZ_5[ind2]<-year_5_dt$sum_LEUKEMIA_BZ_x[ind2]-year_5_dt$sum_LEUKEMIA_BZ_x[ind1]
    year_5_dt$sum_weight_FY_BZ_5[ind2]<-year_5_dt$sum_weight_FY_BZ[ind2]-year_5_dt$sum_weight_FY_BZ[ind1]
    year_5_dt$sum_weight_LEUKEMIA_BZ_5[ind2]<-year_5_dt$sum_weight_LEUKEMIA_BZ[ind2]-year_5_dt$sum_weight_LEUKEMIA_BZ[ind1]
  }
}

head(year_5_dt)

year_5_dt_rate<-year_5_dt%>%mutate(origin_rate_5=sum_LEUKEMIA_BZ_5/sum_FY_BZ_5, 
                                   weight_rate_5=sum_weight_LEUKEMIA_BZ_5/sum_weight_FY_BZ_5)%>%as_tibble()
head(year_5_dt_rate)


gg<-ggplot(year_5_dt_rate, aes(x=YEAR, y=origin_rate_5, 
                                group=UP2))

gg+geom_line(color='red')+labs(y="rate_of_LEUKEMIA_5_year")+
  facet_wrap(~ factor(UP2,levels=unique(result_YEAR_FY_BZ$UP2)),scales="free_y")+
  geom_line(aes(x=YEAR,y=weight_rate_5,
                group=UP2),color='black')

gg+geom_line(color='red')+labs(y="rate_of_LEUKEMIA_5_year")+
  facet_wrap(~ factor(UP2,levels=unique(result_YEAR_FY_BZ$UP2)))+
  geom_line(aes(x=YEAR,y=weight_rate_5,
                group=UP2),color='black')

year_5_sort1<-year_5_dt_rate%>%mutate(SIR_5=abs(origin_rate_5/weight_rate_5))%>%
  arrange(desc(SIR_5))%>%as_tibble()
year_5_sort1[c("UP2","YEAR","SIR_5")]

year_5_sort2<-year_5_dt_rate%>%arrange(desc(origin_rate_5))%>%as_tibble()
year_5_sort2[c("UP2","YEAR","origin_rate_5")]

year_5_sort3<-year_5_dt_rate%>%filter(YEAR==2018)%>%arrange(desc(origin_rate_5))%>%as_tibble()
year_5_sort3[c("UP2","origin_rate_5")]
