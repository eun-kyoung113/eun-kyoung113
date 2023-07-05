### 6월 3일 ver ###
dt<-fread("/shared/data/koshri/ref_leukemia_workplace1.csv")
unique(dt$CAL)

########### 1 ###########
CAL<-unique(dt$CAL)
NA_info<-matrix(0,nrow=length(CAL),ncol=2)
colnames(NA_info)<-c("FY_BZ","ENR_BZ")
rownames(NA_info)<-CAL

for(cal in CAL){
  dt_cal<-dt[dt$CAL==cal,]
  cal_year_lower<-1996+(cal*5)
  cal_year_upper<-2000+(cal*5)
  cal_year<-seq(cal_year_lower, cal_year_upper, by=1)
  
  ind_FY_BZ<-which(is.na(dt_cal$FY_BZ[dt_cal$YEAR %in% cal_year]))
  ind_ENR_BZ<-which(is.na(dt_cal$ENR_BZ[dt_cal$YEAR %in% cal_year]))
  
  NA_info[(cal+1),]<-c(length(ind_FY_BZ),length(ind_ENR_BZ))
  
}

## check ##
dt_cal_0<-dt[dt$CAL==0,]
table(is.na(dt_cal_0$FY_BZ))
#FALSE    TRUE 
#3952578    3830

table(dt_cal_0$YEAR) # 2000년 ~ 2018년
cal_0_year<-seq(1996, 2000, by=1)
ind<-which(is.na(dt_cal_0$FY_BZ[dt_cal_0$YEAR %in% cal_0_year])) # 116개

table(is.na(dt_cal_0$FY_BZ[dt_cal_0$YEAR %in% cal_0_year]))
#FALSE   TRUE 
#208116    116 

(dt_cal_0$FY_BZ[dt_cal_0$YEAR %in% cal_0_year])[ind] # 모두 NA임.

################### 2 ####################
library(dtplyr)
dt_lz<-lazy_dt(dt)
result<-dt_lz%>%group_by(YEAR)%>%
  summarise(sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ),sum_FY=sum(FY_BZ,na.rm = TRUE),sum_ENR=sum(ENR_BZ))%>%
  mutate(rate_per_year_FY=(sum_LEUKEMIA_BZ/sum_FY)*100000, rate_per_year_ENR=(sum_LEUKEMIA_BZ/sum_ENR)*100000)%>%
  as_tibble()

result

LEUKEMIA_BZ_cancer_data<-data.frame("year"=seq(2000,2018,by=1),"LEUKEMIA_BZ"=100000*c(2007, 2209, 2319, 2283, 2372, 2346, 2443, 2477, 2601, 2717, 2760, 2900, 2873, 3072, 3114, 3286, 3437, 3397, 3520),
                                    "CR"=c(4.2, 4.6, 4.8, 4.7, 4.9, 4.8, 5.0, 5.0, 5.3, 5.5, 5.5, 5.8, 5.7, 6.1, 6.1, 6.4, 6.7, 6.6, 6.9))
LEUKEMIA_BZ_cancer_data

plot(LEUKEMIA_BZ_cancer_data$year, LEUKEMIA_BZ_cancer_data$CR, xlab="YEAR", ylab="rate of LEUKEMIA event per year using FY_BZ", ylim=c(0,7), type='c',
     main="Comparison of LEUKEMIA rate per year")
lines(result$YEAR, result$rate_per_year_FY, type='l',col='red')
legend("topleft",legend=c("cancer_data","our_data"),col=c("black","red"),lty=c(5,1))



################## 3 #######################
dt_lz$parent$UP2<-as.character(dt_lz$parent$UP2)
result_UP2<-dt_lz%>%group_by(UP2, YEAR)%>%summarise(sum_LEUKEMIA_BZ=sum(LEUKEMIA_BZ),sum_FY_BZ=sum(FY_BZ,na.rm = TRUE),mean_LEUKEMIA_BZ=mean(LEUKEMIA_BZ))%>%
  mutate(rate_by_UP2=sum_LEUKEMIA_BZ/sum_FY_BZ)%>%arrange(YEAR, desc(rate_by_UP2))%>%as_tibble()

ggplot(result_UP2, aes(x=YEAR,y=rate_by_UP2,group=UP2,colour=UP2)) + 
  geom_line()+facet_wrap(~ UP2)

ggplot(result_UP2, aes(x=YEAR,y=mean_LEUKEMIA_BZ,group=UP2,colour=UP2)) + 
  geom_line()+facet_wrap(~ UP2)











