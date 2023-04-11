
### 1. test Plot file import ------------------------------------------------
test_final<-read.csv("test_finalPlotting_221026.csv")


### 1-1. LEVEL L2b Data 시각화 -------------------------------------------------
L2b<-test_final%>%filter(LEVEL=="L2b")%>%mutate(UP1=substr(NAME,2,2))%>%as_tibble()

library(readr)
code<-read_csv("한국표준산업분류10차.csv",locale = locale("ko", encoding = "euc-kr"))
code_UP1 <- code[c("대분류(21)","...2")]
code_UP1<-na.omit(code_UP1)
colnames(code_UP1)<-c("UP1","name")
code_UP1<-code_UP1[2:nrow(code_UP1),]
L2b_join<-left_join(L2b,code_UP1,by="UP1")

L2b_join<-L2b_join%>%mutate(NAME2=paste0(UP1," : ",name))%>%as_tibble()

ind.01<-which(L2b_join$CAL2==1)
ind.01.raw<-which(L2b_join$CAL2==1 & L2b_join$YEAR<=2018)
ind.01.pred<-which(L2b_join$CAL2==1 & L2b_join$YEAR>=2018)

ind.234<-which(L2b_join$CAL2==234)
ind.234.raw<-which(L2b_join$CAL2==234 & L2b_join$YEAR<=2018)
ind.234.pred<-which(L2b_join$CAL2==234 & L2b_join$YEAR>=2018)


# leukemia + ref ----------------------------------------------------------------
ggplot()+geom_line(data=L2b_join[ind.01.raw,],aes(x=YEAR,y=LEUKEMIA_final,color="red",linetype='solid'),size=0.5)+labs(y="누적 발생 건수")+
  facet_wrap(~ factor(NAME2,levels=unique(L2b_join$NAME2)),scales="free")+
  theme(strip.text.x = element_text(size = 8,colour = "black"))+
  geom_line(data=L2b_join[ind.01.pred,],aes(x=YEAR,y=LEUKEMIA_final,color="red",linetype='solid'),size=0.5)+
  geom_line(data=L2b_join[ind.01,],aes(x=YEAR,y=LEUKEMIA_ref,color="red",linetype='dashed'),size=0.8)+
  ggtitle("사업장(대분류)별 백혈병 누적 발생 건수 추세")+
  geom_line(data=L2b_join[ind.234.raw,],aes(x=YEAR,y=LEUKEMIA_final,color="blue",linetype='solid'),size=0.5)+
  geom_line(data=L2b_join[ind.234.pred,],aes(x=YEAR,y=LEUKEMIA_final,color='blue',linetype='solid'),size=0.5)+
  geom_line(data=L2b_join[ind.234,],aes(x=YEAR,y=LEUKEMIA_ref,color="blue",linetype='dashed'),size=0.8)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='orange', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='입사시기',
                       breaks=c("red","blue"),
                       labels=c("입사시기 : 2000년 까지","입사시기 : 2000년 이후"),
                       guide='legend')+
  scale_linetype_identity(name='Group',
                          breaks=c("solid","dashed"),
                          labels=c("대상 업종","표준군"),
                          guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

# lung + ref ----------------------------------------------------------------
ggplot()+geom_line(data=L2b_join[ind.01.raw,],aes(x=YEAR,y=LUNG_final,color="red",linetype='solid'),size=0.6)+labs(y="누적 발생 건수")+
  facet_wrap(~ factor(NAME2,levels=unique(L2b_join$NAME2)),scales="free")+
  theme(strip.text.x = element_text(size = 8))+
  geom_line(data=L2b_join[ind.01.pred,],aes(x=YEAR,y=LUNG_final,color="red",linetype='solid'),size=0.6)+
  geom_line(data=L2b_join[ind.01,],aes(x=YEAR,y=LUNG_ref,color="red",linetype='dashed'),size=0.8)+
  ggtitle("사업장(대분류)별 폐암 누적 발생 건수 추세")+
  geom_line(data=L2b_join[ind.234.raw,],aes(x=YEAR,y=LUNG_final,color="blue",linetype='solid'),size=0.6)+
  geom_line(data=L2b_join[ind.234.pred,],aes(x=YEAR,y=LUNG_final,color='blue',linetype='solid'),size=0.6)+
  geom_line(data=L2b_join[ind.234,],aes(x=YEAR,y=LUNG_ref,color="blue",linetype='dashed'),size=0.8)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='orange', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='입사시기',
                       breaks=c("red","blue"),
                       labels=c("입사시기 : 2000년 까지","입사시기 : 2000년 이후"),
                       guide='legend')+
  scale_linetype_identity(name='Group',
                          breaks=c("solid","dashed"),
                          labels=c("대상 업종","표준군"),
                          guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))





### 1-2. LEVEL L2ab Data 시각화 -------------------------------------------------
L2ab<-test_final%>%filter(LEVEL=="L2ab")%>%mutate(UP1=substr(NAME,2,2))%>%as_tibble()
L2ab_join<-left_join(L2ab,code_UP1,by="UP1")

L2ab_join<-L2ab_join%>%mutate(NAME2=paste0(UP1," : ",name))%>%as_tibble()

ind.M.01<-which(L2ab_join$SEX=="M" & L2ab_join$CAL2==1)
ind.M.01.raw<-which(L2ab_join$SEX=="M" & L2ab_join$CAL2==1 & L2ab_join$YEAR<=2018)
ind.M.01.pred<-which(L2ab_join$SEX=="M" & L2ab_join$CAL2==1 & L2ab_join$YEAR>=2018)

ind.M.234<-which(L2ab_join$SEX=="M" & L2ab_join$CAL2==234)
ind.M.234.raw<-which(L2ab_join$SEX=="M" & L2ab_join$CAL2==234 & L2ab_join$YEAR<=2018)
ind.M.234.pred<-which(L2ab_join$SEX=="M" & L2ab_join$CAL2==234 & L2ab_join$YEAR>=2018)

ind.F.01<-which(L2ab_join$SEX=="F" & L2ab_join$CAL2==1)
ind.F.01.raw<-which(L2ab_join$SEX=="F" & L2ab_join$CAL2==1 & L2ab_join$YEAR<=2018)
ind.F.01.pred<-which(L2ab_join$SEX=="F" & L2ab_join$CAL2==1 & L2ab_join$YEAR>=2018)

ind.F.234<-which(L2ab_join$SEX=="F" & L2ab_join$CAL2==234)
ind.F.234.raw<-which(L2ab_join$SEX=="F" & L2ab_join$CAL2==234 & L2ab_join$YEAR<=2018)
ind.F.234.pred<-which(L2ab_join$SEX=="F" & L2ab_join$CAL2==234 & L2ab_join$YEAR>=2018)


# leukemia + male + ref ---------------------------------------------------
ggplot()+geom_line(data=L2ab_join[ind.M.01.raw,],aes(x=YEAR,y=LEUKEMIA_final,color="red",linetype='solid'),size=0.6)+labs(y="누적발생건수")+
  facet_wrap(~ factor(NAME2,levels=unique(L2ab_join$NAME2)),scales="free")+
  theme(strip.text.x = element_text(size = 8,colour = "black"))+
  geom_line(data=L2ab_join[ind.M.01.pred,],aes(x=YEAR,y=LEUKEMIA_final,color="red",linetype='solid'),size=0.6)+
  geom_line(data=L2ab_join[ind.M.01,],aes(x=YEAR,y=LEUKEMIA_ref,color="red",linetype='dashed'),size=0.8)+
  ggtitle("사업장(대분류)별 백혈병 누적 발생 건수 추세 - 남성 근로자 집단")+
  geom_line(data=L2ab_join[ind.M.234.raw,],aes(x=YEAR,y=LEUKEMIA_final,color="blue",linetype='solid'),size=0.6)+
  geom_line(data=L2ab_join[ind.M.234.pred,],aes(x=YEAR,y=LEUKEMIA_final,color="blue",linetype='solid'),size=0.6)+
  geom_line(data=L2ab_join[ind.M.234,],aes(x=YEAR,y=LEUKEMIA_ref,color="blue",linetype='dashed'),size=0.8)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='orange', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='입사시기',
                       breaks=c("red","blue"),
                       labels=c("입사시기 : 2000년 까지","입사시기 : 2000년 이후"),
                       guide='legend')+
  scale_linetype_identity(name='Group',
                          breaks=c("solid","dashed"),
                          labels=c("대상 업종","표준군"),
                          guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


# leukemia + female + ref ---------------------------------------------------
ggplot()+geom_line(data=L2ab_join[ind.F.01.raw,],aes(x=YEAR,y=LEUKEMIA_final,color="red",linetype='solid'),size=0.6)+labs(y="누적발생건수")+
  facet_wrap(~ factor(NAME2,levels=unique(L2ab_join$NAME2)),scales="free")+
  theme(strip.text.x = element_text(size = 8,colour = 'black'))+
  geom_line(data=L2ab_join[ind.F.01.pred,],aes(x=YEAR,y=LEUKEMIA_final,color="red",linetype='solid'),size=0.6)+
  geom_line(data=L2ab_join[ind.F.01,],aes(x=YEAR,y=LEUKEMIA_ref,color="red",linetype='dashed'),size=0.8)+
  ggtitle("사업장(대분류)별 백혈병 누적 발생 건수 추세 - 여성 근로자 집단")+
  geom_line(data=L2ab_join[ind.F.234.raw,],aes(x=YEAR,y=LEUKEMIA_final,color="blue",linetype='solid'),size=0.6)+
  geom_line(data=L2ab_join[ind.F.234.pred,],aes(x=YEAR,y=LEUKEMIA_final,color="blue",linetype='solid'),size=0.6)+
  geom_line(data=L2ab_join[ind.F.234,],aes(x=YEAR,y=LEUKEMIA_ref,color="blue",linetype='dashed'),size=0.8)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='orange', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='입사시기',
                       breaks=c("red","blue"),
                       labels=c("입사시기 : 2000년 까지","입사시기 : 2000년 이후"),
                       guide='legend')+
  scale_linetype_identity(name='Group',
                          breaks=c("solid","dashed"),
                          labels=c("대상 업종","표준군"),
                          guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


# lung + male + ref ---------------------------------------------------
ggplot()+geom_line(data=L2ab_join[ind.M.01.raw,],aes(x=YEAR,y=LUNG_final,color="red",linetype='solid'),size=0.6)+labs(y="누적발생건수")+
  facet_wrap(~ factor(NAME2,levels=unique(L2ab_join$NAME2)),scales="free")+
  theme(strip.text.x = element_text(size = 8,colour = 'black'))+
  geom_line(data=L2ab_join[ind.M.01.pred,],aes(x=YEAR,y=LUNG_final,color="red",linetype='solid'),size=0.6)+
  geom_line(data=L2ab_join[ind.M.01,],aes(x=YEAR,y=LUNG_ref,color="red",linetype='dashed'),size=0.8)+
  ggtitle("사업장(대분류)별 폐암 누적 발생 건수 추세 - 남성 근로자 집단")+
  geom_line(data=L2ab_join[ind.M.234.raw,],aes(x=YEAR,y=LUNG_final,color="blue",linetype='solid'),size=0.6)+
  geom_line(data=L2ab_join[ind.M.234.pred,],aes(x=YEAR,y=LUNG_final,color="blue",linetype='solid'),size=0.6)+
  geom_line(data=L2ab_join[ind.M.234,],aes(x=YEAR,y=LUNG_ref,color="blue",linetype='dashed'),size=0.8)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='orange', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='입사시기',
                       breaks=c("red","blue"),
                       labels=c("입사시기 : 2000년 까지","입사시기 : 2000년 이후"),
                       guide='legend')+
  scale_linetype_identity(name='Group',
                          breaks=c("solid","dashed"),
                          labels=c("대상 업종","표준군"),
                          guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

# lung + female + ref ---------------------------------------------------
ggplot()+geom_line(data=L2ab_join[ind.F.01.raw,],aes(x=YEAR,y=LUNG_final,color="red",linetype='solid'),size=0.6)+labs(y="누적발생건수")+
  facet_wrap(~ factor(NAME2,levels=unique(L2ab_join$NAME2)),scales="free")+
  theme(strip.text.x = element_text(size = 8,colour = 'black'))+
  geom_line(data=L2ab_join[ind.F.01.pred,],aes(x=YEAR,y=LUNG_final,color="red",linetype='solid'),size=0.6)+
  geom_line(data=L2ab_join[ind.F.01,],aes(x=YEAR,y=LUNG_ref,color="red",linetype='dashed'),size=0.8)+
  ggtitle("사업장(대분류)별 폐암 누적 발생 건수 추세 - 여성 근로자 집단")+
  geom_line(data=L2ab_join[ind.F.234.raw,],aes(x=YEAR,y=LUNG_final,color="blue",linetype='solid'),size=0.6)+
  geom_line(data=L2ab_join[ind.F.234.pred,],aes(x=YEAR,y=LUNG_final,color="blue",linetype='solid'),size=0.6)+
  geom_line(data=L2ab_join[ind.F.234,],aes(x=YEAR,y=LUNG_ref,color="blue",linetype='dashed'),size=0.8)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='orange', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='입사시기',
                       breaks=c("red","blue"),
                       labels=c("입사시기 : 2000년 까지","입사시기 : 2000년 이후"),
                       guide='legend')+
  scale_linetype_identity(name='Group',
                          breaks=c("solid","dashed"),
                          labels=c("대상 업종","표준군"),
                          guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))



### 1-3. LEVEL L3b Data 시각화 -------------------------------------------------
L3b<-test_final%>%filter(LEVEL=="L3b")%>%as_tibble()

code_UP2 <- code[c("중분류(77)","...4")]
code_UP2<-na.omit(code_UP2)
colnames(code_UP2)<-c("UP2","name")
code_UP2<-code_UP2[2:nrow(code_UP2),]
code_UP2$UP2<-as.numeric(code_UP2$UP2)
L3b_join<-left_join(L3b,code_UP2,by="UP2")

L3b_join<-L3b_join%>%mutate(NAME2=paste0(UP2," : ",name))%>%as_tibble()

ind.01<-which(L3b_join$CAL2==1)
ind.01.raw<-which(L3b_join$CAL2==1 & L3b_join$YEAR<=2018)
ind.01.pred<-which(L3b_join$CAL2==1 & L3b_join$YEAR>=2018)

ind.234<-which(L3b_join$CAL2==234)
ind.234.raw<-which(L3b_join$CAL2==234 & L3b_join$YEAR<=2018)
ind.234.pred<-which(L3b_join$CAL2==234 & L3b_join$YEAR>=2018)


# leukemia + ref ----------------------------------------------------------------
ggplot()+geom_line(data=L3b_join[ind.01.raw,],aes(x=YEAR,y=LEUKEMIA_final,color="red",linetype='solid'),size=0.6)+labs(y="누적 발생 건수")+
  facet_wrap(~ factor(NAME2,levels=unique(L3b_join$NAME2)),scales="free_y",ncol=9)+
  theme(strip.text.x = element_text(size = 6.5,colour = 'black',face='bold'))+
  geom_line(data=L3b_join[ind.01.pred,],aes(x=YEAR,y=LEUKEMIA_final,color="red",linetype='solid'),size=0.6)+
  geom_line(data=L3b_join[ind.01,],aes(x=YEAR,y=LEUKEMIA_ref,color="red",linetype='dashed'),size=0.8)+
  ggtitle("사업장(중분류)별 백혈병 누적 발생 건수 추세")+
  geom_line(data=L3b_join[ind.234.raw,],aes(x=YEAR,y=LEUKEMIA_final,color="blue",linetype='solid'),size=0.6)+
  geom_line(data=L3b_join[ind.234.pred,],aes(x=YEAR,y=LEUKEMIA_final,color='blue',linetype='solid'),size=0.6)+
  geom_line(data=L3b_join[ind.234,],aes(x=YEAR,y=LEUKEMIA_ref,color="blue",linetype='dashed'),size=0.8)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='orange', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='입사시기',
                       breaks=c("red","blue"),
                       labels=c("입사시기 : 2000년 까지","입사시기 : 2000년 이후"),
                       guide='legend')+
  scale_linetype_identity(name='Group',
                          breaks=c("solid","dashed"),
                          labels=c("대상 업종","표준군"),
                          guide='legend')+
  theme(legend.title=element_text(size=12),
        legend.text=element_text(size=10))

# lung + ref ----------------------------------------------------------------
ggplot()+geom_line(data=L3b_join[ind.01.raw,],aes(x=YEAR,y=LUNG_final,color="red",linetype='solid'),size=0.6)+labs(y="누적 발생 건수")+
  facet_wrap(~ factor(NAME2,levels=unique(L3b_join$NAME2)),scales="free_y",ncol=9)+
  theme(strip.text.x = element_text(size = 6.5,colour = 'black',face='bold'))+
  geom_line(data=L3b_join[ind.01.pred,],aes(x=YEAR,y=LUNG_final,color="red",linetype='solid'),size=0.6)+
  geom_line(data=L3b_join[ind.01,],aes(x=YEAR,y=LUNG_ref,color="red",linetype='dashed'),size=0.8)+
  ggtitle("사업장(중분류)별 폐암 누적 발생 건수 추세")+
  geom_line(data=L3b_join[ind.234.raw,],aes(x=YEAR,y=LUNG_final,color="blue",linetype='solid'),size=0.6)+
  geom_line(data=L3b_join[ind.234.pred,],aes(x=YEAR,y=LUNG_final,color='blue',linetype='solid'),size=0.6)+
  geom_line(data=L3b_join[ind.234,],aes(x=YEAR,y=LUNG_ref,color="blue",linetype='dashed'),size=0.8)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='orange', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='입사시기',
                       breaks=c("red","blue"),
                       labels=c("입사시기 : 2000년 까지","입사시기 : 2000년 이후"),
                       guide='legend')+
  scale_linetype_identity(name='Group',
                          breaks=c("solid","dashed"),
                          labels=c("대상 업종","표준군"),
                          guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))



### 1-4. LEVEL L3ab Data 시각화 -------------------------------------------------
L3ab<-test_final%>%filter(LEVEL=="L3ab")%>%as_tibble()
L3ab_join<-left_join(L3ab,code_UP2,by="UP2")

L3ab_join<-L3ab_join%>%mutate(NAME2=paste0(UP2," : ",name))%>%as_tibble()

ind.M.01<-which(L3ab_join$SEX=="M" & L3ab_join$CAL2==1)
ind.M.01.raw<-which(L3ab_join$SEX=="M" & L3ab_join$CAL2==1 & L3ab_join$YEAR<=2018)
ind.M.01.pred<-which(L3ab_join$SEX=="M" & L3ab_join$CAL2==1 & L3ab_join$YEAR>=2018)

ind.M.234<-which(L3ab_join$SEX=="M" & L3ab_join$CAL2==234)
ind.M.234.raw<-which(L3ab_join$SEX=="M" & L3ab_join$CAL2==234 & L3ab_join$YEAR<=2018)
ind.M.234.pred<-which(L3ab_join$SEX=="M" & L3ab_join$CAL2==234 & L3ab_join$YEAR>=2018)

ind.F.01<-which(L3ab_join$SEX=="F" & L3ab_join$CAL2==1)
ind.F.01.raw<-which(L3ab_join$SEX=="F" & L3ab_join$CAL2==1 & L3ab_join$YEAR<=2018)
ind.F.01.pred<-which(L3ab_join$SEX=="F" & L3ab_join$CAL2==1 & L3ab_join$YEAR>=2018)

ind.F.234<-which(L3ab_join$SEX=="F" & L3ab_join$CAL2==234)
ind.F.234.raw<-which(L3ab_join$SEX=="F" & L3ab_join$CAL2==234 & L3ab_join$YEAR<=2018)
ind.F.234.pred<-which(L3ab_join$SEX=="F" & L3ab_join$CAL2==234 & L3ab_join$YEAR>2018)

# leukemia + male + ref ---------------------------------------------------
ggplot()+geom_line(data=L3ab_join[ind.M.01.raw,],aes(x=YEAR,y=LEUKEMIA_final,color="red",linetype='solid'),size=0.6)+labs(y="누적발생건수")+
  facet_wrap(~ factor(NAME2,levels=unique(L3ab_join$NAME2)),scales="free_y",ncol=9)+
  theme(strip.text.x = element_text(size = 6.5,face='bold',colour='black'))+
  geom_line(data=L3ab_join[ind.M.01.pred,],aes(x=YEAR,y=LEUKEMIA_final,color="red",linetype='solid'),size=0.6)+
  geom_line(data=L3ab_join[ind.M.01,],aes(x=YEAR,y=LEUKEMIA_ref,color="red",linetype='dashed'),size=0.8)+
  ggtitle("사업장(중분류)별 백혈병 누적 발생 건수 추세 - 남성 근로자 집단")+
  geom_line(data=L3ab_join[ind.M.234.raw,],aes(x=YEAR,y=LEUKEMIA_final,color="blue",linetype='solid'),size=0.6)+
  geom_line(data=L3ab_join[ind.M.234.pred,],aes(x=YEAR,y=LEUKEMIA_final,color="blue",linetype='solid'),size=0.6)+
  geom_line(data=L3ab_join[ind.M.234,],aes(x=YEAR,y=LEUKEMIA_ref,color="blue",linetype='dashed'),size=0.8)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='orange', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='입사시기',
                       breaks=c("red","blue"),
                       labels=c("입사시기 : 2000년 까지","입사시기 : 2000년 이후"),
                       guide='legend')+
  scale_linetype_identity(name='Group',
                          breaks=c("solid","dashed"),
                          labels=c("대상 업종","표준군"),
                          guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))


# leukemia + female + ref ---------------------------------------------------
ggplot()+geom_line(data=L3ab_join[ind.F.01.raw,],aes(x=YEAR,y=LEUKEMIA_final,color="red",linetype='solid'),size=0.6)+labs(y="누적발생건수")+
  facet_wrap(~ factor(NAME2,levels=unique(L3ab_join$NAME2)),scales="free_y",ncol=9)+
  theme(strip.text.x = element_text(size = 6.5,face='bold',colour='black'))+
  geom_line(data=L3ab_join[ind.F.01.pred,],aes(x=YEAR,y=LEUKEMIA_final,color="red",linetype='solid'),size=0.6)+
  geom_line(data=L3ab_join[ind.F.01,],aes(x=YEAR,y=LEUKEMIA_ref,color="red",linetype='dashed'),size=0.8)+
  ggtitle("사업장(중분류)별 백혈병 누적 발생 건수 추세 - 여성 근로자 집단")+
  geom_line(data=L3ab_join[ind.F.234.raw,],aes(x=YEAR,y=LEUKEMIA_final,color="blue",linetype='solid'),size=0.6)+
  geom_line(data=L3ab_join[ind.F.234.pred,],aes(x=YEAR,y=LEUKEMIA_final,color="blue",linetype='solid'),size=0.6)+
  geom_line(data=L3ab_join[ind.F.234,],aes(x=YEAR,y=LEUKEMIA_ref,color="blue",linetype='dashed'),size=0.8)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='orange', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='입사시기',
                       breaks=c("red","blue"),
                       labels=c("입사시기 : 2000년 까지","입사시기 : 2000년 이후"),
                       guide='legend')+
  scale_linetype_identity(name='Group',
                          breaks=c("solid","dashed"),
                          labels=c("대상 업종","표준군"),
                          guide='legend')+
  theme(legend.title=element_text(size=12),
        legend.text=element_text(size=10))


# lung + male + ref ---------------------------------------------------
ggplot()+geom_line(data=L3ab_join[ind.M.01.raw,],aes(x=YEAR,y=LUNG_final,color="red",linetype='solid'),size=0.6)+labs(y="누적발생건수")+
  facet_wrap(~ factor(NAME2,levels=unique(L3ab_join$NAME2)),scales="free_y",ncol=9)+
  theme(strip.text.x = element_text(size = 6.5,colour='black',face='bold'))+
  geom_line(data=L3ab_join[ind.M.01.pred,],aes(x=YEAR,y=LUNG_final,color="red",linetype='solid'),size=0.6)+
  geom_line(data=L3ab_join[ind.M.01,],aes(x=YEAR,y=LUNG_ref,color="red",linetype='dashed'),size=0.8)+
  ggtitle("사업장(중분류)별 폐암 누적 발생 건수 추세 - 남성 근로자 집단")+
  geom_line(data=L3ab_join[ind.M.234.raw,],aes(x=YEAR,y=LUNG_final,color="blue",linetype='solid'),size=0.6)+
  geom_line(data=L3ab_join[ind.M.234.pred,],aes(x=YEAR,y=LUNG_final,color="blue",linetype='solid'),size=0.6)+
  geom_line(data=L3ab_join[ind.M.234,],aes(x=YEAR,y=LUNG_ref,color="blue",linetype='dashed'),size=0.8)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='orange', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='입사시기',
                       breaks=c("red","blue"),
                       labels=c("입사시기 : 2000년 까지","입사시기 : 2000년 이후"),
                       guide='legend')+
  scale_linetype_identity(name='Group',
                          breaks=c("solid","dashed"),
                          labels=c("대상 업종","표준군"),
                          guide='legend')+
  theme(legend.title=element_text(size=13),
        legend.text=element_text(size=12))

# lung + female + ref ---------------------------------------------------
ggplot()+geom_line(data=L3ab_join[ind.F.01.raw,],aes(x=YEAR,y=LUNG_final,color="red",linetype='solid'),size=0.6)+labs(y="누적발생건수")+
  facet_wrap(~ factor(NAME2,levels=unique(L3ab_join$NAME2)),scales="free_y",ncol=9)+
  theme(strip.text.x = element_text(size = 6.5,colour = 'black',face='bold'))+
  geom_line(data=L3ab_join[ind.F.01.pred,],aes(x=YEAR,y=LUNG_final,color="red",linetype='solid'),size=0.6)+
  geom_line(data=L3ab_join[ind.F.01,],aes(x=YEAR,y=LUNG_ref,color="red",linetype='dashed'),size=0.8)+
  ggtitle("사업장(중분류)별 폐암 누적 발생 건수 추세 - 여성 근로자 집단")+
  geom_line(data=L3ab_join[ind.F.234.raw,],aes(x=YEAR,y=LUNG_final,color="blue",linetype='solid'),size=0.6)+
  geom_line(data=L3ab_join[ind.F.234.pred,],aes(x=YEAR,y=LUNG_final,color="blue",linetype='solid'),size=0.6)+
  geom_line(data=L3ab_join[ind.F.234,],aes(x=YEAR,y=LUNG_ref,color="blue",linetype='dashed'),size=0.8)+
  geom_vline(xintercept=2018, linetype = 'twodash', color='orange', size = 1)+
  labs(x="Year")+
  scale_color_identity(name='입사시기',
                       breaks=c("red","blue"),
                       labels=c("입사시기 : 2000년 까지","입사시기 : 2000년 이후"),
                       guide='legend')+
  scale_linetype_identity(name='Group',
                          breaks=c("solid","dashed"),
                          labels=c("대상 업종","표준군"),
                          guide='legend')+
  theme(legend.title=element_text(size=12),
        legend.text=element_text(size=10))


