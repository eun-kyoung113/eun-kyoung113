## Plotting - 보고서 삽입 -------------------------------------------------------
### 11월 14일 Version ---------------------------------------------------------

### -------------------------------------------------------------------------
### kind_error : 2--> "training_error", 3 --> "validation error"
### -------------------------------------------------------------------------

library(ggplot2)


### LEUEKMIA ----------------------------------------------------------------
### 1) CAL2=0 & UP1 기준 grouping ---------------------------------------------

### 1)-0. Data import ----------------------------------------------------------
LEUKEMIA_01_UP1_full<-read.csv("LEUKEMIA_01_sub_UP1_full.csv", fileEncoding = "CP949", encoding = "UTF-8")
LEUKEMIA_F01_UP1_full<-read.csv("LEUKEMIA_F01_sub_UP1_full.csv", fileEncoding = "CP949", encoding = "UTF-8")
LEUKEMIA_M01_UP1_full<-read.csv("LEUKEMIA_M01_sub_UP1_full.csv", fileEncoding = "CP949", encoding = "UTF-8")


### 1)-1. Data binding ----------------------------------------------------------
### full------------------
LEUKEMIA_01_UP1_tr<-LEUKEMIA_01_UP1_full[,c("NAME","SEX","LEUKEMIA_tr")]
LEUKEMIA_01_UP1_tr$kind_error<-rep(2,length=nrow(LEUKEMIA_01_UP1_tr))
LEUKEMIA_01_UP1_tr$SEX<-"남/여 합계"
colnames(LEUKEMIA_01_UP1_tr)<-c("NAME", "SEX", "error", "kind_error")

LEUKEMIA_01_UP1_val<-LEUKEMIA_01_UP1_full[,c("NAME","SEX","LEUKEMIA_val")]
LEUKEMIA_01_UP1_val$kind_error<-rep(3,length=nrow(LEUKEMIA_01_UP1_val))
LEUKEMIA_01_UP1_val$SEX<-"남/여 합계"
colnames(LEUKEMIA_01_UP1_val)<-c("NAME", "SEX", "error", "kind_error")

### male------------------
LEUKEMIA_M01_UP1_tr<-LEUKEMIA_M01_UP1_full[,c("NAME","SEX","LEUKEMIA_tr")]
LEUKEMIA_M01_UP1_tr$kind_error<-rep(2,length=nrow(LEUKEMIA_M01_UP1_tr))
LEUKEMIA_M01_UP1_tr$SEX<-"남성"
colnames(LEUKEMIA_M01_UP1_tr)<-c("NAME", "SEX", "error", "kind_error")

LEUKEMIA_M01_UP1_val<-LEUKEMIA_M01_UP1_full[,c("NAME","SEX","LEUKEMIA_val")]
LEUKEMIA_M01_UP1_val$kind_error<-rep(3,length=nrow(LEUKEMIA_M01_UP1_val))
LEUKEMIA_M01_UP1_val$SEX<-"남성"
colnames(LEUKEMIA_M01_UP1_val)<-c("NAME", "SEX", "error", "kind_error")

### female------------------
LEUKEMIA_F01_UP1_tr<-LEUKEMIA_F01_UP1_full[,c("NAME","SEX","LEUKEMIA_tr")]
LEUKEMIA_F01_UP1_tr$kind_error<-rep(4,length=nrow(LEUKEMIA_F01_UP1_tr))
LEUKEMIA_F01_UP1_tr$SEX<-"여성"
colnames(LEUKEMIA_F01_UP1_tr)<-c("NAME", "SEX", "error", "kind_error")

LEUKEMIA_F01_UP1_val<-LEUKEMIA_F01_UP1_full[,c("NAME","SEX","LEUKEMIA_val")]
LEUKEMIA_F01_UP1_val$kind_error<-rep(6,length=nrow(LEUKEMIA_F01_UP1_val))
LEUKEMIA_F01_UP1_val$SEX<-"여성"
colnames(LEUKEMIA_F01_UP1_val)<-c("NAME", "SEX", "error", "kind_error")



LEUKEMIA_01_UP1_total<-as.data.frame(rbind(LEUKEMIA_01_UP1_tr, LEUKEMIA_01_UP1_val, LEUKEMIA_M01_UP1_tr, LEUKEMIA_M01_UP1_val,
                                           LEUKEMIA_F01_UP1_tr, LEUKEMIA_F01_UP1_val))

LEUKEMIA_01_UP1_total$kind_error<-as.factor(LEUKEMIA_01_UP1_total$kind_error)
str(LEUKEMIA_01_UP1_total)



### -------------------------------------------------------------------------
### 2) CAL2=1 & UP1 기준 grouping ---------------------------------------------

### 1)-0. Data import ----------------------------------------------------------
LEUKEMIA_234_UP1_full<-read.csv("LEUKEMIA_234_sub_UP1_full.csv", fileEncoding = "CP949", encoding = "UTF-8")
LEUKEMIA_F234_UP1_full<-read.csv("LEUKEMIA_F234_sub_UP1_full.csv", fileEncoding = "CP949", encoding = "UTF-8")
LEUKEMIA_M234_UP1_full<-read.csv("LEUKEMIA_M234_sub_UP1_full.csv", fileEncoding = "CP949", encoding = "UTF-8")


### 1)-1. Data binding ----------------------------------------------------------
### full------------------
LEUKEMIA_234_UP1_tr<-LEUKEMIA_234_UP1_full[,c("NAME","SEX","LEUKEMIA_tr")]
LEUKEMIA_234_UP1_tr$kind_error<-rep(2,length=nrow(LEUKEMIA_234_UP1_tr))
LEUKEMIA_234_UP1_tr$SEX<-"남/여 합계"
colnames(LEUKEMIA_234_UP1_tr)<-c("NAME", "SEX", "error", "kind_error")

LEUKEMIA_234_UP1_val<-LEUKEMIA_234_UP1_full[,c("NAME","SEX","LEUKEMIA_val")]
LEUKEMIA_234_UP1_val$kind_error<-rep(3,length=nrow(LEUKEMIA_234_UP1_val))
LEUKEMIA_234_UP1_val$SEX<-"남/여 합계"
colnames(LEUKEMIA_234_UP1_val)<-c("NAME", "SEX", "error", "kind_error")


### male------------------
LEUKEMIA_M234_UP1_tr<-LEUKEMIA_M234_UP1_full[,c("NAME","SEX","LEUKEMIA_tr")]
LEUKEMIA_M234_UP1_tr$kind_error<-rep(2,length=nrow(LEUKEMIA_M234_UP1_tr))
LEUKEMIA_M234_UP1_tr$SEX<-"남성"
colnames(LEUKEMIA_M234_UP1_tr)<-c("NAME", "SEX", "error", "kind_error")

LEUKEMIA_M234_UP1_val<-LEUKEMIA_M234_UP1_full[,c("NAME","SEX","LEUKEMIA_val")]
LEUKEMIA_M234_UP1_val$kind_error<-rep(3,length=nrow(LEUKEMIA_M234_UP1_val))
LEUKEMIA_M234_UP1_val$SEX<-"남성"
colnames(LEUKEMIA_M234_UP1_val)<-c("NAME", "SEX", "error", "kind_error")


### female------------------
LEUKEMIA_F234_UP1_tr<-LEUKEMIA_F234_UP1_full[,c("NAME","SEX","LEUKEMIA_tr")]
LEUKEMIA_F234_UP1_tr$kind_error<-rep(2,length=nrow(LEUKEMIA_F234_UP1_tr))
LEUKEMIA_F234_UP1_tr$SEX<-"여성"
colnames(LEUKEMIA_F234_UP1_tr)<-c("NAME", "SEX", "error", "kind_error")

LEUKEMIA_F234_UP1_val<-LEUKEMIA_F234_UP1_full[,c("NAME","SEX","LEUKEMIA_val")]
LEUKEMIA_F234_UP1_val$kind_error<-rep(3,length=nrow(LEUKEMIA_F234_UP1_val))
LEUKEMIA_F234_UP1_val$SEX<-"여성"
colnames(LEUKEMIA_F234_UP1_val)<-c("NAME", "SEX", "error", "kind_error")



LEUKEMIA_234_UP1_total<-as.data.frame(rbind(LEUKEMIA_234_UP1_tr, LEUKEMIA_234_UP1_val, LEUKEMIA_M234_UP1_tr, LEUKEMIA_M234_UP1_val,
                                            LEUKEMIA_F234_UP1_tr, LEUKEMIA_F234_UP1_val))


LEUKEMIA_234_UP1_total$kind_error<-as.factor(LEUKEMIA_234_UP1_total$kind_error)
str(LEUKEMIA_234_UP1_total)




### -------------------------------------------------------------------------
### 3) CAL2=0 & UP2 기준 grouping ---------------------------------------------

### 3)-0. Data import ----------------------------------------------------------
LEUKEMIA_01_UP2_full<-read.csv("LEUKEMIA_01_sub_UP2_full.csv", fileEncoding = "CP949", encoding = "UTF-8")
LEUKEMIA_F01_UP2_full<-read.csv("LEUKEMIA_F01_sub_UP2_full.csv", fileEncoding = "CP949", encoding = "UTF-8")
LEUKEMIA_M01_UP2_full<-read.csv("LEUKEMIA_M01_sub_UP2_full.csv", fileEncoding = "CP949", encoding = "UTF-8")


### 3)-1. Data binding ----------------------------------------------------------
### full------------------
LEUKEMIA_01_UP2_tr<-LEUKEMIA_01_UP2_full[,c("NAME","SEX","LEUKEMIA_tr")]
LEUKEMIA_01_UP2_tr$kind_error<-rep(2,length=nrow(LEUKEMIA_01_UP2_tr))
LEUKEMIA_01_UP2_tr$SEX<-"남/여 합계"
colnames(LEUKEMIA_01_UP2_tr)<-c("NAME", "SEX", "error", "kind_error")

LEUKEMIA_01_UP2_val<-LEUKEMIA_01_UP2_full[,c("NAME","SEX","LEUKEMIA_val")]
LEUKEMIA_01_UP2_val$kind_error<-rep(3,length=nrow(LEUKEMIA_01_UP2_val))
LEUKEMIA_01_UP2_val$SEX<-"남/여 합계"
colnames(LEUKEMIA_01_UP2_val)<-c("NAME", "SEX", "error", "kind_error")


### male------------------
LEUKEMIA_M01_UP2_tr<-LEUKEMIA_M01_UP2_full[,c("NAME","SEX","LEUKEMIA_tr")]
LEUKEMIA_M01_UP2_tr$kind_error<-rep(2,length=nrow(LEUKEMIA_M01_UP2_tr))
LEUKEMIA_M01_UP2_tr$SEX<-"남성"
colnames(LEUKEMIA_M01_UP2_tr)<-c("NAME", "SEX", "error", "kind_error")

LEUKEMIA_M01_UP2_val<-LEUKEMIA_M01_UP2_full[,c("NAME","SEX","LEUKEMIA_val")]
LEUKEMIA_M01_UP2_val$kind_error<-rep(3,length=nrow(LEUKEMIA_M01_UP2_val))
LEUKEMIA_M01_UP2_val$SEX<-"남성"
colnames(LEUKEMIA_M01_UP2_val)<-c("NAME", "SEX", "error", "kind_error")


### female------------------
LEUKEMIA_F01_UP2_tr<-LEUKEMIA_F01_UP2_full[,c("NAME","SEX","LEUKEMIA_tr")]
LEUKEMIA_F01_UP2_tr$kind_error<-rep(2,length=nrow(LEUKEMIA_F01_UP2_tr))
LEUKEMIA_F01_UP2_tr$SEX<-"여성"
colnames(LEUKEMIA_F01_UP2_tr)<-c("NAME", "SEX", "error", "kind_error")

LEUKEMIA_F01_UP2_val<-LEUKEMIA_F01_UP2_full[,c("NAME","SEX","LEUKEMIA_val")]
LEUKEMIA_F01_UP2_val$kind_error<-rep(3,length=nrow(LEUKEMIA_F01_UP2_val))
LEUKEMIA_F01_UP2_val$SEX<-"여성"
colnames(LEUKEMIA_F01_UP2_val)<-c("NAME", "SEX", "error", "kind_error")



LEUKEMIA_01_UP2_total<-as.data.frame(rbind(LEUKEMIA_01_UP2_tr, LEUKEMIA_01_UP2_val, LEUKEMIA_M01_UP2_tr, LEUKEMIA_M01_UP2_val,
                                           LEUKEMIA_F01_UP2_tr, LEUKEMIA_F01_UP2_val))

LEUKEMIA_01_UP2_total$kind_error<-as.factor(LEUKEMIA_01_UP2_total$kind_error)




### 4) CAL2=1 & UP2 기준 grouping ---------------------------------------------

### 4)-0. Data import ----------------------------------------------------------
LEUKEMIA_234_UP2_full<-read.csv("LEUKEMIA_234_sub_UP2_full.csv", fileEncoding = "CP949", encoding = "UTF-8")
LEUKEMIA_F234_UP2_full<-read.csv("LEUKEMIA_F234_sub_UP2_full.csv", fileEncoding = "CP949", encoding = "UTF-8")
LEUKEMIA_M234_UP2_full<-read.csv("LEUKEMIA_M234_sub_UP2_full.csv", fileEncoding = "CP949", encoding = "UTF-8")


### 4)-1. Data binding ----------------------------------------------------------
### full------------------
LEUKEMIA_234_UP2_tr<-LEUKEMIA_234_UP2_full[,c("NAME","SEX","LEUKEMIA_tr")]
LEUKEMIA_234_UP2_tr$kind_error<-rep(2,length=nrow(LEUKEMIA_234_UP2_tr))
LEUKEMIA_234_UP2_tr$SEX<-"남/여 합계"
colnames(LEUKEMIA_234_UP2_tr)<-c("NAME", "SEX", "error", "kind_error")

LEUKEMIA_234_UP2_val<-LEUKEMIA_234_UP2_full[,c("NAME","SEX","LEUKEMIA_val")]
LEUKEMIA_234_UP2_val$kind_error<-rep(3,length=nrow(LEUKEMIA_234_UP2_val))
LEUKEMIA_234_UP2_val$SEX<-"남/여 합계"
colnames(LEUKEMIA_234_UP2_val)<-c("NAME", "SEX", "error", "kind_error")


### male------------------
LEUKEMIA_M234_UP2_tr<-LEUKEMIA_M234_UP2_full[,c("NAME","SEX","LEUKEMIA_tr")]
LEUKEMIA_M234_UP2_tr$kind_error<-rep(2,length=nrow(LEUKEMIA_M234_UP2_tr))
LEUKEMIA_M234_UP2_tr$SEX<-"남성"
colnames(LEUKEMIA_M234_UP2_tr)<-c("NAME", "SEX", "error", "kind_error")

LEUKEMIA_M234_UP2_val<-LEUKEMIA_M234_UP2_full[,c("NAME","SEX","LEUKEMIA_val")]
LEUKEMIA_M234_UP2_val$kind_error<-rep(3,length=nrow(LEUKEMIA_M234_UP2_val))
LEUKEMIA_M234_UP2_val$SEX<-"남성"
colnames(LEUKEMIA_M234_UP2_val)<-c("NAME", "SEX", "error", "kind_error")


### female------------------
LEUKEMIA_F234_UP2_tr<-LEUKEMIA_F234_UP2_full[,c("NAME","SEX","LEUKEMIA_tr")]
LEUKEMIA_F234_UP2_tr$kind_error<-rep(2,length=nrow(LEUKEMIA_F234_UP2_tr))
LEUKEMIA_F234_UP2_tr$SEX<-"여성"
colnames(LEUKEMIA_F234_UP2_tr)<-c("NAME", "SEX", "error", "kind_error")

LEUKEMIA_F234_UP2_val<-LEUKEMIA_F234_UP2_full[,c("NAME","SEX","LEUKEMIA_val")]
LEUKEMIA_F234_UP2_val$kind_error<-rep(3,length=nrow(LEUKEMIA_F234_UP2_val))
LEUKEMIA_F234_UP2_val$SEX<-"여성"
colnames(LEUKEMIA_F234_UP2_val)<-c("NAME", "SEX", "error", "kind_error")



LEUKEMIA_234_UP2_total<-as.data.frame(rbind(LEUKEMIA_234_UP2_tr, LEUKEMIA_234_UP2_val, LEUKEMIA_M234_UP2_tr, LEUKEMIA_M234_UP2_val,
                                            LEUKEMIA_F234_UP2_tr, LEUKEMIA_F234_UP2_val))


LEUKEMIA_234_UP2_total$kind_error<-as.factor(LEUKEMIA_234_UP2_total$kind_error)


### LEUEKMIA plotting -------------------------------------------------------
library(gridExtra)

a = ggplot(data=LEUKEMIA_01_UP1_total, aes(x=SEX, y=error, fill=kind_error))+ 
  geom_boxplot(width=0.8)+ theme(legend.position = "bottom")+
  scale_fill_discrete(name = "오류의 종류",labels=c("훈련오류","검증오류"))+
  ggtitle("대분류 기준 & 2000년 이전 입사")+
  labs(x="성별",y="오차비율")+ylim(0,1)



b = ggplot(data=LEUKEMIA_234_UP1_total, aes(x=SEX, y=error, fill=kind_error))+ 
  geom_boxplot(width=0.8)+theme(legend.position="none")+
  ggtitle("대분류 기준 & 2000년 이후 입사")+
  labs(x="성별",y="오차비율")+ylim(0,1)


c = ggplot(data=LEUKEMIA_01_UP2_total, aes(x=SEX, y=error, fill=kind_error))+ 
  geom_boxplot(width=0.8)+theme(legend.position="none")+
  ggtitle("중분류 기준 & 2000년 이전 입사")+
  labs(x="성별",y="오차비율")+ylim(0,1)


d = ggplot(data=LEUKEMIA_234_UP2_total, aes(x=SEX, y=error, fill=kind_error))+ 
  geom_boxplot(width=0.8)+theme(legend.position="none")+
  ggtitle("중분류 기준 & 2000년 이후 입사")+
  labs(x="성별",y="오차비율")+ylim(0,1)



g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(a)

grid.arrange(arrangeGrob(a + theme(legend.position="none"),
                         b + theme(legend.position="none"),
                         c + theme(legend.position="none"),
                         d + theme(legend.position="none"),
                         nrow=2 ),
             mylegend, nrow=2, heights=c(9,1))



### -------------------------------------------------------------------------
### LUNG ----------------------------------------------------------------
### 1) CAL2=0 & UP1 기준 grouping ---------------------------------------------

### 1)-0. Data import ----------------------------------------------------------
LUNG_01_UP1_full<-read.csv("LUNG_01_sub_UP1_full.csv", fileEncoding = "CP949", encoding = "UTF-8")
LUNG_F01_UP1_full<-read.csv("LUNG_F01_sub_UP1_full.csv", fileEncoding = "CP949", encoding = "UTF-8")
LUNG_M01_UP1_full<-read.csv("LUNG_M01_sub_UP1_full.csv", fileEncoding = "CP949", encoding = "UTF-8")


### 1)-1. Data binding ----------------------------------------------------------
### full------------------
LUNG_01_UP1_tr<-LUNG_01_UP1_full[,c("NAME","SEX","LUNG_tr")]
LUNG_01_UP1_tr$kind_error<-rep(2,length=nrow(LUNG_01_UP1_tr))
LUNG_01_UP1_tr$SEX<-"남/여 합계"
colnames(LUNG_01_UP1_tr)<-c("NAME", "SEX", "error", "kind_error")

LUNG_01_UP1_val<-LUNG_01_UP1_full[,c("NAME","SEX","LUNG_val")]
LUNG_01_UP1_val$kind_error<-rep(3,length=nrow(LUNG_01_UP1_val))
LUNG_01_UP1_val$SEX<-"남/여 합계"
colnames(LUNG_01_UP1_val)<-c("NAME", "SEX", "error", "kind_error")

### male------------------
LUNG_M01_UP1_tr<-LUNG_M01_UP1_full[,c("NAME","SEX","LUNG_tr")]
LUNG_M01_UP1_tr$kind_error<-rep(2,length=nrow(LUNG_M01_UP1_tr))
LUNG_M01_UP1_tr$SEX<-"남성"
colnames(LUNG_M01_UP1_tr)<-c("NAME", "SEX", "error", "kind_error")

LUNG_M01_UP1_val<-LUNG_M01_UP1_full[,c("NAME","SEX","LUNG_val")]
LUNG_M01_UP1_val$kind_error<-rep(3,length=nrow(LUNG_M01_UP1_val))
LUNG_M01_UP1_val$SEX<-"남성"
colnames(LUNG_M01_UP1_val)<-c("NAME", "SEX", "error", "kind_error")

### female------------------
LUNG_F01_UP1_tr<-LUNG_F01_UP1_full[,c("NAME","SEX","LUNG_tr")]
LUNG_F01_UP1_tr$kind_error<-rep(2,length=nrow(LUNG_F01_UP1_tr))
LUNG_F01_UP1_tr$SEX<-"여성"
colnames(LUNG_F01_UP1_tr)<-c("NAME", "SEX", "error", "kind_error")

LUNG_F01_UP1_val<-LUNG_F01_UP1_full[,c("NAME","SEX","LUNG_val")]
LUNG_F01_UP1_val$kind_error<-rep(3,length=nrow(LUNG_F01_UP1_val))
LUNG_F01_UP1_val$SEX<-"여성"
colnames(LUNG_F01_UP1_val)<-c("NAME", "SEX", "error", "kind_error")



LUNG_01_UP1_total<-as.data.frame(rbind(LUNG_01_UP1_tr, LUNG_01_UP1_val, LUNG_M01_UP1_tr, LUNG_M01_UP1_val,
                                       LUNG_F01_UP1_tr, LUNG_F01_UP1_val))

LUNG_01_UP1_total$kind_error<-as.factor(LUNG_01_UP1_total$kind_error)
str(LUNG_01_UP1_total)



### -------------------------------------------------------------------------
### 2) CAL2=1 & UP1 기준 grouping ---------------------------------------------

### 1)-0. Data import ----------------------------------------------------------
LUNG_234_UP1_full<-read.csv("LUNG_234_sub_UP1_full.csv", fileEncoding = "CP949", encoding = "UTF-8")
LUNG_F234_UP1_full<-read.csv("LUNG_F234_sub_UP1_full.csv", fileEncoding = "CP949", encoding = "UTF-8")
LUNG_M234_UP1_full<-read.csv("LUNG_M234_sub_UP1_full.csv", fileEncoding = "CP949", encoding = "UTF-8")


### 1)-1. Data binding ----------------------------------------------------------
### full------------------
LUNG_234_UP1_tr<-LUNG_234_UP1_full[,c("NAME","SEX","LUNG_tr")]
LUNG_234_UP1_tr$kind_error<-rep(2,length=nrow(LUNG_234_UP1_tr))
LUNG_234_UP1_tr$SEX<-"남/여 합계"
colnames(LUNG_234_UP1_tr)<-c("NAME", "SEX", "error", "kind_error")

LUNG_234_UP1_val<-LUNG_234_UP1_full[,c("NAME","SEX","LUNG_val")]
LUNG_234_UP1_val$kind_error<-rep(3,length=nrow(LUNG_234_UP1_val))
LUNG_234_UP1_val$SEX<-"남/여 합계"
colnames(LUNG_234_UP1_val)<-c("NAME", "SEX", "error", "kind_error")


### male------------------
LUNG_M234_UP1_tr<-LUNG_M234_UP1_full[,c("NAME","SEX","LUNG_tr")]
LUNG_M234_UP1_tr$kind_error<-rep(2,length=nrow(LUNG_M234_UP1_tr))
LUNG_M234_UP1_tr$SEX<-"남성"
colnames(LUNG_M234_UP1_tr)<-c("NAME", "SEX", "error", "kind_error")

LUNG_M234_UP1_val<-LUNG_M234_UP1_full[,c("NAME","SEX","LUNG_val")]
LUNG_M234_UP1_val$kind_error<-rep(3,length=nrow(LUNG_M234_UP1_val))
LUNG_M234_UP1_val$SEX<-"남성"
colnames(LUNG_M234_UP1_val)<-c("NAME", "SEX", "error", "kind_error")


### female------------------
LUNG_F234_UP1_tr<-LUNG_F234_UP1_full[,c("NAME","SEX","LUNG_tr")]
LUNG_F234_UP1_tr$kind_error<-rep(2,length=nrow(LUNG_F234_UP1_tr))
LUNG_F234_UP1_tr$SEX<-"여성"
colnames(LUNG_F234_UP1_tr)<-c("NAME", "SEX", "error", "kind_error")

LUNG_F234_UP1_val<-LUNG_F234_UP1_full[,c("NAME","SEX","LUNG_val")]
LUNG_F234_UP1_val$kind_error<-rep(3,length=nrow(LUNG_F234_UP1_val))
LUNG_F234_UP1_val$SEX<-"여성"
colnames(LUNG_F234_UP1_val)<-c("NAME", "SEX", "error", "kind_error")



LUNG_234_UP1_total<-as.data.frame(rbind(LUNG_234_UP1_tr, LUNG_234_UP1_val, LUNG_M234_UP1_tr, LUNG_M234_UP1_val,
                                        LUNG_F234_UP1_tr, LUNG_F234_UP1_val))


LUNG_234_UP1_total$kind_error<-as.factor(LUNG_234_UP1_total$kind_error)
str(LUNG_234_UP1_total)




### -------------------------------------------------------------------------
### 3) CAL2=0 & UP2 기준 grouping ---------------------------------------------

### 3)-0. Data import ----------------------------------------------------------
LUNG_01_UP2_full<-read.csv("LUNG_01_sub_UP2_full.csv", fileEncoding = "CP949", encoding = "UTF-8")
LUNG_F01_UP2_full<-read.csv("LUNG_F01_sub_UP2_full.csv", fileEncoding = "CP949", encoding = "UTF-8")
LUNG_M01_UP2_full<-read.csv("LUNG_M01_sub_UP2_full.csv", fileEncoding = "CP949", encoding = "UTF-8")


### 3)-1. Data binding ----------------------------------------------------------
### full------------------
LUNG_01_UP2_tr<-LUNG_01_UP2_full[,c("NAME","SEX","LUNG_tr")]
LUNG_01_UP2_tr$kind_error<-rep(2,length=nrow(LUNG_01_UP2_tr))
LUNG_01_UP2_tr$SEX<-"남/여 합계"
colnames(LUNG_01_UP2_tr)<-c("NAME", "SEX", "error", "kind_error")

LUNG_01_UP2_val<-LUNG_01_UP2_full[,c("NAME","SEX","LUNG_val")]
LUNG_01_UP2_val$kind_error<-rep(3,length=nrow(LUNG_01_UP2_val))
LUNG_01_UP2_val$SEX<-"남/여 합계"
colnames(LUNG_01_UP2_val)<-c("NAME", "SEX", "error", "kind_error")


### male------------------
LUNG_M01_UP2_tr<-LUNG_M01_UP2_full[,c("NAME","SEX","LUNG_tr")]
LUNG_M01_UP2_tr$kind_error<-rep(2,length=nrow(LUNG_M01_UP2_tr))
LUNG_M01_UP2_tr$SEX<-"남성"
colnames(LUNG_M01_UP2_tr)<-c("NAME", "SEX", "error", "kind_error")

LUNG_M01_UP2_val<-LUNG_M01_UP2_full[,c("NAME","SEX","LUNG_val")]
LUNG_M01_UP2_val$kind_error<-rep(3,length=nrow(LUNG_M01_UP2_val))
LUNG_M01_UP2_val$SEX<-"남성"
colnames(LUNG_M01_UP2_val)<-c("NAME", "SEX", "error", "kind_error")


### female------------------
LUNG_F01_UP2_tr<-LUNG_F01_UP2_full[,c("NAME","SEX","LUNG_tr")]
LUNG_F01_UP2_tr$kind_error<-rep(2,length=nrow(LUNG_F01_UP2_tr))
LUNG_F01_UP2_tr$SEX<-"여성"
colnames(LUNG_F01_UP2_tr)<-c("NAME", "SEX", "error", "kind_error")

LUNG_F01_UP2_val<-LUNG_F01_UP2_full[,c("NAME","SEX","LUNG_val")]
LUNG_F01_UP2_val$kind_error<-rep(3,length=nrow(LUNG_F01_UP2_val))
LUNG_F01_UP2_val$SEX<-"여성"
colnames(LUNG_F01_UP2_val)<-c("NAME", "SEX", "error", "kind_error")



LUNG_01_UP2_total<-as.data.frame(rbind(LUNG_01_UP2_tr, LUNG_01_UP2_val, LUNG_M01_UP2_tr, LUNG_M01_UP2_val,
                                       LUNG_F01_UP2_tr, LUNG_F01_UP2_val))

LUNG_01_UP2_total$kind_error<-as.factor(LUNG_01_UP2_total$kind_error)




### 4) CAL2=1 & UP2 기준 grouping ---------------------------------------------

### 4)-0. Data import ----------------------------------------------------------
LUNG_234_UP2_full<-read.csv("LUNG_234_sub_UP2_full.csv", fileEncoding = "CP949", encoding = "UTF-8")
LUNG_F234_UP2_full<-read.csv("LUNG_F234_sub_UP2_full.csv", fileEncoding = "CP949", encoding = "UTF-8")
LUNG_M234_UP2_full<-read.csv("LUNG_M234_sub_UP2_full.csv", fileEncoding = "CP949", encoding = "UTF-8")


### 4)-1. Data binding ----------------------------------------------------------
### full------------------
LUNG_234_UP2_tr<-LUNG_234_UP2_full[,c("NAME","SEX","LUNG_tr")]
LUNG_234_UP2_tr$kind_error<-rep(2,length=nrow(LUNG_234_UP2_tr))
LUNG_234_UP2_tr$SEX<-"남/여 합계"
colnames(LUNG_234_UP2_tr)<-c("NAME", "SEX", "error", "kind_error")

LUNG_234_UP2_val<-LUNG_234_UP2_full[,c("NAME","SEX","LUNG_val")]
LUNG_234_UP2_val$kind_error<-rep(3,length=nrow(LUNG_234_UP2_val))
LUNG_234_UP2_val$SEX<-"남/여 합계"
colnames(LUNG_234_UP2_val)<-c("NAME", "SEX", "error", "kind_error")


### male------------------
LUNG_M234_UP2_tr<-LUNG_M234_UP2_full[,c("NAME","SEX","LUNG_tr")]
LUNG_M234_UP2_tr$kind_error<-rep(2,length=nrow(LUNG_M234_UP2_tr))
LUNG_M234_UP2_tr$SEX<-"남성"
colnames(LUNG_M234_UP2_tr)<-c("NAME", "SEX", "error", "kind_error")

LUNG_M234_UP2_val<-LUNG_M234_UP2_full[,c("NAME","SEX","LUNG_val")]
LUNG_M234_UP2_val$kind_error<-rep(3,length=nrow(LUNG_M234_UP2_val))
LUNG_M234_UP2_val$SEX<-"남성"
colnames(LUNG_M234_UP2_val)<-c("NAME", "SEX", "error", "kind_error")


### female------------------
LUNG_F234_UP2_tr<-LUNG_F234_UP2_full[,c("NAME","SEX","LUNG_tr")]
LUNG_F234_UP2_tr$kind_error<-rep(2,length=nrow(LUNG_F234_UP2_tr))
LUNG_F234_UP2_tr$SEX<-"여성"
colnames(LUNG_F234_UP2_tr)<-c("NAME", "SEX", "error", "kind_error")

LUNG_F234_UP2_val<-LUNG_F234_UP2_full[,c("NAME","SEX","LUNG_val")]
LUNG_F234_UP2_val$kind_error<-rep(3,length=nrow(LUNG_F234_UP2_val))
LUNG_F234_UP2_val$SEX<-"여성"
colnames(LUNG_F234_UP2_val)<-c("NAME", "SEX", "error", "kind_error")



LUNG_234_UP2_total<-as.data.frame(rbind(LUNG_234_UP2_tr, LUNG_234_UP2_val, LUNG_M234_UP2_tr, LUNG_M234_UP2_val,
                                        LUNG_F234_UP2_tr, LUNG_F234_UP2_val))


LUNG_234_UP2_total$kind_error<-as.factor(LUNG_234_UP2_total$kind_error)


### LEUEKMIA plotting -------------------------------------------------------
library(gridExtra)

a = ggplot(data=LUNG_01_UP1_total, aes(x=SEX, y=error, fill=kind_error))+ 
  geom_boxplot(width=0.8)+ theme(legend.position = "bottom")+
  scale_fill_discrete(name = "오류의 종류",labels=c("훈련오류","검증오류"))+
  ggtitle("대분류 기준 & 2000년 이전 입사")+
  labs(x="성별",y="오차비율")+ylim(0,1)



b = ggplot(data=LUNG_234_UP1_total, aes(x=SEX, y=error, fill=kind_error))+ 
  geom_boxplot(width=0.8)+theme(legend.position="none")+
  ggtitle("대분류 기준 & 2000년 이후 입사")+
  labs(x="성별",y="오차비율")+ylim(0,1)


c = ggplot(data=LUNG_01_UP2_total, aes(x=SEX, y=error, fill=kind_error))+ 
  geom_boxplot(width=0.8)+theme(legend.position="none")+
  ggtitle("중분류 기준 & 2000년 이전 입사")+
  labs(x="성별",y="오차비율")+ylim(0,1)


d = ggplot(data=LUNG_234_UP2_total, aes(x=SEX, y=error, fill=kind_error))+ 
  geom_boxplot(width=0.8)+theme(legend.position="none")+
  ggtitle("중분류 기준 & 2000년 이후 입사")+
  labs(x="성별",y="오차비율")+ylim(0,1)



g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(a)

grid.arrange(arrangeGrob(a + theme(legend.position="none"),
                         b + theme(legend.position="none"),
                         c + theme(legend.position="none"),
                         d + theme(legend.position="none"),
                         nrow=2 ),
             mylegend, nrow=2, heights=c(9,1))
