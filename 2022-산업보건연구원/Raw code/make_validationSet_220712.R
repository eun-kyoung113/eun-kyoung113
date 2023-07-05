
## import data

library(tidyverse)
library(data.table)
setDTthreads(24)
library(stringr)

dat1 = fread("/shared/data/koshri/ref_leukemia_bz_v4.csv")
dat2 = fread("/shared/data/koshri/ref_lung_bz_v4.csv")


## pre-processing
dat1$SEX = ifelse(dat1$SEX=="남", "M", "F")
dat1 %>% mutate(CAL2=ifelse( (CAL<=1), "01", "234")) -> dat1
dat1 = dat1[dat1$UP1 != ""]

dat2$SEX = ifelse(dat2$SEX=="남", "M", "F")
dat2 %>% mutate(CAL2=ifelse( (CAL<=1), "01", "234")) -> dat2
dat2 = dat2[dat2$UP1 != ""]


## table 1: meta table for variable name information

### level 1

dat1 %>% 
  filter(YEAR==2018) %>% 
  summarize(FY_LEUKEMIA_2018 = sum(FY_BZ), LEUKEMIA_2018 = sum(LEUKEMIA_BZ)) -> df11
dat2 %>% 
  filter(YEAR==2018) %>% 
  summarize(FY_LUNG_2018 = sum(FY_BZ), LUNG_2018 = sum(LUNG_BZ)) -> df21

df1 = data.frame(LEVEL=1, NAME="X", UP1=NA, UP2=NA, SEX=NA, CAL2=NA, df11, df21)


### level 2

dat1 %>% 
  filter(YEAR==2018) %>% 
  group_by(UP1) %>%
  summarize(FY_LEUKEMIA_2018 = sum(FY_BZ), LEUKEMIA_2018 = sum(LEUKEMIA_BZ)) -> df12
dat2 %>% 
  filter(YEAR==2018) %>% 
  group_by(UP1) %>%
  summarize(FY_LUNG_2018 = sum(FY_BZ), LUNG_2018 = sum(LUNG_BZ)) -> df22

df2 = data.frame(LEVEL=2, NAME=paste0("X", df12$UP1), UP1=df12$UP1, 
                 UP2=NA, SEX=NA, CAL2=NA, 
                 df12[,c("FY_LEUKEMIA_2018", "LEUKEMIA_2018")], df22[,c("FY_LUNG_2018", "LUNG_2018")])


### level 3

dat1 %>% 
  filter(YEAR==2018) %>% 
  group_by(UP1, UP2) %>%
  summarize(FY_LEUKEMIA_2018 = sum(FY_BZ), LEUKEMIA_2018 = sum(LEUKEMIA_BZ)) -> df13
dat2 %>% 
  filter(YEAR==2018) %>% 
  group_by(UP1, UP2) %>%
  summarize(FY_LUNG_2018 = sum(FY_BZ), LUNG_2018 = sum(LUNG_BZ)) -> df23

df3 = data.frame(LEVEL=3, NAME=paste0("X", df13$UP1, df13$UP2), 
                 UP1=df13$UP1, UP2=df13$UP2, SEX=NA, CAL2=NA, 
                 df13[,c("FY_LEUKEMIA_2018", "LEUKEMIA_2018")], df23[,c("FY_LUNG_2018", "LUNG_2018")])


### level 4

dat1 %>% 
  filter(YEAR==2018) %>% 
  group_by(UP1, UP2, SEX) %>%
  summarize(FY_LEUKEMIA_2018 = sum(FY_BZ), LEUKEMIA_2018 = sum(LEUKEMIA_BZ)) -> df14
dat2 %>% 
  filter(YEAR==2018) %>% 
  group_by(UP1, UP2, SEX) %>%
  summarize(FY_LUNG_2018 = sum(FY_BZ), LUNG_2018 = sum(LUNG_BZ)) -> df24

df4 = data.frame(LEVEL=4, NAME=paste0("X", df14$UP1, df14$UP2, df14$SEX), 
                 UP1=df14$UP1, UP2=df14$UP2, SEX=df14$SEX, CAL2=NA, 
                 df14[,c("FY_LEUKEMIA_2018", "LEUKEMIA_2018")], df24[,c("FY_LUNG_2018", "LUNG_2018")])


  
### level 5

dat1 %>% 
  filter(YEAR==2018) %>% 
  group_by(UP1, UP2, SEX, CAL2) %>%
  summarize(FY_LEUKEMIA_2018 = sum(FY_BZ), LEUKEMIA_2018 = sum(LEUKEMIA_BZ)) -> df15
dat2 %>% 
  filter(YEAR==2018) %>% 
  group_by(UP1, UP2, SEX, CAL2) %>%
  summarize(FY_LUNG_2018 = sum(FY_BZ), LUNG_2018 = sum(LUNG_BZ)) -> df25

df5 = data.frame(LEVEL=5, NAME=paste0("X", df15$UP1, df15$UP2, df15$SEX, df15$CAL2), 
                 UP1=df15$UP1, UP2=df15$UP2, SEX=df15$SEX, CAL2=df15$CAL2, 
                 df15[,c("FY_LEUKEMIA_2018", "LEUKEMIA_2018")], df25[,c("FY_LUNG_2018", "LUNG_2018")])


df_meta = rbind(df1, df2, df3, df4, df5)


# 삭제할 group 있나 살펴보고, 


## table 2: true value vs. predicted value table

### level 1

dat1 %>% 
  filter(YEAR>=2015) %>% 
  group_by(YEAR) %>%
  summarize(FY_LEUKEMIA = sum(FY_BZ), LEUKEMIA = sum(LEUKEMIA_BZ)) -> val11
dat2 %>% 
  filter(YEAR>=2015) %>% 
  group_by(YEAR) %>%
  summarize(FY_LUNG = sum(FY_BZ), LUNG = sum(LUNG_BZ)) -> val21

val1 = data.frame(LEVEL=1, NAME="X", UP1=NA, UP2=NA, SEX=NA, CAL2=NA, 
                 YEAR=val11$YEAR, 
                 val11[,c("FY_LEUKEMIA", "LEUKEMIA")], val21[,c("FY_LUNG", "LUNG")])

### level 2

dat1 %>% 
  filter(YEAR>=2015) %>% 
  group_by(UP1, YEAR) %>%
  summarize(FY_LEUKEMIA = sum(FY_BZ), LEUKEMIA = sum(LEUKEMIA_BZ)) -> val12
dat2 %>% 
  filter(YEAR>=2015) %>% 
  group_by(UP1, YEAR) %>%
  summarize(FY_LUNG = sum(FY_BZ), LUNG = sum(LUNG_BZ)) -> val22

val2 = data.frame(LEVEL=2, NAME=paste0("X", val12$UP1), UP1=val12$UP1, 
                 UP2=NA, SEX=NA, CAL2=NA, YEAR=val12$YEAR,
                 val12[,c("FY_LEUKEMIA", "LEUKEMIA")], val22[,c("FY_LUNG", "LUNG")])


### level 3

dat1 %>% 
  filter(YEAR>=2015) %>% 
  group_by(UP1, UP2, YEAR) %>%
  summarize(FY_LEUKEMIA = sum(FY_BZ), LEUKEMIA = sum(LEUKEMIA_BZ)) -> val13
dat2 %>% 
  filter(YEAR>=2015) %>% 
  group_by(UP1, UP2, YEAR) %>%
  summarize(FY_LUNG = sum(FY_BZ), LUNG = sum(LUNG_BZ)) -> val23

val3 = data.frame(LEVEL=3, NAME=paste0("X", val13$UP1, val13$UP2), 
                 UP1=val13$UP1, UP2=val13$UP2, SEX=NA, CAL2=NA, YEAR=val13$YEAR,
                 val13[,c("FY_LEUKEMIA", "LEUKEMIA")], val23[,c("FY_LUNG", "LUNG")])


### level 4

dat1 %>% 
  filter(YEAR>=2015) %>% 
  group_by(UP1, UP2, SEX, YEAR) %>%
  summarize(FY_LEUKEMIA = sum(FY_BZ), LEUKEMIA = sum(LEUKEMIA_BZ)) -> val14
dat2 %>% 
  filter(YEAR>=2015) %>% 
  group_by(UP1, UP2, SEX, YEAR) %>%
  summarize(FY_LUNG = sum(FY_BZ), LUNG = sum(LUNG_BZ)) -> val24

val4 = data.frame(LEVEL=4, NAME=paste0("X", val14$UP1, val14$UP2, val14$SEX), 
                 UP1=val14$UP1, UP2=val14$UP2, SEX=val14$SEX, CAL2=NA, YEAR=val14$YEAR,
                 val14[,c("FY_LEUKEMIA", "LEUKEMIA")], val24[,c("FY_LUNG", "LUNG")])



### level 5

dat1 %>% 
  filter(YEAR>=2015) %>% 
  group_by(UP1, UP2, SEX, CAL2, YEAR) %>%
  summarize(FY_LEUKEMIA = sum(FY_BZ), LEUKEMIA = sum(LEUKEMIA_BZ)) -> val15
dat2 %>% 
  filter(YEAR>=2015) %>% 
  group_by(UP1, UP2, SEX, CAL2, YEAR) %>%
  summarize(FY_LUNG = sum(FY_BZ), LUNG = sum(LUNG_BZ)) -> val25

val5 = data.frame(LEVEL=5, NAME=paste0("X", val15$UP1, val15$UP2, val15$SEX, val15$CAL2), 
                 UP1=val15$UP1, UP2=val15$UP2, SEX=val15$SEX, CAL2=val15$CAL2, YEAR=val15$YEAR,
                 val15[,c("FY_LEUKEMIA", "LEUKEMIA")], val25[,c("FY_LUNG", "LUNG")])


vals = rbind(val1, val2, val3, val4, val5)
write.csv(vals, "validation.csv", row.names=F, col.names=T)

rm(vals)
vals = read.csv("validation.csv", header=T)
# assume that all lung cases after 2014 were forecasted as 10 persons.
vals$pred_method1 = 10

# to calculate MAPE
# if true LUNG is zero, then the resulting value will be infinity (ignore this for now)
MAPE_method1 = vals %>% 
  group_by(NAME) %>%
  summarize(MAPE = mean(abs(LUNG - pred_method1) / LUNG) * 100)

MAPE_method1
