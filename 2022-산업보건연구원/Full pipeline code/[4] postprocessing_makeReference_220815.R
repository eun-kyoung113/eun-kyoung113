# [4] 보고서에 첨부하는 UP1, UP2 별 RR(Relative risk) TOP10 사업장 나타내는 표 생성------------------
## <What to do>
## 1. 각 사업장 별 RR(Relative Risk) 계산
##   - Reference group의 "발병 건수 / 추적 인년 합계" 계산
##   - 가장 하위 level부터 계산해 덧셈 연산 이용해 차례대로 상위 level 계산

## 2. 보고서에 첨부하는 표 생성
##   : 표 종류는 총 9가지
## 2-1) 사업장 code로 grouping 하지 않은 전체 데이터 대상
##    - total : "YEAR"로만 grouping
##    - SEX 별 : "YEAR" & "SEX"로 grouping
##    - CAL2 별 : "YEAR" & "CAL2"로 grouping

## 2-2) 사업장 UP1 code로 grouping 한 데이터 대상
##    - total : "YEAR"로 추가 grouping
##    - SEX 별 : "YEAR" & "SEX"로 추가 grouping
##    - CAL2 별 : "YEAR" & "CAL2"로 추가 grouping

## 2-3) 사업장 UP2 code로 grouping 한 데이터 대상
##    - total : "YEAR"로 추가 grouping
##    - SEX 별 : "YEAR" & "SEX"로 추가 grouping
##    - CAL2 별 : "YEAR" & "CAL2"로 추가 grouping

## 3. 결과 excel file 생성 / 저장
# ----------------------------------------------------------------------------------------------------



### 1. [3]에서 생성한 연도별 final 예측값이 모두 저장된 file & [1]에서 생성한 계층 시계열 file import ---
df = read.csv("test_postprocessed.csv", header=T)
fulls <- read.csv("full_v2.csv")



# -------------------------------------------------------------------------
### 2. data 정리 --------------------------------------------------------------
meta_bs = unique(fulls[ ,1:7])
df = merge(x=meta_bs, y=df, by=c("ID", "NAME"))
df = df[order(df$ID, df$YEAR), ]

### pre-processing: ensemble_adjusted value 중 weird values는 0으로 대체 -------
df$LEUKEMIA_ensem_adj[df$LEUKEMIA_ensem_adj < 0.5] = 0
df$FY_LEUKEMIA_ensem_adj[df$FY_LEUKEMIA_ensem_adj < 0.5] = 0
df$LUNG_ensem_adj[df$LUNG_ensem_adj < 0.5] = 0
df$FY_LUNG_ensem_adj[df$FY_LUNG_ensem_adj < 0.5] = 0


### 반응변수들 중 결측치는 final prediction 값으로 대체 -------
# impute by predicted value
df$LEUKEMIA[is.na(df$LEUKEMIA)] = df$LEUKEMIA_ensem_adj[is.na(df$LEUKEMIA)]
df$FY_LEUKEMIA[is.na(df$FY_LEUKEMIA)] = df$FY_LEUKEMIA_ensem_adj[is.na(df$FY_LEUKEMIA)]
df$LUNG[is.na(df$LUNG)] = df$LUNG_ensem_adj[is.na(df$LUNG)]
df$FY_LUNG[is.na(df$FY_LUNG)] = df$FY_LUNG_ensem_adj[is.na(df$FY_LUNG)]



### 필요한 변수만 가져오기 ----------------------------------------------------
df = df[ ,c("ID", "NAME", "LEVEL", "UP1", "UP2", "SEX", "CAL2", "YEAR",
             "LEUKEMIA", "FY_LEUKEMIA", "LUNG", "FY_LUNG")]
colnames(df)[9:12] = paste0(colnames(df)[9:12], "_final")




# -------------------------------------------------------------------------
### 3. reference group의 발병 건수 , 추적 인년 합계 계산해 column으로 추가 -----
df$LEUKEMIA_ref = NA
df$LUNG_ref = NA


### fill level 3ab ---------
df %>% 
  filter(LEVEL == "L3ab") %>% 
  group_by(SEX, CAL2, YEAR) %>%
  mutate(LEUKEMIA_ref = FY_LEUKEMIA_final * sum(LEUKEMIA_final) / sum(FY_LEUKEMIA_final), 
         LUNG_ref = FY_LUNG_final * sum(LUNG_final) / sum(FY_LUNG_final)) -> df_L3ab
df_L3ab = df_L3ab[order(df_L3ab$ID, df_L3ab$YEAR), ]
df[df$LEVEL == "L3ab", c("LEUKEMIA_ref", "LUNG_ref")] = df_L3ab[ ,c("LEUKEMIA_ref", "LUNG_ref")]



### fill level 3a----------
df_L3a = df_L3ab %>%
  group_by(UP1, UP2, SEX, YEAR) %>%
  summarize(LEUKEMIA_final = sum(LEUKEMIA_final), FY_LEUKEMIA_final = sum(FY_LEUKEMIA_final),
            LUNG_final = sum(LUNG_final), FY_LUNG_final = sum(FY_LUNG_final),
            LEUKEMIA_ref = sum(LEUKEMIA_ref), LUNG_ref = sum(LUNG_ref)
            )
df_L3a = merge(x=meta_bs[meta_bs$LEVEL == "L3a",], 
               y=df_L3a, by=c("UP1", "UP2", "SEX"), sort=FALSE, all.x=FALSE, all.y=TRUE)
df_L3a = df_L3a[order(df_L3a$ID, df_L3a$YEAR), ]

### 올바르게 생성되었는지 double check----------
df_temp = df[df$LEVEL == "L3a", ]
print(df_temp[1:20, ])
print(df_L3a[1:20, ]) # OK
df[df$LEVEL == "L3a", c("LEUKEMIA_ref", "LUNG_ref")] = df_L3a[ ,c("LEUKEMIA_ref", "LUNG_ref")]



### fill level 3b -----------------
df_L3b = df_L3ab %>%
  group_by(UP1, UP2, CAL2, YEAR) %>%
  summarize(LEUKEMIA_final = sum(LEUKEMIA_final), FY_LEUKEMIA_final = sum(FY_LEUKEMIA_final),
            LUNG_final = sum(LUNG_final), FY_LUNG_final = sum(FY_LUNG_final),
            LEUKEMIA_ref = sum(LEUKEMIA_ref), LUNG_ref = sum(LUNG_ref)
  )
df_L3b = merge(x=meta_bs[meta_bs$LEVEL == "L3b",], 
               y=df_L3b, by=c("UP1", "UP2", "CAL2"), sort=FALSE, all.x=FALSE, all.y=TRUE)
df_L3b = df_L3b[order(df_L3b$ID, df_L3b$YEAR), ]

### 올바르게 생성되었는지 double check----------
df_temp = df[df$LEVEL == "L3b", ]
print(df_temp[1:20, ])
print(df_L3b[1:20, ]) # OK
df[df$LEVEL == "L3b", c("LEUKEMIA_ref", "LUNG_ref")] = df_L3b[ ,c("LEUKEMIA_ref", "LUNG_ref")]



### fill level 3 ------------
df_L3 = df_L3ab %>%
  group_by(UP1, UP2,  YEAR) %>%
  summarize(LEUKEMIA_final = sum(LEUKEMIA_final), FY_LEUKEMIA_final = sum(FY_LEUKEMIA_final),
            LUNG_final = sum(LUNG_final), FY_LUNG_final = sum(FY_LUNG_final),
            LEUKEMIA_ref = sum(LEUKEMIA_ref), LUNG_ref = sum(LUNG_ref)
  )
df_L3 = merge(x=meta_bs[meta_bs$LEVEL == "L3",], 
               y=df_L3, by=c("UP1", "UP2"), sort=FALSE, all.x=FALSE, all.y=TRUE)
df_L3 = df_L3[order(df_L3$ID, df_L3$YEAR), ]

### 올바르게 생성되었는지 double check----------
df_temp = df[df$LEVEL == "L3", ]
print(df_temp[1:20, ])
print(df_L3[1:20, ]) # OK
df[df$LEVEL == "L3", c("LEUKEMIA_ref", "LUNG_ref")] = df_L3[ ,c("LEUKEMIA_ref", "LUNG_ref")]



### fill level 2ab ------------------
df %>% 
  filter(LEVEL == "L2ab") %>% 
  group_by(SEX, CAL2, YEAR) %>%
  mutate(LEUKEMIA_ref = FY_LEUKEMIA_final * sum(LEUKEMIA_final) / sum(FY_LEUKEMIA_final), 
         LUNG_ref = FY_LUNG_final * sum(LUNG_final) / sum(FY_LUNG_final)) -> df_L2ab
df_L2ab = df_L2ab[order(df_L2ab$ID, df_L2ab$YEAR), ]
df[df$LEVEL == "L2ab", c("LEUKEMIA_ref", "LUNG_ref")] = df_L2ab[ ,c("LEUKEMIA_ref", "LUNG_ref")]



### fill level 2a ---------
df_L2a = df_L2ab %>%
  group_by(UP1, SEX, YEAR) %>%
  summarize(LEUKEMIA_final = sum(LEUKEMIA_final), FY_LEUKEMIA_final = sum(FY_LEUKEMIA_final),
            LUNG_final = sum(LUNG_final), FY_LUNG_final = sum(FY_LUNG_final),
            LEUKEMIA_ref = sum(LEUKEMIA_ref), LUNG_ref = sum(LUNG_ref)
  )
df_L2a = merge(x=meta_bs[meta_bs$LEVEL == "L2a",], 
               y=df_L2a, by=c("UP1", "SEX"), sort=FALSE, all.x=FALSE, all.y=TRUE)
df_L2a = df_L2a[order(df_L2a$ID, df_L2a$YEAR), ]

### 올바르게 생성되었는지 double check----------
df_temp = df[df$LEVEL == "L2a", ]
print(df_temp[1:20, ])
print(df_L2a[1:20, ]) # OK
df[df$LEVEL == "L2a", c("LEUKEMIA_ref", "LUNG_ref")] = df_L2a[ ,c("LEUKEMIA_ref", "LUNG_ref")]



### fill level 2b ----------------------
df_L2b = df_L2ab %>%
  group_by(UP1, CAL2, YEAR) %>%
  summarize(LEUKEMIA_final = sum(LEUKEMIA_final), FY_LEUKEMIA_final = sum(FY_LEUKEMIA_final),
            LUNG_final = sum(LUNG_final), FY_LUNG_final = sum(FY_LUNG_final),
            LEUKEMIA_ref = sum(LEUKEMIA_ref), LUNG_ref = sum(LUNG_ref)
  )
df_L2b = merge(x=meta_bs[meta_bs$LEVEL == "L2b",], 
               y=df_L2b, by=c("UP1", "CAL2"), sort=FALSE, all.x=FALSE, all.y=TRUE)
df_L2b = df_L2b[order(df_L2b$ID, df_L2b$YEAR), ]

### 올바르게 생성되었는지 double check----------
df_temp = df[df$LEVEL == "L2b", ]
print(df_temp[1:20, ])
print(df_L2b[1:20, ]) # OK
df[df$LEVEL == "L2b", c("LEUKEMIA_ref", "LUNG_ref")] = df_L2b[ ,c("LEUKEMIA_ref", "LUNG_ref")]




### fill level 2 ----------
df_L2 = df_L2ab %>%
  group_by(UP1, YEAR) %>%
  summarize(LEUKEMIA_final = sum(LEUKEMIA_final), FY_LEUKEMIA_final = sum(FY_LEUKEMIA_final),
            LUNG_final = sum(LUNG_final), FY_LUNG_final = sum(FY_LUNG_final),
            LEUKEMIA_ref = sum(LEUKEMIA_ref), LUNG_ref = sum(LUNG_ref)
  )
df_L2 = merge(x=meta_bs[meta_bs$LEVEL == "L2",], 
              y=df_L2, by=c("UP1"), sort=FALSE, all.x=FALSE, all.y=TRUE)
df_L2 = df_L2[order(df_L2$ID, df_L2$YEAR), ]

### 올바르게 생성되었는지 double check----------
df_temp = df[df$LEVEL == "L2", ]
print(df_temp[1:20, ])
print(df_L2[1:20, ]) # OK
df[df$LEVEL == "L2", c("LEUKEMIA_ref", "LUNG_ref")] = df_L2[ ,c("LEUKEMIA_ref", "LUNG_ref")]



### fill level 1s : all the same ---------------
df[df$LEVEL %in% c("L1", "L1a", "L1b", "L1ab"), c("LEUKEMIA_ref", "LUNG_ref")] = 
  df[df$LEVEL %in% c("L1", "L1a", "L1b", "L1ab"), c("LEUKEMIA_final", "LUNG_final")]


### RR 계산---------
df$LEUKEMIA_RR = df$LEUKEMIA_final / df$LEUKEMIA_ref
df$LUNG_RR = df$LUNG_final / df$LUNG_ref


### data csv file로 save -----------------------------------------------------
write.csv(df, "test_finalPlotting_220816.csv", col.names=T, row.names=F)



# -------------------------------------------------------------------------
### 4. 보고서에 들어갈 표 생성 --------------------------------------------
meta_up1 = read.csv("meta_up1.csv", header=T) # UP1 code file import
meta_up1$NAME1 = str_trim(as.character(meta_up1$NAME1)) # 좌우공백 제거

meta_up2 = read.csv("meta_up2.csv", header=T) # UP2 code file import
meta_up2$NAME2 = str_trim(as.character(meta_up2$NAME2)) # 좌우공백 제거
meta_up2$UP2 = as.integer(meta_up2$UP2)

df = merge(x=meta_up1, y=df, by="UP1", all.x=FALSE, all.y=TRUE)
df = merge(x=meta_up2, y=df, by="UP2", all.x=FALSE, all.y=TRUE)
df = df[order(df$ID, df$YEAR), ]
df = df[ ,c("ID", "NAME", "LEVEL", "UP1", "NAME1.y", "UP2", "NAME2.y", "SEX", "CAL2", "YEAR",
            "LEUKEMIA_final", "LEUKEMIA_ref", "LEUKEMIA_RR", 
            "LUNG_final", "LUNG_ref", "LUNG_RR",
            "FY_LEUKEMIA_final", "FY_LUNG_final")]

colnames(df)[c(5,7)]<-c("NAME1", "NAME2")

write.csv(df, "test_finalPlotting_220816_v2.csv", col.names=T, row.names=F)


### 4-1) make wide table for documentation ---------------------------- 
### fill NA with black ------
df$UP1[is.na(df$UP1)] = ""
df$NAME1[is.na(df$NAME1)] = ""
df$UP2[is.na(df$UP2)] = ""
df$NAME2[is.na(df$NAME2)] = ""
df$SEX[is.na(df$SEX)] = ""
df$CAL2[is.na(df$CAL2)] = ""
df$LEUKEMIA_final = round(df$LEUKEMIA_final)
df$LUNG_final = round(df$LUNG_final)
df$FY_LEUKEMIA_final = round(df$FY_LEUKEMIA_final)
df$FY_LUNG_final = round(df$FY_LUNG_final)
df$LEUKEMIA_RR = round(df$LEUKEMIA_RR, 2)
df$LUNG_RR = round(df$LUNG_RR, 2)

require(writexl)
write_xlsx(df, paste0("", "test_finalPlotting_220816_v3.xlsx"),
                         col_names=T, format_headers=F)


### wide format table 만드는 함수 정의 -----------
exportTables = function(targetName, isFile=F, isList=F) {
  table_save = pivot_wider(df[ ,c("ID", "NAME", "LEVEL", "UP1", "NAME1", "UP2", "NAME2",
                                  "SEX", "CAL2", "YEAR", targetName)],
                           names_from = YEAR, values_from = all_of(targetName) )
  table_save[table_save$UP2 != "", c("UP1", "NAME1")] = ""
  table_save[is.na(table_save)] = 0
  table_save = table_save[order(table_save$NAME), ]
  
  table_save_list = list()
  
  table_save_list[[1]] = table_save[ table_save$LEVEL %in% c("L1", "L2", "L3"), ]
  names(table_save_list)[1] = paste0(targetName, "")
  
  table_save_list[[2]] = table_save[ table_save$LEVEL %in% c("L1a", "L2a", "L3a") & table_save$SEX == "F", ]
  names(table_save_list)[2] = paste0(targetName, "_F")
  
  table_save_list[[3]] = table_save[ table_save$LEVEL %in% c("L1a", "L2a", "L3a") & table_save$SEX == "M", ]
  names(table_save_list)[3] = paste0(targetName, "_M")
  
  table_save_list[[4]] = table_save[ table_save$LEVEL %in% c("L1b", "L2b", "L3b") & table_save$CAL2 %in% c("1", "01"), ]
  names(table_save_list)[4] = paste0(targetName, "_01")
  
  table_save_list[[5]] = table_save[ table_save$LEVEL %in% c("L1b", "L2b", "L3b") & table_save$CAL2 == "234", ]
  names(table_save_list)[5] = paste0(targetName, "_234")
  
  table_save_list[[6]] = table_save[ table_save$LEVEL %in% c("L1ab", "L2ab", "L3ab") & table_save$SEX == "F" & table_save$CAL2 %in% c("1", "01"), ]
  names(table_save_list)[6] = paste0(targetName, "_F01")
  
  table_save_list[[7]] = table_save[ table_save$LEVEL %in% c("L1ab", "L2ab", "L3ab") & table_save$SEX == "F" & table_save$CAL2 == "234", ]
  names(table_save_list)[7] = paste0(targetName, "_F234")
  
  table_save_list[[8]] = table_save[ table_save$LEVEL %in% c("L1ab", "L2ab", "L3ab") & table_save$SEX == "M" & table_save$CAL2 %in% c("1", "01"), ]
  names(table_save_list)[8] = paste0(targetName, "_M01")
  
  table_save_list[[9]] = table_save[ table_save$LEVEL %in% c("L1ab", "L2ab", "L3ab") & table_save$SEX == "M" & table_save$CAL2 == "234", ]
  names(table_save_list)[9] = paste0(targetName, "_M234")
  
  if (isFile) {
    require(writexl)
    for (k in 1:9) {
      write_xlsx(table_save_list[[k]], paste0("", names(table_save_list)[k], ".xlsx"),
                 col_names=T, format_headers=F)
    }
  }
  if (isList) { return(table_save_list) }
}


### save files ------------
exportTables("LEUKEMIA_final", isFile=T)
exportTables("LUNG_final", isFile=T)
exportTables("LEUKEMIA_RR", isFile=T)
exportTables("LUNG_RR", isFile=T)




### 4-2) Leukemia 대상 각 level 별 RR TOP10 사업장 추출 & 표 생성  --------------------------
leu_final = exportTables("LEUKEMIA_final", isList=T)
leu_rr = exportTables("LEUKEMIA_RR", isList=T)


### 4-2-1) 전체 : 2000년도 이전 ---------------------------------------------------
head(leu_rr$LEUKEMIA_RR_01$`2022`)

LEUKEMIA_final_01_sub = leu_final$LEUKEMIA_final_01[ ,c("ID", "NAME", "LEVEL", "UP1", "NAME1", "UP2", "NAME2",
                                                        "SEX", "CAL2", "2018", "2022")]
LEUKEMIA_01_sub = cbind(
  leu_final$LEUKEMIA_final_01[ ,c("ID", "NAME", "LEVEL", "UP1", "NAME1", "UP2", "NAME2", "SEX", "CAL2")],
  final_2018=leu_final$LEUKEMIA_final_01$`2018`, RR_2018=leu_rr$LEUKEMIA_RR_01$`2018`,
  final_2022=leu_final$LEUKEMIA_final_01$`2022`, RR_2022=leu_rr$LEUKEMIA_RR_01$`2022`)
LEUKEMIA_01_sub = LEUKEMIA_01_sub[ LEUKEMIA_01_sub$final_2018 > 0, ]
LEUKEMIA_01_sub_UP1 = LEUKEMIA_01_sub[ LEUKEMIA_01_sub$UP1 != "", ]
LEUKEMIA_01_sub_UP2 = LEUKEMIA_01_sub[ LEUKEMIA_01_sub$UP2 != "", ]



### 4-2-2) 대분류 : 2000년도 이전 ------------------
LEUKEMIA_01_sub_UP1_top10 = (LEUKEMIA_01_sub_UP1[order(LEUKEMIA_01_sub_UP1$RR_2022, decreasing=TRUE), ])[
  1:10, c("UP1", "NAME1", "final_2018", "RR_2018", "final_2022", "RR_2022")]
LEUKEMIA_01_sub_UP1_top10 = LEUKEMIA_01_sub_UP1_top10[ ,c("UP1", "NAME1", "final_2018", "RR_2018",
                                                          "final_2022", "RR_2022")]
LEUKEMIA_01_sub_UP1_top10$string_2018 = 
  paste0(LEUKEMIA_01_sub_UP1_top10$final_2018, " (", LEUKEMIA_01_sub_UP1_top10$RR_2018, ")")
LEUKEMIA_01_sub_UP1_top10$string_2022 = 
  paste0(LEUKEMIA_01_sub_UP1_top10$final_2022, " (", LEUKEMIA_01_sub_UP1_top10$RR_2022, ")")
LEUKEMIA_01_sub_UP1_top10 = LEUKEMIA_01_sub_UP1_top10[ ,c("UP1", "NAME1", "string_2018", "string_2022")]
write_xlsx(LEUKEMIA_01_sub_UP1_top10, paste0("", "LEUKEMIA_01_sub_UP1_top10.xlsx"),
           col_names=T, format_headers=F)



### 4-2-3) 중분류 : 2000년도 이전 ------------------
LEUKEMIA_01_sub_UP2_top10 = (LEUKEMIA_01_sub_UP2[order(LEUKEMIA_01_sub_UP2$RR_2022, decreasing=TRUE), ])[
  1:10, c("UP2", "NAME2", "final_2018", "RR_2018", "final_2022", "RR_2022")]
LEUKEMIA_01_sub_UP2_top10 = LEUKEMIA_01_sub_UP2_top10[ ,c("UP2", "NAME2", "final_2018", "RR_2018",
                                                          "final_2022", "RR_2022")]
LEUKEMIA_01_sub_UP2_top10$string_2018 = 
  paste0(LEUKEMIA_01_sub_UP2_top10$final_2018, " (", LEUKEMIA_01_sub_UP2_top10$RR_2018, ")")
LEUKEMIA_01_sub_UP2_top10$string_2022 = 
  paste0(LEUKEMIA_01_sub_UP2_top10$final_2022, " (", LEUKEMIA_01_sub_UP2_top10$RR_2022, ")")
LEUKEMIA_01_sub_UP2_top10 = LEUKEMIA_01_sub_UP2_top10[ ,c("UP2", "NAME2", "string_2018", "string_2022")]
write_xlsx(LEUKEMIA_01_sub_UP2_top10, paste0("", "LEUKEMIA_01_sub_UP2_top10.xlsx"),
           col_names=T, format_headers=F)



### 4-2-4) 전체 : 2000년도 이후 ------------------
head(leu_rr$LEUKEMIA_RR_234$`2022`)

LEUKEMIA_final_234_sub = leu_final$LEUKEMIA_final_234[ ,c("ID", "NAME", "LEVEL", "UP1", "NAME1", "UP2", "NAME2",
                                                        "SEX", "CAL2", "2018", "2022")]
LEUKEMIA_234_sub = cbind(
  leu_final$LEUKEMIA_final_234[ ,c("ID", "NAME", "LEVEL", "UP1", "NAME1", "UP2", "NAME2", "SEX", "CAL2")],
  final_2018=leu_final$LEUKEMIA_final_234$`2018`, RR_2018=leu_rr$LEUKEMIA_RR_234$`2018`,
  final_2022=leu_final$LEUKEMIA_final_234$`2022`, RR_2022=leu_rr$LEUKEMIA_RR_234$`2022`)
LEUKEMIA_234_sub_UP1 = LEUKEMIA_234_sub[ LEUKEMIA_234_sub$UP1 != "", ]
LEUKEMIA_234_sub_UP2 = LEUKEMIA_234_sub[ LEUKEMIA_234_sub$UP2 != "", ]



### 4-2-5) 대분류 : 2000년도 이후 ------------------
LEUKEMIA_234_sub_UP1_top10 = (LEUKEMIA_234_sub_UP1[order(LEUKEMIA_234_sub_UP1$RR_2022, decreasing=TRUE), ])[
  1:10, c("UP1", "NAME1", "final_2018", "RR_2018", "final_2022", "RR_2022")]
LEUKEMIA_234_sub_UP1_top10 = LEUKEMIA_234_sub_UP1_top10[ ,c("UP1", "NAME1", "final_2018", "RR_2018",
                                                          "final_2022", "RR_2022")]
LEUKEMIA_234_sub_UP1_top10$string_2018 = 
  paste0(LEUKEMIA_234_sub_UP1_top10$final_2018, " (", LEUKEMIA_234_sub_UP1_top10$RR_2018, ")")
LEUKEMIA_234_sub_UP1_top10$string_2022 = 
  paste0(LEUKEMIA_234_sub_UP1_top10$final_2022, " (", LEUKEMIA_234_sub_UP1_top10$RR_2022, ")")
LEUKEMIA_234_sub_UP1_top10 = LEUKEMIA_234_sub_UP1_top10[ ,c("UP1", "NAME1", "string_2018", "string_2022")]
write_xlsx(LEUKEMIA_234_sub_UP1_top10, paste0("", "LEUKEMIA_234_sub_UP1_top10.xlsx"),
           col_names=T, format_headers=F)



### 4-2-6) 중분류 : 2000년도 이후 ------------------
LEUKEMIA_234_sub_UP2_top10 = (LEUKEMIA_234_sub_UP2[order(LEUKEMIA_234_sub_UP2$RR_2022, decreasing=TRUE), ])[
  1:10, c("UP2", "NAME2", "final_2018", "RR_2018", "final_2022", "RR_2022")]
LEUKEMIA_234_sub_UP2_top10 = LEUKEMIA_234_sub_UP2_top10[ ,c("UP2", "NAME2", "final_2018", "RR_2018",
                                                          "final_2022", "RR_2022")]
LEUKEMIA_234_sub_UP2_top10$string_2018 = 
  paste0(LEUKEMIA_234_sub_UP2_top10$final_2018, " (", LEUKEMIA_234_sub_UP2_top10$RR_2018, ")")
LEUKEMIA_234_sub_UP2_top10$string_2022 = 
  paste0(LEUKEMIA_234_sub_UP2_top10$final_2022, " (", LEUKEMIA_234_sub_UP2_top10$RR_2022, ")")
LEUKEMIA_234_sub_UP2_top10 = LEUKEMIA_234_sub_UP2_top10[ ,c("UP2", "NAME2", "string_2018", "string_2022")]
write_xlsx(LEUKEMIA_234_sub_UP2_top10, paste0("", "LEUKEMIA_234_sub_UP2_top10.xlsx"),
           col_names=T, format_headers=F)



### 4-2-7) 남성 : 2000년도 이전 ------------------
head(leu_rr$LEUKEMIA_RR_M01$`2022`)

LEUKEMIA_final_M01_sub = leu_final$LEUKEMIA_final_M01[ ,c("ID", "NAME", "LEVEL", "UP1", "NAME1", "UP2", "NAME2",
                                                        "SEX", "CAL2", "2018", "2022")]
LEUKEMIA_M01_sub = cbind(
  leu_final$LEUKEMIA_final_M01[ ,c("ID", "NAME", "LEVEL", "UP1", "NAME1", "UP2", "NAME2", "SEX", "CAL2")],
  final_2018=leu_final$LEUKEMIA_final_M01$`2018`, RR_2018=leu_rr$LEUKEMIA_RR_M01$`2018`,
  final_2022=leu_final$LEUKEMIA_final_M01$`2022`, RR_2022=leu_rr$LEUKEMIA_RR_M01$`2022`)
LEUKEMIA_M01_sub = LEUKEMIA_M01_sub[ LEUKEMIA_M01_sub$final_2018 > 0, ]
LEUKEMIA_M01_sub_UP1 = LEUKEMIA_M01_sub[ LEUKEMIA_M01_sub$UP1 != "", ]
LEUKEMIA_M01_sub_UP2 = LEUKEMIA_M01_sub[ LEUKEMIA_M01_sub$UP2 != "", ]



### 4-2-8) 남성 & 대분류 : 2000년도 이전 ------------------
LEUKEMIA_M01_sub_UP1_top10 = (LEUKEMIA_M01_sub_UP1[order(LEUKEMIA_M01_sub_UP1$RR_2022, decreasing=TRUE), ])[
  1:10, c("UP1", "NAME1", "final_2018", "RR_2018", "final_2022", "RR_2022")]
LEUKEMIA_M01_sub_UP1_top10 = LEUKEMIA_M01_sub_UP1_top10[ ,c("UP1", "NAME1", "final_2018", "RR_2018",
                                                          "final_2022", "RR_2022")]
LEUKEMIA_M01_sub_UP1_top10$string_2018 = 
  paste0(LEUKEMIA_M01_sub_UP1_top10$final_2018, " (", LEUKEMIA_M01_sub_UP1_top10$RR_2018, ")")
LEUKEMIA_M01_sub_UP1_top10$string_2022 = 
  paste0(LEUKEMIA_M01_sub_UP1_top10$final_2022, " (", LEUKEMIA_M01_sub_UP1_top10$RR_2022, ")")
LEUKEMIA_M01_sub_UP1_top10 = LEUKEMIA_M01_sub_UP1_top10[ ,c("UP1", "NAME1", "string_2018", "string_2022")]
write_xlsx(LEUKEMIA_M01_sub_UP1_top10, paste0("", "LEUKEMIA_M01_sub_UP1_top10.xlsx"),
           col_names=T, format_headers=F)



### 4-2-9) 남성 & 중분류 : 2000년도 이전 ------------------
LEUKEMIA_M01_sub_UP2_top10 = (LEUKEMIA_M01_sub_UP2[order(LEUKEMIA_M01_sub_UP2$RR_2022, decreasing=TRUE), ])[
  1:10, c("UP2", "NAME2", "final_2018", "RR_2018", "final_2022", "RR_2022")]
LEUKEMIA_M01_sub_UP2_top10 = LEUKEMIA_M01_sub_UP2_top10[ ,c("UP2", "NAME2", "final_2018", "RR_2018",
                                                          "final_2022", "RR_2022")]
LEUKEMIA_M01_sub_UP2_top10$string_2018 = 
  paste0(LEUKEMIA_M01_sub_UP2_top10$final_2018, " (", LEUKEMIA_M01_sub_UP2_top10$RR_2018, ")")
LEUKEMIA_M01_sub_UP2_top10$string_2022 = 
  paste0(LEUKEMIA_M01_sub_UP2_top10$final_2022, " (", LEUKEMIA_M01_sub_UP2_top10$RR_2022, ")")
LEUKEMIA_M01_sub_UP2_top10 = LEUKEMIA_M01_sub_UP2_top10[ ,c("UP2", "NAME2", "string_2018", "string_2022")]
write_xlsx(LEUKEMIA_M01_sub_UP2_top10, paste0("", "LEUKEMIA_M01_sub_UP2_top10.xlsx"),
           col_names=T, format_headers=F)



### 4-2-10) 남성 : 2000년도 이후 ------------------
head(leu_rr$LEUKEMIA_RR_M234$`2022`)

LEUKEMIA_final_M234_sub = leu_final$LEUKEMIA_final_M234[ ,c("ID", "NAME", "LEVEL", "UP1", "NAME1", "UP2", "NAME2",
                                                          "SEX", "CAL2", "2018", "2022")]
LEUKEMIA_M234_sub = cbind(
  leu_final$LEUKEMIA_final_M234[ ,c("ID", "NAME", "LEVEL", "UP1", "NAME1", "UP2", "NAME2", "SEX", "CAL2")],
  final_2018=leu_final$LEUKEMIA_final_M234$`2018`, RR_2018=leu_rr$LEUKEMIA_RR_M234$`2018`,
  final_2022=leu_final$LEUKEMIA_final_M234$`2022`, RR_2022=leu_rr$LEUKEMIA_RR_M234$`2022`)
LEUKEMIA_M234_sub = LEUKEMIA_M234_sub[ LEUKEMIA_M234_sub$final_2018 > 0, ]
LEUKEMIA_M234_sub_UP1 = LEUKEMIA_M234_sub[ LEUKEMIA_M234_sub$UP1 != "", ]
LEUKEMIA_M234_sub_UP2 = LEUKEMIA_M234_sub[ LEUKEMIA_M234_sub$UP2 != "", ]



### 4-2-11) 남성 & 대분류 : 2000년도 이후 ------------------
LEUKEMIA_M234_sub_UP1_top10 = (LEUKEMIA_M234_sub_UP1[order(LEUKEMIA_M234_sub_UP1$RR_2022, decreasing=TRUE), ])[
  1:10, c("UP1", "NAME1", "final_2018", "RR_2018", "final_2022", "RR_2022")]
LEUKEMIA_M234_sub_UP1_top10 = LEUKEMIA_M234_sub_UP1_top10[ ,c("UP1", "NAME1", "final_2018", "RR_2018",
                                                            "final_2022", "RR_2022")]
LEUKEMIA_M234_sub_UP1_top10$string_2018 = 
  paste0(LEUKEMIA_M234_sub_UP1_top10$final_2018, " (", LEUKEMIA_M234_sub_UP1_top10$RR_2018, ")")
LEUKEMIA_M234_sub_UP1_top10$string_2022 = 
  paste0(LEUKEMIA_M234_sub_UP1_top10$final_2022, " (", LEUKEMIA_M234_sub_UP1_top10$RR_2022, ")")
LEUKEMIA_M234_sub_UP1_top10 = LEUKEMIA_M234_sub_UP1_top10[ ,c("UP1", "NAME1", "string_2018", "string_2022")]
write_xlsx(LEUKEMIA_M234_sub_UP1_top10, paste0("", "LEUKEMIA_M234_sub_UP1_top10.xlsx"),
           col_names=T, format_headers=F)



### 4-2-12) 남성 & 중분류 : 2000년도 이후 ------------------
LEUKEMIA_M234_sub_UP2_top10 = (LEUKEMIA_M234_sub_UP2[order(LEUKEMIA_M234_sub_UP2$RR_2022, decreasing=TRUE), ])[
  1:10, c("UP2", "NAME2", "final_2018", "RR_2018", "final_2022", "RR_2022")]
LEUKEMIA_M234_sub_UP2_top10 = LEUKEMIA_M234_sub_UP2_top10[ ,c("UP2", "NAME2", "final_2018", "RR_2018",
                                                            "final_2022", "RR_2022")]
LEUKEMIA_M234_sub_UP2_top10$string_2018 = 
  paste0(LEUKEMIA_M234_sub_UP2_top10$final_2018, " (", LEUKEMIA_M234_sub_UP2_top10$RR_2018, ")")
LEUKEMIA_M234_sub_UP2_top10$string_2022 = 
  paste0(LEUKEMIA_M234_sub_UP2_top10$final_2022, " (", LEUKEMIA_M234_sub_UP2_top10$RR_2022, ")")
LEUKEMIA_M234_sub_UP2_top10 = LEUKEMIA_M234_sub_UP2_top10[ ,c("UP2", "NAME2", "string_2018", "string_2022")]
write_xlsx(LEUKEMIA_M234_sub_UP2_top10, paste0("", "LEUKEMIA_M234_sub_UP2_top10.xlsx"),
           col_names=T, format_headers=F)



### 4-2-13) 여성 : 2000년도 이전 ------------------
head(leu_rr$LEUKEMIA_RR_F01$`2022`)

LEUKEMIA_final_F01_sub = leu_final$LEUKEMIA_final_F01[ ,c("ID", "NAME", "LEVEL", "UP1", "NAME1", "UP2", "NAME2",
                                                          "SEX", "CAL2", "2018", "2022")]
LEUKEMIA_F01_sub = cbind(
  leu_final$LEUKEMIA_final_F01[ ,c("ID", "NAME", "LEVEL", "UP1", "NAME1", "UP2", "NAME2", "SEX", "CAL2")],
  final_2018=leu_final$LEUKEMIA_final_F01$`2018`, RR_2018=leu_rr$LEUKEMIA_RR_F01$`2018`,
  final_2022=leu_final$LEUKEMIA_final_F01$`2022`, RR_2022=leu_rr$LEUKEMIA_RR_F01$`2022`)
LEUKEMIA_F01_sub = LEUKEMIA_F01_sub[ LEUKEMIA_F01_sub$final_2018 > 0, ]
LEUKEMIA_F01_sub_UP1 = LEUKEMIA_F01_sub[ LEUKEMIA_F01_sub$UP1 != "", ]
LEUKEMIA_F01_sub_UP2 = LEUKEMIA_F01_sub[ LEUKEMIA_F01_sub$UP2 != "", ]



### 4-2-14) 여성 & 대분류 : 2000년도 이전 ------------------
LEUKEMIA_F01_sub_UP1_top10 = (LEUKEMIA_F01_sub_UP1[order(LEUKEMIA_F01_sub_UP1$RR_2022, decreasing=TRUE), ])[
  1:10, c("UP1", "NAME1", "final_2018", "RR_2018", "final_2022", "RR_2022")]
LEUKEMIA_F01_sub_UP1_top10 = LEUKEMIA_F01_sub_UP1_top10[ ,c("UP1", "NAME1", "final_2018", "RR_2018",
                                                            "final_2022", "RR_2022")]
LEUKEMIA_F01_sub_UP1_top10$string_2018 = 
  paste0(LEUKEMIA_F01_sub_UP1_top10$final_2018, " (", LEUKEMIA_F01_sub_UP1_top10$RR_2018, ")")
LEUKEMIA_F01_sub_UP1_top10$string_2022 = 
  paste0(LEUKEMIA_F01_sub_UP1_top10$final_2022, " (", LEUKEMIA_F01_sub_UP1_top10$RR_2022, ")")
LEUKEMIA_F01_sub_UP1_top10 = LEUKEMIA_F01_sub_UP1_top10[ ,c("UP1", "NAME1", "string_2018", "string_2022")]
write_xlsx(LEUKEMIA_F01_sub_UP1_top10, paste0("", "LEUKEMIA_F01_sub_UP1_top10.xlsx"),
           col_names=T, format_headers=F)


### 4-2-15) 여성 & 중분류 : 2000년도 이전 ------------------
LEUKEMIA_F01_sub_UP2_top10 = (LEUKEMIA_F01_sub_UP2[order(LEUKEMIA_F01_sub_UP2$RR_2022, decreasing=TRUE), ])[
  1:10, c("UP2", "NAME2", "final_2018", "RR_2018", "final_2022", "RR_2022")]
LEUKEMIA_F01_sub_UP2_top10 = LEUKEMIA_F01_sub_UP2_top10[ ,c("UP2", "NAME2", "final_2018", "RR_2018",
                                                            "final_2022", "RR_2022")]
LEUKEMIA_F01_sub_UP2_top10$string_2018 = 
  paste0(LEUKEMIA_F01_sub_UP2_top10$final_2018, " (", LEUKEMIA_F01_sub_UP2_top10$RR_2018, ")")
LEUKEMIA_F01_sub_UP2_top10$string_2022 = 
  paste0(LEUKEMIA_F01_sub_UP2_top10$final_2022, " (", LEUKEMIA_F01_sub_UP2_top10$RR_2022, ")")
LEUKEMIA_F01_sub_UP2_top10 = LEUKEMIA_F01_sub_UP2_top10[ ,c("UP2", "NAME2", "string_2018", "string_2022")]
write_xlsx(LEUKEMIA_F01_sub_UP2_top10, paste0("", "LEUKEMIA_F01_sub_UP2_top10.xlsx"),
           col_names=T, format_headers=F)



### 4-2-16) 여성 : 2000년도 이후 ------------------
head(leu_rr$LEUKEMIA_RR_F234$`2022`)

LEUKEMIA_final_F234_sub = leu_final$LEUKEMIA_final_F234[ ,c("ID", "NAME", "LEVEL", "UP1", "NAME1", "UP2", "NAME2",
                                                            "SEX", "CAL2", "2018", "2022")]
LEUKEMIA_F234_sub = cbind(
  leu_final$LEUKEMIA_final_F234[ ,c("ID", "NAME", "LEVEL", "UP1", "NAME1", "UP2", "NAME2", "SEX", "CAL2")],
  final_2018=leu_final$LEUKEMIA_final_F234$`2018`, RR_2018=leu_rr$LEUKEMIA_RR_F234$`2018`,
  final_2022=leu_final$LEUKEMIA_final_F234$`2022`, RR_2022=leu_rr$LEUKEMIA_RR_F234$`2022`)
LEUKEMIA_F234_sub = LEUKEMIA_F234_sub[ LEUKEMIA_F234_sub$final_2018 > 0, ]
LEUKEMIA_F234_sub_UP1 = LEUKEMIA_F234_sub[ LEUKEMIA_F234_sub$UP1 != "", ]
LEUKEMIA_F234_sub_UP2 = LEUKEMIA_F234_sub[ LEUKEMIA_F234_sub$UP2 != "", ]




### 4-2-17) 여성 & 대분류 : 2000년도 이후 ------------------
LEUKEMIA_F234_sub_UP1_top10 = (LEUKEMIA_F234_sub_UP1[order(LEUKEMIA_F234_sub_UP1$RR_2022, decreasing=TRUE), ])[
  1:10, c("UP1", "NAME1", "final_2018", "RR_2018", "final_2022", "RR_2022")]
LEUKEMIA_F234_sub_UP1_top10 = LEUKEMIA_F234_sub_UP1_top10[ ,c("UP1", "NAME1", "final_2018", "RR_2018",
                                                              "final_2022", "RR_2022")]
LEUKEMIA_F234_sub_UP1_top10$string_2018 = 
  paste0(LEUKEMIA_F234_sub_UP1_top10$final_2018, " (", LEUKEMIA_F234_sub_UP1_top10$RR_2018, ")")
LEUKEMIA_F234_sub_UP1_top10$string_2022 = 
  paste0(LEUKEMIA_F234_sub_UP1_top10$final_2022, " (", LEUKEMIA_F234_sub_UP1_top10$RR_2022, ")")
LEUKEMIA_F234_sub_UP1_top10 = LEUKEMIA_F234_sub_UP1_top10[ ,c("UP1", "NAME1", "string_2018", "string_2022")]
write_xlsx(LEUKEMIA_F234_sub_UP1_top10, paste0("", "LEUKEMIA_F234_sub_UP1_top10.xlsx"),
           col_names=T, format_headers=F)



### 4-2-18) 여성& 중분류 : 2000년도 이후 ------------------
LEUKEMIA_F234_sub_UP2_top10 = (LEUKEMIA_F234_sub_UP2[order(LEUKEMIA_F234_sub_UP2$RR_2022, decreasing=TRUE), ])[
  1:10, c("UP2", "NAME2", "final_2018", "RR_2018", "final_2022", "RR_2022")]
LEUKEMIA_F234_sub_UP2_top10 = LEUKEMIA_F234_sub_UP2_top10[ ,c("UP2", "NAME2", "final_2018", "RR_2018",
                                                              "final_2022", "RR_2022")]
LEUKEMIA_F234_sub_UP2_top10$string_2018 = 
  paste0(LEUKEMIA_F234_sub_UP2_top10$final_2018, " (", LEUKEMIA_F234_sub_UP2_top10$RR_2018, ")")
LEUKEMIA_F234_sub_UP2_top10$string_2022 = 
  paste0(LEUKEMIA_F234_sub_UP2_top10$final_2022, " (", LEUKEMIA_F234_sub_UP2_top10$RR_2022, ")")
LEUKEMIA_F234_sub_UP2_top10 = LEUKEMIA_F234_sub_UP2_top10[ ,c("UP2", "NAME2", "string_2018", "string_2022")]
write_xlsx(LEUKEMIA_F234_sub_UP2_top10, paste0("", "LEUKEMIA_F234_sub_UP2_top10.xlsx"),
           col_names=T, format_headers=F)



# -------------------------------------------------------------------------
### 4-3) Lung cancer 대상 각 level 별 RR TOP10 사업장 추출 & 표 생성  --------------------------
lung_final = exportTables("LUNG_final", isList=T)
lung_rr = exportTables("LUNG_RR", isList=T)



### 4-3-1) 전체 : 2000년도 이전 ---------------------------------------------------
head(lung_rr$LUNG_RR_01$`2022`)

LUNG_final_01_sub = lung_final$LUNG_final_01[ ,c("ID", "NAME", "LEVEL", "UP1", "NAME1", "UP2", "NAME2",
                                                        "SEX", "CAL2", "2018", "2022")]
LUNG_01_sub = cbind(
  lung_final$LUNG_final_01[ ,c("ID", "NAME", "LEVEL", "UP1", "NAME1", "UP2", "NAME2", "SEX", "CAL2")],
  final_2018=lung_final$LUNG_final_01$`2018`, RR_2018=lung_rr$LUNG_RR_01$`2018`,
  final_2022=lung_final$LUNG_final_01$`2022`, RR_2022=lung_rr$LUNG_RR_01$`2022`)
LUNG_01_sub = LUNG_01_sub[ LUNG_01_sub$final_2018 > 0, ]
LUNG_01_sub_UP1 = LUNG_01_sub[ LUNG_01_sub$UP1 != "", ]
LUNG_01_sub_UP2 = LUNG_01_sub[ LUNG_01_sub$UP2 != "", ]



### 4-3-2) 전체 & 대분류 : 2000년도 이전 ---------------------------------------------------
LUNG_01_sub_UP1_top10 = (LUNG_01_sub_UP1[order(LUNG_01_sub_UP1$RR_2022, decreasing=TRUE), ])[
  1:10, c("UP1", "NAME1", "final_2018", "RR_2018", "final_2022", "RR_2022")]
LUNG_01_sub_UP1_top10 = LUNG_01_sub_UP1_top10[ ,c("UP1", "NAME1", "final_2018", "RR_2018",
                                                          "final_2022", "RR_2022")]
LUNG_01_sub_UP1_top10$string_2018 = 
  paste0(LUNG_01_sub_UP1_top10$final_2018, " (", LUNG_01_sub_UP1_top10$RR_2018, ")")
LUNG_01_sub_UP1_top10$string_2022 = 
  paste0(LUNG_01_sub_UP1_top10$final_2022, " (", LUNG_01_sub_UP1_top10$RR_2022, ")")
LUNG_01_sub_UP1_top10 = LUNG_01_sub_UP1_top10[ ,c("UP1", "NAME1", "string_2018", "string_2022")]
write_xlsx(LUNG_01_sub_UP1_top10, paste0("", "LUNG_01_sub_UP1_top10.xlsx"),
           col_names=T, format_headers=F)



### 4-3-3) 전체 & 중분류 : 2000년도 이전 ---------------------------------------------------
LUNG_01_sub_UP2_top10 = (LUNG_01_sub_UP2[order(LUNG_01_sub_UP2$RR_2022, decreasing=TRUE), ])[
  1:10, c("UP2", "NAME2", "final_2018", "RR_2018", "final_2022", "RR_2022")]
LUNG_01_sub_UP2_top10 = LUNG_01_sub_UP2_top10[ ,c("UP2", "NAME2", "final_2018", "RR_2018",
                                                          "final_2022", "RR_2022")]
LUNG_01_sub_UP2_top10$string_2018 = 
  paste0(LUNG_01_sub_UP2_top10$final_2018, " (", LUNG_01_sub_UP2_top10$RR_2018, ")")
LUNG_01_sub_UP2_top10$string_2022 = 
  paste0(LUNG_01_sub_UP2_top10$final_2022, " (", LUNG_01_sub_UP2_top10$RR_2022, ")")
LUNG_01_sub_UP2_top10 = LUNG_01_sub_UP2_top10[ ,c("UP2", "NAME2", "string_2018", "string_2022")]
write_xlsx(LUNG_01_sub_UP2_top10, paste0("", "LUNG_01_sub_UP2_top10.xlsx"),
           col_names=T, format_headers=F)



### 4-3-4) 전체 : 2000년도 이후 ---------------------------------------------------
head(lung_rr$LUNG_RR_234$`2022`)

LUNG_final_234_sub = lung_final$LUNG_final_234[ ,c("ID", "NAME", "LEVEL", "UP1", "NAME1", "UP2", "NAME2",
                                                          "SEX", "CAL2", "2018", "2022")]
LUNG_234_sub = cbind(
  lung_final$LUNG_final_234[ ,c("ID", "NAME", "LEVEL", "UP1", "NAME1", "UP2", "NAME2", "SEX", "CAL2")],
  final_2018=lung_final$LUNG_final_234$`2018`, RR_2018=lung_rr$LUNG_RR_234$`2018`,
  final_2022=lung_final$LUNG_final_234$`2022`, RR_2022=lung_rr$LUNG_RR_234$`2022`)
LUNG_234_sub_UP1 = LUNG_234_sub[ LUNG_234_sub$UP1 != "", ]
LUNG_234_sub_UP2 = LUNG_234_sub[ LUNG_234_sub$UP2 != "", ]



### 4-3-5) 전체 & 대분류 : 2000년도 이후 ---------------------------------------------------
LUNG_234_sub_UP1_top10 = (LUNG_234_sub_UP1[order(LUNG_234_sub_UP1$RR_2022, decreasing=TRUE), ])[
  1:10, c("UP1", "NAME1", "final_2018", "RR_2018", "final_2022", "RR_2022")]
LUNG_234_sub_UP1_top10 = LUNG_234_sub_UP1_top10[ ,c("UP1", "NAME1", "final_2018", "RR_2018",
                                                            "final_2022", "RR_2022")]
LUNG_234_sub_UP1_top10$string_2018 = 
  paste0(LUNG_234_sub_UP1_top10$final_2018, " (", LUNG_234_sub_UP1_top10$RR_2018, ")")
LUNG_234_sub_UP1_top10$string_2022 = 
  paste0(LUNG_234_sub_UP1_top10$final_2022, " (", LUNG_234_sub_UP1_top10$RR_2022, ")")
LUNG_234_sub_UP1_top10 = LUNG_234_sub_UP1_top10[ ,c("UP1", "NAME1", "string_2018", "string_2022")]
write_xlsx(LUNG_234_sub_UP1_top10, paste0("", "LUNG_234_sub_UP1_top10.xlsx"),
           col_names=T, format_headers=F)



### 4-3-6) 전체 & 중분류 : 2000년도 이후 ---------------------------------------------------
LUNG_234_sub_UP2_top10 = (LUNG_234_sub_UP2[order(LUNG_234_sub_UP2$RR_2022, decreasing=TRUE), ])[
  1:10, c("UP2", "NAME2", "final_2018", "RR_2018", "final_2022", "RR_2022")]
LUNG_234_sub_UP2_top10 = LUNG_234_sub_UP2_top10[ ,c("UP2", "NAME2", "final_2018", "RR_2018",
                                                            "final_2022", "RR_2022")]
LUNG_234_sub_UP2_top10$string_2018 = 
  paste0(LUNG_234_sub_UP2_top10$final_2018, " (", LUNG_234_sub_UP2_top10$RR_2018, ")")
LUNG_234_sub_UP2_top10$string_2022 = 
  paste0(LUNG_234_sub_UP2_top10$final_2022, " (", LUNG_234_sub_UP2_top10$RR_2022, ")")
LUNG_234_sub_UP2_top10 = LUNG_234_sub_UP2_top10[ ,c("UP2", "NAME2", "string_2018", "string_2022")]
write_xlsx(LUNG_234_sub_UP2_top10, paste0("", "LUNG_234_sub_UP2_top10.xlsx"),
           col_names=T, format_headers=F)



### 4-3-7) 남성 : 2000년도 이전 ---------------------------------------------------
head(lung_rr$LUNG_RR_M01$`2022`)

LUNG_final_M01_sub = lung_final$LUNG_final_M01[ ,c("ID", "NAME", "LEVEL", "UP1", "NAME1", "UP2", "NAME2",
                                                          "SEX", "CAL2", "2018", "2022")]
LUNG_M01_sub = cbind(
  lung_final$LUNG_final_M01[ ,c("ID", "NAME", "LEVEL", "UP1", "NAME1", "UP2", "NAME2", "SEX", "CAL2")],
  final_2018=lung_final$LUNG_final_M01$`2018`, RR_2018=lung_rr$LUNG_RR_M01$`2018`,
  final_2022=lung_final$LUNG_final_M01$`2022`, RR_2022=lung_rr$LUNG_RR_M01$`2022`)
LUNG_M01_sub = LUNG_M01_sub[ LUNG_M01_sub$final_2018 > 0, ]
LUNG_M01_sub_UP1 = LUNG_M01_sub[ LUNG_M01_sub$UP1 != "", ]
LUNG_M01_sub_UP2 = LUNG_M01_sub[ LUNG_M01_sub$UP2 != "", ]



### 4-3-8) 남성 & 대분류 : 2000년도 이전 ---------------------------------------------------
LUNG_M01_sub_UP1_top10 = (LUNG_M01_sub_UP1[order(LUNG_M01_sub_UP1$RR_2022, decreasing=TRUE), ])[
  1:10, c("UP1", "NAME1", "final_2018", "RR_2018", "final_2022", "RR_2022")]
LUNG_M01_sub_UP1_top10 = LUNG_M01_sub_UP1_top10[ ,c("UP1", "NAME1", "final_2018", "RR_2018",
                                                            "final_2022", "RR_2022")]
LUNG_M01_sub_UP1_top10$string_2018 = 
  paste0(LUNG_M01_sub_UP1_top10$final_2018, " (", LUNG_M01_sub_UP1_top10$RR_2018, ")")
LUNG_M01_sub_UP1_top10$string_2022 = 
  paste0(LUNG_M01_sub_UP1_top10$final_2022, " (", LUNG_M01_sub_UP1_top10$RR_2022, ")")
LUNG_M01_sub_UP1_top10 = LUNG_M01_sub_UP1_top10[ ,c("UP1", "NAME1", "string_2018", "string_2022")]
write_xlsx(LUNG_M01_sub_UP1_top10, paste0("", "LUNG_M01_sub_UP1_top10.xlsx"),
           col_names=T, format_headers=F)



### 4-3-9) 남성 & 중분류 : 2000년도 이전 ---------------------------------------------------
LUNG_M01_sub_UP2_top10 = (LUNG_M01_sub_UP2[order(LUNG_M01_sub_UP2$RR_2022, decreasing=TRUE), ])[
  1:10, c("UP2", "NAME2", "final_2018", "RR_2018", "final_2022", "RR_2022")]
LUNG_M01_sub_UP2_top10 = LUNG_M01_sub_UP2_top10[ ,c("UP2", "NAME2", "final_2018", "RR_2018",
                                                            "final_2022", "RR_2022")]
LUNG_M01_sub_UP2_top10$string_2018 = 
  paste0(LUNG_M01_sub_UP2_top10$final_2018, " (", LUNG_M01_sub_UP2_top10$RR_2018, ")")
LUNG_M01_sub_UP2_top10$string_2022 = 
  paste0(LUNG_M01_sub_UP2_top10$final_2022, " (", LUNG_M01_sub_UP2_top10$RR_2022, ")")
LUNG_M01_sub_UP2_top10 = LUNG_M01_sub_UP2_top10[ ,c("UP2", "NAME2", "string_2018", "string_2022")]
write_xlsx(LUNG_M01_sub_UP2_top10, paste0("", "LUNG_M01_sub_UP2_top10.xlsx"),
           col_names=T, format_headers=F)



### 4-3-10) 남성 : 2000년도 이후 ---------------------------------------------------
head(lung_rr$LUNG_RR_M234$`2022`)

LUNG_final_M234_sub = lung_final$LUNG_final_M234[ ,c("ID", "NAME", "LEVEL", "UP1", "NAME1", "UP2", "NAME2",
                                                            "SEX", "CAL2", "2018", "2022")]
LUNG_M234_sub = cbind(
  lung_final$LUNG_final_M234[ ,c("ID", "NAME", "LEVEL", "UP1", "NAME1", "UP2", "NAME2", "SEX", "CAL2")],
  final_2018=lung_final$LUNG_final_M234$`2018`, RR_2018=lung_rr$LUNG_RR_M234$`2018`,
  final_2022=lung_final$LUNG_final_M234$`2022`, RR_2022=lung_rr$LUNG_RR_M234$`2022`)
LUNG_M234_sub = LUNG_M234_sub[ LUNG_M234_sub$final_2018 > 0, ]
LUNG_M234_sub_UP1 = LUNG_M234_sub[ LUNG_M234_sub$UP1 != "", ]
LUNG_M234_sub_UP2 = LUNG_M234_sub[ LUNG_M234_sub$UP2 != "", ]



### 4-3-11) 남성 & 대분류 : 2000년도 이후 ---------------------------------------------------
LUNG_M234_sub_UP1_top10 = (LUNG_M234_sub_UP1[order(LUNG_M234_sub_UP1$RR_2022, decreasing=TRUE), ])[
  1:10, c("UP1", "NAME1", "final_2018", "RR_2018", "final_2022", "RR_2022")]
LUNG_M234_sub_UP1_top10 = LUNG_M234_sub_UP1_top10[ ,c("UP1", "NAME1", "final_2018", "RR_2018",
                                                              "final_2022", "RR_2022")]
LUNG_M234_sub_UP1_top10$string_2018 = 
  paste0(LUNG_M234_sub_UP1_top10$final_2018, " (", LUNG_M234_sub_UP1_top10$RR_2018, ")")
LUNG_M234_sub_UP1_top10$string_2022 = 
  paste0(LUNG_M234_sub_UP1_top10$final_2022, " (", LUNG_M234_sub_UP1_top10$RR_2022, ")")
LUNG_M234_sub_UP1_top10 = LUNG_M234_sub_UP1_top10[ ,c("UP1", "NAME1", "string_2018", "string_2022")]
write_xlsx(LUNG_M234_sub_UP1_top10, paste0("", "LUNG_M234_sub_UP1_top10.xlsx"),
           col_names=T, format_headers=F)



### 4-3-12) 남성 & 중분류 : 2000년도 이후 ---------------------------------------------------
LUNG_M234_sub_UP2_top10 = (LUNG_M234_sub_UP2[order(LUNG_M234_sub_UP2$RR_2022, decreasing=TRUE), ])[
  1:10, c("UP2", "NAME2", "final_2018", "RR_2018", "final_2022", "RR_2022")]
LUNG_M234_sub_UP2_top10 = LUNG_M234_sub_UP2_top10[ ,c("UP2", "NAME2", "final_2018", "RR_2018",
                                                              "final_2022", "RR_2022")]
LUNG_M234_sub_UP2_top10$string_2018 = 
  paste0(LUNG_M234_sub_UP2_top10$final_2018, " (", LUNG_M234_sub_UP2_top10$RR_2018, ")")
LUNG_M234_sub_UP2_top10$string_2022 = 
  paste0(LUNG_M234_sub_UP2_top10$final_2022, " (", LUNG_M234_sub_UP2_top10$RR_2022, ")")
LUNG_M234_sub_UP2_top10 = LUNG_M234_sub_UP2_top10[ ,c("UP2", "NAME2", "string_2018", "string_2022")]
write_xlsx(LUNG_M234_sub_UP2_top10, paste0("", "LUNG_M234_sub_UP2_top10.xlsx"),
           col_names=T, format_headers=F)



### 4-3-13) 여성 : 2000년도 이전 ---------------------------------------------------
head(lung_rr$LUNG_RR_F01$`2022`)

LUNG_final_F01_sub = lung_final$LUNG_final_F01[ ,c("ID", "NAME", "LEVEL", "UP1", "NAME1", "UP2", "NAME2",
                                                          "SEX", "CAL2", "2018", "2022")]
LUNG_F01_sub = cbind(
  lung_final$LUNG_final_F01[ ,c("ID", "NAME", "LEVEL", "UP1", "NAME1", "UP2", "NAME2", "SEX", "CAL2")],
  final_2018=lung_final$LUNG_final_F01$`2018`, RR_2018=lung_rr$LUNG_RR_F01$`2018`,
  final_2022=lung_final$LUNG_final_F01$`2022`, RR_2022=lung_rr$LUNG_RR_F01$`2022`)
LUNG_F01_sub = LUNG_F01_sub[ LUNG_F01_sub$final_2018 > 0, ]
LUNG_F01_sub_UP1 = LUNG_F01_sub[ LUNG_F01_sub$UP1 != "", ]
LUNG_F01_sub_UP2 = LUNG_F01_sub[ LUNG_F01_sub$UP2 != "", ]



### 4-3-14) 여성 & 대분류 : 2000년도 이전 --------------------------------------------------
LUNG_F01_sub_UP1_top10 = (LUNG_F01_sub_UP1[order(LUNG_F01_sub_UP1$RR_2022, decreasing=TRUE), ])[
  1:10, c("UP1", "NAME1", "final_2018", "RR_2018", "final_2022", "RR_2022")]
LUNG_F01_sub_UP1_top10 = LUNG_F01_sub_UP1_top10[ ,c("UP1", "NAME1", "final_2018", "RR_2018",
                                                            "final_2022", "RR_2022")]
LUNG_F01_sub_UP1_top10$string_2018 = 
  paste0(LUNG_F01_sub_UP1_top10$final_2018, " (", LUNG_F01_sub_UP1_top10$RR_2018, ")")
LUNG_F01_sub_UP1_top10$string_2022 = 
  paste0(LUNG_F01_sub_UP1_top10$final_2022, " (", LUNG_F01_sub_UP1_top10$RR_2022, ")")
LUNG_F01_sub_UP1_top10 = LUNG_F01_sub_UP1_top10[ ,c("UP1", "NAME1", "string_2018", "string_2022")]
write_xlsx(LUNG_F01_sub_UP1_top10, paste0("", "LUNG_F01_sub_UP1_top10.xlsx"),
           col_names=T, format_headers=F)



### 4-3-15) 여성 & 중분류 : 2000년도 이전 --------------------------------------------------
LUNG_F01_sub_UP2_top10 = (LUNG_F01_sub_UP2[order(LUNG_F01_sub_UP2$RR_2022, decreasing=TRUE), ])[
  1:10, c("UP2", "NAME2", "final_2018", "RR_2018", "final_2022", "RR_2022")]
LUNG_F01_sub_UP2_top10 = LUNG_F01_sub_UP2_top10[ ,c("UP2", "NAME2", "final_2018", "RR_2018",
                                                            "final_2022", "RR_2022")]
LUNG_F01_sub_UP2_top10$string_2018 = 
  paste0(LUNG_F01_sub_UP2_top10$final_2018, " (", LUNG_F01_sub_UP2_top10$RR_2018, ")")
LUNG_F01_sub_UP2_top10$string_2022 = 
  paste0(LUNG_F01_sub_UP2_top10$final_2022, " (", LUNG_F01_sub_UP2_top10$RR_2022, ")")
LUNG_F01_sub_UP2_top10 = LUNG_F01_sub_UP2_top10[ ,c("UP2", "NAME2", "string_2018", "string_2022")]
write_xlsx(LUNG_F01_sub_UP2_top10, paste0("", "LUNG_F01_sub_UP2_top10.xlsx"),
           col_names=T, format_headers=F)



### 4-3-16) 여성 : 2000년도 이후 --------------------------------------------------
head(lung_rr$LUNG_RR_F234$`2022`)

LUNG_final_F234_sub = lung_final$LUNG_final_F234[ ,c("ID", "NAME", "LEVEL", "UP1", "NAME1", "UP2", "NAME2",
                                                            "SEX", "CAL2", "2018", "2022")]
LUNG_F234_sub = cbind(
  lung_final$LUNG_final_F234[ ,c("ID", "NAME", "LEVEL", "UP1", "NAME1", "UP2", "NAME2", "SEX", "CAL2")],
  final_2018=lung_final$LUNG_final_F234$`2018`, RR_2018=lung_rr$LUNG_RR_F234$`2018`,
  final_2022=lung_final$LUNG_final_F234$`2022`, RR_2022=lung_rr$LUNG_RR_F234$`2022`)
LUNG_F234_sub = LUNG_F234_sub[ LUNG_F234_sub$final_2018 > 0, ]
LUNG_F234_sub_UP1 = LUNG_F234_sub[ LUNG_F234_sub$UP1 != "", ]
LUNG_F234_sub_UP2 = LUNG_F234_sub[ LUNG_F234_sub$UP2 != "", ]



### 4-3-17) 여성 & 대분류 : 2000년도 이후 --------------------------------------------------
LUNG_F234_sub_UP1_top10 = (LUNG_F234_sub_UP1[order(LUNG_F234_sub_UP1$RR_2022, decreasing=TRUE), ])[
  1:10, c("UP1", "NAME1", "final_2018", "RR_2018", "final_2022", "RR_2022")]
LUNG_F234_sub_UP1_top10 = LUNG_F234_sub_UP1_top10[ ,c("UP1", "NAME1", "final_2018", "RR_2018",
                                                              "final_2022", "RR_2022")]
LUNG_F234_sub_UP1_top10$string_2018 = 
  paste0(LUNG_F234_sub_UP1_top10$final_2018, " (", LUNG_F234_sub_UP1_top10$RR_2018, ")")
LUNG_F234_sub_UP1_top10$string_2022 = 
  paste0(LUNG_F234_sub_UP1_top10$final_2022, " (", LUNG_F234_sub_UP1_top10$RR_2022, ")")
LUNG_F234_sub_UP1_top10 = LUNG_F234_sub_UP1_top10[ ,c("UP1", "NAME1", "string_2018", "string_2022")]
write_xlsx(LUNG_F234_sub_UP1_top10, paste0("", "LUNG_F234_sub_UP1_top10.xlsx"),
           col_names=T, format_headers=F)



### 4-3-18) 여성 & 중분류 : 2000년도 이후 --------------------------------------------------
LUNG_F234_sub_UP2_top10 = (LUNG_F234_sub_UP2[order(LUNG_F234_sub_UP2$RR_2022, decreasing=TRUE), ])[
  1:10, c("UP2", "NAME2", "final_2018", "RR_2018", "final_2022", "RR_2022")]
LUNG_F234_sub_UP2_top10 = LUNG_F234_sub_UP2_top10[ ,c("UP2", "NAME2", "final_2018", "RR_2018",
                                                              "final_2022", "RR_2022")]
LUNG_F234_sub_UP2_top10$string_2018 = 
  paste0(LUNG_F234_sub_UP2_top10$final_2018, " (", LUNG_F234_sub_UP2_top10$RR_2018, ")")
LUNG_F234_sub_UP2_top10$string_2022 = 
  paste0(LUNG_F234_sub_UP2_top10$final_2022, " (", LUNG_F234_sub_UP2_top10$RR_2022, ")")
LUNG_F234_sub_UP2_top10 = LUNG_F234_sub_UP2_top10[ ,c("UP2", "NAME2", "string_2018", "string_2022")]
write_xlsx(LUNG_F234_sub_UP2_top10, paste0("", "LUNG_F234_sub_UP2_top10.xlsx"),
           col_names=T, format_headers=F)
