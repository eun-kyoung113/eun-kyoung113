# [3] 업종단위 (level 1 - level 3ab) 계층시계열에 대해 적합한 모형을 바탕으로 검증오류 계산, 앙상블, 계층시계열 조정 ----------------------------
## <What to do>
## 1. 계층 시계열 조정
##   : minT 방법 이용
##   - summing matrix "S" 생성

## 2. ensemble weight 도출
##   : validation set의 예측오차(RMSE) 이용

## 3. validation set, test set에 대해 model 기반 예측값 기반 계층 시계열 조정
##   : minT 방법 기반

## 4. total 결과 file 생성 / 저장
# -------------------------------------------------------------------------------------------------------


### 0. library import -------------------------------------------------------
library(data.table)
setDTthreads(24)
library(tidyverse)
library(stringr)
library(bit64)
library(dtplyr)
library(splines)


# -----------------------------------------------------------------------------------------------------------
### 1. Make "S"(summing matrix) : 계층 시계열 조정에 사용 -----------------------------
### summing matrix 생성 때 [1] benchmark.R에서 생성한 "full_v2" file 이용 -----
fulls <- read.csv("full_v2.csv")
meta_bs <- unique(fulls[ ,c("ID","NAME","LEVEL","UP1","UP2","SEX","CAL2")])

### 1-1) the lowest disaggregate level series 가져오기-----------------------------
### 주어진 fulls data에서 the lowest disaggregate level은 LEVEL이 "L3ab"인 series ----
meta_disagg <- meta_bs[meta_bs$LEVEL == "L3ab", ]
len_L3ab <- nrow(meta_disagg)
names_L3ab <- meta_disagg$NAME

### 1-2) summing matrix "S" 만들기 -----------------------------
S = data.frame(t(rep(1, len_L3ab)))
colnames(S) = names_L3ab


### 참고사항) S matrix의 첫 번째 열은 모두 원소가 "1" ----------
for (i in 2:nrow(meta_bs)) {
  temp_row = rep(0, len_L3ab)
  
  for (j in 1:len_L3ab) {
    
    temp_bs = meta_bs[i, c("UP1", "UP2", "SEX", "CAL2")]
    temp_disagg = meta_disagg[j, c("UP1", "UP2", "SEX", "CAL2")] 
    
    ## meta_bs의 i번째 행과 the lowest disaggregate level series set의 j번째 행이 같은지 확인 -- 결과가 boolean 형태
    temp_ind = (temp_bs == temp_disagg) 
    
    ## boolean 결과가 NA값을 제외하고 모두 TRUE이면 "S" matrix의 i번째 행, j번째 열의 원소값에 1 대입.
    if (sum(temp_ind, na.rm=T) == sum(!is.na(temp_ind))) temp_row[j] = 1
  }
  
  S = rbind(S, temp_row)
}


### 1-3) "S" matrix 저장 ------------------------------------------------------
write.csv(S, "S.csv", row.names=FALSE, col.names=TRUE)
rm(S) # memory에서 제거


### 1-4) 저장한 "S" matrix re-import -------------------------------------------
S = read.csv("S.csv", header=T, row.names = meta_bs$NAME)
S = as.matrix(S)



# -----------------------------------------------------------------------------------------------------------
### 2. Ensemble에 사용하는 weight 얻기 ---------------------------------------------
### 2-1) [2] model.fitting.R file들에서 생성한 validation_final.csv, test_final.csv file import --------
data_val = read.csv("validation_final.csv", header=T)
data_val = data_val[order(match(data_val$NAME, rownames(S)), data_val$YEAR), ]
data_test = read.csv("test_final.csv", header=T)
data_test = data_test[order(match(data_test$NAME, rownames(S)), data_test$YEAR), ]


### poisson regression model fitting 결과는 강제로 제거 : 전체 계층 시계열 series의 안정성을 위함 --------
colinds_poisson = which(grepl("poisson", colnames(data_val)))
data_val = data_val[ ,-colinds_poisson]



### 2-2) Ensemble weight에 사용되는 예측오차 RMSE 계산 --------------------------------------
### 예측오차 계산 위해 validation set 가져오기 : validation set은 "YEAR"가 2014년 이후의 자료 -----
valdf = data_val[data_val$YEAR >= 2015, ]

### column index 가져오기 -------------------------------------------------------
colInds_LEUKEMIA_pred = which( grepl("LEUKEMIA_", colnames(data_val)) & 
  grepl("^((?!FY_).)*$", colnames(data_val), perl=T))

colInds_FY_LEUKEMIA_pred = which( grepl("FY_LEUKEMIA_", colnames(data_val)))

colInds_LUNG_pred = which( grepl("LUNG_", colnames(data_val)) & 
                                 grepl("^((?!FY_).)*$", colnames(data_val), perl=T))

colInds_FY_LUNG_pred = which( grepl("FY_LUNG_", colnames(data_val)))


### define column indices for future use -------------
colInds_pred = c(colInds_LEUKEMIA_pred, colInds_FY_LEUKEMIA_pred, 
  colInds_LUNG_pred, colInds_FY_LUNG_pred)
colInds_meta = 1:3
colnames_pred = colnames(data_val)[colInds_pred]
colnames_meta = colnames(data_val)[colInds_meta]


### 각 질병 별 반응변수에 대한 RMSE 계산 -------------------------------------------------
sqErrs_LEUKEMIA = (valdf[, colInds_LEUKEMIA_pred] - valdf[ , "LEUKEMIA"])^2
sqErrs_FY_LEUKEMIA = (valdf[, colInds_FY_LEUKEMIA_pred] - valdf[ , "FY_LEUKEMIA"])^2
sqErrs_LUNG = (valdf[, colInds_LUNG_pred] - valdf[ , "LUNG"])^2
sqErrs_FY_LUNG = (valdf[, colInds_FY_LUNG_pred] - valdf[ , "FY_LUNG"])^2

sqErrs = cbind(sqErrs_LEUKEMIA, sqErrs_FY_LEUKEMIA,
               sqErrs_LUNG, sqErrs_FY_LUNG)

MSEs = apply(sqErrs, 2, function(t) sqrt(tapply(t, valdf$ID, mean)))
MSEs = cbind(meta_bs[ ,c("ID", "NAME")], MSEs)



#####-------------------------------------------------------------------------
### 2-3) Ensemble에 사용할 weight 계산하는 함수 작성 -------------------------------------
### weight 부여 방법 : MSE가 가장 낮은 모형에 가장 높은 weight를 주도록 exp(-MSE) 이용


### cal_weight 함수 : 반응변수 값이 0인 사업장에 대한 모델 적합값도 0인 관측치 추출하는 역할 ------
cal_weight = function(t) {
  
  if (max(t) > 10^-6) {
    t = - ( t / min(t) ) # negative, normalized MSEs 
    t = t + 1 # make the maximum MSE value as zero
    s = exp(t) / sum(exp(t)) # weight 생성
    s[s < 0.001] = 0 # weight가 낮은 예측치에 대한 weight는 0으로 변환
    s = s / sum(s)
  }
  
  else {
    s = c( 1, rep(0, length(t)-1) ) 
    # import한 error가 모두  0 근처인 관측치에 대해서는 linear model 적합 값에 weight를 1로 
  }
  
  names(s) = names(t)
  return(s)
}



### 2-4) validation set에서 측정한 MSE 이용해 Ensemble weight 생성 ------------
### 각 모형 별 예측값에 대한 column index 가져오기 ----------------------------
colInds_LEUKEMIA = which( grepl("LEUKEMIA_", colnames(MSEs)) & 
                                 grepl("^((?!FY_).)*$", colnames(MSEs), perl=T))

colInds_FY_LEUKEMIA = which( grepl("FY_LEUKEMIA_", colnames(MSEs)))

colInds_LUNG = which( grepl("LUNG_", colnames(MSEs)) & 
                             grepl("^((?!FY_).)*$", colnames(MSEs), perl=T))

colInds_FY_LUNG = which( grepl("FY_LUNG_", colnames(MSEs)))


### ensemble weight 계산 ------------------------------------------------------
weights = cbind(t(apply(MSEs[ ,colInds_LEUKEMIA], 1,cal_weight)),  # LEUKEMIA
                t(apply(MSEs[ ,colInds_FY_LEUKEMIA], 1, cal_weight)),  # FY_LEUKEMIA
                t(apply(MSEs[ ,colInds_LUNG], 1, cal_weight)), # LUNG
                t(apply(MSEs[ ,colInds_FY_LUNG], 1, cal_weight))) # FY_LUNG

weights = cbind(meta_bs[ ,c("ID", "NAME")], weights)
rownames(weights) = NULL 


### 생성한 ensemble weight csv file로 저장 ----------------------------------------
write.csv(weights, "ensemble_weights.csv", col.names=T, row.names=F)



# --------------------------------------------------------------------------------------
### 2-5) Ensemble method 이용해 각 series 별 ensembled predicted value obtain ----------

### 생성한 weight file import --------------------------------------------------
weights = read.csv("ensemble_weights.csv", header=T)

### validation set과 test set에 대한 Ensemble predicted value 생성part --------------
### 빈 테이블 생성 후, for 구문으로 순차적으로 채워넣는 방식 사용 --------------------
data_val_ensem = data.frame(data_val[ ,c("ID", "NAME", "YEAR")],
  LEUKEMIA_ensem = NA, FY_LEUKEMIA_ensem = NA, LUNG_ensem = NA, FY_LUNG_ensem = NA)

data_test_ensem = data.frame(data_test[ ,c("ID","NAME", "YEAR")],
  LEUKEMIA_ensem = NA, FY_LEUKEMIA_ensem = NA, LUNG_ensem = NA, FY_LUNG_ensem = NA)

colInds_LEUKEMIA = which( grepl("LEUKEMIA_", colnames_pred) & 
                            grepl("^((?!FY_).)*$", colnames_pred, perl=T))

colInds_FY_LEUKEMIA = which( grepl("FY_LEUKEMIA_", colnames_pred))

colInds_LUNG = which( grepl("LUNG_", colnames_pred) & 
                        grepl("^((?!FY_).)*$", colnames_pred, perl=T))

colInds_FY_LUNG = which( grepl("FY_LUNG_", colnames_pred))

for (id in 1:max(data_test$ID)) {
  temp_val = as.matrix(data_val[data_val$ID == id, colnames_pred])
  temp_test = as.matrix(data_test[data_test$ID == id, colnames_pred])
  temp_weight = t(as.vector(weights[weights$ID == id, colnames_pred]))
  
  data_val_ensem$LEUKEMIA_ensem[data_val_ensem$ID == id] = temp_val[ ,colInds_LEUKEMIA] %*% temp_weight[colInds_LEUKEMIA]
  data_val_ensem$FY_LEUKEMIA_ensem[data_val_ensem$ID == id] = temp_val[ ,colInds_FY_LEUKEMIA] %*% temp_weight[colInds_FY_LEUKEMIA]
  data_val_ensem$LUNG_ensem[data_val_ensem$ID == id] = temp_val[ ,colInds_LUNG] %*% temp_weight[colInds_LUNG]
  data_val_ensem$FY_LUNG_ensem[data_val_ensem$ID == id] = temp_val[ ,colInds_FY_LUNG] %*% temp_weight[colInds_FY_LUNG]
  
  data_test_ensem$LEUKEMIA_ensem[data_test_ensem$ID == id] = temp_test[ ,colInds_LEUKEMIA] %*% temp_weight[colInds_LEUKEMIA]
  data_test_ensem$FY_LEUKEMIA_ensem[data_test_ensem$ID == id] = temp_test[ ,colInds_FY_LEUKEMIA] %*% temp_weight[colInds_FY_LEUKEMIA]
  data_test_ensem$LUNG_ensem[data_test_ensem$ID == id] = temp_test[ ,colInds_LUNG] %*% temp_weight[colInds_LUNG]
  data_test_ensem$FY_LUNG_ensem[data_test_ensem$ID == id] = temp_test[ ,colInds_FY_LUNG] %*% temp_weight[colInds_FY_LUNG]
}

write.csv(data_val_ensem, "validation_ensembles.csv", col.names=T, row.names=F)
write.csv(data_test_ensem, "test_ensembles.csv", col.names=T, row.names=F)




# -------------------------------------------------------------------------
### 3. Reconciliation - minT 방법 기반 계층 시계열 조정 ------------------------

### 3-1) test set을 fat table form으로 형성 -------------------------------------------
### Category : ally(true value) / allf(fitted values) -----------------------
ally.raw_LEUKEMIA = data_test[ data_test$YEAR <= 2018 ,c("ID", "NAME", "YEAR", "LEUKEMIA")]
allf.raw_LEUKEMIA = data_test_ensem[ ,c("ID", "NAME", "YEAR", "LEUKEMIA_ensem")]
ally.raw_FY_LEUKEMIA = data_test[ data_test$YEAR <= 2018 ,c("ID", "NAME", "YEAR", "FY_LEUKEMIA")]
allf.raw_FY_LEUKEMIA = data_test_ensem[ ,c("ID", "NAME", "YEAR", "FY_LEUKEMIA_ensem")]
ally.raw_LUNG = data_test[ data_test$YEAR <= 2018 ,c("ID", "NAME", "YEAR", "LUNG")]
allf.raw_LUNG = data_test_ensem[ ,c("ID", "NAME", "YEAR", "LUNG_ensem")]
ally.raw_FY_LUNG = data_test[ data_test$YEAR <= 2018 ,c("ID", "NAME", "YEAR", "FY_LUNG")]
allf.raw_FY_LUNG = data_test_ensem[ ,c("ID", "NAME", "YEAR", "FY_LUNG_ensem")]


### 3-2) reconciliation 진행하는 함수 생성 ------------------------------------------
make_coherent = function(ally.raw, allf.raw, S) {
  name_y = setdiff(colnames(ally.raw), c("ID", "NAME", "YEAR"))
  name_f = setdiff(colnames(allf.raw), c("ID", "NAME", "YEAR"))

  require(tidyverse)
  ## pivot_wider : wide 형식의 table 만들어주는 함수
  ally.raw2 = pivot_wider(ally.raw, names_from = YEAR, values_from = all_of(name_y) )
  ally = t(as.matrix(ally.raw2[ ,-c(1,2)]))
  colnames(ally) = ally.raw2$NAME
  ally[is.na(ally)] = 0 # y값이 결측인 관측치는 0으로 값 대체
  
  allf.raw2 = pivot_wider(allf.raw, names_from = YEAR, values_from = all_of(name_f) )
  allf = t(as.matrix(allf.raw2[ ,-c(1,2)]))
  colnames(allf) = allf.raw2$NAME
  allf[is.na(allf)] = 0 # 예측값이 결측인 관측치는 0으로 값 대체
  hat.YH = allf
  
  ## W_h 행렬 계산하기
  tempData4Cov = ally[c("2015", "2016", "2017", "2018"), ] -
    hat.YH[c("2015", "2016", "2017", "2018"), ] # 4년치 residuals들의 variance
  
  require(nlshrink)
  ## variance modeling
  diag_W_h = diag(cov(tempData4Cov))
  diag_W_h[diag_W_h < 0.5] = 0.5 # 분산이 0.5보다 작은 부분은 모두 0.5로 대체
  W_h = diag(diag_W_h)
  inv_W_h = diag(1/diag_W_h)

  # tilde y_h(계층 시계열 조정한 후 얻은 반응변수의 예측값) 계산하기
  temp <- t(S) %*% inv_W_h %*% S
  P <- solve(temp) %*% t(S) %*% inv_W_h
  til.YH <- hat.YH %*% t(P) %*% t(S) # if y = Ax for x with length p, then Y = XA^T for data matrix X (n x p)
  
  result_wide = data.frame(ID=1:ncol(til.YH), NAME=colnames(til.YH), t(til.YH))
  
  results = melt(setDT(result_wide), id.vars=c("ID", "NAME"), variable.name="YEAR")
  results$YEAR = substr(results$YEAR, 2, 5)
  results = data.frame(results[ order(results$ID, results$YEAR),  ])
  colnames(results)[colnames(results) == "value"] = paste0(name_f, "_adj")
  
  return(results)
}

res1 = make_coherent(ally.raw_LEUKEMIA,    allf.raw_LEUKEMIA, S)
res2 = make_coherent(ally.raw_FY_LEUKEMIA, allf.raw_FY_LEUKEMIA, S)
res3 = make_coherent(ally.raw_LUNG,        allf.raw_LUNG, S)
res4 = make_coherent(ally.raw_FY_LUNG,     allf.raw_FY_LUNG, S)

### 3-3) 계층시계열 조정한 ensembled test set 저장 ----------------------------------------------
data_test_ensem_adj = data.frame(res1, res2[ ,4], res3[ ,4], res4[ ,4])
colnames(data_test_ensem_adj)[4:7] = paste0(c("LEUKEMIA", "FY_LEUKEMIA", "LUNG", "FY_LUNG"),
                                            "_ensem_adj")

write.csv(data_test_ensem_adj, "test_ensembles_adj.csv", col.names=T, row.names=F)


data_postprocessed = merge(x=data_test_ensem, y=data_test_ensem_adj,
                           by=c("ID", "NAME", "YEAR"), all.x=TRUE, all.y=FALSE)

data_postprocessed2 = merge(x=data_test, y=data_postprocessed,
                            by=c("ID", "NAME", "YEAR"), all.x=TRUE, all.y=FALSE)

data_postprocessed2 = data_postprocessed2[ ,c(
  "ID", "NAME", "YEAR",
  "LEUKEMIA", "LEUKEMIA_ensem_adj", "LEUKEMIA_ensem", "LEUKEMIA_lin", "LEUKEMIA_linSpline", "LEUKEMIA_quad", 
  "FY_LEUKEMIA", "FY_LEUKEMIA_ensem_adj", "FY_LEUKEMIA_ensem", "FY_LEUKEMIA_lin", "FY_LEUKEMIA_linSpline", "FY_LEUKEMIA_quad",
  "LUNG", "LUNG_ensem_adj", "LUNG_ensem", "LUNG_lin", "LUNG_linSpline", "LUNG_quad",
  "FY_LUNG", "FY_LUNG_ensem_adj", "FY_LUNG_ensem", "FY_LUNG_lin", "FY_LUNG_linSpline", "FY_LUNG_quad"
)]



### 4. 2002년 ~ 2022년 total 결과 저장 -----------------------------------------------
write.csv(data_postprocessed2, "test_postprocessed.csv", col.names=T, row.names=F)
