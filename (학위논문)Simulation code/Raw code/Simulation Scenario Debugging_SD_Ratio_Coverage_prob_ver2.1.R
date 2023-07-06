### Simulation Scenario result Debugging version 2.1 ------------------------------------

### <What TO DO> ----------------------------------------------------------------
## Check the SD Ratio and Coverage probability of DR ATT Sandwich robust variance
##  -- consider variety of Exposure ratio (Mean = 0.5) & variance of PS
## add the code that get the ATE_result, ATT_result table
### -----------------------------------------------------------------------------


## -------------------------------------------------------------------------

### 1. import library & R source file ------------------------------------------------------
library(boot)

source("./Simulation Scenario code/Simulation_Hardcoding_function_ver1.R")
source("./Simulation Scenario code/Scenario1_make_result_ver1.2.R")



### 2. Get Simulation result -------------------------------------------------------------------------

### 2-1) delta0 = -2  ---------------------------------------------------------

## 2-1-1) deltaB = 0.01  ----------------------------------------
N <- 1000
REPL <- 1000
A_RATIO<--2 # p=0.12
A_VAR1<-0.01
A_VAR2<-0.01

Scenario1_result<-Scenario1_result(A_RATIO, A_VAR1, A_VAR2, N, REPL)

### -------------------------------------------------------------------------
### Store ATE result table -----------------------------------------------
ATE_1_result_2_0.01<-as.data.frame(Scenario1_result$ATE)


### -------------------------------------------------------------------------
### Store ATT result table-----------------------------------------------
ATT_1_result_2_0.01<-as.data.frame(Scenario1_result$ATT)


source("./Simulation Scenario code/Simulation_Monte_Carlo_SD_Ratio_ver1.2.R")


### 2-2) ATE_Scenario_1 Performance record ---------------------------------------
A_RATIO<--2
Monte_Carlo_var<-Monte_result(A_RATIO, A_VAR1, A_VAR2, 1000, 100000)

ATE_Monte_carlo<-as.data.frame(Monte_Carlo_var$ATE_var)
ATE_Monte_outcome_reg<-var(ATE_Monte_carlo$Outcome_reg_est) # 0.009638511
ATE_Monte_IPW<-var(ATE_Monte_carlo$IPW_est) # 0.009824517
ATE_Monte_DR<-var(ATE_Monte_carlo$DR_est) # 0.009742594

#ATE_1_result<-read.csv("./Simulation Scenario code/ATE_Scenario_1_p_0.12_result.csv")

ATE_1_performance_2_0.01<-as.data.frame(matrix(NA,nrow=3,ncol=6))
colnames(ATE_1_performance_2_0.01)<-c("Bias","rMSE","Naive_var_coverage","Naive_var_SD_Ratio", "Sandwich_robust_var_coverage",
                                      "Sandwich_var_SD_Ratio")

row.names(ATE_1_performance_2_0.01)<-c("Outcome_reg","IPW","DR")

func_rMSE<-function(x){
  return(mean((x-true_ATE)^2))
}


### ---------------------------
true_ATE<-log(2)
true_ATT<-log(2)

ATE_1_performance_2_0.01[,"Bias"]<-apply(ATE_1_result_2_0.01[,grep("_est",colnames(ATE_1_result_2_0.01))],2,mean)-true_ATE

ATE_1_performance_2_0.01[,"rMSE"]<-apply(ATE_1_result_2_0.01[,grep("_est",colnames(ATE_1_result_2_0.01))],2,FUN=func_rMSE)

ATE_1_performance_2_0.01[,"Naive_var_coverage"]<-c(mean(ATE_1_result_2_0.01$Outcome_reg_coverage), 
                                                   mean(ATE_1_result_2_0.01$Naive_IPW_coverage),
                                                   mean(ATE_1_result_2_0.01$DR_Naive_coverage))

ATE_1_performance_2_0.01[,"Sandwich_robust_var_coverage"]<-c(NA, mean(ATE_1_result_2_0.01$Sandwich_IPW_coverage),
                                                             mean(ATE_1_result_2_0.01$Sandwich_DR_coverage))

ATE_1_performance_2_0.01[,"Naive_var_SD_Ratio"]<-c(mean((ATE_1_result_2_0.01$Outcome_reg_var)^2)/ATE_Monte_outcome_reg,
                                                   mean((ATE_1_result_2_0.01$IPW_Naive_var)^2)/ATE_Monte_IPW,
                                                   mean(ATE_1_result_2_0.01$DR_Naive_var)/ATE_Monte_DR)

ATE_1_performance_2_0.01[,"Sandwich_var_SD_Ratio"]<-c(NA, mean(ATE_1_result_2_0.01$IPW_Sandwich_var)/ATE_Monte_IPW,
                                               mean(ATE_1_result_2_0.01$DR_Sandwich_var)/ATE_Monte_DR)

print(ATE_1_performance_2_0.01)
#                    Bias        rMSE Naive_var_coverage Naive_var_SD_Ratio Sandwich_robust_var_coverage Sandwich_var_SD_Ratio
# Outcome_reg -0.002379192 0.009928812              0.941          0.9906055                           NA                    NA
# IPW         -0.002150330 0.010115352              0.867          0.6172538                        0.982              1.489921
# DR          -0.002304139 0.009961594              0.938          0.9841385                        0.980              1.223564



### -------------------------------------------------------------------------
### 2-3) ATT_Scenario_1 Performance record ---------------------------------------
ATT_Monte_carlo<-as.data.frame(Monte_Carlo_var$ATT_var)
ATT_Monte_outcome_reg<-var(ATT_Monte_carlo$Outcome_reg_est) # 0.009638511
ATT_Monte_IPW<-var(ATT_Monte_carlo$IPW_est) # 0.009645044
ATT_Monte_DR<-var(ATT_Monte_carlo$DR_est) # 0.009642894


#ATT_1_result_2_0.01<-read.csv("./Simulation Scenario code/ATT_Scenario_1_p_0.12_result.csv")

ATT_1_performance_2_0.01<-as.data.frame(matrix(NA,nrow=3,ncol=6))
colnames(ATT_1_performance_2_0.01)<-c("Bias","rMSE","Naive_var_coverage","Naive_var_SD_Ratio","Sandwich_robust_var_coverage",
                                      "Sandwich_var_SD_Ratio")

row.names(ATT_1_performance_2_0.01)<-c("Outcome_reg","IPW","DR")

func_rMSE<-function(x){
  return(mean((x-true_ATT)^2))
}



### ---------------------------
ATT_1_performance_2_0.01[,"Bias"]<-apply(ATT_1_result_2_0.01[,grep("_est",colnames(ATT_1_result_2_0.01))],2,mean)-true_ATT

ATT_1_performance_2_0.01[,"rMSE"]<-apply(ATT_1_result_2_0.01[,grep("_est",colnames(ATT_1_result_2_0.01))],2,FUN=func_rMSE)

ATT_1_performance_2_0.01[,"Naive_var_coverage"]<-c(mean((ATT_1_result_2_0.01$Outcome_reg_coverage)^2), mean(ATT_1_result_2_0.01$Naive_IPW_coverage),
                                                   mean(ATT_1_result_2_0.01$DR_Naive_coverage))

ATT_1_performance_2_0.01[,"Sandwich_robust_var_coverage"]<-c(NA, mean(ATT_1_result_2_0.01$Sandwich_IPW_coverage),
                                                             mean(ATT_1_result_2_0.01$Sandwich_DR_coverage))

ATT_1_performance_2_0.01[,"Naive_var_SD_Ratio"]<-c(mean((ATT_1_result_2_0.01$Outcome_reg_var)^2)/ATT_Monte_outcome_reg,
                                                   mean((ATT_1_result_2_0.01$IPW_Naive_var)^2)/ATT_Monte_IPW,
                                                   mean(ATT_1_result_2_0.01$DR_Naive_var)/ATT_Monte_DR)

ATT_1_performance_2_0.01[,"Sandwich_var_SD_Ratio"]<-c(NA, mean(ATT_1_result_2_0.01$IPW_Sandwich_var)/ATT_Monte_IPW,
                                                      mean(ATT_1_result_2_0.01$DR_Sandwich_var)/ATT_Monte_DR)

print(ATT_1_performance_2_0.01)
#                     Bias        rMSE Naive_var_coverage Naive_var_SD_Ratio Sandwich_robust_var_coverage Sandwich_var_SD_Ratio
# Outcome_reg -0.002379192 0.009928812              0.941          0.9906055                           NA                    NA
# IPW         -0.002438935 0.009934539              0.870          0.6283862                        0.995              2.001384
# DR          -0.002399891 0.009940280              0.377          0.0638348                        1.000              3.812503





## -------------------------------------------------------------------------
## -------------------------------------------------------------------------
## -------------------------------------------------------------------------
## 2-1-2) deltaB = 0.6  ----------------------------------------
N <- 1000
REPL <- 1000
A_RATIO<--2 # p=0.12
A_VAR1<-0.6
A_VAR2<-0.6

Scenario1_result<-Scenario1_result(A_RATIO, A_VAR1, A_VAR2, N, REPL)

### -------------------------------------------------------------------------
### Store ATE result table -----------------------------------------------
ATE_1_result_2_0.6<-as.data.frame(Scenario1_result$ATE)


### -------------------------------------------------------------------------
### Store ATT result table-----------------------------------------------
ATT_1_result_2_0.6<-as.data.frame(Scenario1_result$ATT)


source("./Simulation Scenario code/Simulation_Monte_Carlo_SD_Ratio_ver1.2.R")



### 2-2) ATE_Scenario_1 Performance record ---------------------------------------
A_RATIO<--2
Monte_Carlo_var<-Monte_result(A_RATIO, A_VAR1, A_VAR2, 1000, 100000)

ATE_Monte_carlo<-as.data.frame(Monte_Carlo_var$ATE_var)
ATE_Monte_outcome_reg<-var(ATE_Monte_carlo$Outcome_reg_est) # 0.009638511
ATE_Monte_IPW<-var(ATE_Monte_carlo$IPW_est) # 0.009824517
ATE_Monte_DR<-var(ATE_Monte_carlo$DR_est) # 0.009742594

#ATE_1_result<-read.csv("./Simulation Scenario code/ATE_Scenario_1_p_0.12_result.csv")

ATE_1_performance_2_0.6<-as.data.frame(matrix(NA,nrow=3,ncol=6))
colnames(ATE_1_performance_2_0.6)<-c("Bias","rMSE","Naive_var_coverage","Naive_var_SD_Ratio", "Sandwich_robust_var_coverage",
                                     "Sandwich_var_SD_Ratio")

row.names(ATE_1_performance_2_0.6)<-c("Outcome_reg","IPW","DR")

func_rMSE<-function(x){
  return(mean((x-true_ATE)^2))
}


### ---------------------------
true_ATE<-log(2)
true_ATT<-log(2)

ATE_1_performance_2_0.6[,"Bias"]<-apply(ATE_1_result_2_0.6[,grep("_est",colnames(ATE_1_result_2_0.6))],2,mean)-true_ATE

ATE_1_performance_2_0.6[,"rMSE"]<-apply(ATE_1_result_2_0.6[,grep("_est",colnames(ATE_1_result_2_0.6))],2,FUN=func_rMSE)

ATE_1_performance_2_0.6[,"Naive_var_coverage"]<-c(mean(ATE_1_result_2_0.6$Outcome_reg_coverage), 
                                                  mean(ATE_1_result_2_0.6$Naive_IPW_coverage),
                                                  mean(ATE_1_result_2_0.6$DR_Naive_coverage))

ATE_1_performance_2_0.6[,"Sandwich_robust_var_coverage"]<-c(NA, mean(ATE_1_result_2_0.6$Sandwich_IPW_coverage),
                                                            mean(ATE_1_result_2_0.6$Sandwich_DR_coverage))

ATE_1_performance_2_0.6[,"Naive_var_SD_Ratio"]<-c(mean((ATE_1_result_2_0.6$Outcome_reg_var)^2)/ATE_Monte_outcome_reg,
                                                  mean((ATE_1_result_2_0.6$IPW_Naive_var)^2)/ATE_Monte_IPW,
                                                  mean(ATE_1_result_2_0.6$DR_Naive_var)/ATE_Monte_DR)

ATE_1_performance_2_0.6[,"Sandwich_var_SD_Ratio"]<-c(NA, mean(ATE_1_result_2_0.6$IPW_Sandwich_var)/ATE_Monte_IPW,
                                               mean(ATE_1_result_2_0.6$DR_Sandwich_var)/ATE_Monte_DR)

print(ATE_1_performance_2_0.6)
#                      Bias        rMSE Naive_var_coverage Naive_var_SD_Ratio Sandwich_robust_var_coverage Sandwich_var_SD_Ratio
# Outcome_reg -0.001359273 0.007306984              0.953          0.9910826                           NA                    NA
# IPW         -0.001065766 0.011507225              0.840          0.5380857                        0.968              1.365758
# DR          -0.003097177 0.008945412              0.949          0.9952240                        0.981              1.268540



### -------------------------------------------------------------------------
### 2-3) ATT_Scenario_1 Performance record ---------------------------------------
ATT_Monte_carlo<-as.data.frame(Monte_Carlo_var$ATT_var)
ATT_Monte_outcome_reg<-var(ATT_Monte_carlo$Outcome_reg_est) # 0.009638511
ATT_Monte_IPW<-var(ATT_Monte_carlo$IPW_est) # 0.009645044
ATT_Monte_DR<-var(ATT_Monte_carlo$DR_est) # 0.009642894


#ATT_1_result_2_0.6<-read.csv("./Simulation Scenario code/ATT_Scenario_1_p_0.12_result.csv")

ATT_1_performance_2_0.6<-as.data.frame(matrix(NA,nrow=3,ncol=6))
colnames(ATT_1_performance_2_0.6)<-c("Bias","rMSE","Naive_var_coverage","Naive_var_SD_Ratio","Sandwich_robust_var_coverage",
                                     "Sandwich_var_SD_Ratio")

row.names(ATT_1_performance_2_0.6)<-c("Outcome_reg","IPW","DR")

func_rMSE<-function(x){
  return(mean((x-true_ATT)^2))
}



### ---------------------------
ATT_1_performance_2_0.6[,"Bias"]<-apply(ATT_1_result_2_0.6[,grep("_est",colnames(ATT_1_result_2_0.6))],2,mean)-true_ATT

ATT_1_performance_2_0.6[,"rMSE"]<-apply(ATT_1_result_2_0.6[,grep("_est",colnames(ATT_1_result_2_0.6))],2,FUN=func_rMSE)

ATT_1_performance_2_0.6[,"Naive_var_coverage"]<-c(mean((ATT_1_result_2_0.6$Outcome_reg_coverage)^2), mean(ATT_1_result_2_0.6$Naive_IPW_coverage),
                                                  mean(ATT_1_result_2_0.6$DR_Naive_coverage))

ATT_1_performance_2_0.6[,"Sandwich_robust_var_coverage"]<-c(NA, mean(ATT_1_result_2_0.6$Sandwich_IPW_coverage),
                                                            mean(ATT_1_result_2_0.6$Sandwich_DR_coverage))

ATT_1_performance_2_0.6[,"Naive_var_SD_Ratio"]<-c(mean((ATT_1_result_2_0.6$Outcome_reg_var)^2)/ATT_Monte_outcome_reg,
                                                  mean((ATT_1_result_2_0.6$IPW_Naive_var)^2)/ATT_Monte_IPW,
                                                  mean(ATT_1_result_2_0.6$DR_Naive_var)/ATT_Monte_DR)

ATT_1_performance_2_0.6[,"Sandwich_var_SD_Ratio"]<-c(NA, mean(ATT_1_result_2_0.6$IPW_Sandwich_var)/ATT_Monte_IPW,
                                                     mean(ATT_1_result_2_0.6$DR_Sandwich_var)/ATT_Monte_DR)

print(ATT_1_performance_2_0.6)

#                      Bias        rMSE Naive_var_coverage Naive_var_SD_Ratio Sandwich_robust_var_coverage Sandwich_var_SD_Ratio
# Outcome_reg -0.0013592730 0.007306984              0.953         0.99108256                           NA                    NA
# IPW         -0.0009557775 0.007682229              0.920         0.74769924                        0.992              1.946291
# DR          -0.0005144325 0.007538381              0.450         0.09304479                        1.000              2.782268




## -------------------------------------------------------------------------
## -------------------------------------------------------------------------
## -------------------------------------------------------------------------
## 2-1-3) deltaB = 0.2  ----------------------------------------
library(boot)

source("./Simulation Scenario code/Simulation_Hardcoding_function_ver1.R")
source("./Simulation Scenario code/Scenario1_make_result_ver1.2.R")

N <- 1000
REPL <- 1000
A_RATIO<--2 # p=0.12
A_VAR1<-0.2
A_VAR2<-0.2

Scenario1_result<-Scenario1_result(A_RATIO, A_VAR1, A_VAR2, N, REPL)

### -------------------------------------------------------------------------
### Store ATE result table -----------------------------------------------
ATE_1_result_2_0.2<-as.data.frame(Scenario1_result$ATE)


### -------------------------------------------------------------------------
### Store ATT result table-----------------------------------------------
ATT_1_result_2_0.2<-as.data.frame(Scenario1_result$ATT)


source("./Simulation Scenario code/Simulation_Monte_Carlo_SD_Ratio_ver1.2.R")


### 2-2) ATE_Scenario_1 Performance record ---------------------------------------
#A_RATIO<--2
Monte_Carlo_var<-Monte_result(A_RATIO, A_VAR1, A_VAR2, 1000, 100000)

ATE_Monte_carlo<-as.data.frame(Monte_Carlo_var$ATE_var)
ATE_Monte_outcome_reg<-var(ATE_Monte_carlo$Outcome_reg_est) # 0.009638511
ATE_Monte_IPW<-var(ATE_Monte_carlo$IPW_est) # 0.009824517
ATE_Monte_DR<-var(ATE_Monte_carlo$DR_est) # 0.009742594

#ATE_1_result<-read.csv("./Simulation Scenario code/ATE_Scenario_1_p_0.12_result.csv")

ATE_1_performance_2_0.2<-as.data.frame(matrix(NA,nrow=3,ncol=6))
colnames(ATE_1_performance_2_0.2)<-c("Bias","rMSE","Naive_var_coverage","Naive_var_SD_Ratio", "Sandwich_robust_var_coverage",
                                     "Sandwich_var_SD_Ratio")

row.names(ATE_1_performance_2_0.2)<-c("Outcome_reg","IPW","DR")

func_rMSE<-function(x){
  return(mean((x-true_ATE)^2))
}


### ---------------------------
true_ATE<-log(2)
true_ATT<-log(2)

ATE_1_performance_2_0.2[,"Bias"]<-apply(ATE_1_result_2_0.2[,grep("_est",colnames(ATE_1_result_2_0.2))],2,mean)-true_ATE

ATE_1_performance_2_0.2[,"rMSE"]<-apply(ATE_1_result_2_0.2[,grep("_est",colnames(ATE_1_result_2_0.2))],2,FUN=func_rMSE)

ATE_1_performance_2_0.2[,"Naive_var_coverage"]<-c(mean(ATE_1_result_2_0.2$Outcome_reg_coverage), 
                                                  mean(ATE_1_result_2_0.2$Naive_IPW_coverage),
                                                  mean(ATE_1_result_2_0.2$DR_Naive_coverage))

ATE_1_performance_2_0.2[,"Sandwich_robust_var_coverage"]<-c(NA, mean(ATE_1_result_2_0.2$Sandwich_IPW_coverage),
                                                            mean(ATE_1_result_2_0.2$Sandwich_DR_coverage))

ATE_1_performance_2_0.2[,"Naive_var_SD_Ratio"]<-c(mean((ATE_1_result_2_0.2$Outcome_reg_var)^2)/ATE_Monte_outcome_reg,
                                                  mean((ATE_1_result_2_0.2$IPW_Naive_var)^2)/ATE_Monte_IPW,
                                                  mean(ATE_1_result_2_0.2$DR_Naive_var)/ATE_Monte_DR)

ATE_1_performance_2_0.2[,"Sandwich_var_SD_Ratio"]<-c(NA, mean(ATE_1_result_2_0.2$IPW_Sandwich_var)/ATE_Monte_IPW,
                                               mean(ATE_1_result_2_0.2$DR_Sandwich_var)/ATE_Monte_DR)

print(ATE_1_performance_2_0.2)
#                     Bias        rMSE Naive_var_coverage Naive_var_SD_Ratio Sandwich_robust_var_coverage Sandwich_var_SD_Ratio
# Outcome_reg -0.001817516 0.008770811              0.955          0.9927099                           NA                    NA
# IPW         -0.001943531 0.009628179              0.886          0.6381045                        0.987              1.482649
# DR          -0.002539970 0.009200259              0.953          0.9875051                        0.982              1.239156



### -------------------------------------------------------------------------
### 2-3) ATT_Scenario_1 Performance record ---------------------------------------
ATT_Monte_carlo<-as.data.frame(Monte_Carlo_var$ATT_var)
ATT_Monte_outcome_reg<-var(ATT_Monte_carlo$Outcome_reg_est) # 0.009638511
ATT_Monte_IPW<-var(ATT_Monte_carlo$IPW_est) # 0.009645044
ATT_Monte_DR<-var(ATT_Monte_carlo$DR_est) # 0.009642894


#ATT_1_result_2_0.2<-read.csv("./Simulation Scenario code/ATT_Scenario_1_p_0.12_result.csv")

ATT_1_performance_2_0.2<-as.data.frame(matrix(NA,nrow=3,ncol=6))
colnames(ATT_1_performance_2_0.2)<-c("Bias","rMSE","Naive_var_coverage","Naive_var_SD_Ratio","Sandwich_robust_var_coverage",
                                     "Sandwich_var_SD_Ratio")

row.names(ATT_1_performance_2_0.2)<-c("Outcome_reg","IPW","DR")

func_rMSE<-function(x){
  return(mean((x-true_ATT)^2))
}



### ---------------------------
ATT_1_performance_2_0.2[,"Bias"]<-apply(ATT_1_result_2_0.2[,grep("_est",colnames(ATT_1_result_2_0.2))],2,mean)-true_ATT

ATT_1_performance_2_0.2[,"rMSE"]<-apply(ATT_1_result_2_0.2[,grep("_est",colnames(ATT_1_result_2_0.2))],2,FUN=func_rMSE)

ATT_1_performance_2_0.2[,"Naive_var_coverage"]<-c(mean((ATT_1_result_2_0.2$Outcome_reg_coverage)^2), mean(ATT_1_result_2_0.2$Naive_IPW_coverage),
                                                  mean(ATT_1_result_2_0.2$DR_Naive_coverage))

ATT_1_performance_2_0.2[,"Sandwich_robust_var_coverage"]<-c(NA, mean(ATT_1_result_2_0.2$Sandwich_IPW_coverage),
                                                            mean(ATT_1_result_2_0.2$Sandwich_DR_coverage))

ATT_1_performance_2_0.2[,"Naive_var_SD_Ratio"]<-c(mean((ATT_1_result_2_0.2$Outcome_reg_var)^2)/ATT_Monte_outcome_reg,
                                                  mean((ATT_1_result_2_0.2$IPW_Naive_var)^2)/ATT_Monte_IPW,
                                                  mean(ATT_1_result_2_0.2$DR_Naive_var)/ATT_Monte_DR)

ATT_1_performance_2_0.2[,"Sandwich_var_SD_Ratio"]<-c(NA, mean(ATT_1_result_2_0.2$IPW_Sandwich_var)/ATT_Monte_IPW,
                                                     mean(ATT_1_result_2_0.2$DR_Sandwich_var)/ATT_Monte_DR)

print(ATT_1_performance_2_0.2)
#                     Bias        rMSE Naive_var_coverage Naive_var_SD_Ratio Sandwich_robust_var_coverage Sandwich_var_SD_Ratio
# Outcome_reg -0.001817516 0.008770811              0.955         0.99270988                           NA                    NA
# IPW         -0.001722056 0.008771304              0.905         0.68148855                        0.995              2.016187
# DR          -0.001581589 0.008754451              0.385         0.07198237                        1.000              3.357375






## -------------------------------------------------------------------------
## -------------------------------------------------------------------------
## -------------------------------------------------------------------------

### 2-2) delta0 = 0 -----------------------------------------------------------
library(boot)

source("./Simulation Scenario code/Simulation_Hardcoding_function_ver1.R")
source("./Simulation Scenario code/Scenario1_make_result_ver1.2.R")

## 2-2-1) deltaB = 0.2  ----------------------------------------
N <- 1000
REPL <- 1000
A_RATIO<-0 # p=0.12
A_VAR1<-0.2
A_VAR2<-0.2

Scenario1_result<-Scenario1_result(A_RATIO, A_VAR1, A_VAR2, N, REPL)

### -------------------------------------------------------------------------
### Store ATE result table -----------------------------------------------
ATE_1_result_0_0.2<-as.data.frame(Scenario1_result$ATE)


### -------------------------------------------------------------------------
### Store ATT result table-----------------------------------------------
ATT_1_result_0_0.2<-as.data.frame(Scenario1_result$ATT)


source("./Simulation Scenario code/Simulation_Monte_Carlo_SD_Ratio_ver1.2.R")


### 2-1) ATE_Scenario_1 Performance record ---------------------------------------
A_RATIO<-0
Monte_Carlo_var<-Monte_result(A_RATIO, A_VAR1, A_VAR2, 1000, 100000)

ATE_Monte_carlo<-as.data.frame(Monte_Carlo_var$ATE_var)
ATE_Monte_outcome_reg<-var(ATE_Monte_carlo$Outcome_reg_est) # 0.009638511
ATE_Monte_IPW<-var(ATE_Monte_carlo$IPW_est) # 0.009824517
ATE_Monte_DR<-var(ATE_Monte_carlo$DR_est) # 0.009742594

#ATE_1_result<-read.csv("./Simulation Scenario code/ATE_Scenario_1_p_0.12_result.csv")

ATE_1_performance_0_0.2<-as.data.frame(matrix(NA,nrow=3,ncol=6))
colnames(ATE_1_performance_0_0.2)<-c("Bias","rMSE","Naive_var_coverage","Naive_var_SD_Ratio", "Sandwich_robust_var_coverage",
                                     "Sandwich_var_SD_Ratio")

row.names(ATE_1_performance_0_0.2)<-c("Outcome_reg","IPW","DR")

func_rMSE<-function(x){
  return(mean((x-true_ATE)^2))
}


### ---------------------------
true_ATE<-log(2)
true_ATT<-log(2)

ATE_1_performance_0_0.2[,"Bias"]<-apply(ATE_1_result_0_0.2[,grep("_est",colnames(ATE_1_result_0_0.2))],2,mean)-true_ATE

ATE_1_performance_0_0.2[,"rMSE"]<-apply(ATE_1_result_0_0.2[,grep("_est",colnames(ATE_1_result_0_0.2))],2,FUN=func_rMSE)

ATE_1_performance_0_0.2[,"Naive_var_coverage"]<-c(mean(ATE_1_result_0_0.2$Outcome_reg_coverage), 
                                                  mean(ATE_1_result_0_0.2$Naive_IPW_coverage),
                                                  mean(ATE_1_result_0_0.2$DR_Naive_coverage))

ATE_1_performance_0_0.2[,"Sandwich_robust_var_coverage"]<-c(NA, mean(ATE_1_result_0_0.2$Sandwich_IPW_coverage),
                                                            mean(ATE_1_result_0_0.2$Sandwich_DR_coverage))

ATE_1_performance_0_0.2[,"Naive_var_SD_Ratio"]<-c(mean((ATE_1_result_0_0.2$Outcome_reg_var)^2)/ATE_Monte_outcome_reg,
                                                  mean((ATE_1_result_0_0.2$IPW_Naive_var)^2)/ATE_Monte_IPW,
                                                  mean(ATE_1_result_0_0.2$DR_Naive_var)/ATE_Monte_DR)

ATE_1_performance_0_0.2[,"Sandwich_var_SD_Ratio"]<-c(NA, mean(ATE_1_result_0_0.2$IPW_Sandwich_var)/ATE_Monte_IPW,
                                                     mean(ATE_1_result_0_0.2$DR_Sandwich_var)/ATE_Monte_DR)

print(ATE_1_performance_0_0.2)
#                     Bias        rMSE Naive_var_coverage Naive_var_SD_Ratio Sandwich_robust_var_coverage Sandwich_var_SD_Ratio
# Outcome_reg -0.002385700 0.003875722              0.955          1.0013542                           NA                    NA
# IPW         -0.002457571 0.003881849              0.989          1.4949810                        0.991              1.534238
# DR          -0.002527949 0.003873439              0.955          0.9976173                        0.968              1.053159



### -------------------------------------------------------------------------
### 2-3) ATT_Scenario_1 Performance record ---------------------------------------
ATT_Monte_carlo<-as.data.frame(Monte_Carlo_var$ATT_var)
ATT_Monte_outcome_reg<-var(ATT_Monte_carlo$Outcome_reg_est) # 0.009638511
ATT_Monte_IPW<-var(ATT_Monte_carlo$IPW_est) # 0.009645044
ATT_Monte_DR<-var(ATT_Monte_carlo$DR_est) # 0.009642894


#ATT_1_result_0_0.2<-read.csv("./Simulation Scenario code/ATT_Scenario_1_p_0.12_result.csv")

ATT_1_performance_0_0.2<-as.data.frame(matrix(NA,nrow=3,ncol=6))
colnames(ATT_1_performance_0_0.2)<-c("Bias","rMSE","Naive_var_coverage","Naive_var_SD_Ratio","Sandwich_robust_var_coverage",
                                     "Sandwich_var_SD_Ratio")

row.names(ATT_1_performance_0_0.2)<-c("Outcome_reg","IPW","DR")

func_rMSE<-function(x){
  return(mean((x-true_ATT)^2))
}



### ---------------------------
ATT_1_performance_0_0.2[,"Bias"]<-apply(ATT_1_result_0_0.2[,grep("_est",colnames(ATT_1_result_0_0.2))],2,mean)-true_ATT

ATT_1_performance_0_0.2[,"rMSE"]<-apply(ATT_1_result_0_0.2[,grep("_est",colnames(ATT_1_result_0_0.2))],2,FUN=func_rMSE)

ATT_1_performance_0_0.2[,"Naive_var_coverage"]<-c(mean((ATT_1_result_0_0.2$Outcome_reg_coverage)^2), mean(ATT_1_result_0_0.2$Naive_IPW_coverage),
                                                  mean(ATT_1_result_0_0.2$DR_Naive_coverage))

ATT_1_performance_0_0.2[,"Sandwich_robust_var_coverage"]<-c(NA, mean(ATT_1_result_0_0.2$Sandwich_IPW_coverage),
                                                            mean(ATT_1_result_0_0.2$Sandwich_DR_coverage))

ATT_1_performance_0_0.2[,"Naive_var_SD_Ratio"]<-c(mean((ATT_1_result_0_0.2$Outcome_reg_var)^2)/ATT_Monte_outcome_reg,
                                                  mean((ATT_1_result_0_0.2$IPW_Naive_var)^2)/ATT_Monte_IPW,
                                                  mean(ATT_1_result_0_0.2$DR_Naive_var)/ATT_Monte_DR)

ATT_1_performance_0_0.2[,"Sandwich_var_SD_Ratio"]<-c(NA, mean(ATT_1_result_0_0.2$IPW_Sandwich_var)/ATT_Monte_IPW,
                                                     mean(ATT_1_result_0_0.2$DR_Sandwich_var)/ATT_Monte_DR)

print(ATT_1_performance_0_0.2)
#                     Bias        rMSE Naive_var_coverage Naive_var_SD_Ratio Sandwich_robust_var_coverage Sandwich_var_SD_Ratio
# Outcome_reg -0.002385700 0.003875722              0.955          1.0013542                           NA                    NA
# IPW         -0.003028923 0.004037370              0.987          1.4418739                        0.996              2.008142
# DR          -0.002589178 0.003943840              0.689          0.2570541                        0.971              1.145508







## -------------------------------------------------------------------------
## -------------------------------------------------------------------------
## -------------------------------------------------------------------------
library(boot)

source("./Simulation Scenario code/Simulation_Hardcoding_function_ver1.R")
source("./Simulation Scenario code/Scenario1_make_result_ver1.2.R")

## 2-2-2) deltaB = 0.01  ----------------------------------------
N <- 1000
REPL <- 1000
A_RATIO<-0 # p=0.12
A_VAR1<-0.01
A_VAR2<-0.01

Scenario1_result<-Scenario1_result(A_RATIO, A_VAR1, A_VAR2, N, REPL)

### -------------------------------------------------------------------------
### Store ATE result table -----------------------------------------------
ATE_1_result_0_0.01<-as.data.frame(Scenario1_result$ATE)


### -------------------------------------------------------------------------
### Store ATT result table-----------------------------------------------
ATT_1_result_0_0.01<-as.data.frame(Scenario1_result$ATT)


source("./Simulation Scenario code/Simulation_Monte_Carlo_SD_Ratio_ver1.2.R")


### 2-1) ATE_Scenario_1 Performance record ---------------------------------------
A_RATIO<-0
Monte_Carlo_var<-Monte_result(A_RATIO, A_VAR1, A_VAR2, 1000, 100000)

ATE_Monte_carlo<-as.data.frame(Monte_Carlo_var$ATE_var)
ATE_Monte_outcome_reg<-var(ATE_Monte_carlo$Outcome_reg_est) # 0.009638511
ATE_Monte_IPW<-var(ATE_Monte_carlo$IPW_est) # 0.009824517
ATE_Monte_DR<-var(ATE_Monte_carlo$DR_est) # 0.009742594

#ATE_1_result<-read.csv("./Simulation Scenario code/ATE_Scenario_1_p_0.12_result.csv")

ATE_1_performance_0_0.01<-as.data.frame(matrix(NA,nrow=3,ncol=6))
colnames(ATE_1_performance_0_0.01)<-c("Bias","rMSE","Naive_var_coverage","Naive_var_SD_Ratio", "Sandwich_robust_var_coverage",
                                      "Sandwich_var_SD_Ratio")

row.names(ATE_1_performance_0_0.01)<-c("Outcome_reg","IPW","DR")

func_rMSE<-function(x){
  return(mean((x-true_ATE)^2))
}


### ---------------------------
true_ATE<-log(2)
true_ATT<-log(2)

ATE_1_performance_0_0.01[,"Bias"]<-apply(ATE_1_result_0_0.01[,grep("_est",colnames(ATE_1_result_0_0.01))],2,mean)-true_ATE

ATE_1_performance_0_0.01[,"rMSE"]<-apply(ATE_1_result_0_0.01[,grep("_est",colnames(ATE_1_result_0_0.01))],2,FUN=func_rMSE)

ATE_1_performance_0_0.01[,"Naive_var_coverage"]<-c(mean(ATE_1_result_0_0.01$Outcome_reg_coverage), 
                                                   mean(ATE_1_result_0_0.01$Naive_IPW_coverage),
                                                   mean(ATE_1_result_0_0.01$DR_Naive_coverage))

ATE_1_performance_0_0.01[,"Sandwich_robust_var_coverage"]<-c(NA, mean(ATE_1_result_0_0.01$Sandwich_IPW_coverage),
                                                             mean(ATE_1_result_0_0.01$Sandwich_DR_coverage))

ATE_1_performance_0_0.01[,"Naive_var_SD_Ratio"]<-c(mean((ATE_1_result_0_0.01$Outcome_reg_var)^2)/ATE_Monte_outcome_reg,
                                                   mean((ATE_1_result_0_0.01$IPW_Naive_var)^2)/ATE_Monte_IPW,
                                                   mean(ATE_1_result_0_0.01$DR_Naive_var)/ATE_Monte_DR)

ATE_1_performance_0_0.01[,"Sandwich_var_SD_Ratio"]<-c(NA, mean(ATE_1_result_0_0.01$IPW_Sandwich_var)/ATE_Monte_IPW,
                                                      mean(ATE_1_result_0_0.01$DR_Sandwich_var)/ATE_Monte_DR)

print(ATE_1_performance_0_0.01)
#                     Bias        rMSE Naive_var_coverage Naive_var_SD_Ratio Sandwich_robust_var_coverage Sandwich_var_SD_Ratio
# Outcome_reg -0.002241823 0.003760756              0.965          1.0014697                           NA                    NA
# IPW         -0.002248391 0.003761677              0.986          1.5195091                        0.986              1.522118
# DR          -0.002244212 0.003761608              0.965          0.9974757                        0.972              1.051887



### -------------------------------------------------------------------------
### 2-3) ATT_Scenario_1 Performance record ---------------------------------------
ATT_Monte_carlo<-as.data.frame(Monte_Carlo_var$ATT_var)
ATT_Monte_outcome_reg<-var(ATT_Monte_carlo$Outcome_reg_est) # 0.009638511
ATT_Monte_IPW<-var(ATT_Monte_carlo$IPW_est) # 0.009645044
ATT_Monte_DR<-var(ATT_Monte_carlo$DR_est) # 0.009642894


#ATT_1_result_0_0.01<-read.csv("./Simulation Scenario code/ATT_Scenario_1_p_0.12_result.csv")

ATT_1_performance_0_0.01<-as.data.frame(matrix(NA,nrow=3,ncol=6))
colnames(ATT_1_performance_0_0.01)<-c("Bias","rMSE","Naive_var_coverage","Naive_var_SD_Ratio","Sandwich_robust_var_coverage",
                                      "Sandwich_var_SD_Ratio")

row.names(ATT_1_performance_0_0.01)<-c("Outcome_reg","IPW","DR")

func_rMSE<-function(x){
  return(mean((x-true_ATT)^2))
}



### ---------------------------
ATT_1_performance_0_0.01[,"Bias"]<-apply(ATT_1_result_0_0.01[,grep("_est",colnames(ATT_1_result_0_0.01))],2,mean)-true_ATT

ATT_1_performance_0_0.01[,"rMSE"]<-apply(ATT_1_result_0_0.01[,grep("_est",colnames(ATT_1_result_0_0.01))],2,FUN=func_rMSE)

ATT_1_performance_0_0.01[,"Naive_var_coverage"]<-c(mean((ATT_1_result_0_0.01$Outcome_reg_coverage)^2), mean(ATT_1_result_0_0.01$Naive_IPW_coverage),
                                                   mean(ATT_1_result_0_0.01$DR_Naive_coverage))

ATT_1_performance_0_0.01[,"Sandwich_robust_var_coverage"]<-c(NA, mean(ATT_1_result_0_0.01$Sandwich_IPW_coverage),
                                                             mean(ATT_1_result_0_0.01$Sandwich_DR_coverage))

ATT_1_performance_0_0.01[,"Naive_var_SD_Ratio"]<-c(mean((ATT_1_result_0_0.01$Outcome_reg_var)^2)/ATT_Monte_outcome_reg,
                                                   mean((ATT_1_result_0_0.01$IPW_Naive_var)^2)/ATT_Monte_IPW,
                                                   mean(ATT_1_result_0_0.01$DR_Naive_var)/ATT_Monte_DR)

ATT_1_performance_0_0.01[,"Sandwich_var_SD_Ratio"]<-c(NA, mean(ATT_1_result_0_0.01$IPW_Sandwich_var)/ATT_Monte_IPW,
                                                      mean(ATT_1_result_0_0.01$DR_Sandwich_var)/ATT_Monte_DR)

print(ATT_1_performance_0_0.01)
#                    Bias        rMSE Naive_var_coverage Naive_var_SD_Ratio Sandwich_robust_var_coverage Sandwich_var_SD_Ratio
# Outcome_reg -0.002241823 0.003760756              0.965          1.0014697                           NA                    NA
# IPW         -0.002328212 0.003802594              0.988          1.5137374                        0.997              2.037757
# DR          -0.002253352 0.003789663              0.678          0.2550663                        0.975              1.174636

















### -------------------------------------------------------------------------
### -------------------------------------------------------------------------
### -------------------------------------------------------------------------
library(boot)

source("./Simulation Scenario code/Simulation_Hardcoding_function_ver1.R")
source("./Simulation Scenario code/Scenario1_make_result_ver1.2.R")


## 2-2-1) deltaB = 0.6  ----------------------------------------
N <- 1000
REPL <- 1000
A_RATIO<-0 # p=0.12
A_VAR1<-0.6
A_VAR2<-0.6

Scenario1_result<-Scenario1_result(A_RATIO, A_VAR1, A_VAR2, N, REPL)

### -------------------------------------------------------------------------
### Store ATE result table -----------------------------------------------
ATE_1_result_0_0.6<-as.data.frame(Scenario1_result$ATE)


### -------------------------------------------------------------------------
### Store ATT result table-----------------------------------------------
ATT_1_result_0_0.6<-as.data.frame(Scenario1_result$ATT)


source("./Simulation Scenario code/Simulation_Monte_Carlo_SD_Ratio_ver1.2.R")


### 2-1) ATE_Scenario_1 Performance record ---------------------------------------
A_RATIO<-0
Monte_Carlo_var<-Monte_result(A_RATIO, A_VAR1, A_VAR2, 1000, 100000)

ATE_Monte_carlo<-as.data.frame(Monte_Carlo_var$ATE_var)
ATE_Monte_outcome_reg<-var(ATE_Monte_carlo$Outcome_reg_est) # 0.009638511
ATE_Monte_IPW<-var(ATE_Monte_carlo$IPW_est) # 0.009824517
ATE_Monte_DR<-var(ATE_Monte_carlo$DR_est) # 0.009742594

#ATE_1_result<-read.csv("./Simulation Scenario code/ATE_Scenario_1_p_0.12_result.csv")

ATE_1_performance_0_0.6<-as.data.frame(matrix(NA,nrow=3,ncol=6))
colnames(ATE_1_performance_0_0.6)<-c("Bias","rMSE","Naive_var_coverage","Naive_var_SD_Ratio", "Sandwich_robust_var_coverage",
                                     "Sandwich_var_SD_Ratio")

row.names(ATE_1_performance_0_0.6)<-c("Outcome_reg","IPW","DR")

func_rMSE<-function(x){
  return(mean((x-true_ATE)^2))
}


### ---------------------------
true_ATE<-log(2)
true_ATT<-log(2)

ATE_1_performance_0_0.6[,"Bias"]<-apply(ATE_1_result_0_0.6[,grep("_est",colnames(ATE_1_result_0_0.6))],2,mean)-true_ATE

ATE_1_performance_0_0.6[,"rMSE"]<-apply(ATE_1_result_0_0.6[,grep("_est",colnames(ATE_1_result_0_0.6))],2,FUN=func_rMSE)

ATE_1_performance_0_0.6[,"Naive_var_coverage"]<-c(mean(ATE_1_result_0_0.6$Outcome_reg_coverage), 
                                                  mean(ATE_1_result_0_0.6$Naive_IPW_coverage),
                                                  mean(ATE_1_result_0_0.6$DR_Naive_coverage))

ATE_1_performance_0_0.6[,"Sandwich_robust_var_coverage"]<-c(NA, mean(ATE_1_result_0_0.6$Sandwich_IPW_coverage),
                                                            mean(ATE_1_result_0_0.6$Sandwich_DR_coverage))

ATE_1_performance_0_0.6[,"Naive_var_SD_Ratio"]<-c(mean((ATE_1_result_0_0.6$Outcome_reg_var)^2)/ATE_Monte_outcome_reg,
                                                  mean((ATE_1_result_0_0.6$IPW_Naive_var)^2)/ATE_Monte_IPW,
                                                  mean(ATE_1_result_0_0.6$DR_Naive_var)/ATE_Monte_DR)

ATE_1_performance_0_0.6[,"Sandwich_var_SD_Ratio"]<-c(NA, mean(ATE_1_result_0_0.6$IPW_Sandwich_var)/ATE_Monte_IPW,
                                                     mean(ATE_1_result_0_0.6$DR_Sandwich_var)/ATE_Monte_DR)

print(ATE_1_performance_0_0.6)
#                    Bias        rMSE Naive_var_coverage Naive_var_SD_Ratio Sandwich_robust_var_coverage Sandwich_var_SD_Ratio
# Outcome_reg -0.002941931 0.004282521              0.962          0.9971873                           NA                    NA
# IPW         -0.003451570 0.004709817              0.974          1.2522184                        0.983              1.577980
# DR          -0.003431347 0.004399258              0.957          0.9953517                        0.961              1.054439



### -------------------------------------------------------------------------
### 2-3) ATT_Scenario_1 Performance record ---------------------------------------
ATT_Monte_carlo<-as.data.frame(Monte_Carlo_var$ATT_var)
ATT_Monte_outcome_reg<-var(ATT_Monte_carlo$Outcome_reg_est) # 0.009638511
ATT_Monte_IPW<-var(ATT_Monte_carlo$IPW_est) # 0.009645044
ATT_Monte_DR<-var(ATT_Monte_carlo$DR_est) # 0.009642894


#ATT_1_result_0_0.6<-read.csv("./Simulation Scenario code/ATT_Scenario_1_p_0.12_result.csv")

ATT_1_performance_0_0.6<-as.data.frame(matrix(NA,nrow=3,ncol=6))
colnames(ATT_1_performance_0_0.6)<-c("Bias","rMSE","Naive_var_coverage","Naive_var_SD_Ratio","Sandwich_robust_var_coverage",
                                     "Sandwich_var_SD_Ratio")

row.names(ATT_1_performance_0_0.6)<-c("Outcome_reg","IPW","DR")

func_rMSE<-function(x){
  return(mean((x-true_ATT)^2))
}



### ---------------------------
ATT_1_performance_0_0.6[,"Bias"]<-apply(ATT_1_result_0_0.6[,grep("_est",colnames(ATT_1_result_0_0.6))],2,mean)-true_ATT

ATT_1_performance_0_0.6[,"rMSE"]<-apply(ATT_1_result_0_0.6[,grep("_est",colnames(ATT_1_result_0_0.6))],2,FUN=func_rMSE)

ATT_1_performance_0_0.6[,"Naive_var_coverage"]<-c(mean((ATT_1_result_0_0.6$Outcome_reg_coverage)^2), mean(ATT_1_result_0_0.6$Naive_IPW_coverage),
                                                  mean(ATT_1_result_0_0.6$DR_Naive_coverage))

ATT_1_performance_0_0.6[,"Sandwich_robust_var_coverage"]<-c(NA, mean(ATT_1_result_0_0.6$Sandwich_IPW_coverage),
                                                            mean(ATT_1_result_0_0.6$Sandwich_DR_coverage))

ATT_1_performance_0_0.6[,"Naive_var_SD_Ratio"]<-c(mean((ATT_1_result_0_0.6$Outcome_reg_var)^2)/ATT_Monte_outcome_reg,
                                                  mean((ATT_1_result_0_0.6$IPW_Naive_var)^2)/ATT_Monte_IPW,
                                                  mean(ATT_1_result_0_0.6$DR_Naive_var)/ATT_Monte_DR)

ATT_1_performance_0_0.6[,"Sandwich_var_SD_Ratio"]<-c(NA, mean(ATT_1_result_0_0.6$IPW_Sandwich_var)/ATT_Monte_IPW,
                                                     mean(ATT_1_result_0_0.6$DR_Sandwich_var)/ATT_Monte_DR)

print(ATT_1_performance_0_0.6)

#                     Bias        rMSE Naive_var_coverage Naive_var_SD_Ratio Sandwich_robust_var_coverage Sandwich_var_SD_Ratio
# Outcome_reg -0.002941931 0.004282521              0.962          0.9971873                           NA                    NA
# IPW         -0.005151964 0.006159374              0.936          0.9573452                        0.991              1.773160
# DR          -0.004043887 0.004997942              0.656          0.2137094                        0.976              1.169281







## -------------------------------------------------------------------------
## -------------------------------------------------------------------------
## -------------------------------------------------------------------------

### 2-3) delta0 = 1 -----------------------------------------------------------
library(boot)

source("./Simulation Scenario code/Simulation_Hardcoding_function_ver1.R")
source("./Simulation Scenario code/Scenario1_make_result_ver1.2.R")

## 2-2-1) deltaB = 0.2  ----------------------------------------
N <- 1000
REPL <- 1000
A_RATIO<-1 # p=0.12
A_VAR1<-0.2
A_VAR2<-0.2

Scenario1_result<-Scenario1_result(A_RATIO, A_VAR1, A_VAR2, N, REPL)

### -------------------------------------------------------------------------
### Store ATE result table -----------------------------------------------
ATE_1_result_1_0.2<-as.data.frame(Scenario1_result$ATE)


### -------------------------------------------------------------------------
### Store ATT result table-----------------------------------------------
ATT_1_result_1_0.2<-as.data.frame(Scenario1_result$ATT)


source("./Simulation Scenario code/Simulation_Monte_Carlo_SD_Ratio_ver1.2.R")


### 2-1) ATE_Scenario_1 Performance record ---------------------------------------
A_RATIO<-1
Monte_Carlo_var<-Monte_result(A_RATIO, A_VAR1, A_VAR2, 1000, 100000)

ATE_Monte_carlo<-as.data.frame(Monte_Carlo_var$ATE_var)
ATE_Monte_outcome_reg<-var(ATE_Monte_carlo$Outcome_reg_est) # 0.009638511
ATE_Monte_IPW<-var(ATE_Monte_carlo$IPW_est) # 0.009824517
ATE_Monte_DR<-var(ATE_Monte_carlo$DR_est) # 0.009742594

#ATE_1_result<-read.csv("./Simulation Scenario code/ATE_Scenario_1_p_0.12_result.csv")

ATE_1_performance_1_0.2<-as.data.frame(matrix(NA,nrow=3,ncol=6))
colnames(ATE_1_performance_1_0.2)<-c("Bias","rMSE","Naive_var_coverage","Naive_var_SD_Ratio", "Sandwich_robust_var_coverage",
                                     "Sandwich_var_SD_Ratio")

row.names(ATE_1_performance_1_0.2)<-c("Outcome_reg","IPW","DR")

func_rMSE<-function(x){
  return(mean((x-true_ATE)^2))
}


### ---------------------------
true_ATE<-log(2)
true_ATT<-log(2)

ATE_1_performance_1_0.2[,"Bias"]<-apply(ATE_1_result_1_0.2[,grep("_est",colnames(ATE_1_result_1_0.2))],2,mean)-true_ATE

ATE_1_performance_1_0.2[,"rMSE"]<-apply(ATE_1_result_1_0.2[,grep("_est",colnames(ATE_1_result_1_0.2))],2,FUN=func_rMSE)

ATE_1_performance_1_0.2[,"Naive_var_coverage"]<-c(mean(ATE_1_result_1_0.2$Outcome_reg_coverage), 
                                                  mean(ATE_1_result_1_0.2$Naive_IPW_coverage),
                                                  mean(ATE_1_result_1_0.2$DR_Naive_coverage))

ATE_1_performance_1_0.2[,"Sandwich_robust_var_coverage"]<-c(NA, mean(ATE_1_result_1_0.2$Sandwich_IPW_coverage),
                                                            mean(ATE_1_result_1_0.2$Sandwich_DR_coverage))

ATE_1_performance_1_0.2[,"Naive_var_SD_Ratio"]<-c(mean((ATE_1_result_1_0.2$Outcome_reg_var)^2)/ATE_Monte_outcome_reg,
                                                  mean((ATE_1_result_1_0.2$IPW_Naive_var)^2)/ATE_Monte_IPW,
                                                  mean(ATE_1_result_1_0.2$DR_Naive_var)/ATE_Monte_DR)

ATE_1_performance_1_0.2[,"Sandwich_var_SD_Ratio"]<-c(NA, mean(ATE_1_result_1_0.2$IPW_Sandwich_var)/ATE_Monte_IPW,
                                                     mean(ATE_1_result_1_0.2$DR_Sandwich_var)/ATE_Monte_DR)

print(ATE_1_performance_1_0.2)
#                    Bias        rMSE Naive_var_coverage Naive_var_SD_Ratio Sandwich_robust_var_coverage Sandwich_var_SD_Ratio
# Outcome_reg 0.002063519 0.005596075              0.941          0.9986546                           NA                    NA
# IPW         0.001374973 0.005771056              0.951          1.0969043                        0.974              1.512286
# DR          0.001334201 0.005653815              0.942          0.9928079                        0.944              1.006816




### -------------------------------------------------------------------------
### 2-3) ATT_Scenario_1 Performance record ---------------------------------------
ATT_Monte_carlo<-as.data.frame(Monte_Carlo_var$ATT_var)
ATT_Monte_outcome_reg<-var(ATT_Monte_carlo$Outcome_reg_est) # 0.009638511
ATT_Monte_IPW<-var(ATT_Monte_carlo$IPW_est) # 0.009645044
ATT_Monte_DR<-var(ATT_Monte_carlo$DR_est) # 0.009642894


#ATT_1_result_1_0.2<-read.csv("./Simulation Scenario code/ATT_Scenario_1_p_0.12_result.csv")

ATT_1_performance_1_0.2<-as.data.frame(matrix(NA,nrow=3,ncol=6))
colnames(ATT_1_performance_1_0.2)<-c("Bias","rMSE","Naive_var_coverage","Naive_var_SD_Ratio","Sandwich_robust_var_coverage",
                                     "Sandwich_var_SD_Ratio")

row.names(ATT_1_performance_1_0.2)<-c("Outcome_reg","IPW","DR")

func_rMSE<-function(x){
  return(mean((x-true_ATT)^2))
}



### ---------------------------
ATT_1_performance_1_0.2[,"Bias"]<-apply(ATT_1_result_1_0.2[,grep("_est",colnames(ATT_1_result_1_0.2))],2,mean)-true_ATT

ATT_1_performance_1_0.2[,"rMSE"]<-apply(ATT_1_result_1_0.2[,grep("_est",colnames(ATT_1_result_1_0.2))],2,FUN=func_rMSE)

ATT_1_performance_1_0.2[,"Naive_var_coverage"]<-c(mean((ATT_1_result_1_0.2$Outcome_reg_coverage)^2), mean(ATT_1_result_1_0.2$Naive_IPW_coverage),
                                                  mean(ATT_1_result_1_0.2$DR_Naive_coverage))

ATT_1_performance_1_0.2[,"Sandwich_robust_var_coverage"]<-c(NA, mean(ATT_1_result_1_0.2$Sandwich_IPW_coverage),
                                                            mean(ATT_1_result_1_0.2$Sandwich_DR_coverage))

ATT_1_performance_1_0.2[,"Naive_var_SD_Ratio"]<-c(mean((ATT_1_result_1_0.2$Outcome_reg_var)^2)/ATT_Monte_outcome_reg,
                                                  mean((ATT_1_result_1_0.2$IPW_Naive_var)^2)/ATT_Monte_IPW,
                                                  mean(ATT_1_result_1_0.2$DR_Naive_var)/ATT_Monte_DR)

ATT_1_performance_1_0.2[,"Sandwich_var_SD_Ratio"]<-c(NA, mean(ATT_1_result_1_0.2$IPW_Sandwich_var)/ATT_Monte_IPW,
                                                     mean(ATT_1_result_1_0.2$DR_Sandwich_var)/ATT_Monte_DR)

print(ATT_1_performance_1_0.2)
#                     Bias        rMSE Naive_var_coverage Naive_var_SD_Ratio Sandwich_robust_var_coverage Sandwich_var_SD_Ratio
# Outcome_reg 0.0020635187 0.005596075              0.941          0.9986546                           NA                    NA
# IPW         0.0011014683 0.005981215              0.946          1.0540567                        0.991              1.978902
# DR          0.0009793262 0.005744209              0.653          0.2331804                        0.954              1.098095







## -------------------------------------------------------------------------
## -------------------------------------------------------------------------
## -------------------------------------------------------------------------
library(boot)

source("./Simulation Scenario code/Simulation_Hardcoding_function_ver1.R")
source("./Simulation Scenario code/Scenario1_make_result_ver1.2.R")

## 2-2-2) deltaB = 0.01  ----------------------------------------
N <- 1000
REPL <- 1000
A_RATIO<-1 # p=0.12
A_VAR1<-0.01
A_VAR2<-0.01

Scenario1_result<-Scenario1_result(A_RATIO, A_VAR1, A_VAR2, N, REPL)

### -------------------------------------------------------------------------
### Store ATE result table -----------------------------------------------
ATE_1_result_1_0.01<-as.data.frame(Scenario1_result$ATE)


### -------------------------------------------------------------------------
### Store ATT result table-----------------------------------------------
ATT_1_result_1_0.01<-as.data.frame(Scenario1_result$ATT)


source("./Simulation Scenario code/Simulation_Monte_Carlo_SD_Ratio_ver1.2.R")


### 2-1) ATE_Scenario_1 Performance record ---------------------------------------
A_RATIO<-1
Monte_Carlo_var<-Monte_result(A_RATIO, A_VAR1, A_VAR2, 1000, 100000)

ATE_Monte_carlo<-as.data.frame(Monte_Carlo_var$ATE_var)
ATE_Monte_outcome_reg<-var(ATE_Monte_carlo$Outcome_reg_est) # 0.009638511
ATE_Monte_IPW<-var(ATE_Monte_carlo$IPW_est) # 0.009824517
ATE_Monte_DR<-var(ATE_Monte_carlo$DR_est) # 0.009742594

#ATE_1_result<-read.csv("./Simulation Scenario code/ATE_Scenario_1_p_0.12_result.csv")

ATE_1_performance_1_0.01<-as.data.frame(matrix(NA,nrow=3,ncol=6))
colnames(ATE_1_performance_1_0.01)<-c("Bias","rMSE","Naive_var_coverage","Naive_var_SD_Ratio", "Sandwich_robust_var_coverage",
                                      "Sandwich_var_SD_Ratio")

row.names(ATE_1_performance_1_0.01)<-c("Outcome_reg","IPW","DR")

func_rMSE<-function(x){
  return(mean((x-true_ATE)^2))
}


### ---------------------------
true_ATE<-log(2)
true_ATT<-log(2)

ATE_1_performance_1_0.01[,"Bias"]<-apply(ATE_1_result_1_0.01[,grep("_est",colnames(ATE_1_result_1_0.01))],2,mean)-true_ATE

ATE_1_performance_1_0.01[,"rMSE"]<-apply(ATE_1_result_1_0.01[,grep("_est",colnames(ATE_1_result_1_0.01))],2,FUN=func_rMSE)

ATE_1_performance_1_0.01[,"Naive_var_coverage"]<-c(mean(ATE_1_result_1_0.01$Outcome_reg_coverage), 
                                                   mean(ATE_1_result_1_0.01$Naive_IPW_coverage),
                                                   mean(ATE_1_result_1_0.01$DR_Naive_coverage))

ATE_1_performance_1_0.01[,"Sandwich_robust_var_coverage"]<-c(NA, mean(ATE_1_result_1_0.01$Sandwich_IPW_coverage),
                                                             mean(ATE_1_result_1_0.01$Sandwich_DR_coverage))

ATE_1_performance_1_0.01[,"Naive_var_SD_Ratio"]<-c(mean((ATE_1_result_1_0.01$Outcome_reg_var)^2)/ATE_Monte_outcome_reg,
                                                   mean((ATE_1_result_1_0.01$IPW_Naive_var)^2)/ATE_Monte_IPW,
                                                   mean(ATE_1_result_1_0.01$DR_Naive_var)/ATE_Monte_DR)

ATE_1_performance_1_0.01[,"Sandwich_var_SD_Ratio"]<-c(NA, mean(ATE_1_result_1_0.01$IPW_Sandwich_var)/ATE_Monte_IPW,
                                                      mean(ATE_1_result_1_0.01$DR_Sandwich_var)/ATE_Monte_DR)

print(ATE_1_performance_1_0.01)




### -------------------------------------------------------------------------
### 2-3) ATT_Scenario_1 Performance record ---------------------------------------
ATT_Monte_carlo<-as.data.frame(Monte_Carlo_var$ATT_var)
ATT_Monte_outcome_reg<-var(ATT_Monte_carlo$Outcome_reg_est) # 0.009638511
ATT_Monte_IPW<-var(ATT_Monte_carlo$IPW_est) # 0.009645044
ATT_Monte_DR<-var(ATT_Monte_carlo$DR_est) # 0.009642894


#ATT_1_result_1_0.01<-read.csv("./Simulation Scenario code/ATT_Scenario_1_p_0.12_result.csv")

ATT_1_performance_1_0.01<-as.data.frame(matrix(NA,nrow=3,ncol=6))
colnames(ATT_1_performance_1_0.01)<-c("Bias","rMSE","Naive_var_coverage","Naive_var_SD_Ratio","Sandwich_robust_var_coverage",
                                      "Sandwich_var_SD_Ratio")

row.names(ATT_1_performance_1_0.01)<-c("Outcome_reg","IPW","DR")

func_rMSE<-function(x){
  return(mean((x-true_ATT)^2))
}



### ---------------------------
ATT_1_performance_1_0.01[,"Bias"]<-apply(ATT_1_result_1_0.01[,grep("_est",colnames(ATT_1_result_1_0.01))],2,mean)-true_ATT

ATT_1_performance_1_0.01[,"rMSE"]<-apply(ATT_1_result_1_0.01[,grep("_est",colnames(ATT_1_result_1_0.01))],2,FUN=func_rMSE)

ATT_1_performance_1_0.01[,"Naive_var_coverage"]<-c(mean((ATT_1_result_1_0.01$Outcome_reg_coverage)^2), mean(ATT_1_result_1_0.01$Naive_IPW_coverage),
                                                   mean(ATT_1_result_1_0.01$DR_Naive_coverage))

ATT_1_performance_1_0.01[,"Sandwich_robust_var_coverage"]<-c(NA, mean(ATT_1_result_1_0.01$Sandwich_IPW_coverage),
                                                             mean(ATT_1_result_1_0.01$Sandwich_DR_coverage))

ATT_1_performance_1_0.01[,"Naive_var_SD_Ratio"]<-c(mean((ATT_1_result_1_0.01$Outcome_reg_var)^2)/ATT_Monte_outcome_reg,
                                                   mean((ATT_1_result_1_0.01$IPW_Naive_var)^2)/ATT_Monte_IPW,
                                                   mean(ATT_1_result_1_0.01$DR_Naive_var)/ATT_Monte_DR)

ATT_1_performance_1_0.01[,"Sandwich_var_SD_Ratio"]<-c(NA, mean(ATT_1_result_1_0.01$IPW_Sandwich_var)/ATT_Monte_IPW,
                                                      mean(ATT_1_result_1_0.01$DR_Sandwich_var)/ATT_Monte_DR)

print(ATT_1_performance_1_0.01)




















### -------------------------------------------------------------------------
### -------------------------------------------------------------------------
### -------------------------------------------------------------------------
## 2-2-1) deltaB = 0.6  ----------------------------------------
library(boot)

source("./Simulation Scenario code/Simulation_Hardcoding_function_ver1.R")
source("./Simulation Scenario code/Scenario1_make_result_ver1.2.R")

N <- 1000
REPL <- 1000
A_RATIO<-1 # p=0.12
A_VAR1<-0.6
A_VAR2<-0.6

Scenario1_result<-Scenario1_result(A_RATIO, A_VAR1, A_VAR2, N, REPL)

### -------------------------------------------------------------------------
### Store ATE result table -----------------------------------------------
ATE_1_result_1_0.6<-as.data.frame(Scenario1_result$ATE)


### -------------------------------------------------------------------------
### Store ATT result table-----------------------------------------------
ATT_1_result_1_0.6<-as.data.frame(Scenario1_result$ATT)


source("./Simulation Scenario code/Simulation_Monte_Carlo_SD_Ratio_ver1.2.R")


### 2-1) ATE_Scenario_1 Performance record ---------------------------------------
A_RATIO<-1
Monte_Carlo_var<-Monte_result(A_RATIO, A_VAR1, A_VAR2, 1000, 100000)

ATE_Monte_carlo<-as.data.frame(Monte_Carlo_var$ATE_var)
ATE_Monte_outcome_reg<-var(ATE_Monte_carlo$Outcome_reg_est) # 0.009638511
ATE_Monte_IPW<-var(ATE_Monte_carlo$IPW_est) # 0.009824517
ATE_Monte_DR<-var(ATE_Monte_carlo$DR_est) # 0.009742594

#ATE_1_result<-read.csv("./Simulation Scenario code/ATE_Scenario_1_p_0.12_result.csv")

ATE_1_performance_1_0.6<-as.data.frame(matrix(NA,nrow=3,ncol=6))
colnames(ATE_1_performance_1_0.6)<-c("Bias","rMSE","Naive_var_coverage","Naive_var_SD_Ratio", "Sandwich_robust_var_coverage",
                                     "Sandwich_var_SD_Ratio")

row.names(ATE_1_performance_1_0.6)<-c("Outcome_reg","IPW","DR")

func_rMSE<-function(x){
  return(mean((x-true_ATE)^2))
}


### ---------------------------
true_ATE<-log(2)
true_ATT<-log(2)

ATE_1_performance_1_0.6[,"Bias"]<-apply(ATE_1_result_1_0.6[,grep("_est",colnames(ATE_1_result_1_0.6))],2,mean)-true_ATE

ATE_1_performance_1_0.6[,"rMSE"]<-apply(ATE_1_result_1_0.6[,grep("_est",colnames(ATE_1_result_1_0.6))],2,FUN=func_rMSE)

ATE_1_performance_1_0.6[,"Naive_var_coverage"]<-c(mean(ATE_1_result_1_0.6$Outcome_reg_coverage), 
                                                  mean(ATE_1_result_1_0.6$Naive_IPW_coverage),
                                                  mean(ATE_1_result_1_0.6$DR_Naive_coverage))

ATE_1_performance_1_0.6[,"Sandwich_robust_var_coverage"]<-c(NA, mean(ATE_1_result_1_0.6$Sandwich_IPW_coverage),
                                                            mean(ATE_1_result_1_0.6$Sandwich_DR_coverage))

ATE_1_performance_1_0.6[,"Naive_var_SD_Ratio"]<-c(mean((ATE_1_result_1_0.6$Outcome_reg_var)^2)/ATE_Monte_outcome_reg,
                                                  mean((ATE_1_result_1_0.6$IPW_Naive_var)^2)/ATE_Monte_IPW,
                                                  mean(ATE_1_result_1_0.6$DR_Naive_var)/ATE_Monte_DR)

ATE_1_performance_1_0.6[,"Sandwich_var_SD_Ratio"]<-c(NA, mean(ATE_1_result_1_0.6$IPW_Sandwich_var)/ATE_Monte_IPW,
                                                     mean(ATE_1_result_1_0.6$DR_Sandwich_var)/ATE_Monte_DR)

print(ATE_1_performance_1_0.6)




### -------------------------------------------------------------------------
### 2-3) ATT_Scenario_1 Performance record ---------------------------------------
ATT_Monte_carlo<-as.data.frame(Monte_Carlo_var$ATT_var)
ATT_Monte_outcome_reg<-var(ATT_Monte_carlo$Outcome_reg_est) # 0.009638511
ATT_Monte_IPW<-var(ATT_Monte_carlo$IPW_est) # 0.009645044
ATT_Monte_DR<-var(ATT_Monte_carlo$DR_est) # 0.009642894


#ATT_1_result_1_0.6<-read.csv("./Simulation Scenario code/ATT_Scenario_1_p_0.12_result.csv")

ATT_1_performance_1_0.6<-as.data.frame(matrix(NA,nrow=3,ncol=6))
colnames(ATT_1_performance_1_0.6)<-c("Bias","rMSE","Naive_var_coverage","Naive_var_SD_Ratio","Sandwich_robust_var_coverage",
                                     "Sandwich_var_SD_Ratio")

row.names(ATT_1_performance_1_0.6)<-c("Outcome_reg","IPW","DR")

func_rMSE<-function(x){
  return(mean((x-true_ATT)^2))
}



### ---------------------------
ATT_1_performance_1_0.6[,"Bias"]<-apply(ATT_1_result_1_0.6[,grep("_est",colnames(ATT_1_result_1_0.6))],2,mean)-true_ATT

ATT_1_performance_1_0.6[,"rMSE"]<-apply(ATT_1_result_1_0.6[,grep("_est",colnames(ATT_1_result_1_0.6))],2,FUN=func_rMSE)

ATT_1_performance_1_0.6[,"Naive_var_coverage"]<-c(mean((ATT_1_result_1_0.6$Outcome_reg_coverage)^2), mean(ATT_1_result_1_0.6$Naive_IPW_coverage),
                                                  mean(ATT_1_result_1_0.6$DR_Naive_coverage))

ATT_1_performance_1_0.6[,"Sandwich_robust_var_coverage"]<-c(NA, mean(ATT_1_result_1_0.6$Sandwich_IPW_coverage),
                                                            mean(ATT_1_result_1_0.6$Sandwich_DR_coverage))

ATT_1_performance_1_0.6[,"Naive_var_SD_Ratio"]<-c(mean((ATT_1_result_1_0.6$Outcome_reg_var)^2)/ATT_Monte_outcome_reg,
                                                  mean((ATT_1_result_1_0.6$IPW_Naive_var)^2)/ATT_Monte_IPW,
                                                  mean(ATT_1_result_1_0.6$DR_Naive_var)/ATT_Monte_DR)

ATT_1_performance_1_0.6[,"Sandwich_var_SD_Ratio"]<-c(NA, mean(ATT_1_result_1_0.6$IPW_Sandwich_var)/ATT_Monte_IPW,
                                                     mean(ATT_1_result_1_0.6$DR_Sandwich_var)/ATT_Monte_DR)

print(ATT_1_performance_1_0.6)
