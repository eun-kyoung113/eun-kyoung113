### Scenario1_p_0.12_Performance ver1.1--------------------------------------------

## What to Do -----------------------------------------------------------------
## 1. result table import 

## 2. Performance calculate
##    --> Bias, rMSE, Coverage probability, SD ratio
##        -- SD ratio 분모 : Monte carlo Approximation of variance estimator

## 3. Make Result table
##    --> table name rule : "estimate name"_"scenario number"_"result or performance"
## -------------------------------------------------------------------------


source("Simulation_Monte_Carlo_SD_Ratio_ver1.1.R")


### 1) ATE_Scenario_1 Performance record ---------------------------------------
A_RATIO<--2
Monte_Carlo_var<-Monte_result(A_RATIO, 1000000)

"Outcome_reg_var","IPW_Naive_var","IPW_Sandwich_var",
"DR_Naive_var","DR_Sandwich_var"

ATE_Monte_carlo<-as.data.frame(Monte_Carlo_var$ATE_var)
ATE_Monte_outcome_reg<-var(ATE_Monte_carlo$Outcome_reg_est)
ATE_Monte_IPW_Naive<-var(ATE_Monte_carlo$IPW_Naive_var) 
ATE_Monte_IPW_Sandwich<-var(ATE_Monte_carlo$IPW_Sandwich_var)
ATE_Monte_DR_Naive<-var(ATE_Monte_carlo$DR_est) 

ATE_1_result<-read.csv("ATE_Scenario_1_p_0.12_result.csv")

ATE_1_performance<-as.data.frame(matrix(NA,nrow=3,ncol=6))
colnames(ATE_1_performance)<-c("Bias","rMSE","Naive_var_coverage","Naive_var_SD_Ratio", "Sandwich_robust_var_coverage",
                               "Sandwich_var_SD_Ratio")

row.names(ATE_1_performance)<-c("Outcome_reg","IPW","DR")

func_rMSE<-function(x){
  return(mean((x-true_ATE)^2))
}

### ---------------------------
true_ATE<-log(2)
true_ATT<-log(2)

ATE_1_performance[,"Bias"]<-apply(ATE_1_result[,grep("_est",colnames(ATE_1_result))],2,mean)-true_ATE

ATE_1_performance[,"rMSE"]<-apply(ATE_1_result[,grep("_est",colnames(ATE_1_result))],2,FUN=func_rMSE)

ATE_1_performance[,"Naive_var_coverage"]<-c(mean(ATE_1_result$Outcome_reg_coverage), 
                                            mean(ATE_1_result$Naive_IPW_coverage),
                                            mean(ATE_1_result$DR_Naive_coverage))

ATE_1_performance[,"Sandwich_robust_var_coverage"]<-c(NA, mean(ATE_1_result$Sandwich_IPW_coverage),
                                                      mean(ATE_1_result$Sandwich_DR_coverage))

ATE_1_performance[,"Naive_var_SD_Ratio"]<-c(mean(ATE_1_result$Outcome_reg_var)/ATE_Monte_outcome_reg,
                                            mean(ATE_1_result$IPW_Naive_var)/ATE_Monte_IPW,
                                            mean(ATE_1_result$DR_Naive_var)/ATE_Monte_DR)

ATE_1_performance[,"Sandwich_var_SD_Ratio"]<-c(NA, mean(ATE_1_result$IPW_Sandwich_var)/ATE_Monte_IPW,
                                               mean(ATE_1_result$DR_Sandwich_var)/ATE_Monte_DR)

print(ATE_1_performance)
#                     Bias      rMSE Naive_var_coverage Naive_var_SD_Ratio Sandwich_robust_var_coverage Sandwich_var_SD_Ratio
# Outcome_reg 0.007380725 0.1009436              0.999           3510.852                           NA                    NA
# IPW         0.017084124 0.1205385              0.992           2650.411                        0.949              1670.359
# DR          0.014738284 0.1109874              0.912           1132.655                        0.997              3398.719


### -------------------------------------------------------------------------
### 2) ATT_Scenario_1 Performance record ---------------------------------------
ATT_Monte_carlo<-as.data.frame(Monte_Carlo_var$ATT_var)
ATT_Monte_outcome_reg<-var(ATT_Monte_carlo$Outcome_reg_est) # 9.061485e-05
ATT_Monte_IPW<-var(ATT_Monte_carlo$IPW_est) # 9.063059e-05
ATT_Monte_DR<-var(ATT_Monte_carlo$DR_est) # 9.061574e-05


ATT_1_result<-read.csv("ATT_Scenario_1_p_0.12_result.csv")

ATT_1_performance<-as.data.frame(matrix(NA,nrow=3,ncol=6))
colnames(ATT_1_performance)<-c("Bias","rMSE","Naive_var_coverage","Naive_var_SD_Ratio","Sandwich_robust_var_coverage",
                               "Sandwich_var_SD_Ratio")

row.names(ATT_1_performance)<-c("Outcome_reg","IPW","DR")

func_rMSE<-function(x){
  return(mean((x-true_ATT)^2))
}


### ---------------------------
ATT_1_performance[,"Bias"]<-apply(ATT_1_result[,grep("_est",colnames(ATT_1_result))],2,mean)-true_ATT

ATT_1_performance[,"rMSE"]<-apply(ATT_1_result[,grep("_est",colnames(ATT_1_result))],2,FUN=func_rMSE)

ATT_1_performance[,"Naive_var_coverage"]<-c(mean(ATT_1_result$Outcome_reg_coverage), mean(ATT_1_result$Naive_IPW_coverage),
                                            mean(ATT_1_result$DR_Naive_coverage))

ATT_1_performance[,"Sandwich_robust_var_coverage"]<-c(NA, mean(ATT_1_result$Sandwich_IPW_coverage),
                                                      mean(ATT_1_result$Sandwich_DR_coverage))

ATT_1_performance[,"Naive_var_SD_Ratio"]<-c(mean(ATT_1_result$Outcome_reg_var)/ATT_Monte_outcome_reg,
                                            mean(ATT_1_result$IPW_Naive_var)/ATT_Monte_IPW,
                                            mean(ATT_1_result$DR_Naive_var)/ATT_Monte_DR)

ATT_1_performance[,"Sandwich_var_SD_Ratio"]<-c(NA, mean(ATT_1_result$IPW_Sandwich_var)/ATT_Monte_IPW,
                                               mean(ATT_1_result$DR_Sandwich_var)/ATT_Monte_DR)

print(ATT_1_performance)
#                   Bias      rMSE Naive_var_coverage Naive_var_SD_Ratio Sandwich_robust_var_coverage Sandwich_var_SD_Ratio
# Outcome_reg 0.007380725 0.1009436              1.000        63120.09950                           NA                    NA
# IPW         0.006682156 0.1023299              0.996         2645.90608                           NA              2000.008
# DR          0.007229062 0.1011666              0.413           77.20002                            1             48019.587