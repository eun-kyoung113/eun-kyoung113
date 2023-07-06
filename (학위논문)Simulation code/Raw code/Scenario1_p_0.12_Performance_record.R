### Scenario1_p_0.12_Performance --------------------------------------------

## What to Do -----------------------------------------------------------------
## 1. result table import 

## 2. Performance calculate
##    --> Bias, rMSE, Coverage probability

## 3. Make Result table
##    --> table name rule : "estimate name"_"scenario number"_"result or performance"
## -------------------------------------------------------------------------





### 1) ATE_Scenario_1 Performance record ---------------------------------------
ATE_1_result<-read.csv("ATE_Scenario_1_p_0.12_result.csv")

ATE_1_performance<-as.data.frame(matrix(NA,nrow=3,ncol=4))
colnames(ATE_1_performance)<-c("Bias","rMSE","Naive_var_coverage","Sandwich_robust_var_coverage")
row.names(ATE_1_performance)<-c("Outcome_reg","IPW","DR")

func_rMSE<-function(x){
  return(mean((x-true_ATE)^2))
}

### ---------------------------
true_ATE<-log(2)
true_ATT<-log(2)

ATE_1_performance[,"Bias"]<-apply(ATE_1_result[,grep("_est",colnames(ATE_1_result))],2,mean)-true_ATE

ATE_1_performance[,"rMSE"]<-apply(ATE_1_result[,grep("_est",colnames(ATE_1_result))],2,FUN=func_rMSE)

ATE_1_performance[,"Naive_var_coverage"]<-c(mean(ATE_1_result$Outcome_reg_coverage), mean(ATE_1_result$Naive_IPW_coverage),
                                            mean(ATE_1_result$DR_Naive_coverage))

ATE_1_performance[,"Sandwich_robust_var_coverage"]<-c(NA, mean(ATE_1_result$Sandwich_IPW_coverage),
                                                      mean(ATE_1_result$Sandwich_DR_coverage))


print(ATE_1_performance)



### -------------------------------------------------------------------------
### 2) ATT_Scenario_1 Performance record ---------------------------------------
ATT_1_result<-read.csv("ATT_Scenario_1_p_0.12_result.csv")

ATT_1_performance<-as.data.frame(matrix(NA,nrow=3,ncol=4))
colnames(ATT_1_performance)<-c("Bias","rMSE","Naive_var_coverage","Sandwich_robust_var_coverage")
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

print(ATT_1_performance)
