### Simulation Scenario result Debugging version 1 ------------------------------------

### <What TO DO> ----------------------------------------------------------------
## Check the SD Ratio and Coverage probability of DR ATT Sandwich robust variance
##  -- make Exposure ratio bigger
### -----------------------------------------------------------------------------



### 1. import library & R source file ------------------------------------------------------
library(boot)
source("./Simulation Scenario code/Simulation_Hardcoding_function.R")


### 2. make dataset ---------------------------------------------------
make_data<-function(A_RATIO, N, REPL){
  
  data<-list()
  EFFECT<-c(log(1.5),log(2),log(2))
  
  B1<-matrix(NA, nrow=N, ncol=REPL)
  C1<-matrix(NA, nrow=N, ncol=REPL)
  A1<-matrix(NA, nrow=N, ncol=REPL)
  Y1<-matrix(NA, nrow=N, ncol=REPL)
  
  for(repl in 1:REPL){
    delta0<-A_RATIO
    delta_b<-0.01
    delta_c<-0.01
    
    set.seed(123*repl)
    sample<-replicate(N,expr={
      B<-rbinom(1,1,0.5)
      C<-rnorm(1,0,1)
      p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
      A<-rbinom(1,1,prob=p_z)
      c(B,C,A)
    })
    
    B1[,repl]<-sample[1,]
    C1[,repl]<-sample[2,]
    A1[,repl]<-sample[3,]
    
    X_sample<-cbind(B1[,repl],C1[,repl],A1[,repl])
    
    # Y random sampling - continuous
    Y1[,repl]<-as.matrix(X_sample)%*%EFFECT+rnorm(nrow(X_sample),0,1)
    
  }
  
  data$B<-B1
  data$C<-C1
  data$A<-A1
  data$Y<-Y1
  
  return(data)
}


### 2-1) Check the Exposure ratio ---------------------------------------------------
data_1<-make_data(1, 10, 10)
hist(apply(data_1$A,2,mean)) # Mean : 0.5



### 3. Get the Simulation result --------------------------------------------
source("./Simulation Scenario code/Scenario1_make_result_ver1.R")

N <- 1000
REPL <- 1000
A_RATIO<-0 # Mean : 0.5


Scenario1_result<-Scenario1_result(A_RATIO, N, REPL)


### -------------------------------------------------------------------------
### Store ATE result table -----------------------------------------------
ATE_1_result<-as.data.frame(Scenario1_result$ATE)
#write.csv(ATE_1_result,file="./Simulation Scenario code/ATE_Scenario_1_p_0.12_result.csv",row.names=FALSE)


### -------------------------------------------------------------------------
### Store ATT result table-----------------------------------------------
ATT_1_result<-as.data.frame(Scenario1_result$ATT)



source("./Simulation Scenario code/Simulation_Monte_Carlo_SD_Ratio_ver1.R")


### 1) ATE_Scenario_1 Performance record ---------------------------------------
A_RATIO<-1
Monte_Carlo_var<-Monte_result(A_RATIO, 1000, 100000)

ATE_Monte_carlo<-as.data.frame(Monte_Carlo_var$ATE_var)
ATE_Monte_outcome_reg<-var(ATE_Monte_carlo$Outcome_reg_est) # 0.005118191
ATE_Monte_IPW<-var(ATE_Monte_carlo$IPW_est) # 0.005134841
ATE_Monte_DR<-var(ATE_Monte_carlo$DR_est) # 0.005129583

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
#                     Bias        rMSE Naive_var_coverage Naive_var_SD_Ratio Sandwich_robust_var_coverage Sandwich_var_SD_Ratio
# Outcome_reg 0.0004502813 0.005469130              0.935         13.9684419                           NA                    NA
# IPW         0.0002465322 0.005456610              0.947         15.1795019                        0.976              1.519467
# DR          0.0003177392 0.005460426              0.936          0.9955787                        0.939              1.010190






### -------------------------------------------------------------------------
### 2) ATT_Scenario_1 Performance record ---------------------------------------
ATT_Monte_carlo<-as.data.frame(Monte_Carlo_var$ATT_var)
ATT_Monte_outcome_reg<-var(ATT_Monte_carlo$Outcome_reg_est) 
ATT_Monte_IPW<-var(ATT_Monte_carlo$IPW_est) 
ATT_Monte_DR<-var(ATT_Monte_carlo$DR_est) 


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
#                     Bias        rMSE Naive_var_coverage Naive_var_SD_Ratio Sandwich_robust_var_coverage Sandwich_var_SD_Ratio
# Outcome_reg 0.0004502813 0.005469130              0.935         13.9684419                           NA                    NA
# IPW         0.0001459426 0.005467401              0.948         15.0969679                        0.991              2.037184
# DR          0.0002460947 0.005464088              0.667          0.2475306                        1.000              4.735006