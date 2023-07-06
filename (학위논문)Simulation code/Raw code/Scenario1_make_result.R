### -------------------------------------------------------------------------
### Code of getting result table in Scenario 1 ------------------------------



### -------------------------------------------------------------------------
### Scenario 1) PS model & Outcome model all correctly specified ------------
Scenario1_result<-function(A_RATIO, N, REPL){
  
  result<-list()
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
  
 
  ### replication start -------------------------------------------------------
  z<-qnorm(0.975)
  
  
  ### Make matrix storing result -----------------------------------
  ATE_1_result<-matrix(NA,nrow=REPL,ncol=13)
  colnames(ATE_1_result)<-c("Outcome_reg_est","Outcome_reg_var","Outcome_reg_coverage","IPW_est",
                            "IPW_Naive_var","Naive_IPW_coverage","IPW_Sandwich_var","Sandwich_IPW_coverage",
                            "DR_est","DR_Naive_var","DR_Naive_coverage","DR_Sandwich_var","Sandwich_DR_coverage")
  
  ATT_1_result<-matrix(NA,nrow=REPL,ncol=13)
  colnames(ATT_1_result)<-c("Outcome_reg_est","Outcome_reg_var","Outcome_reg_coverage","IPW_est",
                            "IPW_Naive_var","Naive_IPW_coverage","IPW_Sandwich_var","Sandwich_IPW_coverage",
                            "DR_est","DR_Naive_var","DR_Naive_coverage","DR_Sandwich_var","Sandwich_DR_coverage")
  
  
  true_ATE<-log(2)
  true_ATT<-log(2)
  
  ### Scenario 1 - ATE) --------------------------------------------------------
  for(repl in 1:REPL){
    B<-B1[,repl]
    C<-C1[,repl]
    A<-A1[,repl]
    Y<-Y1[,repl]
    
    data<-as.data.frame(cbind(B,C,A,Y))
    
    cov<-c("B","C")
    cov_type<-c("continuous","continuous","binary")
    var_treat<-"A"
    var_y<-"Y"
    
    ### 1) outcome regression ---------------------------------------------------
    outcome_reg<-lm(Y~., data=data)
    ATE_reg_est<-coef(outcome_reg)[var_treat]
    
    re<-summary(outcome_reg)
    re1<-as.data.frame(re$coefficients)
    outcome_reg_var_ATE<-re1$`Std. Error`[which(rownames(re1)==var_treat)]
    outcome_reg_lo<-ATE_reg_est-z*sqrt(outcome_reg_var_ATE)
    outcome_reg_up<-ATE_reg_est+z*sqrt(outcome_reg_var_ATE)
    coverage_outcome_reg<-ifelse((outcome_reg_lo<=true_ATE & outcome_reg_up>=true_ATE),1,0)
    
    ATE_1_result[repl,c(1:3)]<-c(ATE_reg_est,outcome_reg_var_ATE,coverage_outcome_reg)
    
    
    ### 2) IPW ATE ---------------------------------------------------
    weight_IPW<-weight_make(var_treat,cov,"ATE",data)
    result_obj<-lm(Y~A, weight=weight_IPW$untrimmed)
    IPW_ATE<-coef(result_obj)[var_treat]
    
    re<-summary(result_obj)
    re1<-as.data.frame(re$coefficients)
    Naive_IPW_var_ATE<-re1$`Std. Error`[which(rownames(re1)==var_treat)]
    IPW_Naive_lo<-IPW_ATE-z*sqrt(Naive_IPW_var_ATE)
    IPW_Naive_up<-IPW_ATE+z*sqrt(Naive_IPW_var_ATE)
    coverage_IPW_Naive<-ifelse((IPW_Naive_lo<=true_ATE & IPW_Naive_up>=true_ATE),1,0)
    
    sandwich_IPW_var_ATE<-sandwich_var_ATE(result_obj,data)[2,2]
    IPW_sandwich_lo<-IPW_ATE-z*sqrt(sandwich_IPW_var_ATE)
    IPW_sandwich_up<-IPW_ATE+z*sqrt(sandwich_IPW_var_ATE)
    coverage_IPW_sandwich<-ifelse((IPW_sandwich_lo<=true_ATE & IPW_sandwich_up>=true_ATE),1,0)
    
    ATE_1_result[repl,c(4:8)]<-c(IPW_ATE,Naive_IPW_var_ATE,coverage_IPW_Naive,sandwich_IPW_var_ATE,coverage_IPW_sandwich)
    
    
    ### 3) DR ATE ---------------------------------------------------
    DR_ATE<-DR_estimator("ATE",data,var_treat,var_y,cov)$est
    
    DR_Naive_var_ATE<-DR_Naive_var_estimator("ATE",data,var_treat,var_y,cov)
    DR_Naive_lo<-DR_ATE-z*sqrt(DR_Naive_var_ATE)
    DR_Naive_up<-DR_ATE+z*sqrt(DR_Naive_var_ATE)
    coverage_DR_Naive<-ifelse((DR_Naive_lo<=true_ATE) & (DR_Naive_up>=true_ATE),1,0)
    
    DR_sandwich_var_ATE<-sandwich_var_DR("ATE",data,var_treat,var_y,cov)
    DR_sandwich_lo<-DR_ATE-z*sqrt(DR_sandwich_var_ATE)
    DR_sandwich_up<-DR_ATE+z*sqrt(DR_sandwich_var_ATE)
    coverage_DR_sandwich<-ifelse((DR_sandwich_lo<=true_ATE & DR_sandwich_up>=true_ATE),1,0)
    
    
    ATE_1_result[repl,c(9:ncol(ATE_1_result))]<-c(DR_ATE,DR_Naive_var_ATE,coverage_DR_Naive,DR_sandwich_var_ATE,coverage_DR_sandwich)
    
  }
  
  result$ATE<-ATE_1_result
  
  
  ### -------------------------------------------------------------------------
  ### Scenario 1 - ATT) --------------------------------------------------------
  for(repl in 1:REPL){
    B<-B1[,repl]
    C<-C1[,repl]
    A<-A1[,repl]
    Y<-Y1[,repl]
    
    data<-as.data.frame(cbind(B,C,A,Y))
    cov<-c("B","C")
    cov_type<-c("continuous","continuous","binary")
    var_treat<-"A"
    var_y<-"Y"
    
    
    ### 1) outcome regression ---------------------------------------------------
    outcome_reg<-lm(Y~., data=data)
    
    N1<-sum(data$A==1)
    X_0<-as.matrix(cbind(1,data[,cov],0))
    ATT_reg_est<-sum(data$A*data$Y)/N1 - sum(data$A*(coef(outcome_reg)%*%t(X_0)))/N1
    
    a<-data$A*data$Y-data$A*(coef(outcome_reg)%*%t(X_0))
    outcome_reg_var_ATT<-sum((a-ATT_reg_est)^2)/N1
    
    outcome_reg_lo<-ATT_reg_est-z*sqrt(outcome_reg_var_ATT)
    outcome_reg_up<-ATT_reg_est+z*sqrt(outcome_reg_var_ATT)
    coverage_outcome_reg<-ifelse((outcome_reg_lo<=true_ATT) & (outcome_reg_up>=true_ATT),1,0)
    
    ATT_1_result[repl,c(1:3)]<-c(ATT_reg_est,outcome_reg_var_ATT,coverage_outcome_reg)
    
    
    ### 2) IPW ATT ---------------------------------------------------
    weight_IPW<-weight_make(var_treat,cov,"ATT",data)
    result_obj<-lm(Y~A, weight=weight_IPW$untrimmed)
    IPW_ATT<-coef(result_obj)[var_treat]
    
    re<-summary(result_obj)
    re1<-as.data.frame(re$coefficients)
    Naive_IPW_var_ATT<-re1$`Std. Error`[which(rownames(re1)==var_treat)]
    IPW_Naive_lo<-IPW_ATT-z*sqrt(Naive_IPW_var_ATT)
    IPW_Naive_up<-IPW_ATT+z*sqrt(Naive_IPW_var_ATT)
    coverage_IPW_Naive<-ifelse((IPW_Naive_lo<=true_ATT) & (IPW_Naive_up>=true_ATT),1,0)
    
    sandwich_IPW_var_ATT<-sandwich_var_ATT(weight_IPW,result_obj,data,cov,var_treat,var_y)
    IPW_sandwich_lo<-IPW_ATT-z*sqrt(sandwich_IPW_var_ATT)
    IPW_sandwich_up<-IPW_ATT+z*sqrt(sandwich_IPW_var_ATT)
    coverage_IPW_sandwich <-ifelse((IPW_sandwich_lo<=true_ATT) & (IPW_sandwich_up>=true_ATT),1,0)
    
    ATT_1_result[repl,c(4:8)]<-c(IPW_ATT,Naive_IPW_var_ATT,coverage_IPW_Naive,sandwich_IPW_var_ATT,coverage_IPW_sandwich)
    
    
    ### 3) DR ATT ---------------------------------------------------
    DR_ATT<-DR_estimator("ATT",data,var_treat,var_y,cov)$est
    
    DR_Naive_var_ATT<-DR_Naive_var_estimator("ATT",data,var_treat,var_y,cov)
    DR_Naive_lo<-DR_ATT-z*sqrt(DR_Naive_var_ATT)
    DR_Naive_up<-DR_ATT+z*sqrt(DR_Naive_var_ATT)
    coverage_DR_Naive<-ifelse((DR_Naive_lo<=true_ATT) & (DR_Naive_up>=true_ATT),1,0)
    
    DR_sandwich_var_ATT<-sandwich_var_DR("ATT",data,var_treat,var_y,cov)
    DR_sandwich_lo<-DR_ATT-z*sqrt(DR_sandwich_var_ATT)
    DR_sandwich_up<-DR_ATT+z*sqrt(DR_sandwich_var_ATT)
    coverage_DR_sandwich<-ifelse((DR_sandwich_lo<=true_ATT) & (DR_sandwich_up>=true_ATT),1,0)
    
    ATT_1_result[repl,c(9:ncol(ATT_1_result))]<-c(DR_ATT,DR_Naive_var_ATT,coverage_DR_Naive,DR_sandwich_var_ATT,coverage_DR_sandwich)
    
  }
  
  result$ATT<-ATT_1_result
  
  return(result)
}
  