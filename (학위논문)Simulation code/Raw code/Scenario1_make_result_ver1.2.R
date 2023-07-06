### -------------------------------------------------------------------------
### Code of getting result table in Scenario 1 ------------------------------
### Version 1.2 -------------------------------------------------------------------------
### Change the way to get the estimator of DR ATT Sandwich robust variance


### -------------------------------------------------------------------------
### Scenario 1) PS model & Outcome model all correctly specified ------------
Scenario1_result<-function(A_RATIO, A_VAR1, A_VAR2, N, REPL){
  
  result<-list()
  EFFECT<-c(log(1.5),log(2),log(2))
  
  B1<-matrix(NA, nrow=N, ncol=REPL)
  C1<-matrix(NA, nrow=N, ncol=REPL)
  A1<-matrix(NA, nrow=N, ncol=REPL)
  Y1<-matrix(NA, nrow=N, ncol=REPL)
  
  for(repl in 1:REPL){
    delta0<-A_RATIO
    delta_b<-A_VAR1
    delta_c<-A_VAR2
    
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
  
  
  ### -------------------------------------------------------------------------
  ### Scenario 1 - ATE & ATT --------------------------------------------------------
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
    
    ### -------------------------------------------------------------------------
    ### 1) outcome regression_ATE ---------------------------------------------------
    outcome_reg_ATE<-lm(Y~., data=data)
    
    X_1<-as.matrix(cbind(1,data[,cov],1))
    X_0<-as.matrix(cbind(1,data[,cov],0))
    
    #ATE_reg_est<-sum(data$A*(data$Y-coef(outcome_reg_ATE)%*%t(X_0)) + (1-data$A)*(coef(outcome_reg_ATE)%*%t(X_1)-data$Y))/nrow(data)
    ATE_reg_est<-coef(outcome_reg_ATE)[var_treat]
    
    re_ATE<-summary(outcome_reg_ATE)
    re1_ATE<-as.data.frame(re_ATE$coefficients)
    outcome_reg_var_ATE<-re1_ATE$`Std. Error`[which(rownames(re1_ATE)==var_treat)]
    outcome_reg_ATE_lo<-ATE_reg_est-z*(outcome_reg_var_ATE)
    outcome_reg_ATE_up<-ATE_reg_est+z*(outcome_reg_var_ATE)
    coverage_outcome_reg_ATE<-ifelse((outcome_reg_ATE_lo<=true_ATE & outcome_reg_ATE_up>=true_ATE),1,0)
    
    ATE_1_result[repl,c(1:3)]<-c(ATE_reg_est,outcome_reg_var_ATE,coverage_outcome_reg_ATE)
    
    
    ### -------------------------------------------------------------------------
    ### 1) outcome regression ATT ---------------------------------------------------
    outcome_reg_ATT<-lm(Y~., data=data)
    
    N1<-sum(data$A==1)
    
    #ATT_reg_est<-sum(data$A*data$Y - (data$A*(coef(outcome_reg_ATT)%*%t(X_0))))/N1
    ATT_reg_est<-coef(outcome_reg_ATT)[var_treat] 
    
    re_ATT<-summary(outcome_reg_ATT)
    re1_ATT<-as.data.frame(re_ATT$coefficients)
    outcome_reg_var_ATT<-re1_ATT$`Std. Error`[which(rownames(re1_ATT)==var_treat)]
    
    outcome_reg_ATT_lo<-ATT_reg_est-z*(outcome_reg_var_ATT)
    outcome_reg_ATT_up<-ATT_reg_est+z*(outcome_reg_var_ATT)
    coverage_outcome_reg_ATT<-ifelse((outcome_reg_ATT_lo<=true_ATT) & (outcome_reg_ATT_up>=true_ATT),1,0)
    
    ATT_1_result[repl,c(1:3)]<-c(ATT_reg_est,outcome_reg_var_ATT,coverage_outcome_reg_ATT)
    
    
    
    ### -------------------------------------------------------------------------
    ### 2) IPW ATE ---------------------------------------------------
    weight_IPW_ATE<-weight_make(var_treat,cov,"ATE",data)
    result_obj_ATE<-lm(Y~A, weight=weight_IPW_ATE$untrimmed)
    IPW_ATE<-coef(result_obj_ATE)[var_treat]
    
    re_ATE<-summary(result_obj_ATE)
    re1_ATE<-as.data.frame(re_ATE$coefficients)
    
    Naive_IPW_var_ATE<-re1_ATE$`Std. Error`[which(rownames(re1_ATE)==var_treat)]
    IPW_Naive_lo_ATE<-IPW_ATE-z*(Naive_IPW_var_ATE)
    IPW_Naive_up_ATE<-IPW_ATE+z*(Naive_IPW_var_ATE)
    coverage_IPW_Naive_ATE<-ifelse((IPW_Naive_lo_ATE<=true_ATE & IPW_Naive_up_ATE>=true_ATE),1,0)
    
    sandwich_IPW_var_ATE<-sandwich_var_ATE(result_obj_ATE,data)[2,2]
    IPW_sandwich_lo_ATE<-IPW_ATE-z*sqrt(sandwich_IPW_var_ATE)
    IPW_sandwich_up_ATE<-IPW_ATE+z*sqrt(sandwich_IPW_var_ATE)
    coverage_IPW_sandwich_ATE<-ifelse((IPW_sandwich_lo_ATE<=true_ATE & IPW_sandwich_up_ATE>=true_ATE),1,0)
    
    ATE_1_result[repl,c(4:8)]<-c(IPW_ATE,Naive_IPW_var_ATE,coverage_IPW_Naive_ATE,sandwich_IPW_var_ATE,coverage_IPW_sandwich_ATE)
    
    
    ### -------------------------------------------------------------------------
    ### 2) IPW ATT ---------------------------------------------------
    weight_IPW_ATT<-weight_make(var_treat,cov,"ATT",data)
    result_obj_ATT<-lm(Y~A, weight=weight_IPW_ATT$untrimmed)
    IPW_ATT<-coef(result_obj_ATT)[var_treat]
    
    re_ATT<-summary(result_obj_ATT)
    re1_ATT<-as.data.frame(re_ATT$coefficients)
    
    Naive_IPW_var_ATT<-re1_ATT$`Std. Error`[which(rownames(re1_ATT)==var_treat)]
    IPW_Naive_lo_ATT<-IPW_ATT-z*(Naive_IPW_var_ATT)
    IPW_Naive_up_ATT<-IPW_ATT+z*(Naive_IPW_var_ATT)
    coverage_IPW_Naive_ATT<-ifelse((IPW_Naive_lo_ATT<=true_ATT) & (IPW_Naive_up_ATT>=true_ATT),1,0)
    
    sandwich_IPW_var_ATT<-sandwich_var_ATT(weight_IPW_ATT,result_obj_ATT,data,cov,var_treat,var_y)
    IPW_sandwich_lo_ATT<-IPW_ATT-z*sqrt(sandwich_IPW_var_ATT)
    IPW_sandwich_up_ATT<-IPW_ATT+z*sqrt(sandwich_IPW_var_ATT)
    coverage_IPW_sandwich_ATT <-ifelse((IPW_sandwich_lo_ATT<=true_ATT) & (IPW_sandwich_up_ATT>=true_ATT),1,0)
    
    ATT_1_result[repl,c(4:8)]<-c(IPW_ATT,Naive_IPW_var_ATT,coverage_IPW_Naive_ATT,sandwich_IPW_var_ATT,coverage_IPW_sandwich_ATT)
    
    
    
    ### -------------------------------------------------------------------------
    ### 3) DR ATE ---------------------------------------------------
    DR_ATE<-DR_estimator("ATE",data,var_treat,var_y,cov)$est
    
    DR_Naive_var_ATE<-DR_Naive_var_estimator("ATE",data,var_treat,var_y,cov)
    DR_Naive_lo_ATE<-DR_ATE-z*sqrt(DR_Naive_var_ATE)
    DR_Naive_up_ATE<-DR_ATE+z*sqrt(DR_Naive_var_ATE)
    coverage_DR_Naive_ATE<-ifelse((DR_Naive_lo_ATE<=true_ATE) & (DR_Naive_up_ATE>=true_ATE),1,0)
    
    DR_sandwich_var_ATE<-sandwich_var_DR("ATE",data,var_treat,var_y,cov)
    DR_sandwich_lo_ATE<-DR_ATE-z*sqrt(DR_sandwich_var_ATE)
    DR_sandwich_up_ATE<-DR_ATE+z*sqrt(DR_sandwich_var_ATE)
    coverage_DR_sandwich_ATE<-ifelse((DR_sandwich_lo_ATE<=true_ATE & DR_sandwich_up_ATE>=true_ATE),1,0)
    
    
    ATE_1_result[repl,c(9:ncol(ATE_1_result))]<-c(DR_ATE,DR_Naive_var_ATE,coverage_DR_Naive_ATE,
                                                  DR_sandwich_var_ATE,coverage_DR_sandwich_ATE)
    
    
    ### -------------------------------------------------------------------------
    ### 3) DR ATT ---------------------------------------------------
    DR_ATT<-DR_estimator("ATT",data,var_treat,var_y,cov)$est
    
    DR_Naive_var_ATT<-DR_Naive_var_estimator("ATT",data,var_treat,var_y,cov)
    DR_Naive_lo_ATT<-DR_ATT-z*sqrt(DR_Naive_var_ATT)
    DR_Naive_up_ATT<-DR_ATT+z*sqrt(DR_Naive_var_ATT)
    coverage_DR_Naive_ATT<-ifelse((DR_Naive_lo_ATT<=true_ATT) & (DR_Naive_up_ATT>=true_ATT),1,0)
    
    # DR_sandwich_var_ATT<-sandwich_var_DR("ATT",data,var_treat,var_y,cov)
    # DR_sandwich_lo_ATT<-DR_ATT-z*sqrt(DR_sandwich_var_ATT)
    # DR_sandwich_up_ATT<-DR_ATT+z*sqrt(DR_sandwich_var_ATT)
    j_hat<-J_function(data,data,var_treat,var_y,cov)
    crossprod<-crossprod_est_equ(data,data,var_treat,var_y,cov)
    cov_hat<-solve(j_hat)%*%crossprod%*%t(solve(j_hat))
    
    DR_sandwich_var_ATT<-(cov_hat[1,1]+cov_hat[2,2]-2*cov_hat[1,2])/nrow(data)
    
    DR_sandwich_lo_ATT<-DR_ATT-z*sqrt(DR_sandwich_var_ATT)
    DR_sandwich_up_ATT<-DR_ATT+z*sqrt(DR_sandwich_var_ATT)
    
    coverage_DR_sandwich_ATT<-ifelse((DR_sandwich_lo_ATT<=true_ATT) & (DR_sandwich_up_ATT>=true_ATT),1,0)
    
    ATT_1_result[repl,c(9:ncol(ATT_1_result))]<-c(DR_ATT,DR_Naive_var_ATT,coverage_DR_Naive_ATT,
                                                  DR_sandwich_var_ATT,coverage_DR_sandwich_ATT)
    
  }
  
  result$ATE<-ATE_1_result
  result$ATT<-ATT_1_result
  
  return(result)
}

