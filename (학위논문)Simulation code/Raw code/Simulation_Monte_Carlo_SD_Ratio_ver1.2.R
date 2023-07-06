### -------------------------------------------------------------------------
### Title : Variance Estimator of Monte Carlo Approximation version 1.2 -----------------
###       : give 1000th estimator

### Purpose : Calculate SD Ratio --------------------------------------------


Monte_result<-function(A_RATIO, A_VAR1, A_VAR2, N, REPL){
  
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
  
  
  z<-qnorm(0.975)
  
  
  ### Make matrix storing result -----------------------------------
  ATE_var<-matrix(NA,nrow=REPL,ncol=3)
  colnames(ATE_var)<-c("Outcome_reg_est","IPW_est","DR_est")
  
  ATT_var<-matrix(NA,nrow=REPL,ncol=3)
  colnames(ATT_var)<-c("Outcome_reg_est","IPW_est","DR_est")
  
  
  
  ### -------------------------------------------------------------------------
  ### Simulation --------------------------------------------------------------
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
    
    X_1<-as.matrix(cbind(1,data[,cov],1))
    X_0<-as.matrix(cbind(1,data[,cov],0))
    
    ### -------------------------------------------------------------------------
    ### 1) outcome regression_ATE ---------------------------------------------------
    outcome_reg_ATE<-lm(Y~., data=data)
    
    #ATE_reg_est<-sum(data$A*(data$Y-coef(outcome_reg_ATE)%*%t(X_0)) + (1-data$A)*(coef(outcome_reg_ATE)%*%t(X_1)-data$Y))/nrow(data)
    ATE_reg_est<-coef(outcome_reg_ATE)[var_treat]
    
    ATE_var[repl,1]<-ATE_reg_est
    
    
    ### -------------------------------------------------------------------------
    ### 1) outcome regression ATT ---------------------------------------------------
    outcome_reg_ATT<-lm(Y~., data=data)
    
    N1<-sum(data$A==1)
    
    ATT_reg_est<-coef(outcome_reg_ATT)[var_treat]
    #ATT_reg_est<-(sum(data$A*data$Y) - sum(data$A*(coef(outcome_reg_ATT)%*%t(X_0))))/N1
    
    ATT_var[repl,1]<-ATT_reg_est
    
    
    
    ### -------------------------------------------------------------------------
    ### 2) IPW ATE ---------------------------------------------------
    weight_IPW_ATE<-weight_make(var_treat,cov,"ATE",data)
    result_obj_ATE<-lm(Y~A, weight=weight_IPW_ATE$untrimmed)
    IPW_ATE<-coef(result_obj_ATE)[var_treat]
    
    ATE_var[repl,2]<-IPW_ATE
    
    
    ### -------------------------------------------------------------------------
    ### 2) IPW ATT ---------------------------------------------------
    weight_IPW_ATT<-weight_make(var_treat,cov,"ATT",data)
    result_obj_ATT<-lm(Y~A, weight=weight_IPW_ATT$untrimmed)
    IPW_ATT<-coef(result_obj_ATT)[var_treat]
    
    ATT_var[repl,2]<-IPW_ATT
    
    
    
    ### -------------------------------------------------------------------------
    ### 3) DR ATE ---------------------------------------------------
    DR_ATE<-DR_estimator("ATE",data,var_treat,var_y,cov)$est
    ATE_var[repl,3]<-DR_ATE
    
    
    ### -------------------------------------------------------------------------
    ### 3) DR ATT ---------------------------------------------------
    DR_ATT<-DR_estimator("ATT",data,var_treat,var_y,cov)$est
    ATT_var[repl,3]<- DR_ATT
    
    
    
  }
  
  result$ATE_var<-ATE_var
  result$ATT_var<-ATT_var
  
  return(result)
}
