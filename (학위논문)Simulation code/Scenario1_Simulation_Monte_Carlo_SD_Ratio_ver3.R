### -------------------------------------------------------------------------
### Title : Variance Estimator of Monte Carlo Approximation version 3 -----------------
###       : give 1000th estimator
###       : for Scenario1

### Purpose : Calculate SD Ratio --------------------------------------------


Monte_result_1<-function(A_RATIO, A_VAR1, A_VAR2, A_VAR3, N, REPL){
  
  result<-list()
  theta<-c(1,2.5,2.5,1,1)
 
  B1<-matrix(NA, nrow=N, ncol=REPL)
  C1<-matrix(NA, nrow=N, ncol=REPL)
  D1<-matrix(NA, nrow=N, ncol=REPL)
  U1<-matrix(NA, nrow=N, ncol=REPL)
  A1<-matrix(NA, nrow=N, ncol=REPL)
  Y1<-matrix(NA, nrow=N, ncol=REPL)
  
  for(repl in 1:REPL){
    delta0<-A_RATIO
    delta_b<-A_VAR1
    delta_c<-A_VAR2
    delta_d<-A_VAR3

    set.seed(123*repl)
    B<-rnorm(N,0,1)
    C<-rbinom(N,1,0.5)
    D<-runif(N,0,10)
    U<-runif(N,0,10)
    
    pi.vec<-1/(1+exp(-(A_RATIO + delta_b*B + delta_c*C + delta_d*D)))
    A<-rbinom(N,1,pi.vec)
    Y<-cbind(1,B,C,A,U)%*%theta+rnorm(N)
    
    B1[,repl]<-B
    C1[,repl]<-C
    A1[,repl]<-A
    
    # Y random sampling - continuous
    Y1[,repl]<-Y
  }
  
  
  z<-qnorm(0.975)
  
  
  ### Make matrix storing result -----------------------------------
  # ATE_var<-matrix(NA,nrow=REPL,ncol=3)
  # colnames(ATE_var)<-c("Outcome_reg_est","IPW_est","DR_est")
  
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
    
    cov_outcome<-c("B","C")
    cov_ps<-c("B","C")
    #cov_type<-c("continuous","continuous","binary")
    var_treat<-"A"
    var_y<-"Y"
    
    # X_1<-as.matrix(cbind(1,data[,cov],1))
    # X_0<-as.matrix(cbind(1,data[,cov],0))
    
    ### -------------------------------------------------------------------------
    ### 1) outcome regression_ATE ---------------------------------------------------
    # outcome_reg_ATE<-lm(Y~., data=data)
    # 
    # #ATE_reg_est<-sum(data$A*(data$Y-coef(outcome_reg_ATE)%*%t(X_0)) + (1-data$A)*(coef(outcome_reg_ATE)%*%t(X_1)-data$Y))/nrow(data)
    # ATE_reg_est<-coef(outcome_reg_ATE)[var_treat]
    # 
    # ATE_var[repl,1]<-ATE_reg_est
    
    
    ### -------------------------------------------------------------------------
    ### 1) outcome regression ATT ---------------------------------------------------
    outcome_reg_ATT<-lm(Y~., data=data)
    
    N1<-sum(data$A==1)
    
    ATT_reg_est<-coef(outcome_reg_ATT)[var_treat]
    #ATT_reg_est<-(sum(data$A*data$Y) - sum(data$A*(coef(outcome_reg_ATT)%*%t(X_0))))/N1
    
    ATT_var[repl,1]<-ATT_reg_est
    
    
    
    ### -------------------------------------------------------------------------
    ### 2) IPW ATE ---------------------------------------------------
    # formula<-"A~B+C"
    # weight_IPW_ATE<-weight_make(var_treat,cov_ps,"ATE",data,formula)
    # result_obj_ATE<-lm(Y~A, weight=weight_IPW_ATE$untrimmed)
    # IPW_ATE<-coef(result_obj_ATE)[var_treat]
    # 
    # ATE_var[repl,2]<-IPW_ATE
    # 
    
    ### -------------------------------------------------------------------------
    ### 2) IPW ATT ---------------------------------------------------
    formula<-"A~B+C"
    weight_IPW_ATT<-weight_make(var_treat,cov_ps,"ATT",data,formula)
    result_obj_ATT<-lm(Y~A, weight=weight_IPW_ATT$untrimmed)
    IPW_ATT<-coef(result_obj_ATT)[var_treat]
    
    ATT_var[repl,2]<-IPW_ATT
    
    
    
    ### -------------------------------------------------------------------------
    ### 3) DR ATE ---------------------------------------------------
    # ps_formula<-"A~B+C"
    # outcome_formula<-"Y~A+B+C"
    # 
    # DR_ATE<-DR_estimator("ATE",data,var_treat,var_y,cov_outcome,cov_ps,ps_formula,outcome_formula)$est
    # ATE_var[repl,3]<-DR_ATE
    # 
    
    ### -------------------------------------------------------------------------
    ### 3) DR ATT ---------------------------------------------------
    ps_formula<-"A~B+C"
    outcome_formula<-"Y~A+B+C"
    
    DR_ATT<-DR_estimator("ATT",data,var_treat,var_y,cov_outcome,cov_ps,ps_formula,outcome_formula)$est
    ATT_var[repl,3]<- DR_ATT
    
    
    
  }
  
  #result$ATE_var<-ATE_var
  result$ATT_var<-ATT_var
  
  return(result)
}
