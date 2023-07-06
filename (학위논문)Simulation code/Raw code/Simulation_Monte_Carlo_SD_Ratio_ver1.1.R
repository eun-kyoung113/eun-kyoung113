### -------------------------------------------------------------------------
### Title : Variance Estimator of Monte Carlo Approximation version 1.1 -----------------

### Purpose : Calculate SD Ratio --------------------------------------------


Monte_result<-function(A_RATIO, N){
  
  result<-list()
  EFFECT<-c(log(1.5),log(2),log(2))
  
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
  
  B<-sample[1,]
  C<-sample[2,]
  A<-sample[3,]
  
  X_sample<-cbind(B,C,A)
  
  # Y random sampling - continuous
  Y<-as.matrix(X_sample)%*%EFFECT+rnorm(nrow(X_sample),0,1)
  
  z<-qnorm(0.975)
  
  
  ### Make matrix storing result -----------------------------------
  ATE_var<-matrix(NA,nrow=1,ncol=5)
  colnames(ATE_var)<-c("Outcome_reg_var","IPW_Naive_var","IPW_Sandwich_var",
                       "DR_Naive_var","DR_Sandwich_var")
  
  ATT_var<-matrix(NA,nrow=1,ncol=5)
  colnames(ATT_var)<-c("Outcome_reg_var","IPW_Naive_var","IPW_Sandwich_var",
                       "DR_Naive_var","DR_Sandwich_var")
  
  
  ### -------------------------------------------------------------------------
  ### Simulation --------------------------------------------------------------
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
  
  ATE_reg_est<-coef(outcome_reg_ATE)[var_treat]
  
  re_ATE<-summary(outcome_reg_ATE)
  re1_ATE<-as.data.frame(re_ATE$coefficients)
  outcome_reg_var_ATE<-re1_ATE$`Std. Error`[which(rownames(re1_ATE)==var_treat)]
  
  ATE_var[1,1]<-outcome_reg_var_ATE
  
  
  ### -------------------------------------------------------------------------
  ### 1) outcome regression ATT ---------------------------------------------------
  outcome_reg_ATT<-lm(Y~., data=data)
  
  N1<-sum(data$A==1)
  
  ATT_reg_est<-(sum(data$A*data$Y) - sum(data$A*(coef(outcome_reg_ATT)%*%t(X_0))))/N1
  
  a<-data$A*data$Y-data$A*(coef(outcome_reg_ATT)%*%t(X_0))
  outcome_reg_var_ATT<-sum((a-ATT_reg_est)^2)/N1
  
  ATT_var[1,1]<-outcome_reg_var_ATT
  
  
  ### -------------------------------------------------------------------------
  ### 2) IPW ATE ---------------------------------------------------
  weight_IPW_ATE<-weight_make(var_treat,cov,"ATE",data)
  result_obj_ATE<-lm(Y~A, weight=weight_IPW_ATE$untrimmed)
  IPW_ATE<-coef(result_obj_ATE)[var_treat]
  
  re_ATE<-summary(result_obj_ATE)
  re1_ATE<-as.data.frame(re_ATE$coefficients)
  
  Naive_IPW_var_ATE<-re1_ATE$`Std. Error`[which(rownames(re1_ATE)==var_treat)]
  ATE_var[1,2]<-Naive_IPW_var_ATE
  
  sandwich_IPW_var_ATE<-sandwich_var_ATE(result_obj_ATE,data)[2,2]
  ATE_var[1,3]<-sandwich_IPW_var_ATE
  
  
  ### -------------------------------------------------------------------------
  ### 2) IPW ATT ---------------------------------------------------
  weight_IPW_ATT<-weight_make(var_treat,cov,"ATT",data)
  result_obj_ATT<-lm(Y~A, weight=weight_IPW_ATT$untrimmed)
  IPW_ATT<-coef(result_obj_ATT)[var_treat]
  
  re_ATT<-summary(result_obj_ATT)
  re1_ATT<-as.data.frame(re_ATT$coefficients)
  
  Naive_IPW_var_ATT<-re1_ATT$`Std. Error`[which(rownames(re1_ATT)==var_treat)]
  ATT_var[1,2]<-Naive_IPW_var_ATT
  
  sandwich_IPW_var_ATT<-sandwich_var_ATT(weight_IPW_ATT,result_obj_ATT,data,cov,var_treat,var_y)
  ATT_var[1,3]<-sandwich_IPW_var_ATT
  
  
  
  ### -------------------------------------------------------------------------
  ### 3) DR ATE ---------------------------------------------------
  DR_ATE<-DR_estimator("ATE",data,var_treat,var_y,cov)$est
  
  DR_Naive_var_ATE<-DR_Naive_var_estimator("ATE",data,var_treat,var_y,cov)
  ATE_var[1,4]<-DR_Naive_var_ATE
  
  DR_sandwich_var_ATE<-sandwich_var_DR("ATE",data,var_treat,var_y,cov)
  ATE_var[1,5]<-DR_sandwich_var_ATE
  
  
  
  ### -------------------------------------------------------------------------
  ### 3) DR ATT ---------------------------------------------------
  DR_ATT<-DR_estimator("ATT",data,var_treat,var_y,cov)$est
  
  DR_Naive_var_ATT<-DR_Naive_var_estimator("ATT",data,var_treat,var_y,cov)
  ATT_var[1,4]<-DR_Naive_var_ATT
  
  DR_sandwich_var_ATT<-sandwich_var_DR("ATT",data,var_treat,var_y,cov)
  ATT_var[1,5]<-DR_sandwich_var_ATT
  
  
  result$ATE_var<-ATE_var
  result$ATT_var<-ATT_var
  
  return(result)
}
