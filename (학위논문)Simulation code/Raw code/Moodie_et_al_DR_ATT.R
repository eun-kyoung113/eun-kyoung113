### -------------------------------------------------------------------------
### Code of getting result table in Scenario 1 ------------------------------

### Moodie et al paper ------------------------------------------------------

# -------------------------------------------------------------------------
# ---------------------- Hardcoding function ------------------------------
# -------------------------------------------------------------------------
library(boot)

### DR Estimation -----------------------------------------------------
### DR Estimator function -----------------------------------------------
DR_estimator<-function(estimate,data,var_treat,var_y,cov_outcome,cov_ps){
  
  result<-list()
  
  mydata<-data[c(var_treat,cov_ps)]
  
  myformula<-as.formula(sprintf("%s~.",var_treat))
  fit<-glm(formula=myformula,data=mydata,family='binomial')
  result$fit<-fit
  
  ps<-fit$fitted.values
  
  myformula1<-as.formula(sprintf("%s~.",var_y))
  
  #ind_mu0<-which(data[,var_treat]==0)
  #mu0_df<-data[ind_mu1,c(var_y,cov)]
  
  data2<-data[c(var_y,var_treat,cov_outcome)]
  
  out_model<-lm(formula=myformula1,data=data2)
  new_dt_mu0<-model.matrix(out_model)
  new_dt_mu0[,which(colnames(new_dt_mu0)==var_treat)]<-rep(0,nrow(data2))
  mu0_X<-coef(out_model)%*%t(new_dt_mu0)
  
  result$out_model<-out_model
  result$mu0_X<-mu0_X
  
  data$ps<-ps
  
  if(estimate=='ATE'){
    ind_mu1<-which(data[,var_treat]==1)
    #mu1_df<-data[ind_mu1,c(var_y,cov)]
    
    new_dt_mu1<-model.matrix(out_model)
    new_dt_mu1[,which(colnames(new_dt_mu0)==var_treat)]<-rep(1,nrow(data))
    
    mu1_X<-coef(out_model)%*%t(new_dt_mu1)
    result$mu1_X<-mu1_X
    
    mu1_dr<-mean(data[,var_treat]*data[,var_y]/data$ps - ((data[,var_treat]-data$ps)/data$ps)*(mu1_X))
    mu0_dr<-mean(mu0_X+((1-data[,var_treat])*(data[,var_y]-mu0_X))/(1-data$ps))
    
    result$mu1_dr<-mu1_dr
    result$est<-mu1_dr-mu0_dr
  }
  
  else if(estimate=='ATT'){
    ind_mu1<-which(data[,var_treat]==1)
    result$mu1_dr<-sum(data[,var_y]*data[,var_treat])/length(ind_mu1)
    result$est<-sum(data[,var_y]*data[,var_treat]-(((data[,var_y]*(1-data[,var_treat])*data$ps)+(mu0_X)*(data[,var_treat]-data$ps))/(1-data$ps)))/length(ind_mu1) 
  }
  
  return(result)
}



### -------------------------------------------------------------------------
### Naive variance of DR estimator -------------------------------------
DR_Naive_var_estimator<-function(estimate,data,var_treat,var_y,cov_outcome,cov_ps){
  result<-DR_estimator(estimate,data,var_treat,var_y,cov_outcome,cov_ps)
  
  tau_dr<-result$est
  fit<-result$fit
  
  if(estimate=="ATE"){
    tau_i<-data[,var_treat]*data[,var_y]/fit$fitted.values - 
      ((data[,var_treat]-fit$fitted.values)/fit$fitted.values)*(result$mu1_X)-
      result$mu0_X+((1-data[,var_treat])*(data[,var_y]-result$mu0_X))/(1-fit$fitted.values)
    
    var<-sum((tau_i-tau_dr)^2)/(nrow(data)^2)
  }
  
  else if(estimate=="ATT"){
    tau_i<-(data[,var_y]*data[,var_treat])
    -(((data[,var_y]*(1-data[,var_treat])*fit$fitted.values)+result$mu0_X*(data[,var_treat]-fit$fitted.values))/(1-fit$fitted.values))
    
    var<-sum((tau_i-tau_dr)^2)/(length(data[,var_treat]==1)^2)
  }
  
  
  return(var)
  
}


### -----------------------------------------------------
### J_function -----------------------------------------------
J_function<-function(data,data_L,var_treat,var_y,cov_outcome,cov_ps){
  
  X_P<-as.matrix(cbind(1,data_L[,cov_ps]))
  X_out<-as.matrix(cbind(1,data_L[,var_treat],data_L[,cov_outcome]))
  X_0<-as.matrix(cbind(1,0,data_L[,cov_outcome]))
  X_1<-as.matrix(cbind(1,1,data_L[,cov_outcome]))
  
  # true_ps<-as.vector(inv.logit(-2+as.matrix(data_L[,cov])%*%as.vector(rep(0.01,2))))
  
  DR_ATT<-DR_estimator("ATT",data,var_treat,var_y,cov_outcome,cov_ps)
  #fit<-DR_ATT$fit
  
  #theta<-matrix(c(DR_ATT$mu1_dr, DR_ATT$mu1_dr-DR_ATT$est, coef(DR_ATT$out_model), coef(DR_ATT$fit)), nrow=1)
  
  #nu1<-theta[1]
  #nu0<-theta[2]
  outcome_model_coef<-coef(DR_ATT$out_model)
  ps_model_coef<-coef(DR_ATT$fit)
  
  dim1<-length(outcome_model_coef) + length(ps_model_coef) + 2
  
  exp_beta_X<-exp(ps_model_coef%*%t(X_P))
  fitted_ps<-exp_beta_X/(1+exp_beta_X)
  
  nu_theta_array<-array(0,dim=c(dim1,dim1,nrow(data_L)))
  
  for(i in 1:nrow(data_L)){
    
    nu1_3<-matrix(rep(0,length(outcome_model_coef)),nrow=1)      
    nu1_4<-matrix(rep(0,length(ps_model_coef)),nrow=1)
    nu1<-cbind(1,0,nu1_3,nu1_4)
    colnames(nu1)<-NULL
    
    nu2_3<-((-1)*nrow(data_L)/sum(data_L[,var_treat]==1))*(matrix(((1+exp_beta_X[i])*data_L[,var_treat][i]-exp_beta_X[i])*X_0[i,],nrow=1))
    nu2_4<- ((-1)*nrow(data_L)/sum(data_L[,var_treat]==1))*(as.vector(exp_beta_X[i]*((1-data_L[,var_treat][i])*(data_L[,var_y][i]-(outcome_model_coef%*%X_0[i,]))))*X_P[i,])
    nu2<-cbind(0,1,matrix(nu2_3,nrow=1),matrix(nu2_4,nrow=1))
    colnames(nu2)<-NULL
    
    nu3_3<--X_out[i,]%*%t(X_out[i,])
    nu3_4<-matrix(0,nrow=length(outcome_model_coef),ncol=length(ps_model_coef))
    nu3<-cbind(0,0,nu3_3,nu3_4)
    colnames(nu3)<-NULL
    
    nu4_3<-matrix(0,nrow=length(ps_model_coef),ncol=length(outcome_model_coef))
    nu4_4<- -((fitted_ps[i])*(1-fitted_ps[i]))*(X_P[i,]%*%t(X_P[i,]))
    nu4<-cbind(0,0,nu4_3,nu4_4)
    colnames(nu4)<-NULL
    
    nu_theta<-rbind(nu1,nu2,nu3,nu4)
    nu_theta_array[,,i]<-nu_theta
    
  }
  
  j_theta<-apply(nu_theta_array,c(1,2),mean) 
  
  return(j_theta)
}



### ----------------------------------------------------------------------------
### crossprod of estimating_equation function -------
crossprod_est_equ<-function(data,data_L,var_treat,var_y,cov_outcome,cov_ps){
  
  X_P<-as.matrix(cbind(1,data_L[,cov_ps]))
  X_out<-as.matrix(cbind(1,data_L[,var_treat],data_L[,cov_outcome]))
  X_0<-as.matrix(cbind(1,0,data_L[,cov_outcome]))
  X_1<-as.matrix(cbind(1,1,data_L[,cov_outcome]))
  
  #true_ps<-as.vector(inv.logit(1+as.matrix(data[,cov])%*%as.vector(rep(0.01,2))))
  
  DR_ATT<-DR_estimator("ATT",data,var_treat,var_y,cov_outcome,cov_ps)
  #fit<-DR_ATT$fit
  theta<-matrix(c(DR_ATT$mu1_dr, DR_ATT$mu1_dr-DR_ATT$est, coef(DR_ATT$out_model), coef(DR_ATT$fit)), nrow=1)
  
  nu1<-theta[1]
  nu0<-theta[2]
  outcome_model_coef<-theta[3:(length(cov_outcome)+3+1)]
  ps_model_coef<-theta[(length(theta)-length(cov_ps)):length(theta)]
  
  exp_beta_X<-exp(ps_model_coef%*%t(X_P))
  fitted_ps<-exp_beta_X/(1+exp_beta_X)
  
  # cross product of estimating_equation
  est_1_true<-nu1-(nrow(data_L)/sum(data_L[,var_treat]==1))*(data_L[,var_y]*data_L[,var_treat]) # vector(100 x 1)
  est_2_true<-nu0-(nrow(data_L)/sum(data_L[,var_treat]==1))*((((1-data_L[,var_treat])*data_L[,var_y]*fitted_ps+(outcome_model_coef%*%t(X_0))*(data_L[,var_treat]-fitted_ps)))/(1-fitted_ps))
  est_3_true<-as.vector(data_L[,var_y]-outcome_model_coef%*%t(X_out))*X_out
  est_4_true<-as.vector(data_L[,var_treat]-ps_model_coef%*%t(X_P))*X_P
  
  estimating_equation_true <- as.matrix(cbind(est_1_true,t(est_2_true),est_3_true,est_4_true)) # n by 9
  
  nu_theta_mean<-(t(estimating_equation_true)%*%(estimating_equation_true))/nrow(data_L)
  
  return(nu_theta_mean)
  
}


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------




### -------------------------------------------------------------------------
### Scenario 1) PS model & Outcome model all correctly specified ------------
Scenario1_result<-function(N, REPL){
  
  library(locfit)
  result<-list()
  
  Z1<-matrix(NA, nrow=N, ncol=REPL)
  A1<-matrix(NA, nrow=N, ncol=REPL)
  Y1<-matrix(NA, nrow=N, ncol=REPL)
  
  for(repl in 1:REPL){
    
    set.seed(123*repl)
    sample<-replicate(N,expr={
      Z<-runif(1,min=0,max=10)
      p_z<-expit(-1.75 + 0.3*Z)
      A<-rbinom(1,1,prob=p_z)
      c(Z,A)
    })
    
    Z1[,repl]<-sample[1,]
    A1[,repl]<-sample[2,]
    
    mu=2*A1[,repl]+0.6*A1[,repl]*Z1[,repl]+Z1[,repl]-0.2*(Z1[,repl]^2)
    
    # Y random sampling - continuous
    Y1[,repl]<-rnorm(N,mean=mu,sd=1)
  }
  
  
  ### replication start -------------------------------------------------------
  z<-qnorm(0.975)
  
  
  ### Make matrix storing result -----------------------------------
  ATT_1_result<-matrix(NA,nrow=REPL,ncol=5)
  colnames(ATT_1_result)<-c("DR_est","DR_Naive_var","DR_Naive_coverage","DR_Sandwich_var","Sandwich_DR_coverage")

  true_ATT<-5.749
  
  
  ### -------------------------------------------------------------------------
  ### Scenario 1 - ATE & ATT --------------------------------------------------------
  for(repl in 1:REPL){
    Z<-Z1[,repl]
    A<-A1[,repl]
    Y<-Y1[,repl]
    
    data<-as.data.frame(cbind(Z,A,Y))
    data$Z2<-(data$Z)^2
    
    cov_outcome<-c("Z","Z2")
    cov_ps<-"Z"
    #cov_type<-c("continuous","continuous","binary")
    var_treat<-"A"
    var_y<-"Y"
    
   
    ### -------------------------------------------------------------------------
    ### DR ATT ---------------------------------------------------
    DR_ATT<-DR_estimator("ATT",data,var_treat,var_y,cov_outcome,cov_ps)$est
    
    DR_Naive_var_ATT<-DR_Naive_var_estimator("ATT",data,var_treat,var_y,cov_outcome,cov_ps)
    DR_Naive_lo_ATT<-DR_ATT-z*sqrt(DR_Naive_var_ATT)
    DR_Naive_up_ATT<-DR_ATT+z*sqrt(DR_Naive_var_ATT)
    coverage_DR_Naive_ATT<-ifelse((DR_Naive_lo_ATT<=true_ATT) & (DR_Naive_up_ATT>=true_ATT),1,0)
    
    # DR_sandwich_var_ATT<-sandwich_var_DR("ATT",data,var_treat,var_y,cov)
    # DR_sandwich_lo_ATT<-DR_ATT-z*sqrt(DR_sandwich_var_ATT)
    # DR_sandwich_up_ATT<-DR_ATT+z*sqrt(DR_sandwich_var_ATT)
    j_hat<-J_function(data,data,var_treat,var_y,cov_outcome,cov_ps)
    crossprod<-crossprod_est_equ(data,data,var_treat,var_y,cov_outcome,cov_ps)
    cov_hat<-solve(j_hat)%*%crossprod%*%t(solve(j_hat))
    
    DR_sandwich_var_ATT<-(cov_hat[1,1]+cov_hat[2,2]-2*cov_hat[1,2])/nrow(data)
    
    DR_sandwich_lo_ATT<-DR_ATT-z*sqrt(DR_sandwich_var_ATT)
    DR_sandwich_up_ATT<-DR_ATT+z*sqrt(DR_sandwich_var_ATT)
    
    coverage_DR_sandwich_ATT<-ifelse((DR_sandwich_lo_ATT<=true_ATT) & (DR_sandwich_up_ATT>=true_ATT),1,0)
    
    ATT_1_result[repl,c(1:ncol(ATT_1_result))]<-c(DR_ATT,DR_Naive_var_ATT,coverage_DR_Naive_ATT,
                                                  DR_sandwich_var_ATT,coverage_DR_sandwich_ATT)
    
  }
  
  result$ATT<-ATT_1_result
  
  return(result)
}



# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
### Scenario1 - Performance record ------------------------------------------
N<-500
REPL<-1000
true_ATT<-5.749

Scenario1_result<-Scenario1_result(N, REPL)
ATT_1_result<-as.data.frame(Scenario1_result$ATT)

ATT_1_performance<-as.data.frame(matrix(NA,nrow=1,ncol=4))
colnames(ATT_1_performance)<-c("Bias","rMSE","Naive_var_coverage","Sandwich_robust_var_coverage")

row.names(ATT_1_performance)<-"DR"

func_rMSE<-function(x){
  return(mean((x-true_ATT)^2))
}


### ---------------------------
ATT_1_performance[,"Bias"]<-mean(ATT_1_result[,grep("_est",colnames(ATT_1_result))])-true_ATT

ATT_1_performance[,"rMSE"]<-func_rMSE(ATT_1_result[,grep("_est",colnames(ATT_1_result))])

ATT_1_performance[,"Naive_var_coverage"]<-mean(ATT_1_result$DR_Naive_coverage)

ATT_1_performance[,"Sandwich_robust_var_coverage"]<-mean(ATT_1_result$Sandwich_DR_coverage)



print(ATT_1_performance)
#            Bias       rMSE Naive_var_coverage Sandwich_robust_var_coverage
# DR -0.004229692 0.02670549              0.993                            1






### -------------------------------------------------------------------------
### Scenario 2) PS model correctly specified ------------
Scenario2_result<-function(N, REPL){
  
  library(locfit)
  result<-list()
  
  Z1<-matrix(NA, nrow=N, ncol=REPL)
  A1<-matrix(NA, nrow=N, ncol=REPL)
  Y1<-matrix(NA, nrow=N, ncol=REPL)
  
  for(repl in 1:REPL){
    
    set.seed(123*repl)
    sample<-replicate(N,expr={
      Z<-runif(1,min=0,max=10)
      p_z<-expit(-1.75 + 0.3*Z)
      A<-rbinom(1,1,prob=p_z)
      c(Z,A)
    })
    
    Z1[,repl]<-sample[1,]
    A1[,repl]<-sample[2,]
    
    mu=2*A1[,repl]+0.6*A1[,repl]*Z1[,repl]+Z1[,repl]-0.5*(Z1[,repl]*log(Z1[,repl]))+((5*Z1[,repl])/(1+Z1[,repl]))
    
    # Y random sampling - continuous
    Y1[,repl]<-rnorm(N,mean=mu,sd=1)
  }
  
  
  ### replication start -------------------------------------------------------
  z<-qnorm(0.975)
  
  
  ### Make matrix storing result -----------------------------------
  ATT_2_result<-matrix(NA,nrow=REPL,ncol=5)
  colnames(ATT_2_result)<-c("DR_est","DR_Naive_var","DR_Naive_coverage","DR_Sandwich_var","Sandwich_DR_coverage")
  
  true_ATT<-5.749
  
  
  ### -------------------------------------------------------------------------
  ### Scenario 2 - ATT --------------------------------------------------------
  for(repl in 1:REPL){
    Z<-Z1[,repl]
    A<-A1[,repl]
    Y<-Y1[,repl]
    
    data<-as.data.frame(cbind(Z,A,Y))
    data$Z2<-(data$Z)^2
    
    cov_outcome<-c("Z","Z2")
    cov_ps<-"Z"
    #cov_type<-c("continuous","continuous","binary")
    var_treat<-"A"
    var_y<-"Y"
    
    
    ### -------------------------------------------------------------------------
    ### DR ATT ---------------------------------------------------
    DR_ATT<-DR_estimator("ATT",data,var_treat,var_y,cov_outcome,cov_ps)$est
    
    DR_Naive_var_ATT<-DR_Naive_var_estimator("ATT",data,var_treat,var_y,cov_outcome,cov_ps)
    DR_Naive_lo_ATT<-DR_ATT-z*sqrt(DR_Naive_var_ATT)
    DR_Naive_up_ATT<-DR_ATT+z*sqrt(DR_Naive_var_ATT)
    coverage_DR_Naive_ATT<-ifelse((DR_Naive_lo_ATT<=true_ATT) & (DR_Naive_up_ATT>=true_ATT),1,0)
    
    # DR_sandwich_var_ATT<-sandwich_var_DR("ATT",data,var_treat,var_y,cov)
    # DR_sandwich_lo_ATT<-DR_ATT-z*sqrt(DR_sandwich_var_ATT)
    # DR_sandwich_up_ATT<-DR_ATT+z*sqrt(DR_sandwich_var_ATT)
    j_hat<-J_function(data,data,var_treat,var_y,cov_outcome,cov_ps)
    crossprod<-crossprod_est_equ(data,data,var_treat,var_y,cov_outcome,cov_ps)
    cov_hat<-solve(j_hat)%*%crossprod%*%t(solve(j_hat))
    
    DR_sandwich_var_ATT<-(cov_hat[1,1]+cov_hat[2,2]-2*cov_hat[1,2])/nrow(data)
    
    DR_sandwich_lo_ATT<-DR_ATT-z*sqrt(DR_sandwich_var_ATT)
    DR_sandwich_up_ATT<-DR_ATT+z*sqrt(DR_sandwich_var_ATT)
    
    coverage_DR_sandwich_ATT<-ifelse((DR_sandwich_lo_ATT<=true_ATT) & (DR_sandwich_up_ATT>=true_ATT),1,0)
    
    ATT_2_result[repl,c(1:ncol(ATT_1_result))]<-c(DR_ATT,DR_Naive_var_ATT,coverage_DR_Naive_ATT,
                                                  DR_sandwich_var_ATT,coverage_DR_sandwich_ATT)
    
  }
  
  result$ATT<-ATT_2_result
  
  return(result)
}



# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
### Scenario2 - Performance record ------------------------------------------
Scenario2_result<-Scenario2_result(N, REPL)
ATT_2_result<-as.data.frame(Scenario2_result$ATT)

ATT_2_performance<-as.data.frame(matrix(NA,nrow=1,ncol=4))
colnames(ATT_2_performance)<-c("Bias","rMSE","Naive_var_coverage","Sandwich_robust_var_coverage")

row.names(ATT_2_performance)<-"DR"

func_rMSE<-function(x){
  return(mean((x-true_ATT)^2))
}


### ---------------------------
ATT_2_performance[,"Bias"]<-mean(ATT_2_result[,grep("_est",colnames(ATT_2_result))])-true_ATT

ATT_2_performance[,"rMSE"]<-func_rMSE(ATT_2_result[,grep("_est",colnames(ATT_2_result))])

ATT_2_performance[,"Naive_var_coverage"]<-mean(ATT_2_result$DR_Naive_coverage)

ATT_2_performance[,"Sandwich_robust_var_coverage"]<-mean(ATT_2_result$Sandwich_DR_coverage)




print(ATT_2_performance)
#           Bias       rMSE Naive_var_coverage Sandwich_robust_var_coverage
# DR -0.004190027 0.02480856              0.999                            1


### -------------------------------------------------------------------------
### Scenario 3) Outcome model correctly specified ------------
Scenario3_result<-function(N, REPL){
  
  library(locfit)
  result<-list()
  
  Z1<-matrix(NA, nrow=N, ncol=REPL)
  A1<-matrix(NA, nrow=N, ncol=REPL)
  Y1<-matrix(NA, nrow=N, ncol=REPL)
  
  for(repl in 1:REPL){
    
    set.seed(123*repl)
    sample<-replicate(N,expr={
      Z<-runif(1,min=0,max=10)
      p_z<-expit(-1.75 + 0.3*Z)
      A<-rbinom(1,1,prob=p_z)
      c(Z,A)
    })
    
    Z1[,repl]<-sample[1,]
    A1[,repl]<-sample[2,]
    
    mu=2*A1[,repl]+0.6*A1[,repl]*Z1[,repl]+Z1[,repl]-0.2*(Z1[,repl]^2)
    
    # Y random sampling - continuous
    Y1[,repl]<-rnorm(N,mean=mu,sd=1)
  }
  
  
  ### replication start -------------------------------------------------------
  z<-qnorm(0.975)
  
  
  ### Make matrix storing result -----------------------------------
  ATT_1_result<-matrix(NA,nrow=REPL,ncol=5)
  colnames(ATT_1_result)<-c("DR_est","DR_Naive_var","DR_Naive_coverage","DR_Sandwich_var","Sandwich_DR_coverage")
  
  true_ATT<-5.749
  
  
  ### -------------------------------------------------------------------------
  ### Scenario 2 - ATT --------------------------------------------------------
  for(repl in 1:REPL){
    Z<-Z1[,repl]
    A<-A1[,repl]
    Y<-Y1[,repl]
    
    data<-as.data.frame(cbind(Z,A,Y))
    data$Z2<-(data$Z)^2
    
    cov_outcome<-c("Z","Z2")
    cov_ps<-NULL
    #cov_type<-c("continuous","continuous","binary")
    var_treat<-"A"
    var_y<-"Y"
    
    
    ### -------------------------------------------------------------------------
    ### DR ATT ---------------------------------------------------
    DR_ATT<-DR_estimator("ATT",data,var_treat,var_y,cov_outcome,cov_ps)$est
    
    DR_Naive_var_ATT<-DR_Naive_var_estimator("ATT",data,var_treat,var_y,cov_outcome,cov_ps)
    DR_Naive_lo_ATT<-DR_ATT-z*sqrt(DR_Naive_var_ATT)
    DR_Naive_up_ATT<-DR_ATT+z*sqrt(DR_Naive_var_ATT)
    coverage_DR_Naive_ATT<-ifelse((DR_Naive_lo_ATT<=true_ATT) & (DR_Naive_up_ATT>=true_ATT),1,0)
    
    # DR_sandwich_var_ATT<-sandwich_var_DR("ATT",data,var_treat,var_y,cov)
    # DR_sandwich_lo_ATT<-DR_ATT-z*sqrt(DR_sandwich_var_ATT)
    # DR_sandwich_up_ATT<-DR_ATT+z*sqrt(DR_sandwich_var_ATT)
    j_hat<-J_function(data,data,var_treat,var_y,cov_outcome,cov_ps)
    crossprod<-crossprod_est_equ(data,data,var_treat,var_y,cov_outcome,cov_ps)
    cov_hat<-solve(j_hat)%*%crossprod%*%t(solve(j_hat))
    
    DR_sandwich_var_ATT<-(cov_hat[1,1]+cov_hat[2,2]-2*cov_hat[1,2])/nrow(data)
    
    DR_sandwich_lo_ATT<-DR_ATT-z*sqrt(DR_sandwich_var_ATT)
    DR_sandwich_up_ATT<-DR_ATT+z*sqrt(DR_sandwich_var_ATT)
    
    coverage_DR_sandwich_ATT<-ifelse((DR_sandwich_lo_ATT<=true_ATT) & (DR_sandwich_up_ATT>=true_ATT),1,0)
    
    ATT_1_result[repl,c(1:ncol(ATT_1_result))]<-c(DR_ATT,DR_Naive_var_ATT,coverage_DR_Naive_ATT,
                                                  DR_sandwich_var_ATT,coverage_DR_sandwich_ATT)
    
  }
  
  result$ATT<-ATT_1_result
  
  return(result)
}



# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
### Scenario2 - Performance record ------------------------------------------
Scenario3_result<-Scenario3_result(N, REPL)
ATT_3_result<-as.data.frame(Scenario3_result$ATT)

ATT_3_performance<-as.data.frame(matrix(NA,nrow=1,ncol=4))
colnames(ATT_3_performance)<-c("Bias","rMSE","Naive_var_coverage","Sandwich_robust_var_coverage")

row.names(ATT_3_performance)<-"DR"

func_rMSE<-function(x){
  return(mean((x-true_ATT)^2))
}


### ---------------------------
ATT_3_performance[,"Bias"]<-mean(ATT_3_result[,grep("_est",colnames(ATT_3_result))])-true_ATT

ATT_3_performance[,"rMSE"]<-func_rMSE(ATT_3_result[,grep("_est",colnames(ATT_3_result))])

ATT_3_performance[,"Naive_var_coverage"]<-mean(ATT_3_result$DR_Naive_coverage)

ATT_3_performance[,"Sandwich_robust_var_coverage"]<-mean(ATT_3_result$Sandwich_DR_coverage)


print(ATT_3_performance)
#         Bias      rMSE Naive_var_coverage Sandwich_robust_var_coverage
# DR -0.6041403 0.3853172              0.079                        0.316


