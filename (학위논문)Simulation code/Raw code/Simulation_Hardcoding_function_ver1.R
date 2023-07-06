###  Hardcoding Estimator function ----------------------------------------

### 1) IPW Estimation -----------------------------------------------------
### 1-1) weight generation function - IPTW -----------------------------------------
weight_make<-function(var_treat,var_cov,estimate,data){
  result<-list()
  mydata<-data[c(var_treat,var_cov)]
  
  myformula<-as.formula(sprintf("%s~.",var_treat))
  fit<-glm(formula=myformula,data=mydata,family='binomial')
  ps<-fit$fitted.values
  
  result$model<-fit
  result$ps<-ps
  
  if(estimate=="ATE"){
    weight<-ifelse(data[,var_treat]==1,1/ps,1/(1-ps))
  }
  else if(estimate=='ATT'){
    weight<-ifelse(data[,var_treat]==1,1,ps/(1-ps))
  }
  else{
    weight<-ifelse(data[,var_treat]==1,(1-ps)/ps,1)
  }
  result$untrimmed<-weight
  result$trimmed<-ifelse(weight>quantile(weight,prob=0.99),quantile(weight,prob=0.99),weight)
  return(result)}

### -------------------------------------------------------------------------



### 1-2) Covariate balance check function - with no package - IPTW -----------------
balance_check<-function(var_treat,var_cov,var_weight,cov_type,data){
  balance_table<-matrix(0,nrow=length(var_cov),ncol=2)
  colnames(balance_table)<-c("before_aSD","after_aSD")
  rownames(balance_table)<-var_cov
  
  cov<-data[,var_cov]
  weight<-data[,var_weight]
  
  for(i in 1:length(cov)){
    var_cov<-cov[,i]
    
    if(cov_type[i]=="continuous"){
      #std_var_cov = (var_cov - mean(var_cov))/sd(var_cov)
      simple_M1 = mean(var_cov[data[,var_treat]==1])
      simple_M0 = mean(var_cov[data[,var_treat]==0])
      simple_V1 = var(var_cov[data[,var_treat]==1])
      simple_V0 = var(var_cov[data[,var_treat]==0])
      wgted_M1 = sum((var_cov[data[,var_treat]==1])*(weight[data[,var_treat]==1]))/sum(weight[data[,var_treat]==1])
      wgted_M0 = sum((var_cov[data[,var_treat]==0])*(weight[data[,var_treat]==0]))/sum(weight[data[,var_treat]==0])
      wgted_V1 = sum(((var_cov[data[,var_treat]==1]-wgted_M1)^2)*(weight[data[,var_treat]==1]))*sum(weight[data[,var_treat]==1])/(sum(weight[data[,var_treat]==1])^2-sum(weight[data[,var_treat]==1]^2))
      wgted_V0 = sum(((var_cov[data[,var_treat]==0]-wgted_M0)^2)*(weight[data[,var_treat]==0]))*sum(weight[data[,var_treat]==0])/(sum(weight[data[,var_treat]==0])^2-sum(weight[data[,var_treat]==0]^2))
      
    }
    else{
      #std_var_cov = (var_cov - mean(var_cov))/sd(var_cov)
      simple_M1 = mean(var_cov[data[,var_treat]==1])
      simple_M0 = mean(var_cov[data[,var_treat]==0])
      simple_V1 = simple_M1*(1-simple_M1)
      simple_V0 = simple_M0*(1-simple_M0)
      wgted_M1 = sum((var_cov[data[,var_treat]==1])*(weight[data[,var_treat]==1]))/sum(weight[data[,var_treat]==1])
      wgted_M0 = sum((var_cov[data[,var_treat]==0])*(weight[data[,var_treat]==0]))/sum(weight[data[,var_treat]==0])
      wgted_V1 = wgted_M1*(1-wgted_M1)
      wgted_V0 = wgted_M0*(1-wgted_M0)
      
    }
    
    
    ## aSD calculation ##
    before_aSD<-(simple_M1-simple_M0)/sqrt((simple_V1+simple_V0)/2)
    after_aSD<-(wgted_M1-wgted_M0)/sqrt((wgted_V1+wgted_V0)/2)
    balance_table[i,1]<-before_aSD
    balance_table[i,2]<-after_aSD
  }
  return(balance_table)
}
### -------------------------------------------------------------------------



### 1-3) Sandwich variance function - IPTW ATE ------------------------------
sandwich_var_ATE<-function(obj,data){
  
  X = model.matrix(obj)
  weight<-weights(obj)
  new_X<-sqrt(weight)*X
  bread<-solve(crossprod(new_X))*as.vector(nrow(data))
  psi<-as.vector(obj$residuals)*weight*X
  meat<-crossprod(as.matrix(psi))/nrow(psi)
  vr<-bread%*%meat%*%bread/nrow(data)
  
  return(vr)
  
}
### -------------------------------------------------------------------------


### 1-4) Sandwich variance function - IPTW ATT ------------------------------
sandwich_var_ATT<-function(weight_obj,result_obj,data,cov,var_treat,var_y){
  ps<-weight_obj$ps
  fit<-weight_obj$model
  weight<-weight_obj$untrimmed
  w<-diag(ps*(1-ps))
  X<-model.matrix(fit)
  a11<-(-1)*crossprod(sqrt(w)%*%X)/nrow(data)
  #a12<-matrix(0,nrow=length(cov),ncol=2)
  #a22<-diag(rep(mean(data[,var_treat]),2))
  a21_1<-(-1)*rep(0,(length(data[,cov])+1))
  mu0_hat<-mean(result_obj$fitted.values[data[,var_treat]==0])
  
  #mu0_hat<-log(1.5)*mean(data$B[data[,var_treat]==1]) + log(2)*mean(data$C[data[,var_treat]==1])
  a21_2<-((-1)*((data[,var_y]-mu0_hat)*(1-data[,var_treat])*(ps/(1-ps)))%*%X)/nrow(data)
  a21<-rbind(a21_1,a21_2)
  
  mu1_hat<-mean(result_obj$fitted.values[data[,var_treat]==1])
  #ind<-which(data[,var_treat]==1)
  #mu1_hat<-mu0_hat+log(2)
  b21_1<-(data[,var_treat]*(data[,var_y]-mu1_hat)*(data[,var_treat]-ps))%*%X
  b21_2<-((data[,var_y]-mu0_hat)*(1-data[,var_treat])*(ps/(1-ps))*(data[,var_treat]-ps))%*%X
  b21<-rbind(b21_1,b21_2)/nrow(data)
  
  psi_1<-weight*data[,var_treat]*(data[,var_y]-mu1_hat)
  psi_0<-weight*(data[,var_y]-mu0_hat)*(1-data[,var_treat])
  psi_1_0<-rbind(psi_1,psi_0)
  b22<-(psi_1_0%*%t(psi_1_0))/nrow(data)
  
  p1<-mean(data[,var_treat])
  v_1<-solve(a11)
  v_2<-(1/p1)*v_1%*%t(-a21+b21)
  v_3<-(1/p1)*(-a21+b21)%*%v_1
  v_4<-(1/p1)^2*{(a21-b21)%*%v_1%*%t(a21-b21)-b21%*%v_1%*%t(b21)+b22}
  V_total<-as.matrix(rbind(cbind(v_1,v_2),cbind(v_3,v_4)))
  
  g<-c(0,rep(0,length(cov)),1,-1)
  result<-t(g)%*%V_total%*%g
  return(result/nrow(data))
}

### -------------------------------------------------------------------------



### 2) DR Estimation -----------------------------------------------------
### 2-1) DR Estimator function -----------------------------------------------
DR_estimator<-function(estimate,data,var_treat,var_y,cov){
  
  result<-list()
  
  mydata<-data[c(var_treat,cov)]
  
  myformula<-as.formula(sprintf("%s~.",var_treat))
  fit<-glm(formula=myformula,data=mydata,family='binomial')
  result$fit<-fit
  
  ps<-fit$fitted.values
  
  myformula1<-as.formula(sprintf("%s~.",var_y))
  
  #ind_mu0<-which(data[,var_treat]==0)
  #mu0_df<-data[ind_mu1,c(var_y,cov)]
  
  out_model<-lm(formula=myformula1,data=data)
  new_dt_mu0<-model.matrix(out_model)
  new_dt_mu0[,which(colnames(new_dt_mu0)==var_treat)]<-rep(0,nrow(data))
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

### 2-2) Naive variance of DR estimator -------------------------------------
DR_Naive_var_estimator<-function(estimate,data,var_treat,var_y,cov){
  result<-DR_estimator(estimate,data,var_treat,var_y,cov)
  
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

### -------------------------------------------------------------------------

### 2-3) Sandwich robust variance estimator of DR Estimator -----------------
sandwich_var_DR<-function(estimate,data,var_treat,var_y,cov){
  
  result<-DR_estimator(estimate,data,var_treat,var_y,cov)
  fit<-result$fit
  
  dim1<-length(coef(result$out_model))+length(coef(fit))+2
  nu_theta_array<-array(0,dim=c(dim1,dim1,nrow(data)))
  #nu_hat_array<-array(0,dim=c(dim1,dim1,nrow(data)))
  
  X_1<-as.matrix(cbind(1,1,data[,cov]))
  X_0<-as.matrix(cbind(1,0,data[,cov]))
  exp_beta_X<-fit$fitted.values / (1-fit$fitted.values)
  
  if(estimate=="ATE"){
    
    #### partial derivative ####
    for(i in 1:nrow(data)){
      nu1_3<-matrix(((data[,var_treat][i]*(1/fit$fitted.values[i]))-1)*X_1[i,],nrow=1)         
      nu1_4<-matrix((-1)*as.vector((data[,var_treat][i]*(data[,var_y][i]- coef(result$out_model)%*%X_1[i,]))/exp_beta_X[i])*model.matrix(fit)[i,],nrow=1)
      nu1<-cbind(1,0,nu1_3,nu1_4)
      colnames(nu1)<-NULL
      
      nu2_3<-matrix(((-(1+exp_beta_X[i]))*data[,var_treat][i]+exp_beta_X[i])*X_0[i,],nrow=1)
      nu2_4<- as.vector((-1)*exp_beta_X[i]*((1-data[,var_treat][i])*(data[,var_y][i]-(coef(result$out_model)%*%X_0[i,]))))*model.matrix(fit)[i,]
      nu2<-cbind(0,1,matrix(nu2_3,nrow=1),matrix(nu2_4,nrow=1))
      colnames(nu2)<-NULL
      
      nu3_3<--model.matrix(result$out_model)[i,]%*%t(model.matrix(result$out_model)[i,])
      nu3<-cbind(0,0,nu3_3,
                 matrix(0,ncol=length(coef(fit)),nrow=dim(t(model.matrix(result$out_model))%*%model.matrix(result$out_model))[1]))
      colnames(nu3)<-NULL
      
      nu4_4<- -((fit$fitted.values[i])*(1-fit$fitted.values[i]))*(model.matrix(fit)[i,]%*%t(model.matrix(fit)[i,]))
      nu4<-cbind(0,0,matrix(0,ncol=length(coef(result$out_model)),nrow=dim(nu4_4)[1]),
                 nu4_4)
      colnames(nu4)<-NULL
      
      nu_theta<-rbind(nu1,nu2,nu3,nu4)
      nu_theta_array[,,i]<-nu_theta
      
    }
    nu_theta_total<-apply(nu_theta_array,c(1,2),sum)
    
    ######
    
    ### pi-hat ###
    est_1_hat<-result$mu1_dr-((data[,var_treat]*data[,var_y]-(data[,var_treat]-fit$fitted.values)*(coef(result$out_model) %*%t(X_1)))/fit$fitted.values)
    est_2_hat<-result$mu1_dr-result$est-(((1-data[,var_treat])*data[,var_y]+(data[,var_treat]-fit$fitted.values)*(coef(result$out_model)%*%t(X_0)))/(1-fit$fitted.values))
    est_3_hat<-as.vector(data[,var_y] - coef(result$out_model) %*%t(X_1))*model.matrix(result$out_model)
    est_4_hat<-as.vector(data[,var_treat]-fit$fitted.values)*model.matrix(fit)
    
    estimating_equation_true <- as.matrix(cbind(t(est_1_hat),t(est_2_hat),est_3_hat,est_4_hat)) # n by 9
    
    var_middle<-(t(estimating_equation_true)%*%(estimating_equation_true))
    
    cov_var_matrix<-solve(nu_theta_total)%*%var_middle%*%t(solve(nu_theta_total))
    var_est<-(cov_var_matrix[1,1]+cov_var_matrix[2,2]-2*cov_var_matrix[1,2])
    
  }
  
  
  else if(estimate=="ATT"){
    
    #### partial derivative ####
    for(i in 1:nrow(data)){
      nu1_3<-matrix(rep(0,length(coef(result$out_model))),nrow=1)         
      nu1_4<-matrix(rep(0,length(coef(fit))),nrow=1)
      nu1<-cbind(1,0,nu1_3,nu1_4)
      colnames(nu1)<-NULL
      
      nu2_3<-((-1)*nrow(data)/sum(data[,var_treat]==1))*(matrix((((1+exp_beta_X[i]))*data[,var_treat][i]-exp_beta_X[i])*X_0[i,],nrow=1))
      nu2_4<-((-1)*nrow(data)/sum(data[,var_treat]==1))*(as.vector(exp_beta_X[i]*((1-data[,var_treat][i])*(data[,var_y][i]-(coef(result$out_model)%*%X_0[i,]))))*model.matrix(fit)[i,])
      nu2<-cbind(0,1,matrix(nu2_3,nrow=1),matrix(nu2_4,nrow=1))
      colnames(nu2)<-NULL
      
      nu3_3<--model.matrix(result$out_model)[i,]%*%t(model.matrix(result$out_model)[i,])
      nu3_4<-matrix(0,nrow=length(coef(result$out_model)),ncol=length(coef(fit)))
      nu3<-cbind(0,0,nu3_3,nu3_4)
      colnames(nu3)<-NULL
      
      nu4_3<-matrix(0,nrow=length(coef(fit)),ncol=length(coef(result$out_model)))
      nu4_4<- -((fit$fitted.values[i])*(1-fit$fitted.values[i]))*(model.matrix(fit)[i,]%*%t(model.matrix(fit)[i,]))
      nu4<-cbind(0,0,nu4_3,nu4_4)
      colnames(nu4)<-NULL
      
      nu_theta<-rbind(nu1,nu2,nu3,nu4)
      nu_theta_array[,,i]<-nu_theta
      
    }
    nu_theta_total<-apply(nu_theta_array,c(1,2),sum)
    
    ######
    
    ### theta-hat ###
    nu1_hat<-result$mu1_dr-(nrow(data)/sum(data[,var_treat]==1))*(data[,var_y]*data[,var_treat]) # vector(100 x 1)
    nu2_hat<-(result$mu1_dr-result$est)-(nrow(data)/sum(data[,var_treat]==1))*((((1-data[,var_treat])*data[,var_y]*fit$fitted.values+(coef(result$out_model)%*%t(X_0))*(data[,var_treat][i]-fit$fitted.values))/(1-fit$fitted.values))) # 1 x 100
    nu3_hat<-as.matrix((result$out_model$residuals)*model.matrix(result$out_model)) # 100 x 4
    nu4_hat<-as.matrix(fit$residuals*model.matrix(fit)) # 100 x 3
    
    nu_hat<-as.matrix(cbind(nu1_hat,t(nu2_hat),nu3_hat,nu4_hat))
    nu_hat_total<-t(nu_hat)%*%nu_hat
    
    cov_var_matrix<-solve(nu_theta_total)%*%nu_hat_total%*%solve(t(nu_theta_total))
    var_est<-(cov_var_matrix[1,1]+cov_var_matrix[2,2]-2*cov_var_matrix[1,2])
    
  }
  
  
  return(var_est)
}
### -------------------------------------------------------------------------

### -----------------------------------------------------
### J_function -----------------------------------------------
J_function<-function(data,data_L,var_treat,var_y,cov){
  
  X_P<-as.matrix(cbind(1,data_L[,cov]))
  X_out<-as.matrix(cbind(1,data_L[,var_treat],data_L[,cov]))
  X_0<-as.matrix(cbind(1,0,data_L[,cov]))
  X_1<-as.matrix(cbind(1,1,data_L[,cov]))
  
  # true_ps<-as.vector(inv.logit(-2+as.matrix(data_L[,cov])%*%as.vector(rep(0.01,2))))
  
  DR_ATT<-DR_estimator("ATT",data,var_treat,var_y,cov)
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
crossprod_est_equ<-function(data,data_L,var_treat,var_y,cov){
  
  X_P<-as.matrix(cbind(1,data_L[,cov]))
  X_out<-as.matrix(cbind(1,data_L[,var_treat],data_L[,cov]))
  X_0<-as.matrix(cbind(1,0,data_L[,cov]))
  X_1<-as.matrix(cbind(1,1,data_L[,cov]))
  
  #true_ps<-as.vector(inv.logit(1+as.matrix(data[,cov])%*%as.vector(rep(0.01,2))))
  
  DR_ATT<-DR_estimator("ATT",data,var_treat,var_y,cov)
  #fit<-DR_ATT$fit
  theta<-matrix(c(DR_ATT$mu1_dr, DR_ATT$mu1_dr-DR_ATT$est, coef(DR_ATT$out_model), coef(DR_ATT$fit)), nrow=1)
  
  nu1<-theta[1]
  nu0<-theta[2]
  outcome_model_coef<-theta[3:(length(cov)+2+3-1)]
  ps_model_coef<-theta[(length(theta)-length(cov)):length(theta)]
  
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
