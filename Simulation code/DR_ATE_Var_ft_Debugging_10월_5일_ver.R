# Simulation code 10/5 Version --------------------------------------------
### <What to Do> ------------------------------------------------------------
## 1. Debugging Sandwich Variance Estimator function of DR ATE Estimator
### -------------------------------------------------------------------------



### 1. import library ----------------------------------------------------------
library(boot)
library(tableone)
library(survey)
library(Hmisc)
library(geepack)
library(geex)
library(drgee)


### 2. Hardcoding function made by Me ---------------------------------------

### 2-1) IPW Estimation -----------------------------------------------------
### 2-1-1) weight generation function - IPTW -----------------------------------------
weight_make<-function(var_treat,var_cov,estimate,data){
  result<-list()
  mydata<-data[c(var_treat,var_cov)]
  
  myformula<-as.formula(sprintf("%s~.",var_treat))
  fit<-glm(formula=myformula,data=mydata,family='binomial')
  ps<-fit$fitted.values
  
  result$model<-fit
  result$ps<-ps
  #ps<-predict(psmodel,type='response')
  
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



### 2-1-2) Covariate balance check function - with no package - IPTW -----------------
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



### 2-1-3) Sandwich variance function - IPTW ATE ------------------------------
sandwich_var_ATE<-function(obj,data){
  
  X = model.matrix(obj)
  weight<-weights(obj)
  new_X<-sqrt(weight)*X
  bread<-solve(crossprod(new_X))*as.vector(nrow(data))
  psi<-as.vector(ATE_untrimmed$residuals)*weight*X
  meat<-crossprod(as.matrix(psi))/nrow(psi)
  vr<-bread%*%meat%*%bread/nrow(data)
  
  return(vr)
  
}
### -------------------------------------------------------------------------


### 2-1-4) Sandwich variance function - IPTW ATT ------------------------------
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
  #mu0_hat<-mean(result_obj$fitted.values[data[,var_treat]==0])
  mu0_hat<-log(1.5)*mean(data$B[data[,var_treat]==1]) + log(2)*mean(data$C[data[,var_treat]==1])
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



### 2-2) DR Estimation -----------------------------------------------------
### 2-2-1) DR Estimator function -----------------------------------------------
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



### 2-2-2). Naive variance of DR estimator -------------------------------------
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

### 2-2-3) Sandwich robust variance estimator of DR Estimator -----------------
sandwich_var_DR<-function(estimate,data,var_treat,var_y,cov){
  
  result<-DR_estimator(estimate,data,var_treat,var_y,cov)
  fit<-result$fit
  
  dim1<-length(coef(result$out_model))+length(coef(fit))+2
  nu_theta_array<-array(0,dim=c(dim1,dim1,nrow(data)))
  nu_hat_array<-array(0,dim=c(dim1,dim1,nrow(data)))
  
  X_1<-as.matrix(cbind(1,1,data[,cov]))
  X_0<-as.matrix(cbind(1,0,data[,cov]))
  
  if(estimate=="ATE"){
    exp_beta_X<-fit$fitted.values / (1-fit$fitted.values)
    
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
    dim1<-length(coef(result$out_model))+length(coef(fit))+2
    nu_hat_array<-array(0,dim=c(dim1,1,nrow(data)))
    x_beta<-coef(fit)%*%t(model.matrix(fit))
    
    for(j in 1:nrow(data)){
      
      nu1_hat<-result$mu1_dr-((data[,var_treat][j]*data[,var_y][j]-(data[,var_treat][j]-(exp(x_beta[j])/(1+exp(x_beta[j]))))*(coef(result$out_model)%*%X_1[j,]))/(exp(x_beta[j])/(1+exp(x_beta[j]))))[1]
      nu2_hat<-(result$mu1_dr-result$est)-(((1-data[,var_treat][j])*data[,var_y][j]+(data[,var_treat][j]-(exp(x_beta[j])/(1+exp(x_beta[j]))))*(coef(result$out_model)%*%X_0[j,]))/(1-(exp(x_beta[j])/(1+exp(x_beta[j])))))[1]
      nu3_hat<-as.matrix((result$out_model$residuals[j])*model.matrix(result$out_model)[j,],ncol=1)
      nu4_hat<-as.matrix((exp(x_beta[j])/(1+exp(x_beta[j])))*model.matrix(fit)[j,],ncol=1)
      
      nu_hat<-as.matrix(rbind(nu1_hat,nu2_hat,nu3_hat,nu4_hat))
      nu_hat_array[,,j]<-nu_hat
      
    }
    nu_hat_total<-apply(nu_hat_array,c(1,2),sum)
    
    var_middle<-nu_hat_total%*%t(nu_hat_total)
    
    cov_var_matrix<-solve(nu_theta_total)%*%var_middle%*%t(solve(nu_theta_total))
    var_est<-(cov_var_matrix[1,1]+cov_var_matrix[2,2]-2*cov_var_matrix[1,2])
    
  }
  
  
  else if(estimate=="ATT"){
    exp_beta_X <- fit$fitted.values / (1-fit$fitted.values)
    
    #### partial derivative ####
    for(i in 1:nrow(data)){
      nu1_3<-matrix(rep(0,length(coef(result$out_model))),nrow=1)         
      nu1_4<-matrix(rep(0,length(coef(fit))),nrow=1)
      nu1<-cbind(1,0,nu1_3,nu1_4)
      colnames(nu1)<-NULL
      
      nu2_3<-(nrow(data)/sum(data[,var_treat]))*(matrix(((-(1+exp_beta_X[i]))*data[,var_treat][i]+exp_beta_X[i])*X_0[i,],nrow=1))
      nu2_4<-(nrow(data)/sum(data[,var_treat]))*(as.vector((-1)*exp_beta_X[i]*((1-data[,var_treat][i])*(data[,var_y][i]-(coef(result$out_model)%*%X_0[i,]))))*model.matrix(fit)[i,])
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
    for(j in 1:nrow(data)){
      
      nu1_hat<-result$mu1_dr-((data[,var_y][j]*data[,var_treat][j])*(nrow(data)/sum(data[,var_treat])))
      nu2_hat<-(result$mu1_dr-result$est)-(nrow(data)/sum(data[,var_treat]))*(((1-data[,var_treat][j])*data[,var_y][j]*fit$fitted.values[j]+(coef(result$out_model)%*%X_0[j,])))/(1-fit$fitted.values[j])
      nu3_hat<-as.matrix((result$out_model$residuals[j])*model.matrix(result$out_model)[j,],ncol=1)
      nu4_hat<-as.matrix(fit$residuals[j]*model.matrix(fit)[j,],ncol=1)
      
      nu_hat<-as.matrix(rbind(nu1_hat,nu2_hat,nu3_hat,nu4_hat))
      nu_hat_array[,,j]<-nu_hat%*%t(nu_hat)
      
    }
    
    nu_hat_total<-apply(nu_hat_array,c(1,2),sum)
    
    cov_var_matrix<-solve(nu_theta_total)%*%nu_hat_total%*%solve(t(nu_theta_total))
    var_est<-(cov_var_matrix[1,1]+cov_var_matrix[2,2]-2*cov_var_matrix[1,2])
    
  }
  
  
  return(var_est)
}

### -------------------------------------------------------------------------
### check the function itself - Sandwich variance of DR ATT -----------------
B_sample<-read.csv("B_sample_consistency.csv",header=TRUE)
C_sample<-read.csv("C_sample_consistency.csv",header=TRUE)
#U_sample<-read.csv("U_sample.csv",header=TRUE)
E_sample<-read.csv("E_sample_consistency.csv",header=TRUE)
Y_sample<-read.csv("Y_sample_consistency.csv",header=TRUE)

E<-E_sample[,1]
B<-B_sample[,1]
#U<-U_sample[,1]
C<-C_sample[,1]
Y<-Y_sample[,1]
data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
cov<-c("B","C")
var_treat<-"E"
var_y<-"Y"
estimate<-"ATT"
result<-DR_estimator(estimate,data,var_treat,var_y,cov)

### -------------------------------------------------------------------------



### 2-3) Consistency check of Variance estimator of DR ATT estimator ----------------------------------
### 2-3-1) Data generation --------------------------------------------------
### - with 1000 set which # of obs = 100 -----------------------------------------
B_sample<-matrix(0,nrow=100,ncol=1000)
C_sample<-matrix(0,nrow=100,ncol=1000)
U_sample<-matrix(0,nrow=100,ncol=1000)
E_sample<-matrix(0,nrow=100,ncol=1000)
Y_sample<-matrix(0,nrow=100,ncol=1000)

#effect<-c(log(1.2),log(1.5),log(2),log(2))
effect<-c(log(1.5),log(2),log(2))

### 1000th replication data generating -----------------------------------------
for(repl in 1:1000){
  delta0<--2
  delta_b<-0.01
  delta_c<-0.01
  set.seed(123*repl)
  sample<-replicate(100,expr={
    B<-rbinom(1,1,prob=0.5)
    C<-rnorm(1,0,1)
    #U<-runif(1)
    p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
    E<-rbinom(1,1,prob=p_z)
    c(B,C,E)
  })
  B_sample[,repl]<-sample[1,]
  C_sample[,repl]<-sample[2,]
  #U_sample[,repl]<-sample[3,]
  E_sample[,repl]<-sample[3,]
  #X_sample<-cbind(B_sample[,repl],C_sample[,repl],U_sample[,repl],E_sample[,repl])
  X_sample<-cbind(B_sample[,repl],C_sample[,repl],E_sample[,repl])
  
  # Y random sampling - continuous
  Y_sample[,repl]<-as.matrix(X_sample)%*%effect+rnorm(nrow(X_sample),0,1)
  
  # Y_sample random sampling - binary                    
  #prob<-inv.logit(as.matrix(X_sample)%*%effect)
  #Y_sample_binary<-rbinom(n=nrow(X_sample), size=1, prob=prob)
}

## Data store ##
write.csv(B_sample,file="B_sample_consistency.csv",row.names=FALSE)
write.csv(C_sample,file="C_sample_consistency.csv",row.names=FALSE)
#write.csv(U_sample,file="U_sample.csv",row.names=FALSE)
write.csv(E_sample,file="E_sample_consistency.csv",row.names=FALSE)
write.csv(Y_sample,file="Y_sample_consistency.csv",row.names=FALSE)


### 2-3-1-2) Data import -------------------------------------------------------------
B_sample<-read.csv("B_sample_consistency.csv",header=TRUE)
C_sample<-read.csv("C_sample_consistency.csv",header=TRUE)
#U_sample<-read.csv("U_sample.csv",header=TRUE)
E_sample<-read.csv("E_sample_consistency.csv",header=TRUE)
Y_sample<-read.csv("Y_sample_consistency.csv",header=TRUE)


### 2-3-1-3) 1000th Replication for check the estimating equation hat with true parameter set --------------
z_hat <- matrix(0, nrow=1000, ncol=9)
z_true <- matrix(0, nrow=1000, ncol=9)

for(i in 1:1000){
  E<-E_sample[,i]
  B<-B_sample[,i]
  #U<-U_sample[,1]
  C<-C_sample[,i]
  Y<-Y_sample[,i]
  data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
  cov<-c("B","C")
  cov_type<-c("binary","continuous")
  
  DR_ATE<-DR_estimator("ATE",data,"E","Y",cov)
  fit<-DR_ATE$fit
  
  X_1<-as.matrix(cbind(1,1,data[,cov]))
  X_0<-as.matrix(cbind(1,0,data[,cov]))
  
  est_1_hat<-DR_ATE$mu1_dr-((data$E*data$Y-(data$E-fit$fitted.values)*(coef(DR_ATE$out_model) %*% t(X_1)))/fit$fitted.values)
  est_2_hat<-(DR_ATE$mu1_dr-DR_ATE$est)-(((1-data$E)*data$Y+(data$E-fit$fitted.values)*(coef(DR_ATE$out_model)%*%t(X_0)))/(1-fit$fitted.values))
  est_3_hat<-model.matrix(DR_ATE$out_model)*DR_ATE$out_model$residuals
  est_4_hat<-(data$E-fit$fitted.values)*model.matrix(fit)
  
  estimating_equation_hat <- as.matrix(rbind(est_1_hat,est_2_hat,t(est_3_hat),t(est_4_hat)))
  
  z_hat[i,]<-apply(estimating_equation_hat,1,mean)
  
  true_effect <- c(log(2),log(1.5), log(2))
  true_ps<-inv.logit(-2 + 0.01*data$B + 0.01*data$C)
  
  est_1_true<-(log(1.5)*0.5 + log(2))-((data$E*data$Y-(data$E-true_ps)*(as.vector(c(0, true_effect)) %*% t(X_1)))/true_ps)
  est_2_true<-(log(1.5)*0.5)-(((1-data$E)*data$Y+(data$E-true_ps)*(as.vector(c(0, true_effect))%*%t(X_0)))/(1-true_ps))
  est_3_true<-model.matrix(DR_ATE$out_model)*as.vector(data$Y - as.vector(true_effect)%*%t(as.matrix(data[,c("E",cov)])))
  est_4_true<-(data$E-true_ps)*model.matrix(fit)
  
  estimating_equation_true <- as.matrix(rbind(est_1_true,est_2_true,t(est_3_true),t(est_4_true)))
  
  z_true[i,]<-apply(estimating_equation_true,1,mean)
  
  
}
var(z_hat)
var(z_true)


### 2-3-2) Data generation --------------------------------------------------
### - with 1000 set which # of obs = 1000 -----------------------------------------
B_sample<-matrix(0,nrow=1000,ncol=1000)
C_sample<-matrix(0,nrow=1000,ncol=1000)
U_sample<-matrix(0,nrow=1000,ncol=1000)
E_sample<-matrix(0,nrow=1000,ncol=1000)
Y_sample<-matrix(0,nrow=1000,ncol=1000)

#effect<-c(log(1.2),log(1.5),log(2),log(2))
effect<-c(log(1.5),log(2),log(2))

### 1000th replication data generating -----------------------------------------
for(repl in 1:1000){
  delta0<--2
  delta_b<-0.01
  delta_c<-0.01
  set.seed(123*repl)
  sample<-replicate(1000,expr={
    B<-rbinom(1,1,prob=0.5)
    C<-rnorm(1,0,1)
    #U<-runif(1)
    p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
    E<-rbinom(1,1,prob=p_z)
    c(B,C,E)
  })
  B_sample[,repl]<-sample[1,]
  C_sample[,repl]<-sample[2,]
  #U_sample[,repl]<-sample[3,]
  E_sample[,repl]<-sample[3,]
  #X_sample<-cbind(B_sample[,repl],C_sample[,repl],U_sample[,repl],E_sample[,repl])
  X_sample<-cbind(B_sample[,repl],C_sample[,repl],E_sample[,repl])
  
  # Y random sampling - continuous
  Y_sample[,repl]<-as.matrix(X_sample)%*%effect+rnorm(nrow(X_sample),0,1)
  
  # Y_sample random sampling - binary                    
  #prob<-inv.logit(as.matrix(X_sample)%*%effect)
  #Y_sample_binary<-rbinom(n=nrow(X_sample), size=1, prob=prob)
}

### Data store ---------------------------------------------------------
write.csv(B_sample,file="B_sample_1000_consistency.csv",row.names=FALSE)
write.csv(C_sample,file="C_sample_1000_consistency.csv",row.names=FALSE)
#write.csv(U_sample,file="U_sample.csv",row.names=FALSE)
write.csv(E_sample,file="E_sample_1000_consistency.csv",row.names=FALSE)
write.csv(Y_sample,file="Y_sample_1000_consistency.csv",row.names=FALSE)


### 2-3-2-2) Data import -------------------------------------------------------------
B_sample<-read.csv("B_sample_1000_consistency.csv",header=TRUE)
C_sample<-read.csv("C_sample_1000_consistency.csv",header=TRUE)
#U_sample<-read.csv("U_sample.csv",header=TRUE)
E_sample<-read.csv("E_sample_1000_consistency.csv",header=TRUE)
Y_sample<-read.csv("Y_sample_1000_consistency.csv",header=TRUE)


### 2-3-2-3) 1000th Replication for comparison Estimating equation --------------
z_hat <- matrix(0, nrow=1000, ncol=9)
z_true <- matrix(0, nrow=1000, ncol=9)

for(i in 1:1000){
  E<-E_sample[,i]
  B<-B_sample[,i]
  #U<-U_sample[,1]
  C<-C_sample[,i]
  Y<-Y_sample[,i]
  data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
  cov<-c("B","C")
  cov_type<-c("binary","continuous")
  
  DR_ATE<-DR_estimator("ATE",data,"E","Y",cov)
  fit<-DR_ATE$fit
  
  X_1<-as.matrix(cbind(1,1,data[,cov]))
  X_0<-as.matrix(cbind(1,0,data[,cov]))
  
  est_1_hat<-DR_ATE$mu1_dr-((data$E*data$Y-(data$E-fit$fitted.values)*(coef(DR_ATE$out_model) %*% t(X_1)))/fit$fitted.values)
  est_2_hat<-(DR_ATE$mu1_dr-DR_ATE$est)-(((1-data$E)*data$Y+(data$E-fit$fitted.values)*(coef(DR_ATE$out_model)%*%t(X_0)))/(1-fit$fitted.values))
  est_3_hat<-model.matrix(DR_ATE$out_model)*DR_ATE$out_model$residuals
  est_4_hat<-(data$E-fit$fitted.values)*model.matrix(fit)
  
  estimating_equation_hat <- as.matrix(rbind(est_1_hat,est_2_hat,t(est_3_hat),t(est_4_hat)))
  
  z_hat[i,]<-apply(estimating_equation_hat,1,mean)
  
  true_effect <- c(log(2),log(1.5), log(2))
  true_ps<-inv.logit(-2 + 0.01*data$B + 0.01*data$C)
  
  est_1_true<-(log(1.5)*0.5 + log(2))-((data$E*data$Y-(data$E-true_ps)*(as.vector(c(0, true_effect)) %*% t(X_1)))/true_ps)
  est_2_true<-(log(1.5)*0.5)-(((1-data$E)*data$Y+(data$E-true_ps)*(as.vector(c(0, true_effect))%*%t(X_0)))/(1-true_ps))
  est_3_true<-model.matrix(DR_ATE$out_model)*as.vector(data$Y - as.vector(true_effect)%*%t(as.matrix(data[,c("E",cov)])))
  est_4_true<-(data$E-true_ps)*model.matrix(fit)
  
  estimating_equation_true <- as.matrix(rbind(est_1_true,est_2_true,t(est_3_true),t(est_4_true)))
  
  z_true[i,]<-apply(estimating_equation_true,1,mean)
  
  
}
var(z_hat)
var(z_true)


### 2-3-3) Data generation --------------------------------------------------
### - with 1000 set which # of obs = 10000 -----------------------------------------
B_sample<-matrix(0,nrow=10000,ncol=1000)
C_sample<-matrix(0,nrow=10000,ncol=1000)
U_sample<-matrix(0,nrow=10000,ncol=1000)
E_sample<-matrix(0,nrow=10000,ncol=1000)
Y_sample<-matrix(0,nrow=10000,ncol=1000)

#effect<-c(log(1.2),log(1.5),log(2),log(2))
effect<-c(log(1.5),log(2),log(2))

### 1000th replication data generating -----------------------------------------
for(repl in 1:1000){
  delta0<--2
  delta_b<-0.01
  delta_c<-0.01
  set.seed(123*repl)
  sample<-replicate(10000,expr={
    B<-rbinom(1,1,prob=0.5)
    C<-rnorm(1,0,1)
    #U<-runif(1)
    p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
    E<-rbinom(1,1,prob=p_z)
    c(B,C,E)
  })
  B_sample[,repl]<-sample[1,]
  C_sample[,repl]<-sample[2,]
  #U_sample[,repl]<-sample[3,]
  E_sample[,repl]<-sample[3,]
  #X_sample<-cbind(B_sample[,repl],C_sample[,repl],U_sample[,repl],E_sample[,repl])
  X_sample<-cbind(B_sample[,repl],C_sample[,repl],E_sample[,repl])
  
  # Y random sampling - continuous
  Y_sample[,repl]<-as.matrix(X_sample)%*%effect+rnorm(nrow(X_sample),0,1)
  
  # Y_sample random sampling - binary                    
  #prob<-inv.logit(as.matrix(X_sample)%*%effect)
  #Y_sample_binary<-rbinom(n=nrow(X_sample), size=1, prob=prob)
}

### Data store ---------------------------------------------------------
write.csv(B_sample,file="B_sample_10000_consistency.csv",row.names=FALSE)
write.csv(C_sample,file="C_sample_10000_consistency.csv",row.names=FALSE)
#write.csv(U_sample,file="U_sample.csv",row.names=FALSE)
write.csv(E_sample,file="E_sample_10000_consistency.csv",row.names=FALSE)
write.csv(Y_sample,file="Y_sample_10000_consistency.csv",row.names=FALSE)


### 2-3-3-2) Data import -------------------------------------------------------------
B_sample<-read.csv("B_sample_10000_consistency.csv",header=TRUE)
C_sample<-read.csv("C_sample_10000_consistency.csv",header=TRUE)
#U_sample<-read.csv("U_sample.csv",header=TRUE)
E_sample<-read.csv("E_sample_10000_consistency.csv",header=TRUE)
Y_sample<-read.csv("Y_sample_10000_consistency.csv",header=TRUE)


### 2-3-3-3) 1000th Replication for comparison Estimating equation --------------
z_hat <- matrix(0, nrow=1000, ncol=9)
z_true <- matrix(0, nrow=1000, ncol=9)

for(i in 1:1000){
  E<-E_sample[,i]
  B<-B_sample[,i]
  #U<-U_sample[,1]
  C<-C_sample[,i]
  Y<-Y_sample[,i]
  data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
  cov<-c("B","C")
  cov_type<-c("binary","continuous")
  
  DR_ATE<-DR_estimator("ATE",data,"E","Y",cov)
  fit<-DR_ATE$fit
  
  X_1<-as.matrix(cbind(1,1,data[,cov]))
  X_0<-as.matrix(cbind(1,0,data[,cov]))
  
  est_1_hat<-DR_ATE$mu1_dr-((data$E*data$Y-(data$E-fit$fitted.values)*(coef(DR_ATE$out_model) %*% t(X_1)))/fit$fitted.values)
  est_2_hat<-(DR_ATE$mu1_dr-DR_ATE$est)-(((1-data$E)*data$Y+(data$E-fit$fitted.values)*(coef(DR_ATE$out_model)%*%t(X_0)))/(1-fit$fitted.values))
  est_3_hat<-model.matrix(DR_ATE$out_model)*DR_ATE$out_model$residuals
  est_4_hat<-(data$E-fit$fitted.values)*model.matrix(fit)
  
  estimating_equation_hat <- as.matrix(rbind(est_1_hat,est_2_hat,t(est_3_hat),t(est_4_hat)))
  
  z_hat[i,]<-apply(estimating_equation_hat,1,mean)
  
  true_effect <- c(log(2),log(1.5), log(2))
  true_ps<-inv.logit(-2 + 0.01*data$B + 0.01*data$C)
  
  est_1_true<-(log(1.5)*0.5 + log(2))-((data$E*data$Y-(data$E-true_ps)*(as.vector(c(0, true_effect)) %*% t(X_1)))/true_ps)
  est_2_true<-(log(1.5)*0.5)-(((1-data$E)*data$Y+(data$E-true_ps)*(as.vector(c(0, true_effect))%*%t(X_0)))/(1-true_ps))
  est_3_true<-model.matrix(DR_ATE$out_model)*as.vector(data$Y - as.vector(true_effect)%*%t(as.matrix(data[,c("E",cov)])))
  est_4_true<-(data$E-true_ps)*model.matrix(fit)
  
  estimating_equation_true <- as.matrix(rbind(est_1_true,est_2_true,t(est_3_true),t(est_4_true)))
  
  z_true[i,]<-apply(estimating_equation_true,1,mean)
  
  
}
var(z_hat)
var(z_true)


### 2-3-4) Data generation --------------------------------------------------
### - with 1000 set which # of obs = 20000 -----------------------------------------
B_sample<-matrix(0,nrow=20000,ncol=1000)
C_sample<-matrix(0,nrow=20000,ncol=1000)
U_sample<-matrix(0,nrow=20000,ncol=1000)
E_sample<-matrix(0,nrow=20000,ncol=1000)
Y_sample<-matrix(0,nrow=20000,ncol=1000)

#effect<-c(log(1.2),log(1.5),log(2),log(2))
effect<-c(log(1.5),log(2),log(2))

### 1000th replication data generating -----------------------------------------
for(repl in 1:1000){
  delta0<--2
  delta_b<-0.01
  delta_c<-0.01
  set.seed(123*repl)
  sample<-replicate(20000,expr={
    B<-rbinom(1,1,prob=0.5)
    C<-rnorm(1,0,1)
    #U<-runif(1)
    p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
    E<-rbinom(1,1,prob=p_z)
    c(B,C,E)
  })
  B_sample[,repl]<-sample[1,]
  C_sample[,repl]<-sample[2,]
  #U_sample[,repl]<-sample[3,]
  E_sample[,repl]<-sample[3,]
  #X_sample<-cbind(B_sample[,repl],C_sample[,repl],U_sample[,repl],E_sample[,repl])
  X_sample<-cbind(B_sample[,repl],C_sample[,repl],E_sample[,repl])
  
  # Y random sampling - continuous
  Y_sample[,repl]<-as.matrix(X_sample)%*%effect+rnorm(nrow(X_sample),0,1)
  
  # Y_sample random sampling - binary                    
  #prob<-inv.logit(as.matrix(X_sample)%*%effect)
  #Y_sample_binary<-rbinom(n=nrow(X_sample), size=1, prob=prob)
}

### Data store ---------------------------------------------------------
write.csv(B_sample,file="B_sample_20000_consistency.csv",row.names=FALSE)
write.csv(C_sample,file="C_sample_20000_consistency.csv",row.names=FALSE)
#write.csv(U_sample,file="U_sample.csv",row.names=FALSE)
write.csv(E_sample,file="E_sample_20000_consistency.csv",row.names=FALSE)
write.csv(Y_sample,file="Y_sample_20000_consistency.csv",row.names=FALSE)


### 2-3-4-2) Data import -------------------------------------------------------------
B_sample<-read.csv("B_sample_20000_consistency.csv",header=TRUE)
C_sample<-read.csv("C_sample_20000_consistency.csv",header=TRUE)
#U_sample<-read.csv("U_sample.csv",header=TRUE)
E_sample<-read.csv("E_sample_20000_consistency.csv",header=TRUE)
Y_sample<-read.csv("Y_sample_20000_consistency.csv",header=TRUE)


### 2-3-4-3) Replication for comparison My function with package --------------
DR_ATT_me<-c()

for(i in 1:1000){
  E<-E_sample[,i]
  B<-B_sample[,i]
  #U<-U_sample[,1]
  C<-C_sample[,i]
  Y<-Y_sample[,i]
  data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
  cov<-c("B","C")
  cov_type<-c("binary","continuous")
  
  ## my function
  DR_ATT_me[i]<-DR_estimator("ATT",data,"E","Y",cov)$est
  
}

sample_var_me<-var(DR_ATT_me)*20000
print(sample_var_me)
# [1] 8.968043

sandwich_var_DR("ATT",data,"E","Y",cov)*20000
# [1] 72.88298


### 2-3-5) Data generation --------------------------------------------------
### - with 1000 set which # of obs = 50000 -----------------------------------------
B_sample<-matrix(0,nrow=50000,ncol=1000)
C_sample<-matrix(0,nrow=50000,ncol=1000)
U_sample<-matrix(0,nrow=50000,ncol=1000)
E_sample<-matrix(0,nrow=50000,ncol=1000)
Y_sample<-matrix(0,nrow=50000,ncol=1000)

#effect<-c(log(1.2),log(1.5),log(2),log(2))
effect<-c(log(1.5),log(2),log(2))

### 1000th replication data generating -----------------------------------------
for(repl in 1:1000){
  delta0<--2
  delta_b<-0.01
  delta_c<-0.01
  set.seed(123*repl)
  sample<-replicate(50000,expr={
    B<-rbinom(1,1,prob=0.5)
    C<-rnorm(1,0,1)
    #U<-runif(1)
    p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
    E<-rbinom(1,1,prob=p_z)
    c(B,C,E)
  })
  B_sample[,repl]<-sample[1,]
  C_sample[,repl]<-sample[2,]
  #U_sample[,repl]<-sample[3,]
  E_sample[,repl]<-sample[3,]
  #X_sample<-cbind(B_sample[,repl],C_sample[,repl],U_sample[,repl],E_sample[,repl])
  X_sample<-cbind(B_sample[,repl],C_sample[,repl],E_sample[,repl])
  
  # Y random sampling - continuous
  Y_sample[,repl]<-as.matrix(X_sample)%*%effect+rnorm(nrow(X_sample),0,1)
  
  # Y_sample random sampling - binary                    
  #prob<-inv.logit(as.matrix(X_sample)%*%effect)
  #Y_sample_binary<-rbinom(n=nrow(X_sample), size=1, prob=prob)
}

### Data store ---------------------------------------------------------
write.csv(B_sample,file="B_sample_50000_consistency.csv",row.names=FALSE)
write.csv(C_sample,file="C_sample_50000_consistency.csv",row.names=FALSE)
#write.csv(U_sample,file="U_sample.csv",row.names=FALSE)
write.csv(E_sample,file="E_sample_50000_consistency.csv",row.names=FALSE)
write.csv(Y_sample,file="Y_sample_50000_consistency.csv",row.names=FALSE)


### 2-3-5-2) Data import -------------------------------------------------------------
B_sample<-read.csv("B_sample_50000_consistency.csv",header=TRUE)
C_sample<-read.csv("C_sample_50000_consistency.csv",header=TRUE)
#U_sample<-read.csv("U_sample.csv",header=TRUE)
E_sample<-read.csv("E_sample_50000_consistency.csv",header=TRUE)
Y_sample<-read.csv("Y_sample_50000_consistency.csv",header=TRUE)


### 2-3-5-3) Replication for comparison My function with package --------------
DR_ATT_me<-c()

for(i in 1:1000){
  E<-E_sample[,i]
  B<-B_sample[,i]
  #U<-U_sample[,1]
  C<-C_sample[,i]
  Y<-Y_sample[,i]
  data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
  cov<-c("B","C")
  cov_type<-c("binary","continuous")
  
  ## my function
  DR_ATT_me[i]<-DR_estimator("ATT",data,"E","Y",cov)$est
  
}

sample_var_me<-var(DR_ATT_me)*50000
print(sample_var_me)
# [1] 10.12156

sandwich_var_DR("ATT",data,"E","Y",cov)*50000
# [1] 10.22928


#### variance function -------------------------------------------------------
E<-E_sample[,1]
B<-B_sample[,1]
#U<-U_sample[,1]
C<-C_sample[,1]
Y<-Y_sample[,1]
data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
cov<-c("B","C")
var_treat<-"E"
var_y<-"Y"
estimate<-"ATE"

sandwich_var_DR("ATE",data,"E","Y",cov)*50000
# [1] 0.00642343
