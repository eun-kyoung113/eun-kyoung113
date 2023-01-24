### Coverage probability check Sandwich robust variance estimator of DR ATT ----------------
### 11월 22일 Version ---------------------------------------------------------

### <What to do> ------------------------------------------------------------
### 1. Check Coverage probability Sandwich robust variance estimator of DR ATT Estimator
###    -- D_true(true_theta) version
### -------------------------------------------------------------------------


### 0. Package load ---------------------------------------------------------
library(boot)
library(survey)
library(Hmisc)



### -------------------------------------------------------------------------
### 1. Hardcoding Estimator function -----------------------------------------------------

### DR Estimator function -----------------------------------------------
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
    result$mu0_dr<-mu0_dr
    
    result$est<-mu1_dr-mu0_dr
  }
  
  else if(estimate=='ATT'){
    ind_mu1<-which(data[,var_treat]==1)
    result$mu1_dr<-sum(data[,var_y]*data[,var_treat])/length(ind_mu1)
    result$est<-sum(data[,var_y]*data[,var_treat]-(((data[,var_y]*(1-data[,var_treat])*data$ps)+(mu0_X)*(data[,var_treat]-data$ps))/(1-data$ps)))/length(ind_mu1) 
  }
  
  return(result)
}

### Sandwich robust variance estimator of DR Estimator -----------------
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
    nu2_hat<-(result$mu1_dr-result$est)-(nrow(data)/sum(data[,var_treat]==1))*((((1-data[,var_treat])*data[,var_y]*fit$fitted.values+(coef(result$out_model)%*%t(X_0))*(data[,var_treat]-fit$fitted.values)))/(1-fit$fitted.values)) # 1 x 100
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
J_function<-function(theta,data,var_treat,var_y,cov){
  
  nu1<-theta[1]
  nu0<-theta[2]
  outcome_model_coef<-theta[3:(length(cov)+2+3-1)]
  ps_model_coef<-theta[(length(theta)-length(cov)):length(theta)]
  
  dim1<-length(theta)
  nu_theta_array<-array(NA,dim=c(dim1,dim1,nrow(data)))
  
  X_P<-as.matrix(cbind(1,data[,cov]))
  X_out<-as.matrix(cbind(1,data[,var_treat],data[,cov]))
  X_0<-as.matrix(cbind(1,0,data[,cov]))
  X_1<-as.matrix(cbind(1,1,data[,cov]))
  
  exp_beta_X<-exp(ps_model_coef%*%t(X_P))
  fitted_ps<-exp_beta_X/(1+exp_beta_X)
  
  for(i in 1:nrow(data)){
    
    nu1_3<-matrix(rep(0,length(outcome_model_coef)),nrow=1)      
    nu1_4<-matrix(rep(0,length(ps_model_coef)),nrow=1)
    nu1<-cbind(1,0,nu1_3,nu1_4)
    colnames(nu1)<-NULL
    
    nu2_3<-((-1)*nrow(data)/sum(data[,var_treat]==1))*(matrix((((1+exp_beta_X[i]))*data[,var_treat][i]-exp_beta_X[i])*X_0[i,],nrow=1))
    nu2_4<- ((-1)*nrow(data)/sum(data[,var_treat]==1))*(as.vector(exp_beta_X[i]*((1-data[,var_treat][i])*(data[,var_y][i]-(outcome_model_coef%*%X_0[i,]))))*X_P[i,])
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
### Monte carlo Expectation of crossprod of estimating_equation function -------
crossprod_est_equ<-function(theta,data,var_treat,var_y,cov){
  
  nu1<-theta[1]
  nu0<-theta[2]
  outcome_model_coef<-theta[3:(length(cov)+2+3-1)]
  ps_model_coef<-theta[(length(theta)-length(cov)):length(theta)]
  
  
  X_P<-as.matrix(cbind(1,data[,cov]))
  X_out<-as.matrix(cbind(1,data[,var_treat],data[,cov]))
  X_0<-as.matrix(cbind(1,0,data[,cov]))
  X_1<-as.matrix(cbind(1,1,data[,cov]))
  
  true_ps<-as.vector(inv.logit(1+as.matrix(data[,cov])%*%as.vector(rep(0.01,2))))
  #exp_beta_X<-exp(ps_model_coef%*%t(X_P))
  #fitted_ps<-exp_beta_X/(1+exp_beta_X)
  
  #DR_ATE<-DR_estimator("ATE",data,"E","Y",cov)
  #fit<-DR_ATE$fit
  
  # cross product of estimating_equation
  est_1_true<-nu1-(nrow(data)/sum(data[,var_treat]==1))*(data[,var_y]*data[,var_treat]) # vector(100 x 1)
  est_2_true<-nu0-(nrow(data)/sum(data[,var_treat]==1))*((((1-data[,var_treat])*data[,var_y]*true_ps+(outcome_model_coef%*%t(X_0))*(data[,var_treat]-true_ps)))/(1-true_ps))
  est_3_true<-as.vector(data[,var_y]-outcome_model_coef%*%t(X_out))*X_out
  est_4_true<-as.vector(data[,var_treat]-ps_model_coef%*%t(X_P))*X_P
  
  estimating_equation_true <- as.matrix(cbind(est_1_true,t(est_2_true),est_3_true,est_4_true)) # n by 9
  
  nu_theta_mean<-(t(estimating_equation_true)%*%(estimating_equation_true))/nrow(data)
  
  return(nu_theta_mean)
  
}


### -------------------------------------------------------------------------
### 1-1) Data generation ---------------------------------------------------------
N <- 100000
REPL <- 100
A_RATIO<-1
EFFECT<-c(log(1.5),log(2),log(2))

B<-matrix(NA, nrow=N, ncol=REPL)
C<-matrix(NA, nrow=N, ncol=REPL)
A<-matrix(NA, nrow=N, ncol=REPL)
Y<-matrix(NA, nrow=N, ncol=REPL)


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
  B[,repl]<-sample[1,]
  C[,repl]<-sample[2,]
  A[,repl]<-sample[3,]
  
  X_sample<-cbind(B[,repl],C[,repl],A[,repl])
  
  # Y random sampling - continuous
  Y[,repl]<-as.matrix(X_sample)%*%EFFECT+rnorm(nrow(X_sample),0,1)
}

### Data store ------------------------------------------------------
write.csv(B,file="B_100000.csv",row.names=FALSE)
write.csv(C,file="C_100000.csv",row.names=FALSE)
write.csv(A,file="A_100000.csv",row.names=FALSE)
write.csv(Y,file="Y_100000.csv",row.names=FALSE)




### -------------------------------------------------------------------------
### 2. Coverage probability check ----------------------------------------------

### Data import --------------------------------------------------

### - # of obs = 100000 -----------------------------------------
### Data import ------------------------------------------------------
B_100000<-read.csv("B_100000.csv")
C_100000<-read.csv("C_100000.csv")
A_100000<-read.csv("A_100000.csv")
Y_100000<-read.csv("Y_100000.csv")

theta_true<-c(log(1.5)*0.5+log(2),log(1.5)*0.5, c(0,log(2),log(1.5),log(2)), c(1,0.01,0.01))



### Getting 100th estimator ------------------------------------------------
REPL <- 100
est_DR_100<-rep(NA,length=REPL)

est_100_CV<-matrix(NA,nrow=REPL,ncol=3)
colnames(est_100_CV)<-c("CL","CU","contain")

cov<-c("B","C")
cov_type<-c("binary","continuous")

var_treat<-"A"
var_y<-"Y"

for(rep in 1:REPL){
  
  A<-A_100000[,rep]
  B<-B_100000[,rep]
  C<-C_100000[,rep]
  Y<-Y_100000[,rep]
  data<-data.frame("A"=A,"B"=B,"C"=C,"Y"=Y)
  
  DR_ATT<-DR_estimator("ATT",data,var_treat,var_y,cov)
  #theta_hat<-matrix(c(DR_ATE$mu1_dr, DR_ATE$mu1_dr-DR_ATE$est, coef(DR_ATE$out_model), coef(DR_ATE$fit)), nrow=1)
  
  j_true<-J_function(theta_true,data,var_treat,var_y,cov)
  crossprod<-crossprod_est_equ(theta_true,data,var_treat,var_y,cov)
  
  cov_true_star<-(solve(j_true)%*%crossprod%*%t(solve(j_true)))
  D_true<-sqrt(cov_true_star[1,1]+cov_true_star[2,2]-2*cov_true_star[1,2])
  
  est_DR_100[rep]<-DR_ATT$est
  est_100_CV[rep,1]<-est_DR_100[rep]-1.96*(D_true/sqrt(nrow(data)))
  est_100_CV[rep,2]<-est_DR_100[rep]+1.96*(D_true/sqrt(nrow(data)))
  est_100_CV[rep,3]<-ifelse((est_100_CV[rep,1]<=log(2) & est_100_CV[rep,2]>=log(2)), 1, 0)
  
}

D_true_coverage<-mean(est_100_CV[,3])
# [1] 0.95




