### Debugging sandwich robust variance estimator of DR ATT Estimator --------
### 11 / 11 Version ---------------------------------------------------------

### <What to Do> --------------------------
## D_theta_hat -- coverage probability ##
### ---------------------------------------

library(boot)
library(tableone)
library(survey)
library(Hmisc)
library(geepack)
library(geex)
library(drgee)


### -----------------------------------------------------
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
    #####
    
    #####
    result$est<-mu1_dr-mu0_dr
  }
  
  else if(estimate=='ATT'){
    ind_mu1<-which(data[,var_treat]==1)
    result$mu1_dr<-sum(data[,var_y]*data[,var_treat])/length(ind_mu1)
    result$est<-sum(data[,var_y]*data[,var_treat]-(((data[,var_y]*(1-data[,var_treat])*data$ps)+(mu0_X)*(data[,var_treat]-data$ps))/(1-data$ps)))/length(ind_mu1) 
  }
  
  return(result)
}




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
    
    nu1_3<-matrix(((data[,var_treat][i]*(1/fitted_ps[i]))-1)*X_1[i,],nrow=1)         
    nu1_4<-matrix((-1)*as.vector((data[,var_treat][i]*(data[,var_y][i]- outcome_model_coef%*%X_1[i,]))/exp_beta_X[i])*X_P[i,],nrow=1)
    nu1<-cbind(1,0,nu1_3,nu1_4)
    colnames(nu1)<-NULL
    
    nu2_3<-matrix(((-(1+exp_beta_X[i]))*data[,var_treat][i]+exp_beta_X[i])*X_0[i,],nrow=1)
    nu2_4<- as.vector((-1)*exp_beta_X[i]*((1-data[,var_treat][i])*(data[,var_y][i]-(outcome_model_coef%*%X_0[i,]))))*X_P[i,]
    nu2<-cbind(0,1,matrix(nu2_3,nrow=1),matrix(nu2_4,nrow=1))
    colnames(nu2)<-NULL
    
    nu3_3<--X_out[i,]%*%t(X_out[i,])
    nu3<-cbind(0,0,nu3_3,
               matrix(0,ncol=length(ps_model_coef),nrow=dim(t(X_out)%*%X_out)[1]))
    colnames(nu3)<-NULL
    
    nu4_4<- -((fitted_ps[i])*(1-fitted_ps[i]))*(X_P[i,]%*%t(X_P[i,]))
    nu4<-cbind(0,0,matrix(0,ncol=length(outcome_model_coef),nrow=dim(nu4_4)[1]),
               nu4_4)
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
  
  dim1<-length(theta)
  
  #X_P<-as.matrix(cbind(1,data[,cov]))
  #X_out<-as.matrix(cbind(1,data[,var_treat],data[,cov]))
  X_0<-as.matrix(cbind(1,0,data[,cov]))
  X_1<-as.matrix(cbind(1,1,data[,cov]))
  
  #exp_beta_X<-exp(ps_model_coef%*%t(X_P))
  #fitted_ps<-exp_beta_X/(1+exp_beta_X)
  
  DR_ATE<-DR_estimator("ATE",data,"E","Y",cov)
  fit<-DR_ATE$fit
  
  nu_theta_array<-array(0,dim=c(dim1,dim1,nrow(data)))
  
  for(i in 1:nrow(data)){
    
    est_1_true<-nu1-((data[,var_treat][i]*data[,var_y][i]-(data[,var_treat][i]-fit$fitted.values[i])*(outcome_model_coef %*% X_1[i,]))/fit$fitted.values[i])
    est_2_true<-nu0-(((1-data[,var_treat][i])*data[,var_y][i]+(data[,var_treat][i]-fit$fitted.values[i])*(outcome_model_coef%*%X_0[i,]))/(1-fit$fitted.values[i]))
    est_3_true<-model.matrix(DR_ATE$out_model)[i,]*as.vector(data[,var_y][i] - DR_ATE$out_model$residuals[i])
    est_4_true<-(data[,var_treat][i]-fit$fitted.values[i])*model.matrix(fit)[i,]
    
    estimating_equation_true <- matrix(rbind(est_1_true,est_2_true,matrix(est_3_true,ncol=1),matrix(est_4_true,ncol=1)),ncol=1)
    nu_theta_array[,,i]<-estimating_equation_true%*%t(estimating_equation_true)
    
  }
  
  nu_theta_mean<-apply(nu_theta_array,c(1,2),mean) 
  
  return(nu_theta_mean)
  
}



### Data generation --------------------------------------------------
### - with # of obs = 100000 -----------------------------------------
N1<-100000
effect<-c(log(1.5),log(2),log(2))


### 1th replication data generating -----------------------------------------
delta0<--2
delta_b<-0.01
delta_c<-0.01
set.seed(123)
sample<-replicate(N1,expr={
  B<-rbinom(1,1,prob=0.5)
  C<-rnorm(1,0,1)
  p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
  E<-rbinom(1,1,prob=p_z)
  c(B,C,E)
})

X_sample<-cbind(sample[1,],sample[2,],sample[3,])

# Y random sampling - continuous
Y_sample<-as.matrix(X_sample)%*%effect+rnorm(nrow(X_sample),0,1)


### -------------------------------------------------------------------------
E<-X_sample[,3]
B<-X_sample[,1]
C<-X_sample[,2]
Y<-Y_sample[,1]
data_L<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)







### Data import --------------------------------------------------
### - # of obs = 100 -----------------------------------------
E_100<-read.csv("E_100.csv")
B_100<-read.csv("B_100.csv")
C_100<-read.csv("C_100.csv")
Y_100<-read.csv("Y_100.csv")

REPL<-100

### Getting 1000th estimator ------------------------------------------------
est_DR_100<-rep(NA,length=REPL)

est_100_CV<-matrix(NA,nrow=REPL,ncol=3)
colnames(est_100_CV)<-c("CL","CU","contain")

cov<-c("B","C")
cov_type<-c("binary","continuous")

var_treat<-"E"
var_y<-"Y"

for(rep in 1:REPL){
  
  E<-E_100[,rep]
  B<-B_100[,rep]
  C<-C_100[,rep]
  Y<-Y_100[,rep]
  data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
  
  DR_ATE<-DR_estimator("ATE",data,var_treat,var_y,cov)
  theta_hat<-matrix(c(DR_ATE$mu1_dr, DR_ATE$mu1_dr-DR_ATE$est, coef(DR_ATE$out_model), coef(DR_ATE$fit)), nrow=1)
  
  j_theta_hat<-J_function(theta_hat,data_L,var_treat,var_y,cov)
  crossprod<-crossprod_est_equ(theta_hat,data_L,var_treat,var_y,cov)
  
  cov_theta_star<-(solve(j_theta_hat)%*%crossprod%*%t(solve(j_theta_hat)))
  D_theta_hat<-sqrt(cov_theta_star[1,1]+cov_theta_star[2,2]-2*cov_theta_star[1,2])
  
  est_DR_100[rep]<-DR_ATE$est
  est_100_CV[rep,1]<-est_DR_100[rep]-1.96*(D_theta_hat/sqrt(nrow(data)))
  est_100_CV[rep,2]<-est_DR_100[rep]+1.96*(D_theta_hat/sqrt(nrow(data)))
  est_100_CV[rep,3]<-ifelse((est_100_CV[rep,1]<=log(2) & est_100_CV[rep,2]>=log(2)), 1, 0)
  
}

D_theta_hat_coverage_100<-mean(est_100_CV[,3])





### Data import --------------------------------------------------
### - # of obs = 1000 -----------------------------------------
E_1000<-read.csv("E_1000.csv")
B_1000<-read.csv("B_1000.csv")
C_1000<-read.csv("C_1000.csv")
Y_1000<-read.csv("Y_1000.csv")



### Getting 1000th estimator ------------------------------------------------
est_DR_1000<-rep(NA,length=REPL)

est_1000_CV<-matrix(NA,nrow=REPL,ncol=3)
colnames(est_1000_CV)<-c("CL","CU","contain")

cov<-c("B","C")
cov_type<-c("binary","continuous")

var_treat<-"E"
var_y<-"Y"

for(rep in 1:REPL){
  
  E<-E_1000[,rep]
  B<-B_1000[,rep]
  C<-C_1000[,rep]
  Y<-Y_1000[,rep]
  data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
  
  DR_ATE<-DR_estimator("ATE",data,var_treat,var_y,cov)
  theta_hat<-matrix(c(DR_ATE$mu1_dr, DR_ATE$mu1_dr-DR_ATE$est, coef(DR_ATE$out_model), coef(DR_ATE$fit)), nrow=1)
  
  j_theta_hat<-J_function(theta_hat,data_L,var_treat,var_y,cov)
  crossprod<-crossprod_est_equ(theta_hat,data_L,var_treat,var_y,cov)
  
  cov_theta_star<-(solve(j_theta_hat)%*%crossprod%*%t(solve(j_theta_hat)))
  D_theta_hat<-sqrt(cov_theta_star[1,1]+cov_theta_star[2,2]-2*cov_theta_star[1,2])
  
  est_DR_1000[rep]<-DR_ATE$est
  est_1000_CV[rep,1]<-est_DR_1000[rep]-1.96*(D_theta_hat/sqrt(nrow(data)))
  est_1000_CV[rep,2]<-est_DR_1000[rep]+1.96*(D_theta_hat/sqrt(nrow(data)))
  est_1000_CV[rep,3]<-ifelse((est_1000_CV[rep,1]<=log(2) & est_1000_CV[rep,2]>=log(2)), 1, 0)
  
}

D_theta_hat_coverage_1000<-mean(est_1000_CV[,3])